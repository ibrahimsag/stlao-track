{-# LANGUAGE OverloadedStrings, BangPatterns #-}
module Main where 

import qualified System.Environment as Env
import System.IO (IOMode(..), withFile)
import Text.Printf (hPrintf)
import Text.Read (readMaybe)

import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State
import Control.Applicative ((<|>))
import Data.Foldable (traverse_)

import Data.Word (Word8)

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C

import Data.Array.Unboxed (UArray, (!))
import qualified Data.Array.IArray as A

import Data.Maybe (catMaybes)
import Data.List (transpose, tails, foldl')

import Dequeue (Dequeue(..), BankersDequeue)

data Formula
  = Globally Int Formula
  | Eventually Int Formula
  | Until Int Formula Formula
  | Negation Formula
  | Disjunction Formula Formula
  | Conjunction Formula Formula
  | GreaterThan Int Arithmetic
  | LessThan Int Arithmetic
  deriving Show

data Arithmetic
  = SignalReference Int
  | Addition Arithmetic Arithmetic
  | Subtraction Arithmetic Arithmetic
  | Multiplication Arithmetic Arithmetic
  | Mean Int Arithmetic
  | Variance Int Arithmetic
  deriving Show

type Parser a = 
  StateT C.ByteString Maybe a

run :: Parser a -> C.ByteString -> Maybe a
run = 
  evalStateT

parseInt' :: Parser Int
parseInt' =  StateT C.readInt

parseChar' :: Char -> Parser ()
parseChar' c = StateT readChar
  where
    readChar = (matchChar =<<) . C.uncons
    matchChar (h, r) = if h == c then Just ((), r) else Nothing 

ignoreSpace :: Parser ()
ignoreSpace = parseChar' ' ' <|> return ()

parseInt :: Parser Int
parseInt = ignoreSpace *> parseInt'

parseChar :: Char -> Parser ()
parseChar = (ignoreSpace *>) . parseChar'

parseSignalReference :: Parser Arithmetic
parseSignalReference = SignalReference <$> (parseChar 'x' *> parseInt)

parseAddition :: Parser Arithmetic
parseAddition = Addition <$> (parseChar '+' *> parseArithmetic) <*> parseArithmetic

parseSubtraction :: Parser Arithmetic
parseSubtraction = Subtraction <$> (parseChar '-' *> parseArithmetic) <*> parseArithmetic

parseMultiplication :: Parser Arithmetic
parseMultiplication = Multiplication <$> (parseChar '*' *> parseArithmetic) <*> parseArithmetic

parseMean :: Parser Arithmetic
parseMean = Mean <$> (parseChar 'M' *> parseInt) <*> parseArithmetic

parseVariance :: Parser Arithmetic
parseVariance = Variance <$> (parseChar 'V' *> parseInt) <*> parseArithmetic

parseArithmetic :: Parser Arithmetic
parseArithmetic
  =   parseSignalReference
  <|> parseAddition
  <|> parseSubtraction
  <|> parseMultiplication
  <|> parseMean
  <|> parseVariance

parseGlobally :: Parser Formula
parseGlobally = Globally <$> (parseChar 'G' *> parseInt) <*> parseFormula

parseEventually :: Parser Formula
parseEventually = Eventually <$> (parseChar 'F' *> parseInt) <*> parseFormula

parseUntil :: Parser Formula
parseUntil = Until <$> (parseChar 'U' *> parseInt) <*> parseFormula <*> parseFormula

parseNegation :: Parser Formula
parseNegation = Negation <$> (parseChar '!' *> parseFormula)

parseDisjunction :: Parser Formula
parseDisjunction = Disjunction <$> (parseChar '|' *> parseFormula) <*> parseFormula

parseConjunction :: Parser Formula
parseConjunction = Conjunction <$> (parseChar '&' *> parseFormula) <*> parseFormula

parseGreaterThan :: Parser Formula
parseGreaterThan = GreaterThan <$> (parseChar '>' *> parseInt) <*> parseArithmetic

parseLessThan :: Parser Formula
parseLessThan = LessThan <$> (parseChar '<' *> parseInt) <*> parseArithmetic

parseFormula :: Parser Formula
parseFormula
  =   parseGlobally
  <|> parseEventually
  <|> parseUntil
  <|> parseNegation
  <|> parseDisjunction
  <|> parseConjunction
  <|> parseGreaterThan
  <|> parseLessThan

-- END PARSER

type Signal = [Double]

gfst :: (a, b, c) -> a
gfst (!a, _, _) = a

clearLastElementsFromQueue :: (Double -> Double -> Bool) -> BankersDequeue (Int, Double) -> Double -> BankersDequeue (Int, Double)
clearLastElementsFromQueue pred queue e =
  case popBack queue of
    Nothing -> queue
    Just ((!lastInd, !lastVal), !rest) ->
      if pred lastVal e
      then clearLastElementsFromQueue pred rest e
      else queue

streamingMin :: Int -> Signal -> Signal
streamingMin window es = stream es
  where
    stream = drop window . (map gfst) . (scanl iter (0, 0, empty))
    iter :: (Double, Int, BankersDequeue (Int, Double)) -> Double -> (Double, Int, BankersDequeue (Int, Double))
    iter (_, !ind, q) !e =
      let newQ = pushBack (clearLastElementsFromQueue (>) q e) (ind, e)
      in case popFront newQ of
        Nothing                                -> (0, ind + 1, newQ)
        Just (!(!firstInd, !firstVal), shiftedQ)  ->
          let nextQ = if firstInd + window == ind + 1 then shiftedQ else newQ
          in (firstVal, ind + 1, nextQ)

streamingMax :: Int -> Signal -> Signal
streamingMax window es = stream es
  where
    stream = (drop window) . (map gfst) . (scanl iter (0, 0, empty))
    iter :: (Double, Int, BankersDequeue (Int, Double)) -> Double -> (Double, Int, BankersDequeue (Int, Double))
    iter (_, !ind, !q) !e =
      let newQ = e `seq` pushBack (clearLastElementsFromQueue (<) q e) (ind, e)
      in case popFront newQ of
        Nothing                                -> (0, ind + 1, newQ)
        Just (!(!firstInd, !firstVal), shiftedQ)  ->
          let nextQ = if firstInd + window == ind + 1 then shiftedQ else newQ
          in (firstVal, ind + 1, nextQ)

slidingUntil :: Int -> Signal -> Signal -> Signal
slidingUntil n as bs = stream
  where
    stream =
      fmap
        maximum
        (zipWith
           (zipWith min)
           (ev bs)
           (gl as)
        )
    ev :: [Double] -> [[Double]]
    ev = takeWhile ((==n) . length) . fmap (take n) . tails
    gl :: [Double] -> [[Double]]
    gl = fmap (scanl min 99999999) . ev

streamingMean :: Int -> Signal -> Signal
streamingMean window es = stream es
  where
    w = fromIntegral window
    stream = (drop window) . (map gfst) . (scanl iter (0, 0, empty))
    iterVal :: Double -> Double -> Double -> Double
    iterVal lastMean newItem firstItem = (lastMean * w + newItem - firstItem) / w
    iter :: (Double, Int, BankersDequeue (Int, Double)) -> Double -> (Double, Int, BankersDequeue (Int, Double))
    iter (lastVal, ind, q) newItem =
      case popFront q of
        Nothing                                ->
          -- just starting, the queue has no items yet.
          ( iterVal lastVal newItem 0
          , ind + 1
          , pushBack q (ind, newItem)
          )
        Just ((firstInd, firstItem), shiftedQ)  ->
          if firstInd + window == ind + 1
          -- reached window size, so we pop from front of queue
          then ( iterVal lastVal newItem firstItem
               , ind + 1
               , pushBack shiftedQ (ind, newItem)
               )
          -- not reached window size, so we dont pop
          else ( iterVal lastVal newItem 0
               , ind + 1
               , pushBack q (ind, newItem)
               )

streamingVariance :: Int -> Signal -> Signal
streamingVariance window es = stream es
  where
    w = fromIntegral window
    stream = (drop window) . (map (gfst . gfst)) . (scanl iter ((0, 0, 0), 0, empty))
    iterVal :: (Double, Double, Double) -> Double -> Double -> (Double, Double, Double)
    iterVal (_lastVariance, sumOfSquares, sums) newItem firstItem =
      let sumOfSquares' = sumOfSquares + (newItem * newItem) - (firstItem * firstItem)
          sums' = sums + newItem - firstItem
      in ( (sumOfSquares' * w - (sums' * sums')) / (w * (w - 1))
         , sumOfSquares'
         , sums'
         )

    iter :: ((Double, Double, Double), Int, BankersDequeue (Int, Double)) -> Double -> ((Double, Double, Double), Int, BankersDequeue (Int, Double))
    iter (lastVal, ind, q) newItem =
      case popFront q of
        Nothing                                ->
          -- just starting, the queue has no items yet.
          ( iterVal lastVal newItem 0
          , ind + 1
          , pushBack q (ind, newItem)
          )
        Just ((firstInd, firstItem), shiftedQ)  ->
          if firstInd + window == ind + 1
          -- reached window size, so we pop from front of queue
          then ( iterVal lastVal newItem firstItem
               , ind + 1
               , pushBack shiftedQ (ind, newItem)
               )
          -- not reached window size, so we dont pop
          else ( iterVal lastVal newItem 0
               , ind + 1
               , pushBack q (ind, newItem)
               )

type Trace = [[Double]]

evalArithmetic :: Trace -> Arithmetic -> Signal
evalArithmetic trace formula =
  case formula of
    SignalReference ref  -> trace !! ref
    Addition f1 f2       -> zipWith (+) (evalArithmetic trace f1) (evalArithmetic trace f2)
    Subtraction f1 f2    -> zipWith (-) (evalArithmetic trace f1) (evalArithmetic trace f2)
    Multiplication f1 f2 -> zipWith (*) (evalArithmetic trace f1) (evalArithmetic trace f2)
    Mean r f             -> streamingMean r (evalArithmetic trace f)
    Variance r f         -> streamingVariance r (evalArithmetic trace f)

evalFormula :: Trace -> Formula -> Signal
evalFormula trace formula =
  case formula of
    LessThan v f      -> fmap (\e -> fromIntegral v - e) (evalArithmetic trace f)
    GreaterThan v f   -> fmap (\e -> e - fromIntegral v) (evalArithmetic trace f)
    Conjunction f1 f2 -> zipWith min (evalFormula trace f1) (evalFormula trace f2)
    Disjunction f1 f2 -> zipWith max (evalFormula trace f1) (evalFormula trace f2)
    Negation f        -> fmap (\e -> 0 - e) (evalFormula trace f)
    Until r f1 f2     -> slidingUntil (r + 1) (evalFormula trace f1) (evalFormula trace f2)
    Eventually r f    -> streamingMax (r + 1) (evalFormula trace f)
    Globally r f      -> streamingMin (r + 1) (evalFormula trace f)

readSamples :: String -> [[Double]]
readSamples = transpose . fmap readSamplesOnLine . lines
  where
    readSamplesOnLine = catMaybes . fmap readMaybe . words

entry formulaFile traceFile resultFile = do
  putStrLn ("Formula file: " ++ formulaFile)
  putStrLn ("Trace file: " ++ traceFile)
  putStrLn ("Result file: " ++ resultFile)

  contentsOfFormulaFile <- BS.readFile formulaFile

  let Just (formula, rest) = runStateT parseFormula contentsOfFormulaFile
  putStrLn (show formula)

  contentsOfTraceFile <- readFile traceFile
  let trace = readSamples contentsOfTraceFile
  
  let result = (evalFormula trace formula)
  withFile resultFile WriteMode (\h -> traverse_ (hPrintf h "%.11f\n") result)


main = do
  [formulaFile, traceFile, resultFile] <- Env.getArgs
  entry formulaFile traceFile resultFile
