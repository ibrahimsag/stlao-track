{-# LANGUAGE OverloadedStrings #-}
module Main where 

import qualified System.Environment as Env

import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State
import Control.Applicative ((<|>))

import Data.Word (Word8)

import Data.Bits (Bits)
import qualified Data.Bits as Bits

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C

import Data.Array.Unboxed (UArray, (!))
import qualified Data.Array.IArray as A

import Data.Maybe (catMaybes)
import Data.List (transpose)

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

-- decodeInt :: (Bits a, Num a) => ByteString -> a
-- decodeInt = 
--   BS.foldl' (\n h -> Bits.shiftL n 8 Bits..|. fromIntegral h) 0

-- bsOfSize :: Int -> Parser ByteString
-- bsOfSize = 
--   state . BS.splitAt

-- intOfSize :: (Bits a, Num a) => Int -> Parser a
-- intOfSize = 
--   fmap decodeInt . bsOfSize

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
parseVariance = Variance <$> (parseChar 'N' *> parseInt) <*> parseArithmetic

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

arrayFromList :: A.IArray arr e => [e] -> arr Int e
arrayFromList l = A.listArray (0, length l - 1) l

zipArraysWith :: (Double -> Double -> Double) -> UArray Int Double -> UArray Int Double -> UArray Int Double
zipArraysWith f arr1 arr2 =
   A.listArray
     (min (A.bounds arr1) (A.bounds arr2))
     (zipWith f (A.elems arr1) (A.elems arr2))

type Signal = UArray Int Double

meanAt :: Int -> Int -> Signal -> Double
meanAt i r arr = s / (fromIntegral r)
  where
    s = sum [ arr ! j | j <- A.range (i, i + r - 1) ]

slidingMean :: Int -> Signal -> Signal
slidingMean r arr = arrayFromList meanList 
   where
     meanList =
       [ meanAt i r arr
       | i <- A.range (0, (snd (A.bounds arr) + 1) - r)
       ]

slidingVariance :: Int -> Signal -> Signal
slidingVariance r arr = arrayFromList varianceList
  where
    varianceList =
      [ sum [ ((arr ! j) - (meanAt i r arr)) ** 2
            | j <- A.range (i, i + r - 1)
            ] / (fromIntegral r)
      | i <- A.range (0, (snd (A.bounds arr) + 1) - r)
      ]

slidingMin  :: Int -> Signal -> Signal
slidingMin r arr = arrayFromList minList
  where
    minList =
      [ minimum
          [ arr ! j
          | j <- A.range (i, i + r - 1)
          ]
      | i <- A.range (0, (snd (A.bounds arr) + 1) - r)
      ]

slidingMax  :: Int -> Signal -> Signal
slidingMax r arr = arrayFromList minList
  where
    minList =
      [ maximum
          [ arr ! j
          | j <- A.range (i, i + r - 1)
          ]
      | i <- A.range (0, (snd (A.bounds arr) + 1) - r)
      ]

slidingUntil :: Int -> Signal -> Signal -> Signal
slidingUntil r arr1 arr2 = arrayFromList untilList
  where
    untilList =
      [ maximum
          [ min
              (arr2 ! k)
              (minimum
                [ arr1 ! j
                | j <- A.range (i, k - 1)
                ]
              )
          | k <- A.range (i, i + r - 1)
          ]
      | i <- A.range (0, (snd (A.bounds arr2) + 1) - r)
      ]

type Trace = [Signal]

evalArithmetic :: Trace -> Int -> Arithmetic -> Signal
evalArithmetic trace evalIndex formula =
  case formula of
    SignalReference ref  -> trace !! ref
    Addition f1 f2       -> zipArraysWith (+) (evalArithmetic trace evalIndex f1) (evalArithmetic trace evalIndex f2)
    Subtraction f1 f2    -> zipArraysWith (-) (evalArithmetic trace evalIndex f1) (evalArithmetic trace evalIndex f2)
    Multiplication f1 f2 -> zipArraysWith (*) (evalArithmetic trace evalIndex f1) (evalArithmetic trace evalIndex f2)
    Mean r f             -> slidingMean r (evalArithmetic trace evalIndex f)
    Variance r f         -> slidingVariance r (evalArithmetic trace evalIndex f)

evalFormula :: Trace -> Int -> Formula -> Signal
evalFormula trace i formula =
  case formula of
    LessThan v f      -> A.amap (\e -> fromIntegral v - e) (evalArithmetic trace i f)
    GreaterThan v f   -> A.amap (\e -> e - fromIntegral v) (evalArithmetic trace i f)
    Conjunction f1 f2 -> zipArraysWith (max)  (evalFormula trace i f1) (evalFormula trace i f2)
    Disjunction f1 f2 -> zipArraysWith (min)  (evalFormula trace i f1) (evalFormula trace i f2)
    Negation f        -> A.amap (\e -> 0 - e) (evalFormula trace i f)
    Until r f1 f2     -> slidingUntil r (evalFormula trace i f1) (evalFormula trace i f2)
    Eventually r f    -> slidingMax r (evalFormula trace i f)
    Globally r f      -> slidingMin r (evalFormula trace i f)


readSamplesFromCommaSeparatedByteString :: ByteString -> [[Double]]
readSamplesFromCommaSeparatedByteString = transpose . fmap readSamplesOnLine . C.lines
  where
    readSamplesOnLine = fmap (fromIntegral . fst) . catMaybes . fmap C.readInt . (C.split ',')

main = do
  [formulaFile, traceFile, resultFile] <- Env.getArgs
  putStrLn ("Formula file: " ++ formulaFile)
  putStrLn ("Trace file: " ++ traceFile)
  putStrLn ("Result file: " ++ resultFile)

  contentsOfFormulaFile <- BS.readFile formulaFile
  -- C.putStrLn contentsOfFormulaFile

  let Just (formula, rest) = runStateT parseFormula contentsOfFormulaFile
  putStrLn (show formula)

  contentsOfTraceFile <- BS.readFile traceFile
  let samplesOfTraceFile = readSamplesFromCommaSeparatedByteString contentsOfTraceFile
      traceLength = (length . head) samplesOfTraceFile
      trace :: Trace
      trace = fmap (A.listArray (0, traceLength - 1)) samplesOfTraceFile
  putStrLn (show (samplesOfTraceFile))
  putStrLn (show trace)
  putStrLn ((show . A.elems) (evalFormula trace 0 formula))
