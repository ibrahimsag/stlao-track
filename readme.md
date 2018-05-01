### STLAO trace checking.

As an extention of [Linear temporal logic](https://en.wikipedia.org/wiki/Linear_temporal_logic), STL acts on real-time and real-valued constraints(Signal). An introduction is with [this slides](https://people.eecs.berkeley.edu/~sseshia/fmee/lectures/EECS294-98_Spring2014_STL_Lecture.pdf).

This program checks traces of a signal sampled at uniform points for compliance to a formula of STL with Arithmetic Operations.

To run against space separated values of multiple traces to be read from `trace_file` for a formula in `formula_file` and write the results in `result_file`:

    ./trace_checker formula_file trace_file result_file

See `test_data` folder for small example data with expected results.


### Example Formulas, in prefix form

    (> 5) x0

Trace x0 should always satisfy (> 5).

    F 3 > 0 x0

Trace x0 should satisfy (> 0) eventually (F), within a window of 3. Returning the minimum in the
window.

    G 2 > 10 + x0 x1

Sum of traces x0 and x1 (+ x0 x1) should be (> 10), globally. Returning the maximum in a window of 2. 

    U 0 > 0 x0 < 10 x1

Trace x1 should be (< 10) until(U) x0 becomes (> 0). With a window of 0 this amounts to an `or` operation.
Which checks the (> 0 x0) at every point of the trace and if it is not then we need (< 10 x1) to hold.

    U 1 > 0 x0 < 10 x1

Trace x1 should satisfy (< 10) until x0 satisfies (> 0). With a window of one, we look at two samples
from every point of the signal.
