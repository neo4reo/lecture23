# Egg Eater

![An Egg Eater](https://upload.wikimedia.org/wikipedia/commons/9/97/Dasypeltis_atra.jpg)

[Link to your own repo]()

This assignment implements a _tail call optimization_ for the Diamondback compiler.

## Language

The Egg Eater language has the same semantics as Diamondback, with the addition that
tail calls are guaranteed to be optimized.

Hence, the syntax is unchanged from the last assignment.

## Tail Call Optimization

In this assignment we are interested in optimizing _tail calls_.


### Overview 

So far, each function call requires allocating some stack space to store, for example,
the return address and the function's arguments. Consider the following:

```
def foo(a, x):
  if x < 0:
    a
  else:
    foo(a + 1, x - 1) 
foo(0, 4)
```

To refresh your memory, remember that when the recursive call to `foo`
returns, the _caller_ (the outer call to `foo`) immediately returns to
the original caller. We can therefore optimize the call by _reusing_
the original stack, ultimately returning to the original caller.

### The optimization

Your job in this assignment is to implement the tail call
optimization. Recall that the algorithm for compiling each function
call has been:

```
CompileFunctionCall(f,xs)
1. Store return location on the stack
2. Store old ESP on the stack
3. Store arguments (xs) on the stack
4. Jump to f
```

The new algorithm, to be used when a call is detected in tail position, is:

```
CompileFunctionCallTCO(f,xs):
1. If f(xs) is in tail position:
   i. Store arguments (xs) on the stack (reusing the current stack frame)
   ii. Jump to f
2. Else:
   CompileFunctionCall(f,xs)
```

(See the lecture notes [here](https://github.com/ucsd-cse131-sp17/lectures/blob/master/lecture17/tail-position.pdf) and [here](https://github.com/ucsd-cse131-sp17/lectures/blob/master/lecture18/tail-details.pdf) for more details about tail calls)

### Implementation

The starter code contains our old friend `compile_expr` (and related
helpers) which has been given a new argument, `istail`, which is a
`bool`. The idea is that, given a call `compile_expr e istail ...`,
`istail` is `true` when `e` is in _tail position_ and `false`
otherwise. 

The function `compile_app` uses `istail` to determine if it should
compile a function application as normal (via the helper `compile_app_normal`)
or using the tail call optimization (via the helper `compile_app_tailcall`).

#### TODO

1. The value of `istail` has been given as `_FIXME_BOGUS_TC_FLAG` in the
   starter code. Replace `_FIXME_BOGUS_TC_FLAG` at each _use_ with the correct value.
   That is, if the compiler contains the code
   
   ```
   ...
   compile_expr e _FIXME_BOGUS_TC_FLAG
   ...
   ```
   
   then you should replace `_FIXME_BOGUS_TC_FLAG` with an appropriate value depending
   on whether the expression being compiled is in tail position or not.

2. Implement the helper function `compile_app_tailcall`, which
   implements our tail call optimization.

### Testing

We have provided you with tests. As always, please write more tests to increase your confidence that your implementation is correct. However, we will *not* be evaluating your test suite on buggy compilers as we have done in past assignments.

### Getting Started

A note on support code – a lot is provided, but, as always, you can
feel free to overwrite it with your own implementation if you prefer.

