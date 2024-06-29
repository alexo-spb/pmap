# Yet Another Parallel Alternative for `lists:map/2`

This repository contains an module (pmap) that implements a parallel map
function, which allows you to apply a function to each element in a list
concurrently, using a specified number of worker processes. The pmap module is
designed to enhance performance by leveraging Erlang's powerful concurrency
model.

## Features

**Concurrent Processing**: Utilizes multiple worker processes to apply a
function to elements in a list concurrently, improving performance for large
datasets or computationally intensive operations.

**Error Handling**: Robust error handling and reporting mechanisms to ensure
that errors in any worker process are captured and relayed back to the caller.
Dynamic Worker Management: Automatically manages the number of worker processes,
ensuring that it does not exceed the specified limit (MaxWorkers).

**Graceful Termination**: Ensures that all worker processes are properly
terminated, even in the case of errors or exceptions.

## API

- **pmap:map/3**

```erlang
-spec map(Fun, List1, MaxWorkers) -> List2 when
    Fun :: fun((term()) -> term()),
    List1 :: [term()],
    MaxWorkers :: pos_integer(),
    List2 :: [term()].
```

The `map/3` function applies the given function (`Fun`) to each element in the
list (`List1`) using up to `MaxWorkers` parallel worker processes.

**Parameters**:
- `Fun`: A function of arity 1 that takes a single argument and returns a value.
- `List1`: A list of elements to which the function will be applied.
- `MaxWorkers`: A positive integer specifying the maximum number of worker
  processes to be used.

**Returns**:
- `List2`: A list containing the results of applying Fun to each element in
  List1, in the same order as the original list.

### Example Usage

```erlang
-module(test).

test() ->
    Fun = fun(X) -> X * 2 end,
    List = [1, 2, 3, 4, 5],
    MaxWorkers = 3,
    Result = pmap:map(Fun, List, MaxWorkers),
    io:format("Result: ~p~n", [Result]).
```
