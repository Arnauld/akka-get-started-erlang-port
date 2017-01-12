raw
=====

Raw process-based implementation of the [akka get-started guide](http://doc.akka.io/docs/akka/2.0/intro/getting-started-first-scala.html) to pi calculation.

Build
-----

    $ rebar3 compile
    
Play with
---------

    $ rebar3 do clean, compile, eunit, shell
    
    ===> Cleaning out raw...
    ===> Verifying dependencies...
    ===> Compiling raw
    ===> Verifying dependencies...
    ===> Compiling raw
    ===> Performing EUnit tests...
    ...
    
    Top 3 slowest tests (0.002 seconds, 2.4% of total time):
      worker_tests:approx_test/0
        0.001 seconds
      worker_tests:process_test/0
        0.001 seconds
      worker_tests:simple_test/0
        0.000 seconds
    
    Finished in 0.085 seconds
    3 tests, 0 failures
    ===> Verifying dependencies...
    ===> Compiling raw
    Erlang/OTP 19 [erts-8.1] [source] [64-bit] [smp:4:4] [async-threads:0] [hipe] [kernel-poll:false]
    
    Eshell V8.1  (abort with ^G)
    1> pi:calculate().
    calculate
    Pi approximation: 3.1415926435897883
    Calculation time: 5105
    2> pi:calculate(10000, 10000, 10000).
    calculate
    Pi approximation: 3.141592643589787
    Calculation time: 4779
