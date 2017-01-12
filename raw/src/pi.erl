-module(pi).
-author("Arnauld").

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([calculate/0, calculate/3, listener/0]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------


calculate() ->
  calculate(4, 10000, 10000).

calculate(NrOfWorkers, NrOfElements, NrOfMessages) ->
  Listener = spawn(?MODULE, listener, []),
  Master = master:start(NrOfWorkers, NrOfMessages, NrOfElements, Listener),
  Master ! calculate.


listener() ->
  receive
    {pi_approximation, Pi, Duration} ->
      io:format("Pi approximation: ~p ~nCalculation time: ~p~n", [Pi, Duration])
  end.