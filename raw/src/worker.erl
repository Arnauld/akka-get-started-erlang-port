-module(worker).


%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start/0, loop/0]).
-export([calculatePiFor/2]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start() ->
  spawn(?MODULE, loop, []).

loop() ->
  receive
    {work, From, Start, NrOfElements} ->
      Approx = calculatePiFor(Start, NrOfElements),
%%      io:format("Approx for (~p,~p): ~p~n", [Start, NrOfElements, Approx]),
      From ! {result, Approx},
      loop(); %% recursive call, wait for an other message to process

    stop ->
      ok
  end.

calculatePiFor(Start, NrOfElements) ->
  calculatePiFor(0, Start, Start + NrOfElements).

calculatePiFor(Acc, Max, Max) -> Acc;
calculatePiFor(Acc, I, Max) ->
  NewAcc = Acc + 4 * (1 - 2 * (I rem 2)) / (2 * I + 1),
  calculatePiFor(NewAcc, I + 1, Max).