-module(master).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start/4, loop/3]).

-record(state, {
  nrOfWorkers,
  nrOfMessages,
  nrOfElements,
  listener,
  workers = [],
  startTs
}).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------
-spec start(
    NrOfWorkers :: pos_integer(),
    NrOfMessages :: pos_integer(),
    NrOfElements :: pos_integer(),
    Listener :: pid()) -> pid().
start(NrOfWorkers, NrOfMessages, NrOfElements, Listener) when is_pid(Listener) ->
  State = #state{
    nrOfWorkers = NrOfWorkers,
    nrOfMessages = NrOfMessages,
    nrOfElements = NrOfElements,
    listener = Listener,
    startTs = currentTimeMillis()
  },
  Pi = 0,
  NrOfResults = 0,
  spawn(?MODULE, loop, [State, Pi, NrOfResults]).

loop(State, Pi, NrOfResults) ->
  receive
    calculate ->
      Workers = createWorkers(State#state.nrOfWorkers),
      broadcastWork(0, State#state.nrOfMessages, Workers, [], State#state.nrOfElements),
      loop(State#state{workers = Workers}, Pi, NrOfResults);

    {result, Value} ->
      MaxResults = State#state.nrOfMessages,
      NewPi = Pi + Value,
      NewNrOfResults = NrOfResults + 1,
      case NewNrOfResults of
        MaxResults ->
          Duration = currentTimeMillis() - State#state.startTs,
          State#state.listener ! {pi_approximation, NewPi, Duration},
          stopWorkers(State#state.workers);

        _ ->
          loop(State, NewPi, NewNrOfResults)
      end
  end.

%%
%% MISC
%%

-spec currentTimeMillis() -> integer().
currentTimeMillis() ->
  {Mega, Sec, Micro} = os:timestamp(),
  (Mega * 1000000 + Sec) * 1000 + round(Micro / 1000).

%%
%% WORKERS
%%

createWorkers(NrOfWorkers) ->
  createWorkers(NrOfWorkers, []).

createWorkers(0, Workers) ->
  Workers;
createWorkers(Remaining, Workers) ->
  createWorkers(Remaining - 1, [createWorker() | Workers]).

createWorker() ->
  worker:start().


broadcastWork(Max, Max, _WorkersUnused, _WorkersUsed, _NrOfElements) ->
  ok;
broadcastWork(I, Max, [], WorkersUsed, NrOfElements) -> % no more workers, round-robin them and continue
  broadcastWork(I, Max, lists:reverse(WorkersUsed), [], NrOfElements);
broadcastWork(I, Max, [Worker | Unused], WorkersUsed, NrOfElements) ->
  Worker ! {work, self(), I * NrOfElements, NrOfElements},
  broadcastWork(I + 1, Max, Unused, [Worker | WorkersUsed], NrOfElements).


stopWorkers([]) ->
  ok;
stopWorkers([Worker | Others]) ->
  Worker ! stop,
  stopWorkers(Others).
