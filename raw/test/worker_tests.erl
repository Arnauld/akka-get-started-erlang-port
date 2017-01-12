%%%-------------------------------------------------------------------
%%% @author Arnauld
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 11. janv. 2017 22:21
%%%-------------------------------------------------------------------
-module(worker_tests).
-author("Arnauld").

-include_lib("eunit/include/eunit.hrl").

simple_test() ->
  closeTo(4.0, worker:calculatePiFor(0, 1), 0.001),
  closeTo(-1.333, worker:calculatePiFor(1, 1), 0.001).

approx_test() ->
  Pi = worker:calculatePiFor(0, 10000),
  ?assert(3.14 < Pi),
  ?assert(3.15 > Pi).

process_test() ->
  Pid = worker:start(),
  NrOfElements = 10000,
  Start = 0,
  From = self(),
  Pid ! {work, From, Start, NrOfElements},
  receive
    {result, Approx} ->
      ?assert(3.14 < Approx),
      ?assert(3.15 > Approx)
  after 6000 ->
    fail()
  end.

fail() ->
  erlang:error({assertException,
    [{module, ?MODULE},
      {line, ?LINE}]}).

closeTo(Expected, Actual, Eps) ->
  ?assert(abs(Expected - Actual) < Eps).
