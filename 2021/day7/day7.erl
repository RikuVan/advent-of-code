%%%-------------------------------------------------------------------
%%% @author richardvancamp
%%% @copyright (C) 2021, Monad
%%% @doc
%%%
%%% @end
%%% Created : 08. Dec 2021 22.04
%%%-------------------------------------------------------------------
-module(day7).
-author("richardvancamp").

%% API
-export([do_day/0]).

fold_while([], Total, _Offset, -1, _Calc) ->
  Total;
fold_while([], Total, _Offset, Smallest, _Calc) ->
  if (Total < Smallest) ->
    Total;
    true ->
      Smallest
  end;
fold_while([H|T], Total, Offset, -1, Calc) ->
  fold_while(T, Total + Calc(H, Offset), Offset, -1, Calc);
fold_while([H|T], Total, Offset, Smallest, Calc) ->
  Next = Total + Calc(H, Offset),
  if (Next < Smallest) ->
    fold_while(T, Next, Offset, Smallest, Calc);
    true ->
      Smallest
  end.

fuel_consumption(X, Y) ->
  abs(X - Y).
exp_fuel_consumption(X, Y) ->
  A = abs(X - Y),
  A * (1 + A) div 2.

part_1(Crabs) ->
  Min = lists:min(Crabs),
  Max = lists:max(Crabs),
  lists:foldl(fun(Idx, Acc) -> fold_while(Crabs, 0, Idx, Acc, fun fuel_consumption/2) end, -1, lists:seq(Min, Max)).

part_2(Crabs) ->
  Min = lists:min(Crabs),
  Max = lists:max(Crabs),
  lists:foldl(fun(Idx, Acc) -> fold_while(Crabs, 0, Idx, Acc, fun exp_fuel_consumption/2) end, -1, lists:seq(Min, Max)).


do_day() ->
  {ok, Data} = file:read_file("input.txt"),
  Positions = [binary_to_integer(V) || V<-re:split(Data, "[,\n]+"), V /= <<>>],
  Total = part_1(Positions),
  Total2 = part_2(Positions),
  io:format("Part1 ~w~n", [Total]),
  io:format("Part2 ~w~n", [Total2]).
