%%%-------------------------------------------------------------------
%%% @author richardvancamp
%%% @copyright (C) 2021, Monad
%%% @doc
%%%
%%% @end
%%% Created : 06. Dec 2021 15.41
%%%-------------------------------------------------------------------
-module(day6).
-author("richardvancamp").

%% API
-export([do_day/0]).

run(Fish, 0) ->
  lists:sum(maps:values(Fish));
run(Fish, Days) ->
  run(age(Fish), Days - 1).

age(Fish) ->
  Spawning = maps:get(0, Fish, 0),
  Decremented = lists:foldl(fun decrement/2, Fish, lists:seq(1, 8)),
  Decremented#{8 => Spawning,
    6 => maps:get(6, Decremented, 0) + Spawning}.

decrement(Age, Map) ->
  Map#{Age - 1 => maps:get(Age, Map, 0)}.

do_day() ->
  {ok, Data} = file:read_file("input.txt"),
  Fish = [list_to_integer(binary_to_list(V))|| V<-binary:split(Data, <<",">>, [global])],
  FishMap = lists:foldl(fun(Age, Map) -> maps:update_with(Age, fun(N) -> N + 1 end, 1, Map) end, #{}, Fish),
  TotalFish = run(FishMap, 80),
  TotalFish2 = run(FishMap, 256),
  io:format("Part 1: ~p~nPart 2: ~p~n", [TotalFish, TotalFish2]).