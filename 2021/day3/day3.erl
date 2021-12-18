%%%-------------------------------------------------------------------
%%% @author richardvancamp
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 03. Dec 2021 16.32
%%%-------------------------------------------------------------------
-module(day3).
-author("richardvancamp").

%% API
-export([do_day/0]).


%%% PART 1 %%%

% https://stackoverflow.com/questions/47446788/what-does-the-mean-in-erlang/47446892
% if Ch is 1 then 49 - 48 counts 1
count_ones(<<Ch:8, RBin/binary>>, [C | ROnes]) ->
  [C + Ch - $0 | count_ones(RBin, ROnes)];
count_ones(<<>>, []) ->
  [].

do_part1(Lines) ->
  LinesCount = length(Lines),
  % [0, 0, 0...]
  Accum = lists:duplicate(byte_size(hd(Lines)), 0),
  Ones = lists:foldl(fun count_ones/2, Accum, Lines),
  MostCommon = lists:foldl(fun(X, Acc) -> if X >= LinesCount div 2 -> Acc ++ "1"; true -> Acc ++ "0" end end, "", Ones),
  % I guess the Epsilon could be done with some bitwise magic, ie bnot plus mask, but no idea how to do thatr
  LeastCommon = lists:foldl(fun(X, Acc) ->
    if X >= LinesCount div 2 -> Acc ++ "0"; true -> Acc ++ "1" end end, "", Ones),
  Gamma = list_to_integer(MostCommon, 2),
  Epsilon = list_to_integer(LeastCommon, 2),
  io:format("**Part 1**~n~w~n", [Gamma * Epsilon]).

%%% PART 2 %%%

reduce([Last], _Pos, _Fun) ->
  Last;
reduce(List, Pos, Fun) ->
  ListAtPos = lists:map(fun(Bin) -> binary:at(Bin, Pos) end, List),
  OnesAtPos = lists:foldl(fun(Ch, Acc) -> if Ch == $1 -> Acc + 1; true -> Acc end end, 0, ListAtPos),
  Keeper = Fun(OnesAtPos, List),
  Updated = lists:filter(fun(Bin) -> binary:at(Bin, Pos) == Keeper end, List),
  reduce(Updated, Pos + 1, Fun).

to_int(Bin) ->
  list_to_integer(binary:bin_to_list(Bin), 2).

do_part2(Lines) ->
  Oxygen = to_int(reduce(Lines, 0, fun(OnesAtPos, List) ->
    if OnesAtPos >= (length(List) - OnesAtPos) -> $1; true -> $0 end end)),
  C02 = to_int(reduce(Lines, 0, fun(OnesAtPos, List) ->
    if OnesAtPos < (length(List) - OnesAtPos) -> $1; true -> $0 end end)),
  io:format("**Part 2**~n~w~n", [Oxygen * C02]).

do_day() ->
  {ok, Data} = file:read_file("input.txt"),
  List = binary:split(Data, <<"\n">>, [global]),
  Lines = lists:filter(fun(X) -> X /= <<>> end, List),
  do_part1(Lines),
  do_part2(Lines).


