%%%-------------------------------------------------------------------
%%% @author richardvancamp
%%% @copyright (C) 2021, Monad
%%% @doc
%%%
%%% @end
%%% Created : 01. Dec 2021 21.44
%%%-------------------------------------------------------------------
-module(day1).
-author("richardvancamp").

%% API
-export([do_day/0, inc_depth/1, inc_3_window/1]).

% this could also be replaced with one generic window function...but this it for now
inc_depth([X, Y], Increases) when Y > X -> Increases + 1;
inc_depth([_X, _Y], Increases) -> Increases;
inc_depth([X, Y | Tail], Increases) when Y > X -> inc_depth([Y | Tail], Increases + 1);
inc_depth([_X, Y | Tail], Increases) -> inc_depth([Y | Tail], Increases).
inc_depth(DepthsList) -> inc_depth(DepthsList, 0).

inc_3_window([H1, H2, H3, H4 | Tail], Increases) when H1 + H2 + H3 < H2 + H3 + H4 -> inc_3_window([H2, H3, H4 | Tail], Increases + 1);
inc_3_window([_ | Tail], Increases) -> inc_3_window(Tail, Increases);
inc_3_window([], Increases) -> Increases.
inc_3_window(DepthsList) -> inc_3_window(DepthsList, 0).

do_day() ->
  {ok,File} = file:open("input.txt",[read]),
  DepthsList = read_integers(File),
  io:format("~w~n", [inc_depth(DepthsList)]),
  io:format("~w~n", [inc_3_window(DepthsList)]).


