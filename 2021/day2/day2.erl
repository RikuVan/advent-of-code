%%%-------------------------------------------------------------------
%%% @author richardvancamp
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 02. Dec 2021 22.13
%%%-------------------------------------------------------------------
-module(day2).
-author("richardvancamp").

%% API
-export([do_day/0]).

% https://www.erlang.org/doc/programming_examples/bit_syntax.html
move([<<"up ", N/binary>>| R], Depth, Horizontal) ->
  move(R, Depth - binary_to_integer(N), Horizontal);
move([<<"down ", N/binary>> | R], Depth, Horizontal) ->
  move(R, Depth + binary_to_integer(N), Horizontal);
move([<<"forward ", N/binary>> | R], Depth, Horizontal) ->
  move(R, Depth, Horizontal + binary_to_integer(N));
move([_ | R], Depth, Horizontal) ->
  move(R, Depth, Horizontal);
move([], Depth, Horizontal) ->
  {Depth, Horizontal}.

move_with_aim([<<"up ", N/binary>> | R], Aim, Depth, Horizontal) ->
  move_with_aim(R, Aim - binary_to_integer(N), Depth, Horizontal);
move_with_aim([<<"down ", N/binary>> | R], Aim, Depth, Horizontal) ->
  move_with_aim(R, Aim + binary_to_integer(N), Depth, Horizontal);
move_with_aim([<<"forward ", N/binary>> | R], Aim, Depth, Horizontal) ->
  Forward = binary_to_integer(N),
  move_with_aim(R, Aim, Depth + Forward*Aim, Horizontal + Forward);
move_with_aim([_ | R], Aim, Depth, Horizontal) ->
  move_with_aim(R, Aim, Depth, Horizontal);
move_with_aim([], _Aim, Depth, Horizontal) ->
  {Depth, Horizontal}.

do_part_1(Instructions) ->
  {Depth, Horizontal} = move(Instructions, 0, 0),
  io:format("**Part 1**~n~w~n", [Depth*Horizontal]).

do_part_2(Instructions) ->
  {Depth, Horizontal} = move_with_aim(Instructions, 0, 0, 0),
  io:format("**Part 2**~n~w~n", [Depth*Horizontal]).

do_day() ->
  {ok, File} = file:read_file("input.txt"),
  Instructions = binary:split(File, <<"\n">>, [global]),
  do_part_1(Instructions),
  do_part_2(Instructions).
