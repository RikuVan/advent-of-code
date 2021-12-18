%%%-------------------------------------------------------------------
%%% @author richardvancamp
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 02. Dec 2021 22.16
%%%-------------------------------------------------------------------
-module(utils).
-author("richardvancamp").

%% API
-export([read_integers/1, read_integers/2]).

read_integers(File) ->
  read_integers(File, []).

read_integers(Chars, Acc) ->
  case io:fread(Chars, [], "~d") of
    eof ->
      lists:reverse(Acc);
    {ok, [Num]} ->
      read_integers(Chars, [Num | Acc]);
    {error, What} ->
      io:format("io:fread error: ~w~n", [What]),
      read_integers(Chars, Acc)
  end.
