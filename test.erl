-module(test).
-export([test/1]).
-export([test2/1]).

test([First|Rest]) ->
    io:format("~p~n", [Rest]),
    test(First) + test(Rest);
%test([]) ->
%    1.
test(X) ->
  io:format("~p~n", [X]).

test2(X) ->
  [First | Rest] = X,
  test2(First) + test2(Rest);
test2([]) ->
  io:format("~p~n", ["Empty Array"]);
test2(_) ->
  io:format("~p~n", [_]).
