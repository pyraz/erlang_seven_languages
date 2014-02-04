-module(word).
-export([count_spaces/1]).
-export([count_words/1]).

count_words(String) when length(String) > 0->
  1 + count_spaces(string:strip(String));
count_words(_) ->
  0.

count_spaces([First | Rest]) ->
  count_spaces(First) + count_spaces(Rest);
count_spaces(32) ->
  1;
count_spaces(_) ->
  0.
