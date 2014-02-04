-module(word_tests).
-include_lib("eunit/include/eunit.hrl").

count_words_test() ->
  ?assertEqual(2, word:count_words("hello world")).
