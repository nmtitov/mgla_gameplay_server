-module(size_tests).
-author("nt").

-include_lib("eunit/include/eunit.hrl").


create_test() ->
  {1.0, 2.0} = size:size(1.0, 2.0).

get_width_test() ->
  1.0 = size:width(size:size(1.0, 2.0)).

get_height_test() ->
  2.0 = size:height(size:size(1.0, 2.0)).
