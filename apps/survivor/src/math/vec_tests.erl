-module(vec_tests).
-author("nt").

-include_lib("eunit/include/eunit.hrl").


create_test() ->
  {1.0, 2.0} = vec:vec(1.0, 2.0).

get_x_test() ->
  1.0 = vec:x(vec:vec(1.0, 2.0)).

get_y_test() ->
  2.0 = vec:y(vec:vec(1.0, 2.0)).

get_magnitude_test() ->
  10.0 = vec:magnitude(vec:vec(6.0, 8.0)).

get_unit_test() ->
  {0.6, 0.8} = vec:unit(vec:vec(6.0, 8.0)).

add_test() ->
  {5.0, 10.0} = vec:add(vec:vec(1.0, 2.0), vec:vec(4.0, 8.0)).

subtract_test() ->
  {-3.0, -6.0} = vec:subtract(vec:vec(1.0, 2.0), vec:vec(4.0, 8.0)).

scale_test() ->
  {3.0, 4.0} = vec:scale(vec:vec(6.0, 8.0), 0.5).

scalar_multiply_test() ->
  {12.0, 24.0} = vec:scalar_multiply(vec:vec(6.0, 8.0), vec:vec(2.0, 3.0)).
