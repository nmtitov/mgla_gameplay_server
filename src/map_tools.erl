-module(map_tools).
-author("nt").
-include("../include/block.hrl").
-export([blocks/0, map/0]).

-spec blocks() -> Blocks when Blocks :: [block()].
blocks() ->
  [
    block({{100, 700}, {100, 100}}),
    block({{250, 450}, {100, 100}}),
    block({{400, 700}, {100, 100}}),
    block({{400, 200}, {100, 100}})
  ].

-spec block(A) -> B when A :: rect:rect(), B :: block().
block({{X, Y}, {W, H}} = R) ->
  Offset = 2,
  #block{rect = R, graph_vertices = [
    {X - Offset, Y - Offset},
    {X + W + Offset, Y - Offset},
    {X + W + Offset, Y + H + Offset},
    {X - Offset, Y + H + Offset}
  ]}.

map() ->
  <<"{
  \"type\": \"map\",
  \"body\": {
    \"name\": \"forest\",
    \"size\": {
      \"width\": 600.0,
      \"height\": 1000.0
    },
    \"assets\": [
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 0.0,
          \"y\": 976.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 0.0,
          \"y\": 928.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 0.0,
          \"y\": 880.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 0.0,
          \"y\": 832.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 0.0,
          \"y\": 784.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 0.0,
          \"y\": 736.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 0.0,
          \"y\": 688.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 0.0,
          \"y\": 640.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 0.0,
          \"y\": 592.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 0.0,
          \"y\": 544.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 0.0,
          \"y\": 496.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 0.0,
          \"y\": 448.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 0.0,
          \"y\": 400.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 0.0,
          \"y\": 352.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 0.0,
          \"y\": 304.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 0.0,
          \"y\": 256.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 0.0,
          \"y\": 208.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 0.0,
          \"y\": 160.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 0.0,
          \"y\": 112.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 0.0,
          \"y\": 64.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 0.0,
          \"y\": 16.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 24.0,
          \"y\": 976.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 24.0,
          \"y\": 928.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 24.0,
          \"y\": 880.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 24.0,
          \"y\": 832.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 24.0,
          \"y\": 784.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 24.0,
          \"y\": 736.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 24.0,
          \"y\": 688.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 24.0,
          \"y\": 640.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 24.0,
          \"y\": 592.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 24.0,
          \"y\": 544.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 24.0,
          \"y\": 496.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 24.0,
          \"y\": 448.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 24.0,
          \"y\": 400.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 24.0,
          \"y\": 352.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 24.0,
          \"y\": 304.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 24.0,
          \"y\": 256.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 24.0,
          \"y\": 208.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 24.0,
          \"y\": 160.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 24.0,
          \"y\": 112.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 24.0,
          \"y\": 64.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 24.0,
          \"y\": 16.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 0.0,
          \"y\": 952.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 0.0,
          \"y\": 904.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 0.0,
          \"y\": 856.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 0.0,
          \"y\": 808.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 0.0,
          \"y\": 760.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 0.0,
          \"y\": 712.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 0.0,
          \"y\": 664.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 0.0,
          \"y\": 616.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 0.0,
          \"y\": 568.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 0.0,
          \"y\": 520.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 0.0,
          \"y\": 472.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 0.0,
          \"y\": 424.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 0.0,
          \"y\": 376.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 0.0,
          \"y\": 328.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 0.0,
          \"y\": 280.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 0.0,
          \"y\": 232.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 0.0,
          \"y\": 184.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 0.0,
          \"y\": 136.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 0.0,
          \"y\": 88.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 0.0,
          \"y\": 40.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 0.0,
          \"y\": -8.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 72.0,
          \"y\": 976.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 72.0,
          \"y\": 928.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 72.0,
          \"y\": 880.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 72.0,
          \"y\": 832.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 72.0,
          \"y\": 784.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 72.0,
          \"y\": 736.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 72.0,
          \"y\": 688.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 72.0,
          \"y\": 640.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 72.0,
          \"y\": 592.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 72.0,
          \"y\": 544.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 72.0,
          \"y\": 496.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 72.0,
          \"y\": 448.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 72.0,
          \"y\": 400.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 72.0,
          \"y\": 352.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 72.0,
          \"y\": 304.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 72.0,
          \"y\": 256.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 72.0,
          \"y\": 208.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 72.0,
          \"y\": 160.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 72.0,
          \"y\": 112.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 72.0,
          \"y\": 64.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 72.0,
          \"y\": 16.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 120.0,
          \"y\": 976.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 120.0,
          \"y\": 928.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 120.0,
          \"y\": 880.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 120.0,
          \"y\": 832.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 120.0,
          \"y\": 784.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 120.0,
          \"y\": 736.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 120.0,
          \"y\": 688.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 120.0,
          \"y\": 640.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 120.0,
          \"y\": 592.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 120.0,
          \"y\": 544.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 120.0,
          \"y\": 496.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 120.0,
          \"y\": 448.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 120.0,
          \"y\": 400.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 120.0,
          \"y\": 352.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 120.0,
          \"y\": 304.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 120.0,
          \"y\": 256.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 120.0,
          \"y\": 208.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 120.0,
          \"y\": 160.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 120.0,
          \"y\": 112.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 120.0,
          \"y\": 64.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 120.0,
          \"y\": 16.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 168.0,
          \"y\": 976.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 168.0,
          \"y\": 928.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 168.0,
          \"y\": 880.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 168.0,
          \"y\": 832.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 168.0,
          \"y\": 784.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 168.0,
          \"y\": 736.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 168.0,
          \"y\": 688.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 168.0,
          \"y\": 640.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 168.0,
          \"y\": 592.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 168.0,
          \"y\": 544.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 168.0,
          \"y\": 496.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 168.0,
          \"y\": 448.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 168.0,
          \"y\": 400.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 168.0,
          \"y\": 352.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 168.0,
          \"y\": 304.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 168.0,
          \"y\": 256.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 168.0,
          \"y\": 208.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 168.0,
          \"y\": 160.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 168.0,
          \"y\": 112.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 168.0,
          \"y\": 64.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 168.0,
          \"y\": 16.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 216.0,
          \"y\": 976.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 216.0,
          \"y\": 928.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 216.0,
          \"y\": 880.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 216.0,
          \"y\": 832.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 216.0,
          \"y\": 784.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 216.0,
          \"y\": 736.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 216.0,
          \"y\": 688.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 216.0,
          \"y\": 640.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 216.0,
          \"y\": 592.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 216.0,
          \"y\": 544.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 216.0,
          \"y\": 496.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 216.0,
          \"y\": 448.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 216.0,
          \"y\": 400.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 216.0,
          \"y\": 352.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 216.0,
          \"y\": 304.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 216.0,
          \"y\": 256.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 216.0,
          \"y\": 208.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 216.0,
          \"y\": 160.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 216.0,
          \"y\": 112.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 216.0,
          \"y\": 64.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 216.0,
          \"y\": 16.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 264.0,
          \"y\": 976.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 264.0,
          \"y\": 928.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 264.0,
          \"y\": 880.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 264.0,
          \"y\": 832.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 264.0,
          \"y\": 784.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 264.0,
          \"y\": 736.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 264.0,
          \"y\": 688.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 264.0,
          \"y\": 640.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 264.0,
          \"y\": 592.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 264.0,
          \"y\": 544.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 264.0,
          \"y\": 496.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 264.0,
          \"y\": 448.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 264.0,
          \"y\": 400.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 264.0,
          \"y\": 352.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 264.0,
          \"y\": 304.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 264.0,
          \"y\": 256.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 264.0,
          \"y\": 208.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 264.0,
          \"y\": 160.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 264.0,
          \"y\": 112.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 264.0,
          \"y\": 64.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 264.0,
          \"y\": 16.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 312.0,
          \"y\": 976.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 312.0,
          \"y\": 928.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 312.0,
          \"y\": 880.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 312.0,
          \"y\": 832.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 312.0,
          \"y\": 784.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 312.0,
          \"y\": 736.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 312.0,
          \"y\": 688.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 312.0,
          \"y\": 640.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 312.0,
          \"y\": 592.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 312.0,
          \"y\": 544.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 312.0,
          \"y\": 496.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 312.0,
          \"y\": 448.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 312.0,
          \"y\": 400.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 312.0,
          \"y\": 352.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 312.0,
          \"y\": 304.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 312.0,
          \"y\": 256.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 312.0,
          \"y\": 208.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 312.0,
          \"y\": 160.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 312.0,
          \"y\": 112.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 312.0,
          \"y\": 64.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 312.0,
          \"y\": 16.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 360.0,
          \"y\": 976.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 360.0,
          \"y\": 928.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 360.0,
          \"y\": 880.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 360.0,
          \"y\": 832.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 360.0,
          \"y\": 784.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 360.0,
          \"y\": 736.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 360.0,
          \"y\": 688.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 360.0,
          \"y\": 640.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 360.0,
          \"y\": 592.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 360.0,
          \"y\": 544.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 360.0,
          \"y\": 496.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 360.0,
          \"y\": 448.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 360.0,
          \"y\": 400.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 360.0,
          \"y\": 352.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 360.0,
          \"y\": 304.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 360.0,
          \"y\": 256.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 360.0,
          \"y\": 208.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 360.0,
          \"y\": 160.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 360.0,
          \"y\": 112.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 360.0,
          \"y\": 64.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 360.0,
          \"y\": 16.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 408.0,
          \"y\": 976.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 408.0,
          \"y\": 928.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 408.0,
          \"y\": 880.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 408.0,
          \"y\": 832.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 408.0,
          \"y\": 784.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 408.0,
          \"y\": 736.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 408.0,
          \"y\": 688.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 408.0,
          \"y\": 640.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 408.0,
          \"y\": 592.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 408.0,
          \"y\": 544.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 408.0,
          \"y\": 496.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 408.0,
          \"y\": 448.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 408.0,
          \"y\": 400.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 408.0,
          \"y\": 352.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 408.0,
          \"y\": 304.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 408.0,
          \"y\": 256.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 408.0,
          \"y\": 208.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 408.0,
          \"y\": 160.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 408.0,
          \"y\": 112.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 408.0,
          \"y\": 64.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 408.0,
          \"y\": 16.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 456.0,
          \"y\": 976.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 456.0,
          \"y\": 928.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 456.0,
          \"y\": 880.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 456.0,
          \"y\": 832.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 456.0,
          \"y\": 784.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 456.0,
          \"y\": 736.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 456.0,
          \"y\": 688.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 456.0,
          \"y\": 640.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 456.0,
          \"y\": 592.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 456.0,
          \"y\": 544.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 456.0,
          \"y\": 496.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 456.0,
          \"y\": 448.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 456.0,
          \"y\": 400.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 456.0,
          \"y\": 352.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 456.0,
          \"y\": 304.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 456.0,
          \"y\": 256.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 456.0,
          \"y\": 208.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 456.0,
          \"y\": 160.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 456.0,
          \"y\": 112.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 456.0,
          \"y\": 64.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 456.0,
          \"y\": 16.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 504.0,
          \"y\": 976.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 504.0,
          \"y\": 928.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 504.0,
          \"y\": 880.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 504.0,
          \"y\": 832.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 504.0,
          \"y\": 784.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 504.0,
          \"y\": 736.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 504.0,
          \"y\": 688.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 504.0,
          \"y\": 640.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 504.0,
          \"y\": 592.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 504.0,
          \"y\": 544.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 504.0,
          \"y\": 496.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 504.0,
          \"y\": 448.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 504.0,
          \"y\": 400.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 504.0,
          \"y\": 352.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 504.0,
          \"y\": 304.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 504.0,
          \"y\": 256.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 504.0,
          \"y\": 208.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 504.0,
          \"y\": 160.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 504.0,
          \"y\": 112.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 504.0,
          \"y\": 64.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 504.0,
          \"y\": 16.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 552.0,
          \"y\": 976.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 552.0,
          \"y\": 928.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 552.0,
          \"y\": 880.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 552.0,
          \"y\": 832.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 552.0,
          \"y\": 784.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 552.0,
          \"y\": 736.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 552.0,
          \"y\": 688.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 552.0,
          \"y\": 640.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 552.0,
          \"y\": 592.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 552.0,
          \"y\": 544.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 552.0,
          \"y\": 496.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 552.0,
          \"y\": 448.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 552.0,
          \"y\": 400.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 552.0,
          \"y\": 352.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 552.0,
          \"y\": 304.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 552.0,
          \"y\": 256.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 552.0,
          \"y\": 208.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 552.0,
          \"y\": 160.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 552.0,
          \"y\": 112.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 552.0,
          \"y\": 64.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 552.0,
          \"y\": 16.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 48.0,
          \"y\": 976.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 48.0,
          \"y\": 928.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 48.0,
          \"y\": 880.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 48.0,
          \"y\": 832.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 48.0,
          \"y\": 784.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 48.0,
          \"y\": 736.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 48.0,
          \"y\": 688.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 48.0,
          \"y\": 640.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 48.0,
          \"y\": 592.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 48.0,
          \"y\": 544.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 48.0,
          \"y\": 496.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 48.0,
          \"y\": 448.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 48.0,
          \"y\": 400.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 48.0,
          \"y\": 352.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 48.0,
          \"y\": 304.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 48.0,
          \"y\": 256.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 48.0,
          \"y\": 208.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 48.0,
          \"y\": 160.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 48.0,
          \"y\": 112.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 48.0,
          \"y\": 64.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 48.0,
          \"y\": 16.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 96.0,
          \"y\": 976.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 96.0,
          \"y\": 928.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 96.0,
          \"y\": 880.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 96.0,
          \"y\": 832.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 96.0,
          \"y\": 784.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 96.0,
          \"y\": 736.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 96.0,
          \"y\": 688.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 96.0,
          \"y\": 640.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 96.0,
          \"y\": 592.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 96.0,
          \"y\": 544.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 96.0,
          \"y\": 496.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 96.0,
          \"y\": 448.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 96.0,
          \"y\": 400.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 96.0,
          \"y\": 352.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 96.0,
          \"y\": 304.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 96.0,
          \"y\": 256.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 96.0,
          \"y\": 208.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 96.0,
          \"y\": 160.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 96.0,
          \"y\": 112.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 96.0,
          \"y\": 64.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 96.0,
          \"y\": 16.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 144.0,
          \"y\": 976.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 144.0,
          \"y\": 928.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 144.0,
          \"y\": 880.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 144.0,
          \"y\": 832.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 144.0,
          \"y\": 784.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 144.0,
          \"y\": 736.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 144.0,
          \"y\": 688.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 144.0,
          \"y\": 640.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 144.0,
          \"y\": 592.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 144.0,
          \"y\": 544.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 144.0,
          \"y\": 496.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 144.0,
          \"y\": 448.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 144.0,
          \"y\": 400.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 144.0,
          \"y\": 352.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 144.0,
          \"y\": 304.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 144.0,
          \"y\": 256.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 144.0,
          \"y\": 208.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 144.0,
          \"y\": 160.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 144.0,
          \"y\": 112.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 144.0,
          \"y\": 64.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 144.0,
          \"y\": 16.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 192.0,
          \"y\": 976.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 192.0,
          \"y\": 928.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 192.0,
          \"y\": 880.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 192.0,
          \"y\": 832.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 192.0,
          \"y\": 784.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 192.0,
          \"y\": 736.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 192.0,
          \"y\": 688.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 192.0,
          \"y\": 640.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 192.0,
          \"y\": 592.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 192.0,
          \"y\": 544.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 192.0,
          \"y\": 496.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 192.0,
          \"y\": 448.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 192.0,
          \"y\": 400.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 192.0,
          \"y\": 352.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 192.0,
          \"y\": 304.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 192.0,
          \"y\": 256.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 192.0,
          \"y\": 208.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 192.0,
          \"y\": 160.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 192.0,
          \"y\": 112.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 192.0,
          \"y\": 64.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 192.0,
          \"y\": 16.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 240.0,
          \"y\": 976.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 240.0,
          \"y\": 928.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 240.0,
          \"y\": 880.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 240.0,
          \"y\": 832.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 240.0,
          \"y\": 784.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 240.0,
          \"y\": 736.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 240.0,
          \"y\": 688.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 240.0,
          \"y\": 640.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 240.0,
          \"y\": 592.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 240.0,
          \"y\": 544.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 240.0,
          \"y\": 496.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 240.0,
          \"y\": 448.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 240.0,
          \"y\": 400.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 240.0,
          \"y\": 352.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 240.0,
          \"y\": 304.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 240.0,
          \"y\": 256.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 240.0,
          \"y\": 208.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 240.0,
          \"y\": 160.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 240.0,
          \"y\": 112.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 240.0,
          \"y\": 64.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 240.0,
          \"y\": 16.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 288.0,
          \"y\": 976.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 288.0,
          \"y\": 928.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 288.0,
          \"y\": 880.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 288.0,
          \"y\": 832.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 288.0,
          \"y\": 784.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 288.0,
          \"y\": 736.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 288.0,
          \"y\": 688.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 288.0,
          \"y\": 640.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 288.0,
          \"y\": 592.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 288.0,
          \"y\": 544.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 288.0,
          \"y\": 496.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 288.0,
          \"y\": 448.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 288.0,
          \"y\": 400.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 288.0,
          \"y\": 352.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 288.0,
          \"y\": 304.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 288.0,
          \"y\": 256.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 288.0,
          \"y\": 208.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 288.0,
          \"y\": 160.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 288.0,
          \"y\": 112.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 288.0,
          \"y\": 64.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 288.0,
          \"y\": 16.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 336.0,
          \"y\": 976.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 336.0,
          \"y\": 928.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 336.0,
          \"y\": 880.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 336.0,
          \"y\": 832.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 336.0,
          \"y\": 784.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 336.0,
          \"y\": 736.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 336.0,
          \"y\": 688.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 336.0,
          \"y\": 640.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 336.0,
          \"y\": 592.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 336.0,
          \"y\": 544.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 336.0,
          \"y\": 496.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 336.0,
          \"y\": 448.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 336.0,
          \"y\": 400.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 336.0,
          \"y\": 352.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 336.0,
          \"y\": 304.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 336.0,
          \"y\": 256.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 336.0,
          \"y\": 208.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 336.0,
          \"y\": 160.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 336.0,
          \"y\": 112.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 336.0,
          \"y\": 64.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 336.0,
          \"y\": 16.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 384.0,
          \"y\": 976.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 384.0,
          \"y\": 928.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 384.0,
          \"y\": 880.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 384.0,
          \"y\": 832.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 384.0,
          \"y\": 784.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 384.0,
          \"y\": 736.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 384.0,
          \"y\": 688.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 384.0,
          \"y\": 640.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 384.0,
          \"y\": 592.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 384.0,
          \"y\": 544.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 384.0,
          \"y\": 496.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 384.0,
          \"y\": 448.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 384.0,
          \"y\": 400.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 384.0,
          \"y\": 352.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 384.0,
          \"y\": 304.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 384.0,
          \"y\": 256.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 384.0,
          \"y\": 208.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 384.0,
          \"y\": 160.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 384.0,
          \"y\": 112.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 384.0,
          \"y\": 64.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 384.0,
          \"y\": 16.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 432.0,
          \"y\": 976.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 432.0,
          \"y\": 928.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 432.0,
          \"y\": 880.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 432.0,
          \"y\": 832.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 432.0,
          \"y\": 784.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 432.0,
          \"y\": 736.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 432.0,
          \"y\": 688.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 432.0,
          \"y\": 640.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 432.0,
          \"y\": 592.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 432.0,
          \"y\": 544.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 432.0,
          \"y\": 496.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 432.0,
          \"y\": 448.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 432.0,
          \"y\": 400.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 432.0,
          \"y\": 352.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 432.0,
          \"y\": 304.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 432.0,
          \"y\": 256.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 432.0,
          \"y\": 208.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 432.0,
          \"y\": 160.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 432.0,
          \"y\": 112.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 432.0,
          \"y\": 64.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 432.0,
          \"y\": 16.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 480.0,
          \"y\": 976.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 480.0,
          \"y\": 928.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 480.0,
          \"y\": 880.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 480.0,
          \"y\": 832.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 480.0,
          \"y\": 784.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 480.0,
          \"y\": 736.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 480.0,
          \"y\": 688.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 480.0,
          \"y\": 640.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 480.0,
          \"y\": 592.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 480.0,
          \"y\": 544.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 480.0,
          \"y\": 496.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 480.0,
          \"y\": 448.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 480.0,
          \"y\": 400.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 480.0,
          \"y\": 352.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 480.0,
          \"y\": 304.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 480.0,
          \"y\": 256.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 480.0,
          \"y\": 208.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 480.0,
          \"y\": 160.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 480.0,
          \"y\": 112.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 480.0,
          \"y\": 64.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 480.0,
          \"y\": 16.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 528.0,
          \"y\": 976.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 528.0,
          \"y\": 928.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 528.0,
          \"y\": 880.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 528.0,
          \"y\": 832.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 528.0,
          \"y\": 784.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 528.0,
          \"y\": 736.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 528.0,
          \"y\": 688.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 528.0,
          \"y\": 640.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 528.0,
          \"y\": 592.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 528.0,
          \"y\": 544.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 528.0,
          \"y\": 496.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 528.0,
          \"y\": 448.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 528.0,
          \"y\": 400.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 528.0,
          \"y\": 352.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 528.0,
          \"y\": 304.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 528.0,
          \"y\": 256.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 528.0,
          \"y\": 208.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 528.0,
          \"y\": 160.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 528.0,
          \"y\": 112.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 528.0,
          \"y\": 64.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 528.0,
          \"y\": 16.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 576.0,
          \"y\": 976.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 576.0,
          \"y\": 928.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 576.0,
          \"y\": 880.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 576.0,
          \"y\": 832.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 576.0,
          \"y\": 784.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 576.0,
          \"y\": 736.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 576.0,
          \"y\": 688.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 576.0,
          \"y\": 640.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 576.0,
          \"y\": 592.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 576.0,
          \"y\": 544.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 576.0,
          \"y\": 496.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 576.0,
          \"y\": 448.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 576.0,
          \"y\": 400.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 576.0,
          \"y\": 352.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 576.0,
          \"y\": 304.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 576.0,
          \"y\": 256.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 576.0,
          \"y\": 208.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 576.0,
          \"y\": 160.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 576.0,
          \"y\": 112.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 576.0,
          \"y\": 64.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 576.0,
          \"y\": 16.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 24.0,
          \"y\": 952.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 24.0,
          \"y\": 904.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 24.0,
          \"y\": 856.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 24.0,
          \"y\": 808.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 24.0,
          \"y\": 760.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 24.0,
          \"y\": 712.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 24.0,
          \"y\": 664.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 24.0,
          \"y\": 616.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 24.0,
          \"y\": 568.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 24.0,
          \"y\": 520.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 24.0,
          \"y\": 472.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 24.0,
          \"y\": 424.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 24.0,
          \"y\": 376.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 24.0,
          \"y\": 328.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 24.0,
          \"y\": 280.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 24.0,
          \"y\": 232.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 24.0,
          \"y\": 184.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 24.0,
          \"y\": 136.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 24.0,
          \"y\": 88.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 24.0,
          \"y\": 40.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 24.0,
          \"y\": -8.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 48.0,
          \"y\": 952.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 48.0,
          \"y\": 904.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 48.0,
          \"y\": 856.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 48.0,
          \"y\": 808.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 48.0,
          \"y\": 760.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 48.0,
          \"y\": 712.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 48.0,
          \"y\": 664.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 48.0,
          \"y\": 616.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 48.0,
          \"y\": 568.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 48.0,
          \"y\": 520.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 48.0,
          \"y\": 472.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 48.0,
          \"y\": 424.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 48.0,
          \"y\": 376.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 48.0,
          \"y\": 328.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 48.0,
          \"y\": 280.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 48.0,
          \"y\": 232.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 48.0,
          \"y\": 184.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 48.0,
          \"y\": 136.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 48.0,
          \"y\": 88.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 48.0,
          \"y\": 40.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 48.0,
          \"y\": -8.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 96.0,
          \"y\": 952.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 96.0,
          \"y\": 904.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 96.0,
          \"y\": 856.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 96.0,
          \"y\": 808.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 96.0,
          \"y\": 760.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 96.0,
          \"y\": 712.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 96.0,
          \"y\": 664.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 96.0,
          \"y\": 616.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 96.0,
          \"y\": 568.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 96.0,
          \"y\": 520.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 96.0,
          \"y\": 472.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 96.0,
          \"y\": 424.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 96.0,
          \"y\": 376.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 96.0,
          \"y\": 328.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 96.0,
          \"y\": 280.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 96.0,
          \"y\": 232.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 96.0,
          \"y\": 184.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 96.0,
          \"y\": 136.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 96.0,
          \"y\": 88.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 96.0,
          \"y\": 40.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 96.0,
          \"y\": -8.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 144.0,
          \"y\": 952.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 144.0,
          \"y\": 904.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 144.0,
          \"y\": 856.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 144.0,
          \"y\": 808.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 144.0,
          \"y\": 760.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 144.0,
          \"y\": 712.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 144.0,
          \"y\": 664.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 144.0,
          \"y\": 616.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 144.0,
          \"y\": 568.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 144.0,
          \"y\": 520.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 144.0,
          \"y\": 472.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 144.0,
          \"y\": 424.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 144.0,
          \"y\": 376.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 144.0,
          \"y\": 328.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 144.0,
          \"y\": 280.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 144.0,
          \"y\": 232.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 144.0,
          \"y\": 184.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 144.0,
          \"y\": 136.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 144.0,
          \"y\": 88.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 144.0,
          \"y\": 40.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 144.0,
          \"y\": -8.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 192.0,
          \"y\": 952.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 192.0,
          \"y\": 904.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 192.0,
          \"y\": 856.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 192.0,
          \"y\": 808.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 192.0,
          \"y\": 760.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 192.0,
          \"y\": 712.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 192.0,
          \"y\": 664.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 192.0,
          \"y\": 616.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 192.0,
          \"y\": 568.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 192.0,
          \"y\": 520.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 192.0,
          \"y\": 472.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 192.0,
          \"y\": 424.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 192.0,
          \"y\": 376.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 192.0,
          \"y\": 328.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 192.0,
          \"y\": 280.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 192.0,
          \"y\": 232.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 192.0,
          \"y\": 184.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 192.0,
          \"y\": 136.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 192.0,
          \"y\": 88.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 192.0,
          \"y\": 40.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 192.0,
          \"y\": -8.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 240.0,
          \"y\": 952.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 240.0,
          \"y\": 904.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 240.0,
          \"y\": 856.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 240.0,
          \"y\": 808.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 240.0,
          \"y\": 760.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 240.0,
          \"y\": 712.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 240.0,
          \"y\": 664.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 240.0,
          \"y\": 616.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 240.0,
          \"y\": 568.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 240.0,
          \"y\": 520.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 240.0,
          \"y\": 472.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 240.0,
          \"y\": 424.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 240.0,
          \"y\": 376.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 240.0,
          \"y\": 328.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 240.0,
          \"y\": 280.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 240.0,
          \"y\": 232.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 240.0,
          \"y\": 184.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 240.0,
          \"y\": 136.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 240.0,
          \"y\": 88.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 240.0,
          \"y\": 40.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 240.0,
          \"y\": -8.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 288.0,
          \"y\": 952.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 288.0,
          \"y\": 904.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 288.0,
          \"y\": 856.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 288.0,
          \"y\": 808.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 288.0,
          \"y\": 760.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 288.0,
          \"y\": 712.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 288.0,
          \"y\": 664.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 288.0,
          \"y\": 616.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 288.0,
          \"y\": 568.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 288.0,
          \"y\": 520.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 288.0,
          \"y\": 472.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 288.0,
          \"y\": 424.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 288.0,
          \"y\": 376.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 288.0,
          \"y\": 328.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 288.0,
          \"y\": 280.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 288.0,
          \"y\": 232.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 288.0,
          \"y\": 184.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 288.0,
          \"y\": 136.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 288.0,
          \"y\": 88.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 288.0,
          \"y\": 40.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 288.0,
          \"y\": -8.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 336.0,
          \"y\": 952.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 336.0,
          \"y\": 904.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 336.0,
          \"y\": 856.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 336.0,
          \"y\": 808.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 336.0,
          \"y\": 760.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 336.0,
          \"y\": 712.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 336.0,
          \"y\": 664.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 336.0,
          \"y\": 616.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 336.0,
          \"y\": 568.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 336.0,
          \"y\": 520.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 336.0,
          \"y\": 472.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 336.0,
          \"y\": 424.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 336.0,
          \"y\": 376.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 336.0,
          \"y\": 328.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 336.0,
          \"y\": 280.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 336.0,
          \"y\": 232.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 336.0,
          \"y\": 184.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 336.0,
          \"y\": 136.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 336.0,
          \"y\": 88.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 336.0,
          \"y\": 40.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 336.0,
          \"y\": -8.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 384.0,
          \"y\": 952.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 384.0,
          \"y\": 904.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 384.0,
          \"y\": 856.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 384.0,
          \"y\": 808.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 384.0,
          \"y\": 760.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 384.0,
          \"y\": 712.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 384.0,
          \"y\": 664.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 384.0,
          \"y\": 616.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 384.0,
          \"y\": 568.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 384.0,
          \"y\": 520.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 384.0,
          \"y\": 472.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 384.0,
          \"y\": 424.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 384.0,
          \"y\": 376.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 384.0,
          \"y\": 328.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 384.0,
          \"y\": 280.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 384.0,
          \"y\": 232.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 384.0,
          \"y\": 184.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 384.0,
          \"y\": 136.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 384.0,
          \"y\": 88.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 384.0,
          \"y\": 40.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 384.0,
          \"y\": -8.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 432.0,
          \"y\": 952.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 432.0,
          \"y\": 904.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 432.0,
          \"y\": 856.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 432.0,
          \"y\": 808.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 432.0,
          \"y\": 760.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 432.0,
          \"y\": 712.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 432.0,
          \"y\": 664.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 432.0,
          \"y\": 616.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 432.0,
          \"y\": 568.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 432.0,
          \"y\": 520.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 432.0,
          \"y\": 472.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 432.0,
          \"y\": 424.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 432.0,
          \"y\": 376.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 432.0,
          \"y\": 328.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 432.0,
          \"y\": 280.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 432.0,
          \"y\": 232.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 432.0,
          \"y\": 184.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 432.0,
          \"y\": 136.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 432.0,
          \"y\": 88.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 432.0,
          \"y\": 40.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 432.0,
          \"y\": -8.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 480.0,
          \"y\": 952.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 480.0,
          \"y\": 904.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 480.0,
          \"y\": 856.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 480.0,
          \"y\": 808.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 480.0,
          \"y\": 760.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 480.0,
          \"y\": 712.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 480.0,
          \"y\": 664.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 480.0,
          \"y\": 616.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 480.0,
          \"y\": 568.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 480.0,
          \"y\": 520.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 480.0,
          \"y\": 472.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 480.0,
          \"y\": 424.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 480.0,
          \"y\": 376.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 480.0,
          \"y\": 328.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 480.0,
          \"y\": 280.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 480.0,
          \"y\": 232.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 480.0,
          \"y\": 184.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 480.0,
          \"y\": 136.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 480.0,
          \"y\": 88.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 480.0,
          \"y\": 40.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 480.0,
          \"y\": -8.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 528.0,
          \"y\": 952.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 528.0,
          \"y\": 904.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 528.0,
          \"y\": 856.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 528.0,
          \"y\": 808.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 528.0,
          \"y\": 760.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 528.0,
          \"y\": 712.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 528.0,
          \"y\": 664.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 528.0,
          \"y\": 616.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 528.0,
          \"y\": 568.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 528.0,
          \"y\": 520.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 528.0,
          \"y\": 472.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 528.0,
          \"y\": 424.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 528.0,
          \"y\": 376.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 528.0,
          \"y\": 328.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 528.0,
          \"y\": 280.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 528.0,
          \"y\": 232.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 528.0,
          \"y\": 184.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 528.0,
          \"y\": 136.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 528.0,
          \"y\": 88.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 528.0,
          \"y\": 40.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 528.0,
          \"y\": -8.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 576.0,
          \"y\": 952.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 576.0,
          \"y\": 904.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 576.0,
          \"y\": 856.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 576.0,
          \"y\": 808.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 576.0,
          \"y\": 760.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 576.0,
          \"y\": 712.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 576.0,
          \"y\": 664.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 576.0,
          \"y\": 616.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 576.0,
          \"y\": 568.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 576.0,
          \"y\": 520.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 576.0,
          \"y\": 472.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 576.0,
          \"y\": 424.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 576.0,
          \"y\": 376.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 576.0,
          \"y\": 328.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 576.0,
          \"y\": 280.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 576.0,
          \"y\": 232.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 576.0,
          \"y\": 184.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 576.0,
          \"y\": 136.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 576.0,
          \"y\": 88.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 576.0,
          \"y\": 40.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass2\",
        \"position\": {
          \"x\": 576.0,
          \"y\": -8.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 1
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 72.0,
          \"y\": 952.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 72.0,
          \"y\": 904.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 72.0,
          \"y\": 856.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 72.0,
          \"y\": 808.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 72.0,
          \"y\": 760.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 72.0,
          \"y\": 712.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 72.0,
          \"y\": 664.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 72.0,
          \"y\": 616.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 72.0,
          \"y\": 568.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 72.0,
          \"y\": 520.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 72.0,
          \"y\": 472.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 72.0,
          \"y\": 424.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 72.0,
          \"y\": 376.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 72.0,
          \"y\": 328.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 72.0,
          \"y\": 280.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 72.0,
          \"y\": 232.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 72.0,
          \"y\": 184.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 72.0,
          \"y\": 136.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 72.0,
          \"y\": 88.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 72.0,
          \"y\": 40.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 72.0,
          \"y\": -8.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 120.0,
          \"y\": 952.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 120.0,
          \"y\": 904.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 120.0,
          \"y\": 856.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 120.0,
          \"y\": 808.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 120.0,
          \"y\": 760.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 120.0,
          \"y\": 712.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 120.0,
          \"y\": 664.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 120.0,
          \"y\": 616.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 120.0,
          \"y\": 568.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 120.0,
          \"y\": 520.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 120.0,
          \"y\": 472.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 120.0,
          \"y\": 424.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 120.0,
          \"y\": 376.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 120.0,
          \"y\": 328.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 120.0,
          \"y\": 280.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 120.0,
          \"y\": 232.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 120.0,
          \"y\": 184.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 120.0,
          \"y\": 136.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 120.0,
          \"y\": 88.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 120.0,
          \"y\": 40.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 120.0,
          \"y\": -8.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 168.0,
          \"y\": 952.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 168.0,
          \"y\": 904.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 168.0,
          \"y\": 856.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 168.0,
          \"y\": 808.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 168.0,
          \"y\": 760.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 168.0,
          \"y\": 712.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 168.0,
          \"y\": 664.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 168.0,
          \"y\": 616.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 168.0,
          \"y\": 568.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 168.0,
          \"y\": 520.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 168.0,
          \"y\": 472.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 168.0,
          \"y\": 424.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 168.0,
          \"y\": 376.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 168.0,
          \"y\": 328.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 168.0,
          \"y\": 280.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 168.0,
          \"y\": 232.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 168.0,
          \"y\": 184.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 168.0,
          \"y\": 136.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 168.0,
          \"y\": 88.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 168.0,
          \"y\": 40.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 168.0,
          \"y\": -8.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 216.0,
          \"y\": 952.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 216.0,
          \"y\": 904.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 216.0,
          \"y\": 856.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 216.0,
          \"y\": 808.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 216.0,
          \"y\": 760.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 216.0,
          \"y\": 712.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 216.0,
          \"y\": 664.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 216.0,
          \"y\": 616.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 216.0,
          \"y\": 568.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 216.0,
          \"y\": 520.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 216.0,
          \"y\": 472.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 216.0,
          \"y\": 424.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 216.0,
          \"y\": 376.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 216.0,
          \"y\": 328.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 216.0,
          \"y\": 280.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 216.0,
          \"y\": 232.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 216.0,
          \"y\": 184.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 216.0,
          \"y\": 136.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 216.0,
          \"y\": 88.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 216.0,
          \"y\": 40.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 216.0,
          \"y\": -8.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 264.0,
          \"y\": 952.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 264.0,
          \"y\": 904.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 264.0,
          \"y\": 856.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 264.0,
          \"y\": 808.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 264.0,
          \"y\": 760.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 264.0,
          \"y\": 712.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 264.0,
          \"y\": 664.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 264.0,
          \"y\": 616.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 264.0,
          \"y\": 568.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 264.0,
          \"y\": 520.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 264.0,
          \"y\": 472.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 264.0,
          \"y\": 424.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 264.0,
          \"y\": 376.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 264.0,
          \"y\": 328.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 264.0,
          \"y\": 280.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 264.0,
          \"y\": 232.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 264.0,
          \"y\": 184.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 264.0,
          \"y\": 136.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 264.0,
          \"y\": 88.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 264.0,
          \"y\": 40.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 264.0,
          \"y\": -8.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 312.0,
          \"y\": 952.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 312.0,
          \"y\": 904.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 312.0,
          \"y\": 856.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 312.0,
          \"y\": 808.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 312.0,
          \"y\": 760.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 312.0,
          \"y\": 712.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 312.0,
          \"y\": 664.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 312.0,
          \"y\": 616.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 312.0,
          \"y\": 568.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 312.0,
          \"y\": 520.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 312.0,
          \"y\": 472.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 312.0,
          \"y\": 424.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 312.0,
          \"y\": 376.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 312.0,
          \"y\": 328.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 312.0,
          \"y\": 280.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 312.0,
          \"y\": 232.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 312.0,
          \"y\": 184.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 312.0,
          \"y\": 136.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 312.0,
          \"y\": 88.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 312.0,
          \"y\": 40.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 312.0,
          \"y\": -8.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 360.0,
          \"y\": 952.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 360.0,
          \"y\": 904.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 360.0,
          \"y\": 856.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 360.0,
          \"y\": 808.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 360.0,
          \"y\": 760.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 360.0,
          \"y\": 712.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 360.0,
          \"y\": 664.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 360.0,
          \"y\": 616.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 360.0,
          \"y\": 568.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 360.0,
          \"y\": 520.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 360.0,
          \"y\": 472.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 360.0,
          \"y\": 424.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 360.0,
          \"y\": 376.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 360.0,
          \"y\": 328.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 360.0,
          \"y\": 280.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 360.0,
          \"y\": 232.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 360.0,
          \"y\": 184.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 360.0,
          \"y\": 136.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 360.0,
          \"y\": 88.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 360.0,
          \"y\": 40.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 360.0,
          \"y\": -8.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 408.0,
          \"y\": 952.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 408.0,
          \"y\": 904.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 408.0,
          \"y\": 856.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 408.0,
          \"y\": 808.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 408.0,
          \"y\": 760.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 408.0,
          \"y\": 712.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 408.0,
          \"y\": 664.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 408.0,
          \"y\": 616.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 408.0,
          \"y\": 568.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 408.0,
          \"y\": 520.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 408.0,
          \"y\": 472.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 408.0,
          \"y\": 424.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 408.0,
          \"y\": 376.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 408.0,
          \"y\": 328.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 408.0,
          \"y\": 280.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 408.0,
          \"y\": 232.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 408.0,
          \"y\": 184.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 408.0,
          \"y\": 136.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 408.0,
          \"y\": 88.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 408.0,
          \"y\": 40.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 408.0,
          \"y\": -8.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 456.0,
          \"y\": 952.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 456.0,
          \"y\": 904.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 456.0,
          \"y\": 856.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 456.0,
          \"y\": 808.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 456.0,
          \"y\": 760.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 456.0,
          \"y\": 712.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 456.0,
          \"y\": 664.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 456.0,
          \"y\": 616.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 456.0,
          \"y\": 568.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 456.0,
          \"y\": 520.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 456.0,
          \"y\": 472.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 456.0,
          \"y\": 424.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 456.0,
          \"y\": 376.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 456.0,
          \"y\": 328.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 456.0,
          \"y\": 280.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 456.0,
          \"y\": 232.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 456.0,
          \"y\": 184.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 456.0,
          \"y\": 136.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 456.0,
          \"y\": 88.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 456.0,
          \"y\": 40.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 456.0,
          \"y\": -8.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 504.0,
          \"y\": 952.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 504.0,
          \"y\": 904.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 504.0,
          \"y\": 856.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 504.0,
          \"y\": 808.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 504.0,
          \"y\": 760.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 504.0,
          \"y\": 712.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 504.0,
          \"y\": 664.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 504.0,
          \"y\": 616.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 504.0,
          \"y\": 568.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 504.0,
          \"y\": 520.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 504.0,
          \"y\": 472.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 504.0,
          \"y\": 424.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 504.0,
          \"y\": 376.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 504.0,
          \"y\": 328.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 504.0,
          \"y\": 280.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 504.0,
          \"y\": 232.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 504.0,
          \"y\": 184.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 504.0,
          \"y\": 136.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 504.0,
          \"y\": 88.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 504.0,
          \"y\": 40.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 504.0,
          \"y\": -8.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 552.0,
          \"y\": 952.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 552.0,
          \"y\": 904.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 552.0,
          \"y\": 856.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 552.0,
          \"y\": 808.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 552.0,
          \"y\": 760.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 552.0,
          \"y\": 712.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 552.0,
          \"y\": 664.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 552.0,
          \"y\": 616.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 552.0,
          \"y\": 568.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 552.0,
          \"y\": 520.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 552.0,
          \"y\": 472.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 552.0,
          \"y\": 424.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 552.0,
          \"y\": 376.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 552.0,
          \"y\": 328.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 552.0,
          \"y\": 280.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 552.0,
          \"y\": 232.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 552.0,
          \"y\": 184.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 552.0,
          \"y\": 136.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 552.0,
          \"y\": 88.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 552.0,
          \"y\": 40.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      },
      {
        \"name\": \"grass1\",
        \"position\": {
          \"x\": 552.0,
          \"y\": -8.0
        },
        \"size\": {
          \"width\": 24.0,
          \"height\": 24.0
        },
        \"z\": 0
      }
    ],
    \"blocks\": [
      {
        \"type\": \"block\",
        \"position\": {
          \"x\": 100.0,
          \"y\": 700.0
        },
        \"size\": {
          \"width\": 100.0,
          \"height\": 100.0
        }
      },
      {
        \"type\": \"block\",
        \"position\": {
          \"x\": 250.0,
          \"y\": 450.0
        },
        \"size\": {
          \"width\": 100.0,
          \"height\": 100.0
        }
      },
      {
        \"type\": \"block\",
        \"position\": {
          \"x\": 400.0,
          \"y\": 700.0
        },
        \"size\": {
          \"width\": 100.0,
          \"height\": 100.0
        }
      },
      {
        \"type\": \"block\",
        \"position\": {
          \"x\": 400.0,
          \"y\": 200.0
        },
        \"size\": {
          \"width\": 100.0,
          \"height\": 100.0
        }
      }
    ]
  }
}">>.
