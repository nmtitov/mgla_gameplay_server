%%%-------------------------------------------------------------------
%%% @author nt
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 18. Jan 2018 01:48
%%%-------------------------------------------------------------------
-module(gameplay_server).
-author("nt").

-export([start/0, stop/0]).

start() ->
  ok = application:start(crypto),
  ok = application:start(cowlib),
  ok = application:start(asn1),
  ok = application:start(public_key),
  ok = application:start(ssl),
  ok = application:start(ranch),
  ok = application:start(cowboy),
  ok = application:start(jsx),
  ok = application:start(gproc),
  ok = application:start(compiler),
  ok = application:start(syntax_tools),
  ok = application:start(goldrush),
  ok = application:start(lager),
  ok = application:start(gameplay_server),
  lager:info("[start]").

stop() ->
  lager:info("[stop]"),
  ok = application:stop(gameplay_server),
  ok = application:stop(lager),
  ok = application:stop(goldrush),
  ok = application:stop(syntax_tools),
  ok = application:stop(compiler),
  ok = application:stop(gproc),
  ok = application:stop(jsx),
  ok = application:stop(cowboy),
  ok = application:stop(ssl),
  ok = application:stop(asn1),
  ok = application:stop(public_key),
  ok = application:stop(ranch),
  ok = application:stop(cowlib),
  ok = application:stop(crypto).
