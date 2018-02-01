all: clean compile run

cowlib:
	rm -rf lib/cowlib/ebin/*.beam
	erlc +debug_info -I lib/cowlib/include -o lib/cowlib/ebin lib/cowlib/src/*.erl

cowboy:
	rm -rf lib/cowboy/ebin/*.beam
	erlc +debug_info -I lib -o lib/cowboy/ebin lib/cowboy/src/*.erl

goldrush:
	rm -rf lib/goldrush/ebin
	mkdir -p lib/goldrush/ebin
	erlc +debug_info -o lib/goldrush/ebin lib/goldrush/src/*.erl
	cp lib/goldrush/src/goldrush.app.src lib/goldrush/ebin/goldrush.app

gproc:
	rm -rf lib/gproc/ebin/*
	erlc +debug_info -o lib/gproc/ebin -I lib/gproc/include lib/gproc/src/*.erl
	cp lib/gproc/src/gproc.app.src lib/gproc/ebin/gproc.app

jsx:
	rm -rf lib/jsx/ebin
	mkdir -p lib/jsx/ebin
	erlc +debug_info -Dmaps_support=true -o lib/jsx/ebin lib/jsx/src/*.erl
	cp lib/jsx/src/jsx.app.src lib/jsx/ebin/jsx.app

lager:
	rm -rf lib/lager/ebin
	mkdir -p lib/lager/ebin
	erlc +debug_info -o lib/lager/ebin -I lib/lager/include lib/lager/src/*.erl
	cp lib/lager/src/lager.app.src lib/lager/ebin/lager.app

ranch:
	rm -rf lib/ranch/ebin/*.beam
	erlc +debug_info -o lib/ranch/ebin lib/ranch/src/*.erl

proper:
	rm -rf lib/proper/ebin/*.beam
	./lib/proper/write_compile_flags lib/proper/include/compile_flags.hrl
	erlc +debug_info -I lib/proper/include -o lib/proper/ebin lib/proper/src/vararg.erl
	erlc +debug_info "+{parse_transform, vararg}" -I lib/proper/include -pa lib/proper/ebin -o lib/proper/ebin lib/proper/src/*.erl

deps: cowboy cowlib goldrush gproc jsx lager ranch proper

build:
	erlc +debug_info -I lib/proper/include "+{parse_transform, lager_transform}" "+{parse_transform, proper_unused_imports_remover}" -pa lib/lager/ebin/ -pa lib/proper/ebin -pa lib -o ebin/ `find src tests -type f -iname "*.erl" -print0 | xargs -0`

clean:
	rm -rf ebin/*.beam

compile: clean build dialyzer

run: compile shell

eunit:
	erl -noshell -pa ebin/		\
	-eval "eunit:test([av_d, av_d_attack, av_d_health, av_d_mana, av_d_position], [verbose])" -s init stop

test: compile eunit

shell:
	erl -pa ebin/			\
	lib/cowboy/ebin/	\
	lib/cowlib/ebin/	\
	lib/goldrush/ebin/	\
	lib/gproc/ebin/		\
	lib/jsx/ebin/ 		\
	lib/lager/ebin/ 	\
	lib/ranch/ebin/ 	\
	lib/proper/ebin 	\
	-s gameplay_server

noshell:
	erl -pa ebin/ lib/cowboy/ebin/ lib/cowlib/ebin/ lib/goldrush/ebin/ lib/gproc/ebin/ lib/jsx/ebin/ lib/lager/ebin/ lib/ranch/ebin/ -noshell -s gameplay_server

dialyzer:
	dialyzer --no_check_plt ebin 

