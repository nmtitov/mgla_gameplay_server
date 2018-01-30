all: clean compile run

cowlib:
	rm -rf lib/cowlib/ebin/*.erl
	erlc +debug_info -I lib/cowlib/include -o lib/cowlib/ebin lib/cowlib/src/*.erl

cowboy:
	rm -rf lib/cowboy/ebin/*.erl
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
	rm -rf lib/ranch/ebin/*.erl
	erlc +debug_info -o lib/ranch/ebin lib/ranch/src/*.erl	

deps: cowboy cowlib goldrush gproc jsx lager ranch

build:
	erlc +debug_info "+{parse_transform, lager_transform}" -pa lib/lager/ebin/ -o ebin/ `find src tests -type f -iname "*.erl" -print0 | xargs -0`

compile: build dialyzer

clean:
	rm -rf ebin/*.beam

run: compile shell

eunit:
	erl -noshell -pa ebin/		\
	-eval "eunit:test(avatar_data, [verbose])" -s init stop

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
	-s gameplay_server

noshell:
	erl -pa ebin/ lib/cowboy/ebin/ lib/cowlib/ebin/ lib/goldrush/ebin/ lib/gproc/ebin/ lib/jsx/ebin/ lib/lager/ebin/ lib/ranch/ebin/ -noshell -s gameplay_server

dialyzer:
	dialyzer --no_check_plt ebin 

