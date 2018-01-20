all: clean compile run

compile:
	@erlc "+{parse_transform, lager_transform}" -pa lib/lager/ebin/ -v -o ebin/ -I src/* && cp src/gameplay_server.app.src ebin/gameplay_server.app

clean:
	@rm -rf ebin/*

run: compile shell

shell:
	@erl -pa ebin/ lib/cowboy/ebin/ lib/cowlib/ebin/ lib/goldrush/ebin/ lib/gproc/ebin/ lib/jsx/ebin/ lib/lager/ebin/ lib/ranch/ebin/ -s gameplay_server

