
build:
	rebar compile

build_sim:
	rebar -D SIMULATION_MODE compile

shell: 
	erl  -pz deps/*/ebin -pz ebin

test: build test/gpio_SUITE.erl
	ct_run -noshell -pa deps/*/ebin -pa ebin -sname ct -env TEST_DIR test -dir test


clean:
	rm -rf ebin/*.beam test/*.beam
