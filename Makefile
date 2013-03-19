
UNAME := $(shell uname)

ifeq ($(UNAME), Darwin)
ERL_LIB=/usr/local/lib/erlang/lib/erl_interface-3.7.9
CFLAGS=-Wall -I/usr/local/include -I$(ERL_LIB)/include
endif

ifeq ($(UNAME), Linux)
ERL_LIB=/usr/lib/erlang/lib/erl_interface-3.7.9
CFLAGS=-Wall -I/usr/local/include -I$(ERL_LIB)/include
endif



LDFLAGS=-L. -L$(ERL_LIB)/lib 


build:
	rebar compile

build_sim:
	rebar -D simulation_mode  compile

build_examples:
	erlc -o examples examples/*.erl

shell: 
	erl  -sname e1 -setcookie secretcookie -pz deps/*/ebin -pz ebin -pz examples

test: test/gpio_SUITE.erl
	ct_run -noshell -pa deps/*/ebin -pa ebin -sname ct -env TEST_DIR test -dir test


gpio_node: priv/gpio_node.o
	gcc ${LDFLAGS} $< -lerl_interface -lei -lpthread -o $@

priv/%.o: c_src/%.c
	gcc -g $(CFLAGS) -o $@ -c $<


clean_ct:
	rm -rf ct_run*
	rm -rf ct_default.css
	rm -rf index.html
	rm -rf all_runs.html
	rm -rf jquery*
	rm -rf variables*@*

clean:
	rm -rf ebin/*.beam test/*.beam
