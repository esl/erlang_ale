
UNAME := $(shell uname)

ifeq ($(UNAME), Darwin)
XC_DIR=/usr/local/gcc-4.8.0-for-linux32
ERL_LIB=/usr/local/lib/erlang/lib/erl_interface-3.7.11
CFLAGS=-Wall -I/usr/local/include -I$(ERL_LIB)/include -I$(XC_DIR)/include -Ideps/erlang-portutil -Ideps/pihwm/lib
CC=$(XC_DIR)/bin/i586-pc-linux-gcc
endif

ifeq ($(UNAME), Linux)
ERL_LIB=/usr/lib/erlang/lib/erl_interface-3.7.9
CFLAGS=-Wall -I/usr/local/include -I$(ERL_LIB)/include -Ideps/erlang-portutil -Ideps/pihwm/lib
CC=gcc
endif

LDFLAGS=-L. -L$(ERL_LIB)/lib -Ldeps/pihwm/lib -Lpriv

PIHWMLIB = pihwm pi_gpio 


all:	library gpio_port build

library: $(PIHWMLIB)

$(PIHWMLIB):
	@echo Building library: $@...
	$(CC) $(CFLAGS) -o priv/$@.o -c -lpthread deps/pihwm/lib/$@.c 


#build: rebar_plugin
build: 
	rebar compile

#rebar_plugin: plugins/compile-deps/src/rebar_compiledeps_plugin.beam

#plugins/compile-deps/src/%.beam: plugins/compile-deps/src/%.erl
#	erlc -o plugins/compile-deps/src $<

build_pihwm:
	gcc 
build_sim:
	rebar -D simulation_mode  compile

build_examples:
	erlc -o examples examples/*.erl

shell: 
	erl  -sname e1 -setcookie secretcookie -pz deps/*/ebin -pz ebin -pz examples

test:
	ct_run -noshell -pa deps/*/ebin -pa ebin -sname ct -env TEST_DIR test -dir test

run_test:
	ct_run -noshell -pa deps/*/ebin -pa ebin -sname ct -env TEST_DIR test -dir test


gpio_port: priv/gpio_port.o priv/port_comms.o library
	$(CC)  ${LDFLAGS} priv/port_comms.o priv/pihwm.o priv/pi_gpio.o  priv/gpio_port.o -lerl_interface -lei -lpthread -o priv/gpio_port

gpio_test: priv/gpio_test.o
	$(CC) $(LDFLAGS) $< pihwm.o pi_gpio.o -lpthread -o $@

priv/port_comms.o: deps/erlang-portutil/port_comms.c
	$(CC) -g $(CFLAGS)  -c -o $@ $<

priv/%.o: c_src/%.c
	$(CC) -g $(CFLAGS) -c -o $@ $<


clean_ct:
	rm -rf ct_run*
	rm -rf ct_default.css
	rm -rf index.html
	rm -rf all_runs.html
	rm -rf jquery*
	rm -rf variables*@*

clean:
	rm -rf ebin/*.beam test/*.beam
