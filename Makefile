
UNAME := $(shell uname)

ifeq ($(UNAME), Darwin)
XC_DIR=/usr/local/gcc-4.8.0-for-linux32
ERL_LIB=/usr/local/lib/erlang/lib/erl_interface-3.7.11
CFLAGS=-Wall -I/usr/local/include -I$(ERL_LIB)/include -I$(XC_DIR)/include
CC=$(XC_DIR)/bin/i586-pc-linux-gcc
endif

ifeq ($(UNAME), Linux)
ERL_LIB=/usr/lib/erlang/lib/erl_interface-3.7.9
CFLAGS=-Wall -I/usr/local/include -I$(ERL_LIB)/include
CC=gcc
endif

LDFLAGS=-L. -L$(ERL_LIB)/lib 

PIHWMLIB = pihwm pi_gpio port_comms 


all:	library port_comms.o gpio_port gpio_port.beam

library: $(PIHWMLIB)

$(PIHWMLIB):
	@echo Building library: $@...
	$(CC) $(CFLAGS) -c -lpthread -lerl_interface -lei lib/$@.c 


build:
	rebar compile

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


gpio_port: priv/gpio_port.o
	$(CC) ${LDFLAGS} $< port_comms.o -lerl_interface -lei -lpthread -o $@

gpio_test: priv/gpio_test.o
	$(CC) $(LDFLAGS) $< pihwm.o pi_gpio.o -lpthread -o $@


priv/%.o: c_src/%.c
	$(CC) -g $(CFLAGS) -o $@ -c $<


clean_ct:
	rm -rf ct_run*
	rm -rf ct_default.css
	rm -rf index.html
	rm -rf all_runs.html
	rm -rf jquery*
	rm -rf variables*@*

clean:
	rm -rf ebin/*.beam test/*.beam
