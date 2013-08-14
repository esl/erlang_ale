PROJECT=erlang_ale

UNAME := $(shell uname)
EI_LIB:=$(shell find /usr/local/lib/erlang /usr/lib/erlang -name ei.h -printf '%h' 2> /dev/null | head -1)

# Cross compilation bits for Mac, for development purposes only
ifeq ($(UNAME), Darwin)
XC_DIR=/usr/local/gcc-4.8.0-for-linux32
CFLAGS=-Wall -I/usr/local/include -I$(EI_LIB) -I$(XC_DIR)/include -Ideps/erlang_portutil -Ideps/pihwm/lib
CC=$(XC_DIR)/bin/i586-pc-linux-gcc
endif

ifeq ($(UNAME), Linux)
CFLAGS=-Wall -I/usr/local/include -I$(EI_LIB) -Ideps/erlang_portutil -Ideps/pihwm/lib
CC=gcc
endif

#DEPS = edown gen_leader gproc meck pihwm erlang_portutil
DEPS =  gproc meck pihwm erlang_portutil

#dep_edown = https://github.com/esl/edown.git master
#dep_gen_leader = https://github.com/abecciu/gen_leader_revival.git master
dep_gproc = https://github.com/lehoff/gproc.git master
dep_meck = https://github.com/eproxus/meck.git 0.7.2
dep_pihwm = https://github.com/omerk/pihwm.git master "./configure"
dep_erlang_portutil = https://github.com/omerk/erlang_portutil master

include erlang.mk

REBAR_DEPS_DIR=${DEPS_DIR}
ERL_LIBS:=./deps:${ERL_LIBS}
LDFLAGS=-L. -L$(ERL_LIB)/lib -Ldeps/pihwm/lib -Lpriv

all: init library examples

init:
	mkdir -p priv

library: gpio_port pwm_nif

gpio_port: priv/gpio_port.o deps/erlang_portutil/portutil.o
	$(CC) ${LDFLAGS} deps/erlang_portutil/portutil.o deps/pihwm/lib/pihwm.o deps/pihwm/lib/pi_gpio.o  priv/gpio_port.o -lerl_interface -lei -lpthread -o priv/gpio_port

pwm_nif:
	$(CC) $(LDFLAGS) $< -o priv/pwm_nif.so -fpic -shared c_src/pwm_nif.c deps/pihwm/lib/pihwm.c deps/pihwm/lib/pi_pwm.c

examples:
	erlc -o examples examples/*.erl

shell: 
	sudo erl  -sname e1 -setcookie secretcookie -pz deps/*/ebin -pz ebin -pz examples

test:
	ct_run -noshell -pa deps/*/ebin -pa ebin -sname ct -env TEST_DIR test -dir test

run_test:
	ct_run -noshell -pa deps/*/ebin -pa ebin -sname ct -env TEST_DIR test -dir test

clean_ct:
	rm -rf ct_run*
	rm -rf ct_default.css
	rm -rf index.html
	rm -rf all_runs.html
	rm -rf jquery*
	rm -rf variables*@*

priv/%.o: c_src/%.c
	$(CC) -g $(CFLAGS) -c -o $@ $<

.PHONY: all library init shell

