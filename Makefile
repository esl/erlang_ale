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

# PIHWMLIB = pihwm pi_gpio 

all: init library

#library: gpio_port pwm_nif i2c_lib spi_lib examples
library: gpio pwm i2c spi examples

init:
	mkdir -p priv ebin

gpio_port: priv/gpio_port.o deps/erlang_portutil/portutil.o
	$(CC) ${LDFLAGS} deps/erlang_portutil/portutil.o deps/pihwm/lib/pihwm.o deps/pihwm/lib/pi_gpio.o  priv/gpio_port.o -lerl_interface -lei -lpthread -o priv/gpio_port

gpio: gpio_port port_lib.beam
	erlc -o ./ebin src/gpio.erl src/gpio_sup.erl

# PWM
pwm_nif:
	$(CC) $(CFLAGS) $(LDFLAGS) $< -o priv/pwm_nif.so -fpic -shared c_src/pwm_nif.c deps/pihwm/lib/pihwm.c deps/pihwm/lib/pi_pwm.c

pwm: pwm_nif
	erlc -o ./ebin src/pwm.erl src/pwm_sup.erl

# I2C
i2c_ei.o: c_src/i2c_ei.c
	$(CC) $(CFLAGS) -c c_src/i2c_ei.c -o priv/i2c_ei.o

i2c_lib: i2c_ei.o deps/erlang_portutil/portutil.o
	$(CC) $(CFLAGS) -o priv/i2c_lib -I$(ERL_LIB)/include -lpthread -L$(ERL_LIB)/lib priv/i2c_ei.o deps/erlang_portutil/portutil.o deps/pihwm/lib/pihwm.o deps/pihwm/lib/pi_i2c.o -lerl_interface -lei 
	rm -rf priv/i2c_ei.o

port_lib.beam:
	erlc -o ./ebin src/port_lib.erl

i2c: i2c_lib port_lib.beam
	erlc -o ./ebin src/i2c.erl src/i2c_sup.erl

# SPI
spi_ei.o: c_src/spi_ei.c
	$(CC) $(CFLAGS) -c c_src/spi_ei.c -o priv/spi_ei.o

spi_lib: spi_ei.o deps/erlang_portutil/portutil.o
	$(CC) $(CFLAGS) -o priv/spi_lib -I$(ERL_LIB)/include -lpthread -L$(ERL_LIB)/lib priv/spi_ei.o deps/erlang_portutil/portutil.o deps/pihwm/lib/pihwm.o deps/pihwm/lib/pi_spi.o -lerl_interface -lei
	rm -rf priv/spi_ei.o

spi: spi_lib port_lib.beam
	erlc -o ./ebin src/spi.erl src/spi_sup.erl

# EXAMPLE
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

.PHONY: examples all library init shell

# DOCUMENTATION
docs:
	rm -rf doc/interface doc/erlang-api
	mkdir -p doc/erlang-api
	erl -noshell -run edoc files 'src/i2c.erl' 'src/i2c_sup.erl' 'src/spi.erl' 'src/spi_sup.erl' 'src/pwm.erl' 'src/pwm_sup.erl' 'src/gpio.erl' 'src/gpio_sup.erl' -s init stop
	mv edoc-info erlang.png *.html stylesheet.css doc/erlang-api/
	doxygen doc/doxygen.conf

	mv doc/html doc/interface
