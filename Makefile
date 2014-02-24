PROJECT=erlang_ale
EILOC:=$(shell find /usr/local/lib/erlang /usr/lib/erlang -name ei.h -printf '%h\n' 2> /dev/null | head -1)
LDFLAGS=-Lpriv -L/usr/lib/erlang/usr/lib -L$(shell find /usr/local/lib/erlang /usr/lib/erlang -name libei.a -printf '%h\n' 2> /dev/null | head -1)
UNAME := $(shell uname)

# Dependencies
DEPS =  gproc meck pihwm erlang_portutil
dep_gproc = https://github.com/lehoff/gproc.git master
dep_meck = https://github.com/eproxus/meck.git 0.7.2
dep_pihwm = https://github.com/omerk/pihwm.git master 
dep_erlang_portutil = https://github.com/omerk/erlang_portutil master
REBAR_DEPS_DIR=${DEPS_DIR} ## ??

# Cross compilation bits for Mac, for development purposes only
ifeq ($(UNAME), Darwin)
  XC_DIR=/usr/local/gcc-4.8.0-for-linux32
  CFLAGS=-Wall -I/usr/local/include -I$(EILOC) -I$(XC_DIR)/include -Ideps/erlang_portutil -Ideps/pihwm/lib
  CC=$(XC_DIR)/bin/i586-pc-linux-gcc
endif

ifeq ($(UNAME), Linux)
  #CFLAGS=-Wall -I/usr/local/include -I$(EILOC) -Ideps/erlang_portutil -Ideps/pihwm/lib
  CFLAGS=-I/usr/local/include -I$(EILOC) -I/usr/lib/erlang/usr/include/ -Ideps/erlang_portutil -Ideps/pihwm/lib
  CC=gcc
endif

include erlang.mk

###################################

all: init portutil pihwm portlib library examples

init:
	mkdir -p priv ebin

portutil:
	$(CC) $(CFLAGS) -c -o priv/portutil.o deps/erlang_portutil/portutil.c 

pihwm:
	$(CC) $(CFLAGS) -c -o priv/pihwm.o deps/pihwm/lib/pihwm.c 
	$(CC) $(CFLAGS) -c -o priv/pi_gpio.o -lpthread deps/pihwm/lib/pi_gpio.c 
	$(CC) $(CFLAGS) -c -o priv/pi_i2c.o deps/pihwm/lib/pi_i2c.c 
	$(CC) $(CFLAGS) -c -o priv/pi_spi.o deps/pihwm/lib/pi_spi.c 
	$(CC) $(CFLAGS) -c -o priv/pi_pwm.o deps/pihwm/lib/pi_pwm.c 

portlib:
	erlc -o ./ebin src/port_lib.erl

library: gpio pwm i2c spi 

examples:
	erlc -o examples examples/*.erl

shell: 
	sudo erl -sname ale -setcookie secretc00kie -pz deps/*/ebin -pz ebin -pz examples

#clean:
#	rm -rf priv ebin

###################################

### GPIO
gpio: gpio_port portlib
	erlc -o ./ebin src/gpio.erl 

gpio_port: pihwm portutil
	$(CC) $(LDFLAGS) $(CFLAGS) -o priv/gpio_port \
		priv/portutil.o priv/pihwm.o priv/pi_gpio.o c_src/gpio_port.c \
		-lpthread -lerl_interface -lei

### PWM
pwm: pwm_nif
	erlc -o ./ebin src/pwm.erl 

pwm_nif:
	# we need to re-compile bits of pihwm here because NIFs require -fPIC
	$(CC) $(LDFLAGS) $(CFLAGS) -o priv/pwm_nif.so -fPIC -shared \
		deps/pihwm/lib/pihwm.c deps/pihwm/lib/pi_gpio.c c_src/pwm_nif.c \
		-lpthread


### I2C
i2c: i2c_lib portlib
	erlc -o ./ebin src/i2c.erl

i2c_lib: pihwm portutil 
	$(CC) $(LDFLAGS) $(CFLAGS) -o priv/i2c_lib \
		priv/portutil.o priv/pihwm.o priv/pi_i2c.o c_src/i2c_ei.c \
		-lpthread -lerl_interface -lei


### SPI
spi: spi_lib portlib
	erlc -o ./ebin src/spi.erl 

spi_lib: pihwm portutil
	$(CC) $(LDFLAGS) $(CFLAGS) -o priv/spi_lib \
		priv/portutil.o priv/pihwm.o priv/pi_spi.o c_src/spi_ei.c \
		-lpthread -lerl_interface -lei

###################################

# Tests
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

# Documentation
docs:
	rm -rf doc/interface doc/erlang-api
	mkdir -p doc/erlang-api
	erl -noshell -run edoc files 'src/i2c.erl' 'src/spi.erl' 'src/pwm.erl' 'src/gpio.erl' -s init stop
	mv edoc-info erlang.png *.html stylesheet.css doc/erlang-api/
	doxygen doc/doxygen.conf

	mv doc/html doc/interface

.PHONY: examples 
