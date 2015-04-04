ALE_EBIN_DIR=$(abspath $(wildcard ../_build/default/lib))/*/ebin

all: compile

compile:
	erlc *.erl

gpio_counter: compile
	erl -pa ${ALE_EBIN_DIR} -pa . -run gpio_counter start_link

.PHONY: all compile gpio_counter
