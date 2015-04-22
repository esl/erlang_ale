ALE_EBIN_DIR=$(abspath $(wildcard ../_build/default/lib))/*/ebin

all: clean compile

compile:
	erlc -I ../include/ *.erl
	
clean:
	rm -rf *.beam

gpio_counter: compile
	erl -pa ${ALE_EBIN_DIR} -pa . -run gpio_counter start_link

ale_handler_gpio_int_test: compile
	erl -pa ${ALE_EBIN_DIR} -pa . -run ale_handler_gpio_int_test start_link

shell:
	erl -pa ${ALE_EBIN_DIR} -pa .
	
.PHONY: all compile clean shell gpio_counter

