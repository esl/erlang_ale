REBAR3_LIB_DIR=$(abspath $(wildcard ../_build/default/lib))
ifeq ($(strip ${REBAR3_LIB_DIR}),)
	ALE_EBIN_DIR=$(abspath ../ebin)
else
	ALE_EBIN_DIR=${REBAR3_LIB_DIR}/*/ebin 
endif

all: clean compile

compile:
	erlc -I ../include/ *.erl
	
clean:
	rm -rf *.beam

shell:
	erl -pa ${ALE_EBIN_DIR} -pa .
	
gpio_counter: compile
	erl -pa ${ALE_EBIN_DIR} -pa . -run gpio_counter start_link

ex_gpio_int: compile
	erl -pa ${ALE_EBIN_DIR} -pa . -run ex_gpio_int start_link

ex_mcp23x17: compile
	erl -pa ${ALE_EBIN_DIR} -pa . -run mcp23x17 start_link
	
ex_mcp7940n: compile
	erl -pa ${ALE_EBIN_DIR} -pa . -run ex_mcp7940n start_link
	
.PHONY: all compile clean shell gpio_counter ex_gpio_int ex_mcp23x17 ex_mcp7940n
