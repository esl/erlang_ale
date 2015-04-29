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

gpio_counter: compile
	erl -pa ${ALE_EBIN_DIR} -pa . -run gpio_counter start_link

shell:
	erl -pa ${ALE_EBIN_DIR} -pa .
	
.PHONY: all compile clean shell gpio_counter