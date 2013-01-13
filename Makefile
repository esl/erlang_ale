
build:
	rebar compile



shell: 
	erl  -pz deps/*/ebin -pz ebin

clean:
	rm -rf ebin/*.beam 
