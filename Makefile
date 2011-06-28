

test:
	./rebar compile
	./rebar eunit skip_deps=true
	dialyzer --no_check_plt ebin/*.beam

shell:
	erl -pz ./ebin -pz deps/triq/ebin

.PHONY: test
