all: compile test

compile:
	./rebar compile

test: eunit

eunit:
	./rebar eunit

clean:
	./rebar clean