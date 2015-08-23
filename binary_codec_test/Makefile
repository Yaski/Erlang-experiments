REBAR = `which rebar`

all: deps compile

deps:
	@( $(REBAR) get-deps )

compile: clean
	@( $(REBAR) compile )

clean:
	@( $(REBAR) clean )

run:
	@( erl -pa ebin deps/*/ebin -s binary_codec_test )

.PHONY: all deps compile clean run
