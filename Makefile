APP = fermat
SOURCES := $(shell find . -name *.erl -or -name *.app.src -or -name *.hrl) rebar.config

.PHONY: all dialyze

all: $(APP)

_build/default/bin/$(APP): $(SOURCES)
	@./rebar3 escriptize

$(APP): _build/default/bin/$(APP)
	@cp _build/default/bin/$(APP) .

dialyze:
	@./rebar3 dialyzer

eunit:
	@./rebar3 eunit
