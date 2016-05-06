REBAR=rebar
APP=alkani
ENV=prod

OVERLAY_VARS=vars/$(ENV).config

REL_VERSION=`git describe --tags --always`
RPM_VERSION=`git describe --tags --always | sed 's/-/~/g'`


all: compile-all

deps:
	$(REBAR) get-deps

compile:
	$(REBAR) compile skip_deps=true

compile-all:
	$(REBAR) compile

docs:
	$(REBAR) doc

check: test itest

test: compile
	mkdir -p logs
	env ERL_LIBS=deps ERL_AFLAGS='-config test/sys -s lager' $(REBAR) eunit skip_deps=true verbose=1

itest: compile
	mkdir -p logs temp
	env ERL_LIBS=deps ERL_AFLAGS='-config test/sys -s lager' $(REBAR) ct skip_deps=true || grep Testing logs/raw.log

rtest: compile
	mkdir -p logs temp/mnesia
	env ERL_LIBS=deps erl -pa ebin -pa itest -config test/sys -mnesia dir '"temp/mnesia"' -s $(APP) -s sync

shell: compile
	mkdir -p logs
	env ERL_LIBS=deps erl -pa ebin -config test/sys -s lager

clean: clean-itest
	$(REBAR) clean skip_deps=true

clean-all: clean-itest
	$(REBAR) clean
	rm -rf rel/$(APP)

clean-itest:
	rm -f itest/*.beam

release-fresh: clean clean-deps deps compile-all release

release:
	cd rel && rm -rf $(APP) && $(REBAR) generate overlay_vars=$(OVERLAY_VARS)
	cd rel/$(APP)/releases/$(REL_VERSION) && \
	    cp $(APP).rel $(APP)-$(REL_VERSION).rel && \
	    cp $(APP).boot $(APP)-$(REL_VERSION).boot && \
	    cp $(APP).boot start.boot

.PHONY: all deps compile compile-all check test itest clean clean-all clean-itest release-fresh release


