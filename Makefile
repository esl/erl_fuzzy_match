.PHONY: all compile clean distclean test dialyze

all: compile dialyze test

ERL_MAKE=case make:all( $(ERL_MAKE_OPTS) ) of up_to_date -> halt(0); error -> halt(1) end.

ERL_MAKE_OPTS=[ debug_info, report ]

compile:
	erl -noinput -eval '$(ERL_MAKE)'

test:
	-mkdir -p test/logs
	PWD=`pwd` ct_run \
		-pa ebin \
		-dir test \
		-logdir test/logs \
		-cover test/cover.spec \
		-ct_hooks cth_surefire "[{path,\"$$PWD/test/logs/report.xml\"}]"

dialyze: .dialyzer_plt
	dialyzer --plt .dialyzer_plt ebin

.dialyzer_plt:
	dialyzer --build_plt --output_plt .dialyzer_plt --apps erts kernel stdlib

clean:
	-rm ebin/*.beam
	-rm -rf test/logs

distclean: clean
	-rm .dialyzer_plt
