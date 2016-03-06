.SUFFIXES: .erl .beam

.erl.beam:
	erlc -W $<

ERL = erl -boot start_clean

MODS = made_file

all: compile
	${ERL} -run -pa $(CURDIR) -s made_file test -run init stop -noshell

compile: ${MODS:%=%.beam}

made_file:
	erlc made_file

clean:
	rm -rf *.beam erl_crash.dump
