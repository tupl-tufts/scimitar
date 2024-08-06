all: setup

install:
	raco pkg install --deps search-auto scimitar/

uninstall:
	raco pkg remove scimitar

setup:
	raco setup --avoid-main --pkgs scimitar

debug:
	SCIMITAR_CONTRACTS=true raco setup --avoid-main --pkgs scimitar

examples: setup
	make -C examples

examples-debug: debug
	SCIMITAR_CONTRACTS=true make -C examples

clean:
	raco setup --fast-clean --pkgs scimitar

check-deps:
	raco setup --check-pkg-deps --pkgs scimitar

test: examples
	make test -C examples
