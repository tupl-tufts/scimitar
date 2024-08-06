all:
	raco make ./common.rkt ./malloc.rkt ./rosette.rkt

test: all
	@echo "Malloc"
	@bash -c "time racket ./malloc.rkt"
