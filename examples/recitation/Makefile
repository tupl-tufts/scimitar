all:
	raco make ./common.rkt ./recitation.rkt ./rosette.rkt

test: all
	@echo Recitation
	@bash -c "time racket ./recitation.rkt"
