all:
	raco make ./grammar.rkt ./translate.rkt \
	          ./semantics.ir.rkt ./compiler.rkt ./logistics.rkt \
	          ./simple.logistics.rkt ./example.logistics.rkt \
	          ./simple.ir.rkt ./example.ir.rkt ./runner.rkt ./rosette.rkt

test-simple: all
	@echo "Logistics Simple"
	@bash -c "time racket ./simple.logistics.rkt"

test-example: all
	@echo "Logistics Example"
	@bash -c "time racket ./example.logistics.rkt"
