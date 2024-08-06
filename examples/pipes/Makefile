all:
	raco make ./grammar.rkt ./translate.rkt ./graph.rkt ./compiler.rkt ./pipes.rkt \
	          ./example.pipes.rkt ./example.ir.rkt ./runner.rkt ./rosette.rkt

test: all
	@echo "Pipes"
	@bash -c "time racket ./example.pipes.rkt"
