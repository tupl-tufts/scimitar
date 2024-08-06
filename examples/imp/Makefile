all:
	raco make ./grammar.rkt ./translate.rkt \
	          ./semantics/ba-helpers.rkt ./semantics/ba.ir.rkt \
	          ./semantics/bv-helpers.rkt ./semantics/bv.ir.rkt \
	          ./semantics/env.ir.ast.rkt \
	          ./elab.rkt ./compiler.rkt ./imp.rkt \
	          ./example.imp.rkt ./sum.imp.rkt

test-sum: all
	@echo "Imp sum"
	@bash -c "time racket ./sum.imp.rkt"

test-example: all
	@echo "Imp example"
	@bash -c "time racket ./example.imp.rkt"
