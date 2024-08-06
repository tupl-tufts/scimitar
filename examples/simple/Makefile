all:
	raco make ./example.simple.rkt
	raco make ./example.minimal.rkt
	raco make ./example.inner.rkt

test: all
	@echo "Simple"
	@bash -c "time racket ./example.simple.rkt"

test-minimal: all
	@echo "Simple Minimal"
	@bash -c "time racket ./example.minimal.rkt"

test-inner: all
	@echo "Simple Inner"
	@bash -c "time racket ./example.inner.rkt"
