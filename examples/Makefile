all:
	make -C pipes
	make -C logistics
	make -C trivial
	make -C simple
	make -C sum-to-n
	make -C malloc
	make -C recitation
	make -C imp

test: all
	make -C pipes test
	make -C logistics test-simple
	make -C logistics test-example
	make -C trivial test
	make -C simple test
	make -C sum-to-n test
	make -C malloc test
	make -C recitation test
	make -C imp test-sum
	make -C imp test-example
