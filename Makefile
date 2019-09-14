.PHONY: build
build:
	stack build --fast

.PHONY: install
install:
	stack install
	
.PHONY: test
test:
	stack test