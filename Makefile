
.PHONY: all
all:  build test-codegen test-generatekt install test run

.PHONY: run
run: test
	java -jar kotlin/bin.jar

.PHONY: test
test:
	kotlinc kotlin/*.kt kotlin/Foreign/*.kt -include-runtime -d kotlin/bin.jar
	

test-generatekt: build
	stack exec -- pskt \
		-i "test/output/Main/corefn.json"\
		-o ./kotlin/\
		-i "test/output/*/corefn.json"\


.PHONY: test-codegen
test-codegen: build
	cd test && spago build -- -g corefn && cd ..

.PHONY: test-arrays
test-arrays: build
	purs compile --codegen corefn "test/.spago/arrays/*/test/**.purs" "test/.spago/arrays/*/src/**.purs"


.PHONY: build
build:
	stack build --fast

.PHONY: install
install:
	stack install