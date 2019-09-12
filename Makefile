.PHONY: build
build:
	stack build --fast

.PHONY: install
install:
	stack install
	
.PHONY: all
all:  build test-codegen test-generatekt install 

.PHONY: run
run: test
	java -jar kotlin/bin.jar

.PHONY: test
test:
	kotlinc kotlin/*.kt kotlin/foreigns/*.kt -include-runtime -d kotlin/bin.jar
	

test-generatekt: build
	stack exec -- pskt\
		test/output/Main/corefn.json\
		-f "../foreigns/*.kt"\
		-o ./kotlin/\

		# -i "test/output/*/corefn.json"\


.PHONY: test-codegen
test-codegen: build
	cd test && spago build -- -g corefn && cd ..

.PHONY: test-arrays
test-arrays: build
	purs compile --codegen corefn "test/.spago/arrays/*/test/**.purs" "test/.spago/arrays/*/src/**.purs"


