.PHONY: test
test:
	stack build --fast && purs compile --codegen corefn test/Test.purs  && stack exec -- psgo ./output/Test/corefn.json ./kotlin/Test.kt
