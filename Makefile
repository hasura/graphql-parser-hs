.PHONY: format
format:
	find src test bench -name '*.hs' | xargs ormolu -ie
	cabal-fmt -i graphql-parser.cabal
