.PHONY: format
format:
	cabal-fmt -i graphql-parser.cabal
	find src test bench \
	  -type f \( -name "*.hs" -o -name "*.hs-boot" \) | \
	  xargs ormolu -ie
