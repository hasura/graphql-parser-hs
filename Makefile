.PHONY: format
format:
	find src test bench \
	  -type f \( -name "*.hs" -o -name "*.hs-boot" \) | \
	  xargs ormolu -ie
	cabal-fmt -i graphql-parser.cabal
