.PHONY: format
format:
	find src test bench -name '*.hs' | xargs ormolu -ie
