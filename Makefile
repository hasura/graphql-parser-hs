.PHONY: format
format:
	cabal-fmt -i graphql-parser.cabal
	find src test bench \
	  -type f \( -name "*.hs" -o -name "*.hs-boot" \) | \
	  xargs ormolu -ie

PROJECT ?= cabal.project
CABAL = cabal --project=$(PROJECT)

.PHONY: build-deps
build-deps:
	$(CABAL) build \
	  --only-dependencies \
	  --enable-tests \
	  --enable-benchmarks \
	  all

.PHONY: ghcid
ghcid:
	ghcid --command "\
	  $(CABAL) repl \
	    --repl-option='-fobject-code' \
	    --repl-option='-O0' \
	    graphql-parser \
	  "

.PHONY: repl
repl:
	$(CABAL) repl \
	    --repl-option='-fobject-code' \
	    --repl-option='-O0' \
	    graphql-parser

.PHONY: ghcid-test
ghcid-test:
	ghcid \
	  --command "\
	    $(CABAL) repl \
	      --repl-option '-fobject-code' \
	      --repl-option '-O0' \
	      graphql-parser-test \
	    " \
	--test ":main"

