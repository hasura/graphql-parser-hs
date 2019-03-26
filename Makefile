build:
	stack build --fast

test: build
	stack test --fast

bench: build
	stack bench --ba --output=ws/graphql_parser_benchmark.html
