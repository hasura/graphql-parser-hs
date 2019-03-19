build:
	stack build --fast

test: build
	stack test --fast

benchmark: build
	stack bench --ba --output=ws/graphql_parser_benchmark.html
