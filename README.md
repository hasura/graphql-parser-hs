# graphql-parser

`graphql-parser` helps you parse the GraphQL Schema and Executable documents into Haskell data types (_SchemaDocument_ and _ExecutableDocument_ respectively).

## Examples

### Parsing GraphQL executable document

```haskell
{-# LANGUAGE OverloadedStrings #-}
import Language.GraphQL (parseExecutableDoc)

main = do
  let ast = parseExecutableDoc "{ cat }"
  either (fail . show) f ast
  where
    f _ = return () -- The function which uses the ast goes here
```

### Parsing GraphQL schema document

```haskell
{-# LANGUAGE OverloadedStrings #-}
import Language.GraphQL (parseSchemaDoc)

main :: do
  let schema = parseSchemaDoc "type cat {name: String!}"
  either (fail . show) f ast
  where
    f _ = return () -- The function which uses the schema goes here
```
