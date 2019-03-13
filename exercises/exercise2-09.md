If we change the definition, an errorsome `state` will not be printed since `eval state` for the errorsome `state` will not return anything.

With current implementation, `eval state` always return `state:someChunk` and `someChunk` will be evaluated later when needed.
