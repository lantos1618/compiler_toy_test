

most people implement a language like this:
token -> lexer -> parser -> interpreter -> code


what we can do is:
cranelift -> ast -> parser -> lexer -> token


what we want to do is:
json -> ast -> codegen



Development cycle:
1. requirements -> TESTS FIRST <-> feed the errors into AI.


• outline cranelift requirements.
• create ast
• create parser
• create lexer
• create token
