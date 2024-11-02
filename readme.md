

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




# thoughts


loop(0..10) |i, e, lh|  {
    if i == 5 {
        lh.continue;
    }
    loop(0..10) |j, e2, lh2| {
            if j == 5 {
                lh2.continue;
            }
        print(j);
    }
    print(i);
}





loop {
    cond,
    body: func(index: int, lh: LoopHandler)
}
loop(0..10, (i, lh) {
    if i == 5 {
        lh.continue;
    }
    print(i);
})