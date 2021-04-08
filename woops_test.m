#!wolframscript
Import["woops.m"]

c := Class[
    <|
        "a"->0
    |>,
    <|
        "add1" -> Function[
            {x},
            x + 1
        ],
        "add1ToA" -> Function[
            #this["a"] + 1
        ],
        "mutateAndReturnTrue" -> Function[
            out = #this["a", 4];
            {out, True}
        ]
    |>
]

o := New[c, <|"a"->1|>]

Assert[o["a"] == 1]
Assert[o["add1ToA", <||>] == 2]

o = o["a", 3]
Assert[o["a"] == 3]

res = o["mutateAndReturnTrue", <||>]
Assert[res[[2]]]
o = res[[1]]
Assert[o["a"] == 4]