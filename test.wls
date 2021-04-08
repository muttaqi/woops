#!wolframscript
Import["woops.wls"]

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
        ]
    |>
]

o := New[c, <|"a"->1|>]

Print[o]

v := o["a"]
Print[v]
Print[o["add1ToA", <||>]]