#!wolframscript
Import["woops.m"]
On[Assert]

c := Class[
    <|
        "a"->0
    |>,
    <|
        "add1" -> Function[
            {x},
            x + 1
        ],
        "_private" -> Function[
            {x},
            x + 1
        ],
        "add1ToA" -> Function[
            {this},
            this["a"] + 1
        ],
        "mutateAndReturnTrue" -> Function[
            {this},
            (
                out = this["a", 4];
                {out["__ToInstance__"], True}
            )
        ],
        "callPrivate" -> Function[
            {this},
            this["_private", {2}]
        ]
    |>
]

o := New[c, <|"a"->1|>]

Assert[o["a"] == 1]
Assert[o["add1", {2}] == 3]
Assert[o["add1ToA", {}] == 2]

o = o["a", 3]
Assert[o["a"] == 3]

res = o["mutateAndReturnTrue", {}]
Assert[res[[2]]]
o = res[[1]]
Assert[o["a"] == 4]

Assert[Catch[o["_private", {4}]] == "Unknown function _private"]
Assert[o["callPrivate", {}] == 3]
