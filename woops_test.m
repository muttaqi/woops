#!wolframscript
Import["woops.m"]
On[Assert]

c := Class[
    (* variables *)
    <|
        "a"->0
    |>,
    (* functions *)
    <|
        (* function name as string *)
        "add1" -> Function[
            (* arguments *)
            {x},
            (* output *)
            x + 1
        ],
        (* private function, underscore prefixed *)
        "_private" -> Function[
            {x},
            x + 1
        ],
        "add1ToA" -> Function[
            (* class variable access *)
            {this},
            this["a"] + 1
        ],
        "mutateAndReturnTrue" -> Function[
            {this},
            (
                (* mutation simulated in a functional style *)
                out = this["a", 4];
                {out["__ToInstance__"], True}
            )
        ],
        "callPrivate" -> Function[
            (* calling a private function *)
            {this},
            this["_private", {2}]
        ]
    |>
]

o := New[c, <|"a"->1|>]

(* variable access *)
Assert[o["a"] == 1]
(* function call, arguments in a list *)
Assert[o["add1", {2}] == 3]
Assert[o["add1ToA", {}] == 2]

(* variable mutation *)
o = o["a", 3]
Assert[o["a"] == 3]

res = o["mutateAndReturnTrue", {}]
Assert[res[[2]]]
o = res[[1]]
Assert[o["a"] == 4]

(* calling private function outside of class *)
Assert[Catch[o["_private", {4}]] == "Unknown function _private"]
(* calling private function through intermediate function *)
Assert[o["callPrivate", {}] == 3]

(* subclass *)
subC := Extend[c,
    <|
        "a" -> 2,
        "b" -> 5
    |>,
    <|
        "mutateAndReturnTrue" -> Function[
            {this},
            (
                out = this["a", 4];
                {out["__ToInstance__"], False}
            )
        ],
        "callSuperMutateAndReturnTrue" -> Function[
            {this},
            (
                (* accessing super-class's function *)
                this["_super"]["mutateAndReturnTrue", {}]
            )
        ]
    |>
]

subO := New[subC]

Assert[subO["a"] == 2]
Assert[subO["b"] == 5]

res := subO["mutateAndReturnTrue", {}]
Assert[Not[res[[2]]]]

res = subO["callSuperMutateAndReturnTrue", {}]
Assert[res[[2]]]