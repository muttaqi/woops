#!wolframscript
Class[vars_, funcs_] := {vars, funcs}

New[default_, vars_] := (
    outVars := default[[1]];
    Do[
        If[
            KeyExistsQ[outVars, varName],
            (
                outVars = <|outVars, varName -> vars@varName|>;
            ),
            Throw["Unknown variable " <> varName]
        ],
        {
            varName,
            Keys[vars]
        }
    ];

    Instance[{outVars, default[[2]]}]
)

Instance[tuple_][name_] := (
    If[
        KeyExistsQ[tuple[[1]], name],
        tuple[[1]][name],
        Throw["Unknown variable " <> name]
    ]
)

Instance[tuple_][name_, args_Association] := (
    If[
        KeyExistsQ[tuple[[2]], name],
        tuple[[2]][name][<|args, "this" -> Instance[tuple]|>],
        Throw["Unknown function " <> name]
    ]
)

Instance[tuple_][name_, val_] := (
    If[
        KeyExistsQ[tuple[[1]], name],
        Instance[{<|tuple[[1]], name -> val|>, tuple[[2]]}],
        Throw["Unknown function " <> name]
    ]
)
