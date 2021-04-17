#!wolframscript

(* Class tuple *)
Class[vars_, funcs_] := {vars, funcs}

(* Sub-class tuple *)
Extend[tuple_, vars_, funcs_] := (
    outVars := tuple[[1]];
    outFuncs := tuple[[2]];

    inVars = <|vars, "_super" -> {<||>, outFuncs}|>;

    Do[
        (
            outVars = <|outVars, varName -> inVars@varName|>;
        ),
        {
            varName,
            Keys[inVars]
        }
    ]; 

    Do[
        (
            outFuncs = <|outFuncs, funcName -> funcs@funcName|>;
        ),
        {
            funcName,
            Keys[funcs]
        }
    ]; 

    {outVars, outFuncs}
)

(* Class instantiation *)
New[default_] := Instance[default]

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

(* Variable access or conversion to meta-instance *)
Instance[tuple_][name_] := (
    If [
        name == "__ToMeta__",
        MetaInstance[tuple],
        If[
            StringTake[name, 1] == "_",
            Throw["Unknown variable " <> name],    
            If[
                KeyExistsQ[tuple[[1]], name],
                tuple[[1]][name],
                Throw["Unknown variable " <> name]
            ]
        ]
    ]    
)

(* Function access *)
Instance[tuple_][name_, args_List] := (
    If[
        StringTake[name, 1] == "_",
        Throw["Unknown function " <> name],    
        If[
            KeyExistsQ[tuple[[2]], name],
            tuple[[2]][name]@@Append[args, MetaInstance[tuple]],
            Throw["Unknown function " <> name]
        ]
    ]
)

(* Private variable access or conversion to instance*)
MetaInstance[tuple_][name_] := (
    If [
        name == "__ToInstance__",
        Instance[tuple],
        If[
            KeyExistsQ[tuple[[1]], name],
            If[
                name == "_super",
                LazySuperInstance[tuple[[1]][name], tuple[[1]]],
                tuple[[1]][name]
            ],
            Throw["Unknown variable " <> name]
        ]
    ]
)

(* Private function access *)
MetaInstance[tuple_][name_, args_List] := (
    If[
        KeyExistsQ[tuple[[2]], name],
        tuple[[2]][name]@@Append[args, MetaInstance[tuple]],
        Throw["Unknown function " <> name]
    ]
)

(* Variable mutation *)
Instance[tuple_][name_, val_] := (
    If[
        StringTake[name, 1] == "_",
        Throw["Unknown variable " <> name],
        If[
            KeyExistsQ[tuple[[1]], name],
            Instance[{<|tuple[[1]], name -> val|>, tuple[[2]]}],
            Throw["Unknown variable " <> name]
        ]
    ]
)

(* Private variable mutation *)
MetaInstance[tuple_][name_, val_] := (  
    If[
        KeyExistsQ[tuple[[1]], name],
        MetaInstance[{<|tuple[[1]], name -> val|>, tuple[[2]]}],
        Throw["Unknown variable " <> name]
    ]
)

(* Inject subclass variables lazily into super class instance *)
LazySuperInstance[tuple_, subVars_] := (
    Instance[{KeyDrop[subVars, "_super"], tuple[[2]]}]
)