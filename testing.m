Assert[actual_, expected_] := (
    If[
        actual == expected,
        Print[True],
        Throw["Failed. Expected: " <> expected <> ". Actual: " <> actual]
    ]
)