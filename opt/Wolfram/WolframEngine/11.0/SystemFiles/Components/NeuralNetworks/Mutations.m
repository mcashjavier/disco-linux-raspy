Package["NeuralNetworks`"]

PackageImport["GeneralUtilities`"]


PackageExport["ReplaceNetPart"]

ReplaceNetPart[head_Symbol[assoc_Association], specs_] := CatchFailure @ Scope[
	$assoc = assoc;
	Scan[replacePart, Developer`ToList[specs]];
	ConstructWithInference[head, $assoc]
];

ReplaceNetPart::pne = "Part `` does not exist in network."
replacePart[(NetPort[part__] | {part__}) -> value_] := (
	If[!PartExistsQ[$assoc, part], ThrowFailure["pne", {part}]];
	$assoc[[part]] = value;
);

ReplaceNetPart::invpspec = "`` is not a valid part replacement spec."
replacePart[pspec_] := ThrowFailure["invpspec", pspec];