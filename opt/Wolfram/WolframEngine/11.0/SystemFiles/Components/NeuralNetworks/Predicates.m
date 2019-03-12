Package["NeuralNetworks`"]

PackageImport["GeneralUtilities`"]


PackageExport["FullySpecifiedNetQ"]
PackageScope["FullySpecifiedTest"]

FullySpecifiedNetQ[net_NetP] := 
	System`Private`ValidQ[net] || 
		If[Call[net, FullySpecifiedTest], 
			System`Private`SetValid[net]; True, 
			False
		];
FullySpecifiedNetQ[_] := False;

FullySpecifiedTest[type_] := FullySpecifiedLayerQ;

FullySpecifiedLayerQ[assoc_] := ConcreteLayerQ[assoc] && InitializedLayerQ[assoc];


NNSetUsage @ "
FullySpecifiedNetQ[net] gives True if a net's arrays are all initialized and its parameters, inputs, and outputs have concrete types.
The Valid flag will be recursively set on the net's assocs when this is the case to make subsequent lookups free.
"


PackageExport["ConcreteNetQ"]
PackageScope["ConcreteTest"]

ConcreteNetQ[net_NetP] := System`Private`ValidQ[net] || Call[net, ConcreteTest];
ConcreteNetQ[_] := False;

ConcreteTest[type_] := ConcreteLayerQ;

ConcreteLayerQ[assoc_] := And[
	AllTrue[assoc["Arrays"], ConcreteParameterQ],
	AllTrue[assoc["Inputs"], FullySpecifiedTypeQ],
	AllTrue[assoc["Outputs"], FullySpecifiedTypeQ],
	AllTrue[assoc["Parameters"], ConcreteParameterQ]
];
(* TODO: When we have a separate Types field, we use FullySpecifiedTypeQ there,
and for Parameters we use ConcreteParameterQ, which will check for actual integers, etc*)

NNSetUsage @ "
ConcreteNetQ[net] gives True if a net's parameters, inputs, and outputs have concrete types.
"


PackageExport["InitializedNetQ"]
PackageScope["InitializedTest"]

InitializedNetQ[net_NetP] := System`Private`ValidQ[net] || Call[net, InitializedTest];
InitializedNetQ[_] := False;

InitializedTest[type_] := InitializedLayerQ;

InitializedLayerQ[assoc_] := AllTrue[assoc["Arrays"], Developer`RawArrayQ[#] || # === None&];

NNSetUsage @ "
InitializedNetQ[net] gives True if a net's parameters are all initialized to RawArrays.
"
