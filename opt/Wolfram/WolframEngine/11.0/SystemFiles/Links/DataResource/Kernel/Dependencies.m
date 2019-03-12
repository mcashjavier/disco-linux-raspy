(* Wolfram Language Package *)

(Unprotect[#]; Clear[#])& /@ {}

BeginPackage["DataResource`"]

Begin["`Private`"] (* Begin Private Context *) 

checkResourceSystemClient[]:=Block[{paclets},
	paclets=PacletFind["ResourceSystemClient"];
	If[Length[paclets]>0,
		True
		,
		paclets=PacletFindRemote["ResourceSystemClient"];
		If[Length[paclets]>0,
			True
			,
			False
		]
	]
]

If[TrueQ[checkResourceSystemClient[]],
	Needs["ResourceSystemClient`"]
	,
	Message[ResourceData::norsys]
]

End[];

EndPackage[]

SetAttributes[{},
   {ReadProtected, Protected}
];