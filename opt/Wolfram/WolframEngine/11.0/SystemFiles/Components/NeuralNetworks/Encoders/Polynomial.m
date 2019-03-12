Output: VectorT[$Outputs]

Parameters:
	$Inputs: SizeT
	$Order: Defaulting[SizeT, 2]
	$Outputs: ComputedType[SizeT, numberOutputs[$Inputs, $Order]]

ToEncoderFunction: Function @ With[
	{cfunc = toPowerFunction[#Inputs, #Order], len = #Inputs, order = #Order},
	If[#2,
		Function[in, 
			If[MatrixQ[in, NumberQ] && Dimensions[in][[2]] == len, 
				cfunc[in], 
				EncodeFail["wrong input dimensions"]
			]
		],
		Function[in, 
			If[Dimensions[in] =!= {len}, 
				EncodeFail["wrong input dimensions"]];
			If[!VectorQ[in, AtomQ],
				EncodeFail["input vector must be list of atoms"]];
			If[VectorQ[in, NumberQ], 
				cfunc[in], 
				ProductsToPower[in, order]
			]
		]
	]
]

AcceptsLists: Function[
	True
]

MLType: Function[{"NumericalVector", #Outputs}]

numberOutputs[m_, n_] := ((n+1) * Binomial[m + n, 1 + n])/m - 1;

toPowerFunction[inputs_, order_] := Scope[
	expr = ProductsToPower[part[x, #]& /@ Range[inputs], order];
	body = Compose[Hold, expr] /. part -> Part;
	PrependTo[body, {{x, _Real, 1}}];
	AppendTo[body, RuntimeAttributes -> {Listable}];
	Compile @@ body
];

ProductsToPower[list_, n_] := 
	Flatten @ Table[makeProducts[list, i], {i, n}];

makeProducts[list_, 1] := list;

makeProducts[list_, n_] :=
	DeleteDuplicates @ Flatten @ Outer[Times, Sequence @@ ConstantArray[list, n]];