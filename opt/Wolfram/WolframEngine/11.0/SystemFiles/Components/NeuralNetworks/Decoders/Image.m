Input: TensorT[3]

Parameters:
	$ColorSpace: Defaulting[ColorSpaceT, "RGB"]

ArrayDepth: 3

ToDecoderFunction: Function @ With[{cspace = #ColorSpace},
	MapB[#2, 
		Function[input, 
			If[cspace === "RGB" && Length[input] =!= 3,
				ColorConvert[Image[input, Interleaving -> False], "RGB"],
				Image[input, ColorSpace -> cspace, Interleaving -> False]
			]
		]
	]
]


