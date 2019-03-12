Input: ChannelT[$$Count, TensorT[$$Rank, $$InputSize]]

Output: ComputedType[
	ListT[$$Count, TensorT[]],
	ListT[$$Count,
		TensorT @ If[$Flatten,
			$$InputSize,
			Prepend[$$InputSize, 1]
		]
	]
]

Parameters:
	$Flatten: Defaulting[BooleanT, True]
	$$Count: SizeT
	$$Rank: SizeT
	$$InputSize: SizeListT[$$Rank]

MXNet:
	Name: "SliceChannel"
	Writer: Function[{
		"axis" -> "1",
		"squeeze_axis" -> If[#Flatten, "true", "false"],
		"num_outputs" -> IntegerString[#$Count]
	}]

