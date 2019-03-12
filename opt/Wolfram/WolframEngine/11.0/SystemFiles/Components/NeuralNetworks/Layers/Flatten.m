Input: TensorT[$$Rank, $Dimensions]

Output: VectorT[$$OutputSize]

Parameters:
	$Dimensions: SizeListT[]
	$$Rank: NaturalT
	$$OutputSize: ComputedType[SizeT, Times @@ $Dimensions]

MXNet:
	Name: "Flatten"
