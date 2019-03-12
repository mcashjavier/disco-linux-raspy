Output: PosIntegerT

Parameters:
	$Labels: ListT[$Dimension, ExpressionT]
	$Dimension: SizeT

ToEncoderFunction: Function[
	If[#2, Replace1, Replace] @ makeDispatch[#Labels, #Dimension]
]

MLType: Function["Nominal"]

makeDispatch[labels_, dim_] :=
	Thread[labels -> Range[dim]] //
	Append[_ -> 0] //
	Dispatch;

