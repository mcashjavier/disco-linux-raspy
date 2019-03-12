Output: VectorT[$Dimension]

Parameters:
	$Labels: ListT[$Dimension, ExpressionT]
	$Dimension: SizeT

ToEncoderFunction: Function[
	If[#2, Replace1, Replace] @ makeDispatch[#Labels, #Dimension]
]

MLType: Function["Nominal"]

makeDispatch[labels_, dim_] :=
	Thread[labels -> IdentityMatrix[dim]] //
	Append[l_ :> EncodeFail[StringForm["`` is not one of ``", l, labels]]] //
	Dispatch;
