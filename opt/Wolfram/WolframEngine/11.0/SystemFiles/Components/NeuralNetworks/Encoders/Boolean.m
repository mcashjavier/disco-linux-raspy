Output: ScalarT

ToEncoderFunction: Function[
	If[#2, 
		Replace1 @ $BooleanDispatch,
		Replace[$BooleanDispatch]
	]
]

MLType: Function["Boolean"]

$BooleanDispatch = Dispatch[{
	True -> 1, 
	False -> 0, 
	l_ :> EncodeFail[StringForm["`` should be either True or False", l]]
}];