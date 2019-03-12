Inputs: 
	$Input: TensorT[$$Rank, $$Dimensions]
	$Target: TensorT[$$Rank, $$Dimensions]

Outputs:
	$Loss: ScalarT 

Parameters:
	$$Dimensions: SizeListT[$$Rank]
	$$Rank: NaturalT

MXNet:
	Name: "MeanLoss"
	Writer: Function["loss_type" -> "L1"]
