Package["MXNetLink`"]

PackageImport["GeneralUtilities`"]

(******************************************************************************)
(***** Classification Metrics ******)

(******************************************************************************)
(* Softmax *)

PackageExport[MXMetricSoftmax]

MXSetUsage @ "
MXMetricSoftmax[probs$, labels$] calculates the softmax loss given the predicted probabilities and labels, which can be either NDArray[$$] or standard WL arrays.
"

Options[MXMetricSoftmax] = {
	"LogProbabilities" -> True 
};

MXMetricSoftmax[probPredict_NDArray, labels_NDArray, OptionsPattern[]] := CatchFailure @ Scope[
	(* check consistency of labels *)
	If[Length@probPredict =!= Length@labels, ThrowFailure["inconsistentLength"]];
	probs = NDArrayPartThread[probPredict, labels];
	If[FailureQ@probs, Return@probs];
	If[OptionValue@"LogProbabilities", Developer`ComposeTo[probs, Log]];
	-First@Normal@Total@probs/Length@labels
];

MXMetricSoftmax[probPredict_, labels_, opts:OptionsPattern[]] := 
	MXMetricSoftmax[NDArrayCreate[probPredict], NDArrayCreate[labels], opts] 

MXMetricSoftmax::inconsistentLength = "The label and predicted probabilities don't have the same length."

(******************************************************************************)
(* Accuracy *)

PackageExport[MXMetricAccuracy]

MXSetUsage @ "
MXMetricAccuracy[probs$, labels$] calculates the average accuracy loss given the predicted probabilities and labels.
";

MXMetricAccuracy[probPredict_NDArray, labels_NDArray] := CatchFailure @ Scope[
	(* Most probable predictions *)
	maxProbs = NDArrayRowIndexMax@probPredict;
	maxProbsNormal = Round@Normal@maxProbs;
	labelsNormal = Round@Normal@labels; (* labels start from 0 *)
	trues = Count[labelsNormal - maxProbsNormal, 0];
	N@trues/Length@labelsNormal
];

MXMetricAccuracy[probPredict_, labels_, opts:OptionsPattern[]] := 
	MXMetricAccuracy[NDArrayCreate[probPredict], NDArrayCreate[labels], opts] 

MXMetricAccuracy::inconsistentLength = "The label and predicted probabilities don't have the same length."

(* iterator version *)


(******************************************************************************)
(***** Regression Metrics ******)
		
		
(*MXMetricEuclidean[yTrue_, yPredict_] := Mean@Flatten[Abs[yTrue - yPredict]]*)
		
		
(*LossFunction["MeanAbsolute", yTrue_, yPredict_, opts:OptionsPattern[]] := Mean@Flatten[Abs[yTrue - yPredict]]

LossFunction["MeanSquare", yTrue_, yPredict_, opts:OptionsPattern[]] := Mean@Flatten[(yTrue - yPredict)^2]

LossFunction["Accuracy", yTrue_, yPredict_, opts:OptionsPattern[]] :=  Module[
	{m, nclass, yTrue2, yPredict2}
	,
	nclass = 1; (* method doesn't care about number of classes *)
	{m, nclass, yTrue2, yPredict2} = preprocessLoss[yTrue, yPredict, nclass];
	- Mean@MapThread[(Length@Intersection[#1, #2]/Length@Union[#1, #2])&, {N@yTrue2, N@yPredict2}]
];*)

(******************************************************************************)
(*evaluationMetricIterator[predictIter_Iterator, predictName_String, labelName_String] := CatchFailure @ Scope[
	pred = Read@predictIter;
	
	While[Not@IteratorExhaustedQ@predictIter,
		label = pred["Inputs", labelName];
		prediction = pred["Outputs", predictName];
		MXMetricAccuracy
		(* Read another value *)
		pred = Read@predictIter;
	];
];*)
