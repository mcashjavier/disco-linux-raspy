Package["MXNetLink`"]

PackageImport["GeneralUtilities`"]


(******************************************************************************)
(* MXPredict *)

PackageExport["MXPredictIterator"]

MXSetUsage[MXPredictIterator,
"MXPredictIterator[MXExecutorData[$$], arrays$] creates an Iterator[$$] object which returns the \
prediction of a network defined by the network MXExecutorData[$$], given an Association of input \
NDArray's arrays$. 
MXPredictIterator[MXExecutorData[$$], Iterator[$$]] is the same as MXPredictIterator[MXExecutorData[$$], arrays$], \
only the arrays are given in the form of an Iterator[$$] object.
Note1: all inputs inputAssoc$ must be batched parameters.
Note2: the datatype and context information is inherited from the MXExecutorData[$$] object. The following \
Options are available: 

'ReturnInputs' -> False: whether to return the inputs along with the outputs.
'ExecutorResizing' -> True: use executor resizing to evaluate data smaller than MXExecutor[$$] is \
initialized to deal with. If False, padding will be used, which can be slower.
"]

(* Iterator version *)
Options[MXPredictIterator] = {
	"ReturnInputs" -> False,
	"PaddingMethod" -> None
}

MXPredictIterator[executor_MXExecutorData, data_Iterator, OptionsPattern[]] := CatchFailure @ Scope[
	MapIterator[batchPredictionFunction[executor, #, OptionValue@"ReturnInputs"]&, data]
]

MXPredictIterator[executor_MXExecutorData, data_Association, OptionsPattern[]] := CatchFailure @ Scope[
	UnpackOptions[returnInputs, paddingMethod];
	
	firstInputName = First@Keys@data;
	batchSizeExec = NDArrayLength@executor["ArgumentArrays", firstInputName];	
	iter = MXBatchIterator[data, batchSizeExec, "PaddingMethod" -> paddingMethod];
	MapIterator[batchPredictionFunction[executor, #, OptionValue@"ReturnInputs"]&, iter]
]

batchPredictionFunction[exec_MXExecutorData, {data_, pad_}, returnInputs_] := CatchFailure @ Scope[
	(* We don't know batch size: *)
	firstInputName = First@Keys@data;
	batchSizeData = NDArrayLength@data[firstInputName];
	
	(* infer batch size from initialized NDArrays *)
	batchSizeExec = NDArrayLength@exec["ArgumentArrays", firstInputName];
	(* Don't allow data batch larger than executor batch *)
	If[batchSizeData > batchSizeExec, ThrowFailure["invalidDataBatchDim"]];
	(* If data batch is smaller than executor + no padding, dynamically resize executor *)
	execFinal = exec;
	If[batchSizeData < batchSizeExec, (* check whether we need to do exec resizing *)
		shapes = Dimensions /@ data;
		execFinal = MXExecutorReshape[exec, shapes]
	];
	
	(* Set input vals *)
	KeyValueMap[NDArraySet[execFinal["ArgumentArrays", #1], #2]&, data];	
	(* Do forward pass *)
	MXExecutorForward[execFinal["Executor"], False];
	
	(* deal with outputs *)
	outputs = execFinal["OutputArrays"];
	If[IntegerQ@pad,  (* check whether we need to do slicing *)
		outputs = NDArraySlice[#, 1, pad]& /@ outputs
	];
	
	(* deal with non-return input case *)
	If[returnInputs === False, Return@outputs];
	
	(* deal with return input case *)		
	inputs = data;
	If[IntegerQ@pad, (* check whether we need to do slicing *)
		inputs = NDArraySlice[#, 1, pad]& /@ inputs
	];	
	<|
		"Outputs" -> outputs,
		"Inputs" -> inputs
	|>
]

(******************************************************************************)
PackageExport["MXEvaluateND"]

MXSetUsage[MXEvaluateND,
"MXPredict[MXExecutorData[$$], input$] does a forward pass on a network \
defined by the executor MXExecutorData[$$] using the Association of input NDArray's input$. 
Note1: this function assumes all inputs and outputs are batched.
Note2: this function automatically batches the input. The following options are available:

'Context' -> {'CPU', 0}: the context of the output NDArray's.
'DataType' -> 'Real32': the data type of the output NDArray's.
'ExecutorReshape' -> use executor resizing to evaluate data smaller than MXExecutor[$$] is \
initialized to deal with. If False, padding will be used, which may be slower.
"]

Options[MXEvaluateND] = {
	"Context" :> $DefaultContext,
	"DataType" -> "Real32",
	"ExecutorReshape" -> True
}

MXEvaluateND[executor_MXExecutorData, input_Association, OptionsPattern[]] := CatchFailure @ Scope[
	UnpackOptions[context, dataType, executorReshape];
	
	(* Create outputs *)
	firstInputName = First@Keys@input;
	batchSizeData = NDArrayLength@input[firstInputName];
	batchSizeExecutor = NDArrayLength@executor["ArgumentArrays", firstInputName];
	outputDimExec = Dimensions /@ executor["OutputArrays"];
	outputDim = Join[{batchSizeData}, Rest@#]& /@ outputDimExec;
	
	(* Create data iterator *)
	paddingMethod = If[executorReshape, None, "RandomChoice"];
	iter = 
		MXPredictIterator[executor, input, "PaddingMethod" -> paddingMethod, "ReturnInputs" -> False];

	(* Create output *)
	outputs = NDArrayCreateEmpty[#, "Context" -> context, "DataType" -> dataType]& /@ outputDim;
	outputsSliced = NDArrayBatchSlice[#, batchSizeExecutor]& /@ outputs;
	(* Loop over iterator *)
	Do[
		outsIteration = Read@iter;
		If[outsIteration === IteratorExhausted, Break[]];
		outputArrays = #[[i]]& /@ outputsSliced;
		NDArraySet[outputArrays, outsIteration];
		,
		{i, Infinity}
	];
	outputs
]	


(******************************************************************************)
PackageExport["MXEvaluate"]

MXSetUsage @ "
MXEvaluate[MXExecutorData[$$], <|key$1 -> {$$}, $$|>, {encoder$1, $$}, {outname$1, $$}] \
does a forward pass on a network defined by the executor MXExecutorData[$$] using the Association of \
input data <|key$1 -> {$$}, $$|>. The encoders {encoder$1, $$} are applied to the input data in canonical order. \
Only the outputs in {outname$1, $$} are returned as a list in the same order. 
"

MXEvaluate[executorData_MXExecutorData, input_Association, encoder_List, outNames_List, ndsetter_:NDArraySet] := Scope[
	(* Create outputs *)
	inputs = Values@input;
	inputND = Lookup[executorData["ArgumentArrays"], Keys[input]];
	outputND = Lookup[executorData["OutputArrays"], outNames];
	executor = executorData["Executor"];
	(* get batch dims: assume there is at least one input array *)	
	batchSizeData = Length@First@inputs;
	batchSizeExecutor = NDArrayLength@First@inputND;
	(* Will store outputs in Bag *)
	outputBags = Table[Internal`Bag[], Length@outputND];
	(* Loop over slices *)
	Do[
		(* Get data slice + apply encoders *)
		maxIndex = Min[batchSizeData, batchSizeExecutor * i];
		minIndex = batchSizeExecutor * (i - 1) + 1;
		inputSlice = #[[minIndex ;; maxIndex]]& /@ inputs;
		inputSlice = MapThread[#1[#2]&, {encoder, inputSlice}];
		(* Check whether to use Executor resizing *)
		If[(maxIndex - minIndex + 1) < batchSizeExecutor,
			sliceDims = Dimensions /@ inputSlice;
			executorDataReshape = MXExecutorReshape[executorData, AssociationThread[Keys@input -> sliceDims]];
			inputND = executorDataReshape["ArgumentArrays", #]& /@ Keys@input;
			outputND = executorDataReshape["OutputArrays", #]& /@ outNames;
			executor = executorDataReshape["Executor"];
		];	
		(* Do forward pass *)
		ndsetter[inputND, inputSlice];
		MXExecutorForward[executor, False];
		(* add output to bags *)
		MapThread[Internal`StuffBag[#1, Normal@#2, 1]&, {outputBags, outputND}];
		,
		{i, 1, Ceiling[batchSizeData/batchSizeExecutor]}
	];
	(* return outputs *)
	Internal`BagPart[#, All]& /@ outputBags
]	

(******************************************************************************)

(* Utility function: loads a model parameter file into form 
<| \"AuxilliaryArrays\" -> NDArray[...], \"ArgumentArrays\" -> NDArray[...]|>
*)

PackageExport["MXModelLoadParameters"]

MXSetUsage @ "
MXModelLoadParameters[file$] loads a MXNet parameter file, returning an association \
<| 'AuxilliaryArrays' -> {$$}, 'ArgumentArrays' -> {$$} |>.
"

Options[MXModelLoadParameters] =
{
	"Context" :> $DefaultContext,
	"DataType" -> "Real32" 
};

MXModelLoadParameters[parameterFile_, opts:OptionsPattern[]] := CatchFailure @ Scope[
	UnpackOptions[context, dataType];	
	params = NDArrayImport[parameterFile];
	If[FailureQ[params], Return[params]];
	params = KeyMap[StringSplit[#, ":"]&, params];
	
	auxParams = KeySelect[params, (First@# == "aux")&];
	argParams = KeySelect[params, (First@# == "arg")&];
	auxParams = Association@KeyValueMap[
		(Last@#1 -> NDArrayCreate[#2, "Context" -> context, "DataType" -> dataType])&, 
		auxParams
	];
	argParams = Association@KeyValueMap[
		(Last@#1 -> NDArrayCreate[#2, "Context" -> context, "DataType" -> dataType])&, 
		argParams
	];
	<|"ArgumentArrays" -> argParams, "AuxilliaryArrays" -> auxParams|>
]


