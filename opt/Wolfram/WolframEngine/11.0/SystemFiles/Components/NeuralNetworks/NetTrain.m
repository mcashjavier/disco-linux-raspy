Package["NeuralNetworks`"]

PackageImport["GeneralUtilities`"]
PackageImport["MXNetLink`"]


PackageExport["NetTrain"]

Options[NetTrain] = {
	BatchSize -> Automatic,
	MaxTrainingRounds -> Automatic,
	Method -> Automatic,
	TargetDevice -> "CPU",
	ValidationSet -> None,
	"ShowTrainingProgress" -> True
};

NetTrain[net_, data_, opts:OptionsPattern[]] := NetTrain[net, data, Automatic, opts];

$NetTrainMethods := $NetTrainMethods = <|
	"ADAM" -> {{"ADAM", #}&, <|
		"InitialLearningRate" -> Defaulting[IntervalScalarT[0,1], 0.001],
		"Beta1" -> Defaulting[IntervalScalarT[0,1], 0.93],
		"Beta2" -> Defaulting[IntervalScalarT[0,1], 0.999],
		"Epsilon" -> Defaulting[IntervalScalarT[0,0.01], 10^-8],
		"L2Regularization" -> Defaulting[ScalarT, 0.]
	|>},
	"StochasticGradientDescent" -> {{"SGD", #}&, <|
		"InitialLearningRate" -> Defaulting[IntervalScalarT[0,1], 0.001],
		"Momentum" -> Defaulting[IntervalScalarT[0.,1.], 0.93],
		"RescaleGradient" -> Defaulting[ScalarT, 1],
		"GradientClipping" -> Defaulting[EitherT[{ScalarT, MatchT[None]}], None],
		"L2Regularization" -> Defaulting[ScalarT, 0.0],
		"LearningRateSchedule" -> Defaulting[ExpressionT, "Polynomial"]
	|>}
|>;

AverageNorm[arr_NDArray] := AverageNorm[Normal[arr]];
AverageNorm[arr_] := RootMeanSquare[Flatten[arr]];

NetTrain::invnet = "First argument to NetTrain should be a fully specified net."
NetTrain::invrt = "Invalid setting for option TargetDevice. TargetDevice should be either \"CPU\" or \"GPU\"."
NetTrain::incmptrloss = "Given training data spec can only be used when net has one input and one output."
NetTrain::nodata = "No training data provided."
NetTrain::contmiss = "NetTrain does not currently support data that contains missing values."
NetTrain::invdataset = "Datasets provided to NetTrain must consist of a list of associations with fixed keys."

processData[data_, net_] := Scope[
	
	If[data === {} || data === <||>, ThrowFailure["nodata"]];

	If[Head[data] === Rule && Not[Length[Inputs[net]] == 1 && Length[Outputs[net]] == 1],
		ThrowFailure["incmptrloss"];
	];

	If[Head[data] === Dataset,
		If[!MatchQ[Dataset`GetType[data], TypeSystem`Vector[_TypeSystem`Struct, _]],
			ThrowFailure["invdataset"]
		];
		data = AssociationTranspose @ Normal[data];
	];

	If[Developer`AssociationVectorQ[data],
		data = AssociationTranspose @ data;
	];

	If[VectorQ[data, RuleQ], 
		data = Keys[data] -> Values[data];
	];

	If[TrueQ @ Catch @ checkForMissing[data], ThrowFailure["contmiss"]];

	data
];

checkForMissing[assoc_Association] := Scan[checkForMissing, Values[assoc]]; 
checkForMissing[list_List] := If[!Developer`PackedArrayQ[list], Scan[checkForMissing, list]];
checkForMissing[a_ -> b_] := (checkForMissing[a]; checkForMissing[b]);
checkForMissing[_Missing] := Throw[True];
checkForMissing[_] := Null;

PackageExport["$EarlyStopping"]
$EarlyStopping = True;

NetTrain::unspecloss = "Provided loss layer is not fully specified.";

NetTrain::noparams = "Net contains no trainable parameters."

General::optpi = "The value of `` -> `` should be a positive machine-sized integer."

NetTrain[net_, data_, lossSpec_:Except[_Rule], OptionsPattern[]] := CatchFailure @ Scope @ Internal`WithLocalSettings[
	Null
	,
	If[!ConcreteNetQ[net], ThrowFailure["invnet"]];

	(* TODO: canonicalize input data 'data', first, and complain if completely malformed *)

	data = processData[data, net];

	dataLen = GetDataLength[data];

	{trainnet, prefix, lossports} = AttachLoss[net, lossSpec];

	If[!ConcreteNetQ[trainnet], ThrowFailure["unspecloss"]];

	UnpackOptions[method, batchSize, maxTrainingRounds, targetDevice, showTrainingProgress, validationSet];

	$DefaultContext = Match[targetDevice,
		"CPU" :> {"CPU", 0},
		"GPU" :> {"GPU", 0},
		dev:{"GPU", _Integer} :> dev,
		ThrowFailure["invrt"];
	];
	TestContext @ $DefaultContext;

	trainnet = NetInitialize[trainnet];
	batchSize = Replace[batchSize, Automatic -> 64];

	If[!Internal`PositiveMachineIntegerQ[batchSize],
		ThrowFailure["optpi", BatchSize, batchSize];
	];

	batchSize = Min[batchSize, dataLen];
	{executor, auxArrayMapping, inputs, outputs, encoders, decoders} = 
		ToMXNetExecutor[trainnet, lossports, batchSize, "Write", $DefaultContext];

	inputRate = 0;
	batchesPerRound = Ceiling[dataLen / batchSize];

	trainnet = Null; (* ensure the temp net gets garbage collected *)

	iteratorFactories = parseTrainingSpec[data, inputs, encoders];
	If[validationSet =!= None,
		doValidation = True;
		validationDir = CreateDirectory[];
		validationSet = processData[validationSet, net];
		validationFactories = parseTrainingSpec[validationSet, inputs, encoders];
		,
		doValidation = False;
	];

	mxSymbol = executor["Symbol"];
	mxGradients = executor["GradientArrays"];
	mxArguments = executor["ArgumentArrays"];
	mxAuxArrays = executor["AuxilliaryArrays"];

	grads = Values[mxGradients];
	params = Values[mxArguments];
	paramKeys = Keys[mxArguments];

	loss = None;
	meanloss = None;
	batchNumber = 0;
	roundLoss = 0;
	roundNumber = 0;
	finalRoundLoss = $none;
	currentLoss = $none;
	batchCount = 0;
	validationLoss = $none;
	bestFile = None;
	bestValidationLoss = Infinity;
	validationLossHistory = Internal`Bag[];
	lossHistory = Internal`Bag[];

	realParamIndices = SelectIndices[paramKeys, StringFreeQ[".Input"]];
	realParams = params[[realParamIndices]];
	savedParams = Join[
		mxArguments[[realParamIndices]],
		KeyMap[auxArrayMapping, mxAuxArrays]
	];
	realGrads = grads[[realParamIndices]];
	
	(* nothing to train *)
	If[Length[realGrads] === 0, 
		Message[NetTrain::noparams];
		Return[net]];

	numberParams = None;
	If[method === Automatic,
		numberParams = getParamCount[realParams];
		If[numberParams < 128,
			method = "StochasticGradientDescent";
		]
	];

	lossArrays = Values[outputs];
	unitLosses = NDArrayCreateEmpty[NDArrayDimensions[#]]& /@ lossArrays;
	
	gradSale = 1.0 / batchSize;
	Scan[NDArraySet[#, gradSale]&, unitLosses];
	Scan[NDArraySet[#, 0.]&, grads];

	If[maxTrainingRounds === Automatic, 
		columnIterators = Through[iteratorFactories[]];
		lastTime = AbsoluteTime[];
		Catch[
			advanceIterators[columnIterators],
			EncodeFail,
			Return[$Failed, Block]&
		];
		MXExecutorForward[executor, True];
		MXExecutorBackward[executor, unitLosses];
		columnIterators = None;
		Scan[Normal, lossArrays];
		NDArrayWaitForAll[];
		timeForBatch = Max[AbsoluteTime[] - lastTime, 0.00001];
		timeForRound = 2 * timeForBatch * batchesPerRound;
		maxTrainingRounds = Clip[Ceiling[20. / timeForRound], {10, 10000}];
	];

	If[!Internal`PositiveMachineIntegerQ[maxTrainingRounds],
		ThrowFailure["optpi", MaxTrainingRounds, maxTrainingRounds];
	];

	maxBatches = maxTrainingRounds * batchesPerRound;

	optimizerData = ParseMethod[method, $NetTrainMethods];

	stop = False;
	lastLossHistoryPlot = ToBoxes @ LossHistoryPlot[{}, {}, 1];

	lastCheckDivergenceTime = lastValidationTime = lastPlotTime = startTime = AbsoluteTime[];
	progress = 0.;

	Which[
		showTrainingProgress === "Force",
			showTrainingProgress = True,
		TrueQ[$CloudEvaluation] || MatchQ[$LicenseType, "Player" | "Player Pro"] || !$Notebooks,
			(* see 318529 *)
			showTrainingProgress = False
	];

	If[showTrainingProgress,
	cell = PrintTemporary @ TrainingBox[{
		None :> Item[ProgressIndicator[progress], Alignment -> Center],
		"round" :> Row[{roundNumber, " / ", maxTrainingRounds}],
		If[batchesPerRound > 1, "batch" :> Row[{batchNumber, " / ", batchesPerRound}], Nothing],
		"inputs/second" :> Round[inputRate],
		"time elapsed" :> TimeString[AbsoluteTime[] - startTime],
		"time remaining" :> If[progress == 0, "", TimeString[(1.0 - progress) / (progress / (AbsoluteTime[] - startTime))]],
		"batch loss" :> NumberForm[currentLoss, 3],
		"round loss" :> NumberForm[finalRoundLoss, 3],
		If[doValidation, "validation loss" :> NumberForm[validationLoss, 3], Nothing],
		None :> RawBoxes[lastLossHistoryPlot],
		None -> NiceButton["Stop", stop = True]
		},
		"Training Progress"
	]];

	optimizer = CreateOptimizer[optimizerData[[1]], realParams, realGrads, maxBatches, optimizerData[[2]]];
	
	plotBatchFactor = 1;
	lastRoundTime = lastTime = Indeterminate;
	meanrate = 0;
	stuckTime = 0;

	Catch[
	Do[
		batchNumber = 0;
		roundLoss = 0;
		roundNumber++;
		columnIterators = Through[iteratorFactories[]];
		lastRoundTime = First @ AbsoluteTiming @ While[!stop, lastTime = First @ AbsoluteTiming[
			If[!advanceIterators[columnIterators], Break[]];
			If[stop, Break[]];
			batchNumber++; batchCount++; progress = N[batchCount / maxBatches];
			inputRate = batchSize / lastTime;
			If[inputRate > 0, If[meanrate === 0, 
				meanrate = inputRate,
				meanrate = 0.8 * meanrate + 0.2 * inputRate;
			]];
			PreemptProtect[
				MXExecutorForward[executor, True];
				MXExecutorBackward[executor, unitLosses];
				RunOptimizerStep[optimizer];
			];
			If[stop, Break[]];
			NDArrayWaitForAll[];
			PreemptProtect[
				loss = getMeanLoss[lossArrays];
			];
			currentLoss = loss;
			Internal`StuffBag[lossHistory, Log @ Max[loss, $MachineEpsilon]];
			roundLoss += loss;
			If[Mod[batchCount, plotBatchFactor] == 0 && AbsoluteTime[] - lastPlotTime > 1.0,
				lastPlotTime = AbsoluteTime[];
				lastLossHistoryPlot = ToBoxes @ LossHistoryPlot[
					Internal`BagPart[lossHistory, All], 
					Internal`BagPart[validationLossHistory, All],
					plotBatchFactor
				];
				plotBatchFactor = Ceiling[batchCount / 125.];
			];
		]];
		If[stop, Break[]];
		lastFinalRoundLoss = finalRoundLoss;
		finalRoundLoss = roundLoss / batchNumber;
		If[$EarlyStopping,
			If[finalRoundLoss == lastFinalRoundLoss,
				stuckTime += lastRoundTime;
				If[stuckTime > 1.0, Break[]],
				stuckTime = 0
			];
		];
		If[AbsoluteTime[] - lastCheckDivergenceTime > 0.2, 
			checkDivergence[realParams];
			lastCheckDivergenceTime = AbsoluteTime[];
		];
		If[doValidation && AbsoluteTime[] - lastValidationTime > 0.2,
			validationIterators = Through[validationFactories[]];
			nextValidationLoss = 0;
			lossBatches = 0;
			While[advanceIterators[validationIterators],
				lossBatches++;
				PreemptProtect[
					MXExecutorForward[executor, True];
					NDArrayWaitForAll[];
					nextValidationLoss += getMeanLoss[lossArrays];
				];
			];
			lastValidationTime = AbsoluteTime[];
			validationLoss = nextValidationLoss / lossBatches;
			Internal`StuffBag[validationLossHistory, {batchCount, Log @ validationLoss}];
			If[validationLoss < bestValidationLoss, 
				file = FileNameJoin[{validationDir, IntegerString[round, 10, 5]}];
				PreemptProtect @ NDArrayExport[file, savedParams];
				bestFile = file;
				bestValidationLoss = validationLoss
			]
		];
	,
		{round, maxTrainingRounds}
	];
	,
	EncodeFail,
	Return[$Failed, Block]&];
	NDArrayWaitForAll[];
	checkDivergence[realParams];
	If[showTrainingProgress, NotebookDelete[cell]];
	$LastLossHistory ^= Internal`BagPart[lossHistory, All];
	If[doValidation,
		$LastValidationLossHistory ^= Internal`BagPart[validationLossHistory, All];
	];
	(* put the new weights back into the original net, taking care of the
	prefixy, if any, introduced by adding the loss *)
	netdata = Normal[net];
	portPattern = Append[prefix, p___];

	If[doValidation && StringQ[bestFile],
		PreemptProtect[
			savedParams = NDArrayImport[bestFile];
		];
	];
	KeyValueScan[Function[{name, array},
		Match[MXUnmanglePort[name], 
			NetPort["Inputs", ___] :> Null, (* <- obselete *)
			portPattern :> Set[netdata[p], Replace[array, nd_NDArray :> NDArrayGet[nd]]],
			Null
		]],
		savedParams
	];
	System`Private`ConstructNoEntry[Head[net], netdata]
	,
	If[StringQ[validationDir] && DirectoryQ[validationDir],
		DeleteDirectory[validationDir, DeleteContents -> True];
	];
];

getParamCount[arrays_] := Total @ Map[Times @@ NDArrayDimensions[#]&, arrays];

getMeanLoss[lossArrays_] := Total @ Map[Mean[Total[Normal[#], {2, Infinity}]]&, lossArrays];

NetTrain::netdiverge = "Parameters of net diverged during training, try specifying a lower InitialLearningRate."
checkDivergence[arrays_] := Scope[
	Do[
		max = Abs[Max[Normal[array]]];
		If[max === $MaxMachineNumber, ThrowFailure["netdiverge"]],
		{array, arrays}
	]
];

$none = Style["\[Dash]", Gray];

advanceIterators[iters_] := FreeQ[GeneralUtilities`PackageScope`PullIterator /@ iters, IteratorExhausted];

PackageExport["$LastLossHistory"]
PackageExport["$LastValidationLossHistory"]

(* this is available as a stop-gap, eventually you'll be able to get a NetTrainObject back 
with this info in it *)
$LastLossHistory = None;
$LastValidationLossHistory = None;

PackageExport["TrainingBox"]

keystyle[x_] := Style[x, GrayLevel[0.4]];	

$TrainingBoxColor = RGBColor[0.9802, 0.9802, 0.9802];

TrainingBox[data_, title_] := Scope[
	grid = Dynamic[
		Grid[
			If[#1 === None, 
					{#2, SpanFromLeft},
					{keystyle[#1], #2}
			]& @@@ data,
			Dividers -> {
				{False, {Opacity[0.15]}, False}, 
				{}
			},
			ColumnWidths -> {Automatic, 15},
			ColumnAlignments -> {Right, Left},
			ColumnSpacings -> {1.6, 2.5},
			RowSpacings -> 2
		],
		TrackedSymbols :> {}, UpdateInterval -> 0.5
	];
	titleItem = Item[
		Framed[
			Style[title, Bold, 12, "SuggestionsBarText"],
			FrameMargins -> {{10,10},{-5,5}},
			FrameStyle -> None
		],
		ItemSize -> {Automatic,1},
		Alignment -> {Left, Bottom},
		FrameStyle -> Opacity[0.1],
		Background -> Darker[$TrainingBoxColor, 0.05],
		Frame -> {{False,False},{True, False}}
	];
	gridItem = Item[
		Framed[grid, FrameMargins -> {{10,10},{10,5}}, FrameStyle -> None],
		BaseStyle -> {FontWeight -> "Light", FontFamily -> CurrentValue["PanelFontFamily"], NumberMarks -> False, Deployed -> False},
		Alignment -> Left	
	];
	Deploy[
		Style[Framed[
			Column[{
				titleItem,
				gridItem
				},
				ColumnWidths -> Automatic,
				ColumnAlignments -> Left,
				RowLines -> False,
				RowSpacings -> {3,1},
				StripOnInput -> True
			],
			Background -> $TrainingBoxColor,
			FrameMargins -> {{0,0},{0,0}},
			FrameStyle -> LightGray,
			RoundingRadius -> 5
		], LineBreakWithin -> False]
	]
];


PackageExport["LossHistoryPlot"]

LossHistoryPlot[e_ /; Length[e] <= 2, _, _] := Spacer[{250,100}];

LossHistoryPlot[trainingLoss_, validationLoss_, batchFactor_] := Scope[
	n = Length[trainingLoss];
	dividers = chooseDividers[n];
	coords = makeLinePlot[trainingLoss, batchFactor];
	max = Max[coords[[All, 2]]]; 
	Graphics[
		{
			AbsoluteThickness[1.25], 
			Text[Round[#], Offset[{-2,0}, {#, max}], {1.,0.}]& /@ dividers,
			{Orange, Line[coords]},
			If[Length[validationLoss] < 25, 
				{Hue[.59, .9, .6], Point[validationLoss]}, 
				Nothing
			],
			{Hue[.59, .9, .9], Dotted, Line[validationLoss]}
		},
		ImageSize -> {250,100}, Frame -> True,
		AspectRatio -> Full, 
		BaseStyle -> {FontFamily -> "Verdana", FontSize -> 8, FontColor -> Gray},
		PlotRange -> {{0, n}, All},
		GridLines -> {dividers, None},
		PlotRangePadding -> {0, Scaled[0.05]},
		PlotRangeClipping -> True,
		FrameStyle -> Gray,
		FrameTicks -> None
	]
];

chooseDividers[n_] := Scope[
	{man,exp} = MantissaExponent[N[n]];
	Which[
		man > 0.5, {0.2, 0.4, 0.6, 0.8, 1.0},
		man > 0.3, {0.1, 0.2, 0.3, 0.4, 0.5},
		man > 0.15, {0.05, 0.1, 0.15, 0.2, 0.25, 0.3},
		man == 0.10, {0.02, 0.04, 0.06, 0.08, 0.1},
		True, {0.025, 0.05, 0.075, 0.100, 0.125, 0.150}
	] * Power[10, exp]
];

makeLinePlot = Compile[{{loss, _Real, 1}, {b, _Integer}}, 
	Table[
		{i-1 + (b/2), Mean[loss[[i ;; i+b-1]]]}, 
		{i, 1, Max[Length[loss]-1, 1], b}
	]
];


General::invinlen = "Inconsistent numbers of examples provided to ports.";
General::invincnt = "Input spec cannot be used with the given net."
General::invsimplein = "Input spec of the form input -> output can only be used on a network with a single input and a single output."

parseTrainingSpec[inputs_List -> outputs_List, KeyValuePattern[{"Input" -> inarray_, name_ -> outarray_}], encoders_] := (
	If[Length[inputs] != Length[outputs], ThrowFailure["invinlen"]];
	If[Length[encoders] =!= 2, ThrowFailure["invsimplein"]];
	MapThread[
		makeNDArrayEpochIterator,
		{{inarray, outarray}, encoders, {inputs, outputs}, {"Input", name}}
	]
);

parseTrainingSpec[_Rule, inputs_, _] := ThrowFailure["invincnt"];

NetTrain::invindim = "Data provided to port `` should be a list of numeric arrays of dimensions ``."

makeNDArrayEpochIterator[ndarray_, Identity, data_, name_] := Scope[
	datadims = MachineArrayDimensions[data];
	nddims = Rest @ NDArrayDimensions[ndarray];
	If[FailureQ[datadims] || nddims =!= Rest[datadims],
		ThrowFailure["invindim", name, nddims]];
	NDArrayEpochIterator[ndarray, Identity, data]
];

makeNDArrayEpochIterator[ndarray_, enc_, data_, name_] :=
	NDArrayEpochIterator[ndarray, enc, data];

(* TODO: Shift length checks into here! *)

NetTrain::invtdata = "Training data should be an association of lists, or a rule from input to output examples."

GetDataLength[slots_Association /; AllTrue[slots, ListQ]] := 
	Length @ First @ slots

GetDataLength[a_List -> b_List] := Length[a];

GetDataLength[_] := ThrowFailure["invtdata"];


General::missinslot = "Specification for slot `` is missing.";
General::invinslot = "No slot named `` is present."

parseTrainingSpec[slots_Association, inarrays_, encoders_] := Scope[
	If[Length[inarrays] =!= Length[slots], ThrowFailure["invincnt"]];
	If[!AllSameBy[Values[slots], Length], General::invinlen];
	MapAssocAssoc[
		makeNDArrayEpochIterator[#2, Each[encoders], #3, #1]&,
		inarrays, slots,
		ThrowFailure["missinslot", #]&,
		ThrowFailure["invinslot", #]&
	]
];

parseTrainingSpec[input_, _, _] :=
	ThrowFailure["invtdata"];

mangle[field_, assoc_Association, keys_] := Scope[
	If[Length[assoc] =!= Length[keys], ThrowFailure["invincnt"]];
	KeyValueMap[Function[{k, v},
		mangledKey = MXManglePort[NetPort[field, k]];
		If[!MemberQ[keys, mangledKey], ThrowFailure["nvinslot", k]];
		mangledKey -> toNDArray[v]
	], assoc]
];