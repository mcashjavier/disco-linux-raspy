Package["MXNetLink`"]

PackageImport["GeneralUtilities`"]

(****** Symbol Exports ******)

PackageExport["MXExecutor"]

(******************************************************************************)

(****** Load Library Functions ******)

DeclareLibraryFunction[mxExecutorBind, "WL_MXExecutorBind", 
	{
		Integer,					(* symbol_handle *)
		Integer,					(* dev_type *)
		Integer,					(* dev_id *)
		{Integer, 1, "Constant"}, 	(* in_args *)					
		{Integer, 1, "Constant"}, 	(* arg_grad_store *)	
		{Integer, 1, "Constant"}, 	(* grad_req_type *)	
		{Integer, 1, "Constant"}, 	(* aux_states *)	
		Integer 					(* executor handle *)
	}, 
	"Void"						
]	

DeclareLibraryFunction[mxExecutorBindEX, "WL_MXExecutorBindEX", 
	{
		Integer,					(* symbol_handle *)
		Integer,					(* dev_type *)
		Integer,					(* dev_id *)
		"UTF8String", 				(* ctxkeys *)		
		{Integer, 1, "Constant"}, 	(* map_dev_types *)		
		{Integer, 1, "Constant"}, 	(* map_dev_ids *)		
		{Integer, 1, "Constant"}, 	(* in_args *)					
		{Integer, 1, "Constant"}, 	(* arg_grad_store *)	
		{Integer, 1, "Constant"}, 	(* grad_req_type *)	
		{Integer, 1, "Constant"}, 	(* aux_states *)	
		Integer, 					(* shared executor handle *)
		Integer 					(* executor handle *)
	}, 
	"Void"						
]	

DeclareLibraryFunction[mxExecutorPrint, "WL_MXExecutorPrint", 
	{
		Integer 	(* executor handle *)
	}, 
	"UTF8String"						
]

DeclareLibraryFunction[mxExecutorOutputs, "WL_MXExecutorOutputs", 
	{
		Integer, 		(* executor handle *)
		{Integer, 1} 	(* output array handles *)
	}, 
	"Void"						
]	

DeclareLibraryFunction[mxExecutorForward, "WL_MXExecutorForward", 
	{
		Integer, 		(* executor handle *)
		Integer 		(* bool, train -> 1 or not train -> 0  *)
	}, 
	"Void"						
]

DeclareLibraryFunction[mxExecutorBackward, "WL_MXExecutorBackward", 
	{
		Integer, 		(* executor handle *)
		{Integer, 1} 	(* output gradients  *)
	}, 
	"Void"						
]

(******************************************************************************)
PackageExport["MXSymbolBind"]

MXSetUsage @ "
MXSymbolBind[MXSymbol[$$], args$, opts$] instantiates the Variable symbols in the provided MXSymbol \
with NDArrays and returns both an MXExecutor[$$] object and an association of initialized arrays.
An association of arguments args$ can consist of RawArrays, dimension lists, or NDArray's. If NDArrays are \
specified, they will be used without copying. RawArrays will be copied. Sufficient args$ (or their shapes) must be specified to infer \
all other shapes. \n
The following options are available:
'Context' -> {'CPU', 0}: the context of the output executor, along with the automatically generated NDArray's.
'DataType' -> 'Real32': the type of the automatically generated NDArray's bound to the Executor. 
'AuxilliaryArrays' -> <||>: supply auxilliary arrays to bind to Executor.
'ContextGroup' -> <||>: The dict mapping the con attribute to the context assignment.
'SharedExecutor' -> None: Executor to share memory with. This is intended for runtime reshaping, \
variable length sequences, etc. The returned executor shares state with shared_exec, and should not be \
used in parallel with it.
'GradientUpdateMethod' -> 'Write': ('Write', 'Add', None) specifies what method to use to update gradients during \
a backward pass. Can also be an Association specifying different update rules per bound NDArray. 
"

MXSymbolBind::CannotInferShapes = 
	"The shapes of all auxilliary and argument arrays cannot be inferred from the given information."

MXSymbolBind::InvGradUpdate = 
	"`` is an invalid gradient update specification. Must be one of {Add, Write, Null}."
MXSymbolBind::InvNumGradUpdateMethod = 
	"`` gradient update method specifications were given, `` are required."

Options[MXSymbolBind] =
{
	"Context" :> $DefaultContext,
	"DataType" -> "Real32",
	"AuxilliaryArrays" -> <||>,
	"GradientArrays" -> None,
	"ContextGroup" -> <||>, 
	"SharedExecutor" -> None,
	"GradientUpdateMethod" -> "Write" (* can also be "Add" or None, or association of these *)
};

MXSymbolBind[symbol_MXSymbol, argumentArrays_Association, opts:OptionsPattern[]] := Scope[
	UnpackOptions[
		context, dataType, gradientArrays,
		auxilliaryArrays, gradientUpdateMethod,
		contextGroup, sharedExecutor
	];
	(* Infer Shape of all Arrays *)
	knownShapes = If[ListQ@#, #, Dimensions@#]& /@ argumentArrays;
	inferredShapes = MXSymbolInferShape[symbol, knownShapes];
	If[FailureQ@inferredShapes, Return@inferredShapes];
	(* check that all shapes have been inferred *)
	If[Not@inferredShapes["Complete"], ReturnFailed["CannotInferShapes"]];

	(* Need to keep in canonical order *)
	options = Sequence["DataType" -> dataType, "Context" -> context];
	mxArgumentArrays = Join[inferredShapes["ArgumentArrays"], argumentArrays];
	mxAuxilliaryArrays = Join[inferredShapes["AuxilliaryArrays"], auxilliaryArrays];	

	initArrays = Function[array, Which[
		NDArrayQ@array, array,
		Developer`RawArrayQ@array, NDArrayCreate[array, options],
		ListQ@array,  NDArrayCreateZero[array, options]
	]];
	
	(* Initialize argument + aux arrays *)
	mxArgumentArrays = initArrays /@ mxArgumentArrays;
	mxAuxilliaryArrays = initArrays /@ mxAuxilliaryArrays;
	
	(* If single gradient update method given, use for all*)
	If[Not@AssociationQ@gradientUpdateMethod,
		gradientUpdateMethod = Association@Thread[Keys@mxArgumentArrays -> gradientUpdateMethod],
		(* otherwise, ensure canonical ordering + completeness *)
		gradientUpdateMethodTemp = Association@Thread[Keys@mxArgumentArrays -> None];
		gradientUpdateMethod = Join[gradientUpdateMethodTemp, gradientUpdateMethod]
	];
	
	(* init gradients: three cases, 1) supplied gradient NDArray 2) Null grad 3) need to init to zero *)
	gradInitFunc = Function[key,
		Which[
			gradientUpdateMethod[key] === None, 
				None,
			(gradientArrays =!= None) && KeyExistsQ[gradientArrays, key], 
				gradientArrays[key],
			True, 
				NDArrayCreateZero[inferredShapes["ArgumentArrays", key], options]
		]
	];
	mxGradientArrays = AssociationMap[gradInitFunc, Keys@mxArgumentArrays];
	(* check whether existing executor can be used. If not, create null executor *)
	If[sharedExecutor === None, 
		sharedExecutor = CreateManagedLibraryExpression["MXExecutor", MXExecutor]
	];

	mxSymbolBindFast[
		symbol, context, contextGroup, mxArgumentArrays, 
		mxGradientArrays, mxAuxilliaryArrays, gradientUpdateMethod,
		sharedExecutor
	]
]

(* fast binding: assumes all preprocessing is already done. Used internally *)
mxSymbolBindFast[
		symbol_MXSymbol, context_List, contextGroup_, 
		argArrays_Association, gradArrays_Association, auxArrays_Association, 
		gradUpdateMethod_Association, sharedExecutor_MXExecutor] := Scope[
	(* Process context group *)
	ctxKeys = Developer`WriteRawJSONString@Keys@contextGroup;
	ctxDeviceTypeID = toDeviceCode /@ Values[contextGroup][[;;, 1]];
	ctxID = Values[contextGroup][[;;, 2]];
	(* Create Output executor *)
	executorHandle = System`Private`SetNoEntry@CreateManagedLibraryExpression["MXExecutor", MXExecutor];
	
	(* Deal with some grad arrays being None. Use null array in this case *)
	$nullArray = CreateManagedLibraryExpression["NDArray", NDArray];
	newGradArrays = If[# === None, $nullArray, #]& /@ gradArrays;

	(* bind *)
	MXNetInvoke[
		mxExecutorBindEX, 
		MLEID@symbol, 
		toDeviceCode@First[context], 
		Last@context, 
		ctxKeys, 
		ctxDeviceTypeID,
		ctxID,
		MLEID /@ Values[argArrays], 
		MLEID /@ Values[newGradArrays],
		$GradientUpdateCode /@ Values[gradUpdateMethod],
		MLEID /@ Values[auxArrays],
		MLEID@sharedExecutor,
		MLEID@executorHandle
	];

	(* Outputs are given to us by mxExecutorOutputs *)
	outputs = MXSymbolOutputs@symbol;
	If[FailureQ@outputs, Return@outputs];
	outputHandles = Table[
		System`Private`SetNoEntry@CreateManagedLibraryExpression["NDArray", NDArray], 
			{Length@outputs}
		];
	outputArrays = AssociationThread[outputs, outputHandles];
	MXNetInvoke[mxExecutorOutputs, MLEID@executorHandle, MLEID /@ outputHandles];

	(* Replace Null array with None *)
	newGradArrays = If[(MLEID@# === MLEID@$nullArray), None, #]& /@ newGradArrays;
	
	(* return executor object *)
	System`Private`SetNoEntry @ MXExecutorData[
		<|
		"Executor" -> executorHandle,
		"Symbol" -> MXSymbolCopy@symbol, (* copy original symbol *)
		"Context" -> context,
		"ContextGroup" -> contextGroup,
		"GradientArrays" -> newGradArrays,
		"GradientUpdateMethod" -> gradUpdateMethod,
		"OutputArrays" -> outputArrays,
		"AuxilliaryArrays" -> auxArrays,
		"ArgumentArrays" -> argArrays
		|>
	]
]

(******************************************************************************)
(* Define MXExecutorData object *)

PackageExport["MXExecutorData"]

(* Custom Display form *)

DefineCustomBoxes[MXExecutorData,
	exec_MXExecutorData ? System`Private`NoEntryQ :> MXExecutorDataBoxes[exec]
];

MXExecutorDataBoxes[exec:MXExecutorData[data_]] := Scope[
	plot = Dynamic[MXSymbolPlot[data@"Symbol"], TrackedSymbols :> {}];
	context = data["Context"];
	execID = ManagedLibraryExpressionID@data["Executor"];
	args = MXSymbolArguments[sym];
	arrays = Lookup[data, {"GradientArrays", "ArgumentArrays", "OutputArrays", "AuxilliaryArrays"}];
	counts = MapThread[makeCount, {{"gradients", "arguments", "outputs", "auxilliaries"}, arrays}];
	BoxForm`ArrangeSummaryBox[
		MXExecutorData,
		exec,
		None,
		{makeItem["ID", execID], 
		 makeItem["Context", context],
		 makeItem["Arrays", Column[counts, Spacings -> 0.2]]
		},
		{makeItem["Plot", plot]},
		StandardForm
	]
];

makeCount[label_, arrays_] := Row[{Length[arrays], " ", label}];

makeItem[name_, value_] := BoxForm`MakeSummaryItem[{Pane[name <> ": ", {60, Automatic}], value}, StandardForm];

(* Normal Form *)

MXExecutorData /: Normal[MXExecutorData[data_]] := data

(* Query *)
MXExecutorData[data_][query__String] := data[query]

(******************************************************************************)
PackageExport["MXExecutorPrint"]

MXSetUsage @
"MXExecutorPrint[MXExecutor[$$]] returns a string containing the execution plan.
MXExecutorPrint[MXExecutorData[$$]] is equivalent to MXExecutorPrint[MXExecutor[$$]].
";

MXExecutorPrint[executor_MXExecutor] := CatchFailure @ MXNetInvoke[mxExecutorPrint, MLEID@executor]
MXExecutorPrint[executor_MXExecutorData] := MXExecutorPrint[executor["Executor"]]

(******************************************************************************)
PackageExport["MXExecutorForward"]

MXSetUsage @
"MXExecutorForward[MXExecutor[$$], trainMode$] does a forward pass, modifying the output NDArrays of the computation graph.
MXExecutorForward[MXExecutorData[$$]] is equivalent to MXExecutorForward[MXExecutor[$$]]. trainMode$ is True \
if intermediate results in the forward pass need to be memorized, and False if not.
";
		
MXExecutorForward[executor_MXExecutor, trainMode_:False] :=
	MXNetInvoke[mxExecutorForward, MLEID@executor, Boole @ TrueQ @ trainMode];

MXExecutorForward[data_MXExecutorData, trainMode_:False] := 
	MXExecutorForward[data["Executor"], trainMode];

(******************************************************************************)
PackageExport["MXExecutorBackward"]

MXSetUsage @
"MXExecutorBackward[MXExecutor[$$]] does a backward pass to get the gradients.
MXExecutorBackward[MXExecutorData[$$]] is equivalent to MXExecutorBackward[MXExecutor[$$]]. 
Note: Need to do MXExecutorForward[MXExecutor[$$], 'TrainingMode' -> True] first. \
If the output is not a loss symbol, you need to specify NDArray gradient output to do backprop \
using the option 'OutputGradients'. For multiple outputs, give a list of NDArrays in \
canonical order.
";
		
MXExecutorBackward[executor_MXExecutor, outGrads_] := 
	MXNetInvoke[
		mxExecutorBackward, 
		MLEID@executor, 
		Which[
			outGrads === None, {},
			ListQ[outGrads], Map[MLEID, outGrads],
			NDArrayQ[outGrads], {MLEID @ outGrads},
			True, Panic["InvalidOutGrads"]
		]
	];

MXExecutorBackward[data_MXExecutorData, outGrads_] := 
	MXExecutorBackward[data["Executor"], outGrads]

(******************************************************************************)
PackageExport["MXExecutorReshape"]

MXSetUsage @
"MXExecutorReshape[MXExecutor[$$]] does a backward pass to get the gradients.
MXExecutorReshape[MXExecutorData[$$]] is equivalent to MXExecutorReshape[MXExecutor[$$]]. 
Note: Need to do MXExecutorForward[MXExecutor[$$], 'TrainingMode' -> True] first. \
If the output is not a loss symbol, you need to specify NDArray gradient output to do backprop \
using the option 'OutputGradients'. For multiple outputs, give a list of NDArrays in \
canonical order.
";

MXExecutorReshape[executordata_MXExecutorData, shapes_Association] := CatchFailure @ Scope[	
	symbol = executordata["Symbol"];
	(* Infer shapes *)
	inferredShapes = MXSymbolInferShape[symbol, shapes];
	If[FailureQ@inferredShapes, Return@inferredShapes];
	If[Not@inferredShapes["Complete"], ReturnFailed["CannotInferShapes"]];
	(* get old and new shapes *)
	reshaper = Function[{execName, inferName}, 
		Association@KeyValueMap[
			#1 -> iNDArrayReshape[#2, inferredShapes[inferName, #1]]&, 
			executordata[execName]
		]
	];
	newArgs = reshaper["ArgumentArrays", "ArgumentArrays"];
	newAux = reshaper["AuxilliaryArrays", "AuxilliaryArrays"];
	gradArr = reshaper["GradientArrays", "ArgumentArrays"];
	exec = executordata["Executor"];
	mxSymbolBindFast[
		symbol, executordata["Context"], executordata["ContextGroup"], 
		newArgs, gradArr, newAux, 
		executordata["GradientUpdateMethod"], exec
	]
]

MXExecutorReshape[data_MXExecutorData, opts:OptionsPattern[]] := 
	MXExecutorReshape[data["Executor"], opts]
	

(******************************************************************************)
PackageExport["MXExecutorRequiredMemory"]

MXSetUsage @
"MXExecutorRequiredMemory[MXExecutor[$$]] returns the maximum memory required to evaluate \
the executor MXExecutor[$$].
MXExecutorRequiredMemory[MXExecutorData[$$]] is equivalent to MXExecutorRequiredMemory[MXExecutor[$$]]. 
";

MXExecutorRequiredMemory::nostringmatch = "Cannot extract memory requirement."
MXExecutorRequiredMemory[executor_MXExecutor] := Scope[
	printed = MXExecutorPrint@executor;
	If[FailureQ@printed, Return@printed];
	
	mem = StringCases[printed, 
		___ ~~ "Total " ~~ x : NumberString ~~ " MB allocated" ~~ ___ :> ToExpression@x
	];
	If[Length@mem === 0, Message[MXExecutorRequiredMemory::nostringmatch]; Return@$Failed];
	First@mem
]

MXExecutorRequiredMemory[data_MXExecutorData] := MXExecutorRequiredMemory[data["Executor"]]
