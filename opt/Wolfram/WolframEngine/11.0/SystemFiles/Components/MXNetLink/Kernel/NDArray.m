Package["MXNetLink`"]

PackageImport["GeneralUtilities`"]
PackageImport["Developer`"]

(****** Symbol Exports ******)


PackageExport["$DefaultContext"]

$DefaultContext = {"CPU", 0};


PackageExport["NDArray"]

(******************************************************************************)
(****** Load Library Functions ******)

DeclareLibraryFunction[mxNDArrayCreateEx, "WL_MXNDArrayCreateEx", 
	{
		Integer, 					(* ndarray handle key *)
		{"RawArray", "Constant"}, 	(* tensor of dims *)
		Integer,					(* device type *)
		Integer, 					(* device id *)
		Integer, 					(* delay_alloc *)
		Integer						(* data type *)
	}, 
	"Void"						
	]	

DeclareLibraryFunction[mxNDArrayCreateNone, "WL_MXNDArrayCreateNone", 
	{
		Integer 					(* ndarray handle key *)
	}, 
	"Void"						
	]	

DeclareLibraryFunction[mxNDArrayWaitAll, "WL_MXNDArrayWaitAll", 
	{
	}, 
	"Void"						
	]

DeclareLibraryFunction[copyFromRawArray, "WL_CopyFromRawArray", 
	{
		{"RawArray", "Constant"}, 	(* tensor to be copied *)
		Integer 					(* ndArray handle key *)
	}, 
	"Void"						
	]

DeclareLibraryFunction[copyFromPackedArray, "WL_CopyFromPackedArray", 
	{
		{Real, _, "Constant"}, 		(* tensor to be copied *)
		Integer 					(* ndArray handle key *)
	}, 
	"Void"						
	]

DeclareLibraryFunction[randomPaddedDataCopy, "WL_RandomPaddedDataCopy", 
	{
		{Real, _, "Constant"}, 		(* tensor to be copied *)
		{Integer, 1, "Constant"}, 	(* random indices *)
		Integer 					(* ndArray handle key *)
	}, 
	"Void"						
	]

DeclareLibraryFunction[mxNDArrayToRawArray, "NDArrayToRawArray", 
	{Integer}, 	(* ndarray handle key *)
	"RawArray"  (* Output tensor *)
	]
	
DeclareLibraryFunction[mxNDArraySave, "WL_MXNDArraySave", 
	{
		"UTF8String", (* parameter file path *)
		"UTF8String",  		(* names string *)
		{Integer, 1}		(* ndarray keys *)
	},
	"Void"
	]	
	
DeclareLibraryFunction[mxNDArrayLoad, "WL_MXNDArrayLoad", 
	{
		"UTF8String" 	(* FileName *)
	}, 	
	"UTF8String"  		
	]	
	
DeclareLibraryFunction[mxNDArrayFree, "WL_NDArrayFree", 
	{Integer}, 	(* ndarray handle key *)
	"Void"  		
	]	
	
DeclareLibraryFunction[mxNDArrayGetContext, "WL_MXNDArrayGetContext", 
	{Integer}, 	(* ndarray handle key *)
	"UTF8String"  		
	]	

DeclareLibraryFunction[mxNDArrayDimensions, "WL_NDArrayDimensions", 
	{Integer}, 	(* ndarray handle key *)
	{Integer, 1}  		
	]
	
DeclareLibraryFunction[mxNDArrayGetDType, "WL_MXNDArrayGetDType", 
	{Integer}, 	(* ndarray handle key *)
	Integer  		
	]	
	
PackageScope["mxNDArraySlice"]	
DeclareLibraryFunction[mxNDArraySlice, "WL_MXNDArraySlice", 
	{
		Integer, 		(* NDArray Handle *)
		Integer,		(* slice begin *)
		Integer,		(* slice end *)
		Integer			(* NDArray output handle *)
	}, 	
	"Void"  		
	]		

PackageScope["mxNDArrayReshape"]	
DeclareLibraryFunction[mxNDArrayReshape, "WL_MXNDArrayReshape", 
	{
		Integer, 		(* NDArray Handle *)
		Integer,		(* out NDArray Handle *)
		{Integer, 1}	(* new dim *)
	}, 	
	"Void"  		
	]	

(*DeclareLibraryFunction[loadModelHandles, "RawArrayToNDArray", {"UTF8String"}, "UTF8String"];*)
(*DeclareLibraryFunction[loadArray, "WL_NDArrayGetData", {Integer}, "RawArray"];*)


(******************************************************************************)
PackageExport["NDArrayType"]

MXSetUsage @ "
NDArrayType[NDArray[$$]] returns the data type of array.
"

NDArrayType[ndarray_] := 
	CatchFailure @ $DatatypeCodeReverse @ MXNetInvoke[mxNDArrayGetDType, MLEID@ndarray]

(******************************************************************************)
PackageExport["NDArrayDimensions"]

MXSetUsage @ "
NDArrayDimensions[NDArray[$$]] returns dimensions of the array.
"

NDArrayDimensions[ndarray_NDArray] := CatchFailure @ MXNetInvoke[mxNDArrayDimensions, MLEID@ndarray]

NDArray /: Dimensions[nd_NDArray] := NDArrayDimensions @ nd;

(******************************************************************************)
PackageExport["NDArrayLength"]

MXSetUsage[NDArrayLength, "
NDArrayLength[NDArray[$$]] returns length of the array.
"]

NDArray /: Length[nd_NDArray] := NDArrayLength[nd];

NDArrayLength[ndarray_NDArray] := CatchFailure @ First @ MXNetInvoke[mxNDArrayDimensions, MLEID@ndarray]

(******************************************************************************)
PackageExport["NDArrayWaitForAll"]

MXSetUsage[NDArrayWaitForAll, "
NDArrayWaitForAll[] wait for all asynchronous operations to complete.
"]

NDArrayWaitForAll[] := CatchFailure @ MXNetInvoke[mxNDArrayWaitAll]

(******************************************************************************)
PackageExport["NDArrayContext"]

MXSetUsage @
"NDArrayContext[NDArray[$$]] returns tuple of the form {device type, device id}."

NDArrayContext[ndarray_] := CatchFailure @ Scope[
	result = MXNetInvoke[mxNDArrayGetContext, MLEID@ndarray];
	(* Return Metadata *)
	data = Developer`ReadRawJSONString@result;
	(* Convert MXNet device int encoding to name *)
	{$DeviceCodeReverse@data["Device"], data["DeviceID"]}
]

(******************************************************************************)
PackageExport["NDArrayMetadata"]
PackageScope["iNDArrayMetadata"]

MXSetUsage @ "
NDArrayMetadata[$$]] returns the list {'Context' -> $$, 'DataType' -> $$}.
"

NDArrayMetadata[ndarray_] := CatchFailure @ iNDArrayMetadata[ndarray]

iNDArrayMetadata[ndarray_] := CatchFailure @ Scope[
	context = NDArrayContext@ndarray;
	If[FailureQ@context, ThrowFailure["context"]];
	type = NDArrayType@ndarray;
	If[FailureQ@type, ThrowFailure["type"]];
	Normal@<|"Context" -> context, "DataType" -> type|>
]

NDArrayMetadata::context = "Could not determine the NDArray device or DeviceID."
NDArrayMetadata::type = "Could not determine the NDArray type."

(******************************************************************************)
PackageExport["NDArrayCreateEmpty"]

MXSetUsage @ "
NDArrayCreateEmpty[dims$] creates an uninitialized NDArray of dimensions dims$.
"
		
Options[NDArrayCreateEmpty] =
{
	"Context" :> $DefaultContext,
	"DataType" -> "Real32",
	"DelayAllocation" -> False
};
	
NDArrayCreateEmpty[dims:{__Integer}, opts:OptionsPattern[]] := Scope[
	UnpackOptions[context, dataType, delayAllocation];
	device = toDeviceCode[First@context];
	deviceID = Last@context;
	dataType = toDatatypeCode[dataType];
	(* Check for incorrect settings *)
	delayAllocation ! BooleanQ ! Panic["InvalidDelayAllocation"];
	deviceID ! IntegerQ ! Panic["InvalidDeviceID"];

	(* Invoke *)
	mxInit[];
	handle = CreateManagedLibraryExpression["NDArray", NDArray];
	dims2 = RawArray["UnsignedInteger32", dims];
	
	MXNetInvoke[mxNDArrayCreateEx, MLEID@handle, dims2, device, deviceID, Boole@delayAllocation, dataType];
	(* Return Handle *)
	System`Private`SetNoEntry @ handle
]

(******************************************************************************)
PackageExport["NDArrayCreateZero"]

MXSetUsage @ "
NDArrayCreateZero[dims$] creates an NDArray of dimensions dims$ whose entries are equal to zero.
"
		
Options[NDArrayCreateZero] =
{
	"Context" :> $DefaultContext,
	"DataType" -> "Real32",
	"DelayAllocation" -> False
};
	
NDArrayCreateZero[dims:{__Integer}, opts:OptionsPattern[]] := Scope[
	arr = NDArrayCreateEmpty[dims, opts];
	NDArraySet[arr, 0.];
	arr
]

(******************************************************************************)
PackageExport["NDArrayCreate"]

MXSetUsage @ "
NDArrayCreate[data$] creates an NDArray from data$, where data$ is either a RawArray or a numeric array. 
The type of the resulting NDArray can be specified via the 'DataType' option. If this is not specified, \
the type will be inherited from data$ if it is a RawArray. Otherwise, it will default to type 'Real32\'.
"
		
Options[NDArrayCreate] =
{
	"Context" :> $DefaultContext,
	"DataType" -> Automatic
};

NDArray::notmatrix = "Argument to NDArray was not a numeric matrix."
NDArray[data_List] := If[NumericArrayQ[data], NDArrayCreate[data], Message[NDArray::notmatrix]; $Failed];

NDArrayCreate::invdata = "Cannot convert data to RawArray.";
NDArrayCreate::invdim = "Initial data `` cannot have Dimension {}.";

NDArrayCreate[data_, opts:OptionsPattern[]] := Scope[
	UnpackOptions[context, dataType];
	(* If DataType is Automatic: Either inherit type from raw array, or use default Real32 *)
	If[dataType === Automatic,
		dataType = If[RawArrayQ@data,
			Developer`RawArrayType@data,
			"Real32"
		];
	];
	
	(* Dimensions: check that dimensions are ok *)
	dim = Dimensions@data;
	If[dim === {}, ThrowFailure["invdim", data]];

	(* Create empty handle *)
	handle = NDArrayCreateEmpty[
		dim, 
		"Context" -> context,
		"DataType" -> dataType,
		"DelayAllocation" -> False
		];
	(* Copy RawArray to NDArray *)
	NDArraySet[handle, data];
	System`Private`SetNoEntry @ handle
]

(******************************************************************************)
PackageExport["NDArrayCreateDataRandom"]

MXSetUsage @ "
NDArrayCreateDataRandom[data$, batchsize$] is equivalent to NDArrayCreate[RandomSample@data], where \
data$ is a packed array. However, it avoids producing an extra copy of data. If the input data$ is \
not a multiple of batchsize$ it will be padded with random elements until it is.
Note: currently, only NDArray[$$] objects of type 'Real32' and of context 'CPU' can be returned. 
"

NDArrayCreateDataRandom[data_List, indices_, batchSize_] := Scope[
	dataDim = Dimensions@data;
	exampleNum = First@dataDim;
	padding = If[batchSize === None, 0, batchSize - Mod[exampleNum, batchSize]];
	(* use zero indexing as will use in C++ *)
	indRange = Range[0, exampleNum - 1];
	randIndex = Join[RandomSample@indRange, RandomChoice[indRange, padding]];
	(* Create NDArray *)
	outputDim = Join[{exampleNum + padding}, Rest@dataDim];
	nd = NDArrayCreateEmpty[outputDim, "Context" -> {"CPU", 0}, "DataType" -> "Real32"];
	MXNetInvoke[
		randomPaddedDataCopy, 
		ToPackedArray@data, 
		ToPackedArray@randIndex, 
		MLEID@nd
	];
	nd
]


(******************************************************************************)

PackageExport["NDArrayCreateSampled"]

MXSetUsage @ "
NDArrayCreateSampled[data$, rows$] constructs an NDArray by taking the given rows from data$, which
should be a numeric tensor.
Note: currently, only NDArray[$$] objects of type 'Real32' and of context 'CPU' can be returned. 
"

NDArrayCreateSampled[data_List, indices_List] := Scope[
	outputDim = Prepend[Rest @ Dimensions[data], Length[indices]];
	nd = NDArrayCreateEmpty[outputDim, "Context" -> {"CPU", 0}, "DataType" -> "Real32"];
	MXNetInvoke[
		randomPaddedDataCopy, 
		ToPackedArray@data, 
		ToPackedArray[indices-1], 
		MLEID@nd
	];
	nd
]


(******************************************************************************)
PackageExport["NDArraySet"]

MXSetUsage @ "
NDArraySet[NDArray[$$], scalar$] sets all entires of an array to a scalar value.
NDArraySet[NDArray[$$], array$] sets the contents of an array to another array, which can be numeric array, another NDArray, or a RawArray.
"

NDArraySet[to_NDArray, from_RawArray] := CatchFailure @ Scope[
	toType = NDArrayType@to;
	fromType = RawArrayType@to;
	
	(* Special Case: Real16. Need intermediate array *)
	If[toType === "Real16",
		dim = Dimensions@to;
		If[FailureQ@dim, Return@dim];
		tempArray = NDArrayCreateEmpty[dim, "Device" -> "CPU", "DataType" -> "Real32"];
		newRaw = RawArrayConvert[from, "Real32"];
		MXNetInvoke[copyFromRawArray, newRaw, MLEID@tempArray];
		MXNetInvoke[mxFuncInvoke, "_copyto", {MLEID@tempArray}, {}, {MLEID@to}];
		Return@Null
	];
	(* Note: RawArrayConvert doesn't make copy if already correct type*)
	MXNetInvoke[copyFromRawArray, RawArrayConvert[from, toType], MLEID@to];
]

(* General array *)
NDArraySet[to_NDArray, from_] := CatchFailure @ Scope[
	toType = NDArrayType@to;
	toContext = NDArrayContext@to;
	If[FailureQ@toType, Return@toType];
	If[FailureQ@toContext, Return@toContext];
	
	(* can avoid copying to RawArray if output NDArray is type Real32 and CPU *)
	If[toType === "Real32" && toContext === {"CPU", 0},
		MXNetInvoke[copyFromPackedArray, ToPackedArray@from, MLEID@to];
		Return@Null;
	];
	
	(* Deal with Real16 special case *)
	rawArray = If[toType === "Real16",
		RawArray["Real32", from],
		RawArray[toType, from]
	];
	
	(* Main check that supplied array was valid *)
	If[Not@RawArrayQ@rawArray, ThrowFailure["invdata"]];
	NDArraySet[to, rawArray]
];

NDArraySet[dst_NDArray, src_NDArray] := NDArrayCopyTo[dst, src];

(* Simple case: NDArray to NDArray *)
NDArraySet[to_NDArray, from_?MNumberQ] := CatchFailure @ MXNetInvoke[mxFuncInvoke, "_set_value", {}, {N[from]}, {MLEID@to}];

NDArraySet[to_List, from_List] := 
	If[Length[from] != Length[to], 
		Panic[],
		ScanThread[NDArraySet, {to, from}]
	];

NDArraySet[to_Association, from_List] :=
	If[Length[from] != Length[to],
		Panic[],
		ScanThread[NDArraySet, {Values[to], from}]
	];

NDArraySet[to_Association, from_Association] := 
	If[Length[from] != Length[to], 
		Panic[],
		KeyValueScan[NDArraySet[#2, Lookup[from, #1, Panic[]]]&, to]
	];


General::invmxval = "`` is not a scalar, RawArray, NDArray, or tensor of machine numbers.";
NDArraySet::invdata = "Cannot convert data to RawArray.";
NDArraySet[args___] := Panic["NDArraySet", "`` invalid args args", {args}];


(******************************************************************************)
PackageExport["NDArrayFree"]

MXSetUsage @
"NDArrayFree[NDArray[$$]] frees the memory associated with an array."

NDArrayFree[nd_NDArray] := CatchFailure @ MXNetInvoke[mxNDArrayFree, MLEID@nd];


(******************************************************************************)
(* NOTE: need a better way of dealing with Real16! *)


(******************************************************************************)
(****** Selection of arithmetic functions, avoiding top-level dispatch ********)
(******************************************************************************)

(* NOTE: All of these are mutating. *)

(* This macro is used to make it easy to declare symbols *)

$symbols = {x1, x2, x3, x4, x5};
MakeNDArrayFunction[symbol_, mxname_, ndin_List, scalarin_List, ndout_List:{1}] := Scope[
	$slots = Table[Null, 5]; $max = 0;
	Quiet[
		body = Construct[Hold[MXNetInvoke], Hold[mxFuncInvoke], mxname,
			proc[ndin, procND], 
			proc[scalarin, procScalar], 
			proc[ndout, procND] 
		],
		RuleDelayed::rhs
	];
	args = DeleteCases[$slots, Null];
	lhs = Construct[Hold[symbol], Sequence @@ args];
	statement = Construct[HoldComplete, lhs, body] //. Hold[h_] :> h;
	symbol[___] := Panic["InvalidArgs", "Invalid arguments were provided to ``", symbol];
	SetDelayed @@ statement
];
MakeNDArrayFunction[___] := Panic["InvalidCall"];

proc[0, _] := {};
proc[spec_, f_] := spec /. i_Integer :> f[i, $symbols[[i]]];
Quiet[
	procND[i_, sym_] := ($slots[[i]] = sym_NDArray; Hold @ MLEID[sym]);
	procScalar[i_, sym_] := ($slots[[i]] = sym_?MNumberQ; Hold @ N[sym]);
	,
	RuleDelayed::rhs
];
(******************************************************************************)
(* This first section covers convenience functions whose target is also one of
their sources, and correspond to the in-place mutation functions in top-level
like AddTo or TimesBy. *)
(******************************************************************************)

PackageExport["NDArrayCopyTo"]

MakeNDArrayFunction[NDArrayCopyTo, "_copyto", {2}, {}];

MXSetUsage @ "
NDArrayCopyTo[dst$, src$] copies src to dst$.
"


PackageExport["NDArrayAddTo"]

MakeNDArrayFunction[NDArrayAddTo, "_plus",        {1, 2}, {}];
MakeNDArrayFunction[NDArrayAddTo, "_plus_scalar", {1}, {2}];

MXSetUsage @ "
NDArrayAddTo[dst$, src$] sets dst$ to dst$ + src$.
NDArrayAddTo[dst$, scalar$] sets dst$ to dst$ + scalar$.
"

PackageExport["NDArraySubtractFrom"]

MakeNDArrayFunction[NDArraySubtractFrom, "_minus",        {1, 2}, {}];
MakeNDArrayFunction[NDArraySubtractFrom, "_minus_scalar", {1}, {2}];

MXSetUsage @ "
NDArraySubtractFrom[dst$, src$] sets dst$ to dst$ - src$.
NDArraySubtractFrom[dst$, scalar$] sets dst$ to dst$ - scalar$.
"


PackageExport["NDArrayScaledAddTo"]

MakeNDArrayFunction[NDArrayScaledAddTo, "_scaled_plus", {1, 3}, {1.0, 2}];

MXSetUsage @ "NDArrayScaledAddTo[dst$, scalar$, src$] sets dst$ to scalar$ * src$."


PackageExport["NDArrayTimesBy"]

MakeNDArrayFunction[NDArrayTimesBy, "_mul",        {2}, {}];
MakeNDArrayFunction[NDArrayTimesBy, "_mul_scalar", {}, {2}];

MXSetUsage @ "
NDArrayAddTo[dst$, src$] sets dst$ to dst$ + src$.
NDArrayAddTo[dst$, scalar$] sets dst$ to dst$ + scalar$.
"


(******************************************************************************)
(* The remainder here take their destination separately from the source. *)
(******************************************************************************)
PackageExport["NDArraySetPlus"]

MakeNDArrayFunction[NDArraySetPlus, "_plus", {2, 3}, {}];
MakeNDArrayFunction[NDArraySetPlus, "_plus_scalar", {2}, {3}];

MXSetUsage @ "
NDArraySetPlus[dst$, src$1, src$2] sets dst$ to src$1 + src$2.
NDArraySetPlus[dst$, src$1, scalar$] sets dst$ to src$1 + scalar$.
"


PackageExport["NDArraySetScaledPlus"]

MakeNDArrayFunction[NDArraySetScaledPlus, "_scaled_plus", {3,5}, {2,4}];

MXSetUsage @ "NDArraySetScaledPlus[dst$, scalar$1, src$1, scalar$2, src$2] sets dst$ to scalar$1 * src$1 + scalar$2 * src$2."


PackageExport["NDArraySetSubtract"]

MakeNDArrayFunction[NDArraySetSubtract, "_minus", {2, 3}, {}];
MakeNDArrayFunction[NDArraySetSubtract, "_minus_scalar", {2}, {3}];

MXSetUsage @ "
NDArraySetSubtract[dst$, src$1, src$2] sets dst$ to src$1 - src$2.
NDArraySetSubtract[dst$, src$1, scalar$] sets dst$ to src$1 - scalar$.
"


PackageExport["NDArraySetTimes"]

MakeNDArrayFunction[NDArraySetTimes, "_mul", {2, 3}, {}];
MakeNDArrayFunction[NDArraySetTimes, "_mul_scalar", {2}, {3}];

MXSetUsage @ "
NDArraySetTimes[dst$, src$1, src$2] sets dst$ to src$1 * src$2.
NDArraySetTimes[dst$, src$, scalar$] sets dst$ to src$ * scalar$.
"


PackageExport["NDArraySetDivide"]

MakeNDArrayFunction[NDArraySetDivide, "_div", {2, 3}, {}];
MakeNDArrayFunction[NDArraySetDivide, "_div_scalar", {2}, {3}];

MXSetUsage @ "
NDArraySetDivide[dst$, src$1, src$2] sets dst$ to src$1 / src$2.
NDArraySetDivide[dst$, src$, scalar$] sets dst$ to src$ / scalar$.
"


PackageExport["NDArraySetSqrt"]

MakeNDArrayFunction[NDArraySetSqrt, "sqrt", {2}, {}];

MXSetUsage @ "NDArraySetSqrt[dst$, src$] sets dst$ to Sqrt[$src]."


PackageExport["NDArraySetSquare"]

MakeNDArrayFunction[NDArraySetSquare, "square", {2}, {}];

MXSetUsage @ "NDArraySetSquare[dst$, src$] sets dst$ to $src^2."


PackageExport["NDArraySetClip"]

MakeNDArrayFunction[NDArraySetClip, "clip", {2}, {3,4}];

MXSetUsage @ "NDArraySetClip[dst$, src$, min$, max$] sets dst$ to $src, clipping between min$ and max$."


PackageExport["NDArraySetExp"]

MakeNDArrayFunction[NDArraySetExp, "exp", {2}, {}];

MXSetUsage @ "NDArraySetExp[dst$, src$] sets dst$ to Exp[$src]."


PackageExport["NDArraySetAbs"]

MakeNDArrayFunction[NDArraySetAbs, "abs", {2}, {}];

MXSetUsage @ "NDArraySetAbs[lhs$, src$1] sets dst$ to Abs[src$1]."


PackageExport["NDArraySetLog"]

MakeNDArrayFunction[NDArraySetLog, "log", {2}, {}];

MXSetUsage @ "NDArraySetLog[lhs$, src$1] sets dst$ to Log[src$1]."


(******************************************************************************)
PackageExport["NDArrayGet"]

MXSetUsage @ "
NDArrayGet[NDArray[$$]] returns a RawArray of the same data type as the NDArray, \
except in the case of 'Real16', which is returned as 'Real64'.
"

NDArrayGet[ndarray_NDArray] := CatchFailure @ Scope[
	type = NDArrayType@ndarray;
	If[FailureQ@type, Return@type];
	(* Deal with special case of Real16 *)
	If[type === "Real16",
		MXNetInvoke[mxNDArrayToRawArray, MLEID@NDArrayCopy[ndarray, "DataType" -> "Real64"]],
		MXNetInvoke[mxNDArrayToRawArray, MLEID@ndarray]
	]
]

NDArrayGet[ndarray_Integer] := NDArrayGet[NDArray[ndarray]]

(* For internal use *)
iNDArrayGetNoCheck[ndarray_Integer] := MXNetInvoke[mxNDArrayToRawArray, ndarray]

NDArray /: Normal[nd_NDArray] := Normal @ NDArrayGet[nd]

(*******************************************************************************

Model Importing Functions 

*******************************************************************************)
PackageExport["NDArrayImport"]

MXSetUsage @ "
NDArrayImport[file$] imports an NDArray stored on disk using MXNet's binary format. Returns \
either an association or a list of RawArray objects.
NDArrayImport[file$, {NDArray[$$], $$}] imports the NDArray's in file file$ and copies them into \
existing NDArrays. 
NDArrayImport[file$, <|key1 -> NDArray[$$], $$|>] imports the NDArray's in file file$ and copies them into \
existing NDArrays. 
"

NDArrayImport[file_String] := CatchFailure @ Scope[
	{names, arrays} = getImport@file;
	(* Get raw array *)
	rawArrays = iNDArrayGetNoCheck /@ arrays;
	return = If[Length@names > 0, 
		AssociationThread[names -> rawArrays],
		rawArrays
	];
	(* Free NDArrays *)
	NDArrayFree /@ arrays;
	(* Return *)
	return
]

NDArrayImport[file_String, list_List] := CatchFailure @ Scope[
	{names, arrays} = getImport@file;
	(* Check that input and output lengths are equivalent *)
	If[Length@arrays =!= Length@list, ThrowFailure["IncompatibleLengths"]];
	MapThread[
		MXNetInvoke[mxFuncInvoke, "_copyto", {#1}, {}, {MLEID@#2}]&
		, 
		{arrays, list}
	];
	(* Free NDArrays *)
	NDArrayFree /@ arrays;
]

NDArrayImport[file_String, assoc_Association] := CatchFailure @ Scope[
	{names, arrays} = getImport@file;
	arrayAssoc = If[Length@names === 0, 
		AssociationThread[Keys@assoc -> arrays],
		AssociationThread[names -> arrays]
	];
		
	Map[
		MXNetInvoke[mxFuncInvoke, "_copyto", {arrayAssoc[#]}, {}, {MLEID@assoc[#]}]&
		, 
		Keys@assoc
	];
	(* Free NDArrays *)
	NDArrayFree /@ arrays;
]

(* Utility function for getting arrays + names of ndarrays *)
getImport[file_] := Scope[
	file = ExpandFileName[file];
	If[Not@FileExistsQ@file, ThrowFailure["FileNotExist"]];
	json = MXNetInvoke[mxNDArrayLoad, file];
	model = Developer`ReadRawJSONString@json;
	{model["out_names"], model["out_arr_handles"]}
]


(******************************************************************************)
PackageExport["NDArrayExport"]

MXSetUsage[NDArrayExport, "
NDArrayExport[file$, <|key1 -> NDArray[$$], $$|>] exports an association of NDArray[$$] objects \
to MXNet's binary export format with filename file$.  
NDArrayExport[file$, {NDArray[$$], $$}] exports a list of NDArray[$$] objects \
to MXNet's binary export format.  
"];

NDArrayExport[file_String, param_Association] := 
	CatchFailure @ ndExport[file, Keys@param, Values@param]

NDArrayExport[file_String, param_List] := CatchFailure @ ndExport[file, {}, param]

ndExport[file_, names_, ndKeys_] := Scope[
	namesString = Developer`WriteRawJSONString@names;
	MXNetInvoke[
		mxNDArraySave, 
		ExpandFileName @ file, 
		namesString, 
		ToPackedArray[MLEID /@ ndKeys]
	];
	file
]

(******************************************************************************)

Language`SetMutationHandler[NDArray, mutHandler];

PackageScope["mutHandler"]

Clear[mutHandler];

SetAttributes[mutHandler, HoldAllComplete];

NDMutate[funcName_, nds_, scalars_] := Scope[
	res = CatchFailure @ MXNetInvoke[mxFuncInvoke, funcName, MLEID /@ nds, N @ scalars, MLEID /@ Take[nds, 1]];
	If[FailureQ[res], Message[res]; $Failed, First[nds]]
]

onFailure[res_, failure_] := If[FailureQ[res], failure[res], res]; 

mutHandler[Unset[lhs_]] := (NDArrayFree[lhs]; Language`MutationFallthrough);

(* Only mutate when rhs is not an NDArray *)
mutHandler[Set[lhs_, rhs_/; Not@NDArrayQ@rhs]] := (
	onFailure[NDArraySet[lhs, rhs], (Message[#]; Fail)&];
	lhs
);

mutHandler[Increment[lhs_]] := NDMutate["_plus_scalar", {lhs}, {1.0}];
mutHandler[Decrement[lhs_]] := NDMutate["_plus_scalar", {lhs}, {-1.0}];

mutHandler[AddTo[lhs_, rhs_ ? NumericQ]] := NDMutate["_plus_scalar", {lhs}, {rhs}];
mutHandler[AddTo[lhs_, rhs_NDArray]] := NDMutate["_plus", {lhs, rhs}, {1.0}];

mutHandler[SubtractFrom[a_Symbol, rhs_ ? NumericQ]] := NDMutate["_plus_scalar", {lhs}, {rhs}];
mutHandler[SubtractFrom[a_Symbol, rhs_NDArray]] := NDMutate["_rminus", {lhs, rhs}, {1.0}];

mutHandler[TimesBy[lhs_, rhs_ ? NumericQ]] := NDMutate["_mul_scalar", {lhs}, {rhs}];
mutHandler[TimesBy[lhs_, rhs_NDArray]] := If[lhs === rhs, NDMutate["square", {lhs}, {}], NDMutate["_mul", {lhs, rhs}, {1.0}]];

mutHandler[DivideBy[lhs_, rhs_ ? NumericQ]] := NDMutate["_div_scalar", {lhs}, {rhs}];
mutHandler[DivideBy[lhs_, rhs_NDArray]] := NDMutate["_div", {lhs, rhs}, {1.0}];

makeElementWise[sym_Symbol] := makeElementWise[sym -> ToLowerCase[SymbolName[sym]]];
makeElementWise[sym_Symbol -> name_String] := (
	mutHandler[ComposeTo[lhs_, sym]] := NDMutate[name, {lhs}, {}];

);

Scan[makeElementWise, {Log, Abs, Exp, Sqrt, Sin, Cos, Floor, Sign, Ceiling -> "ceil", Round -> "ceil"}];

mutHandler[ComposeTo[lhs_, Clip]] := NDMutate["clip", {lhs}, {-1., 1.}];
mutHandler[ComposeTo[lhs_, Clip[#, {a_, b_}]&]] := NDMutate["clip", {lhs}, {a, b}];
mutHandler[ComposeTo[lhs_, Power[#, 2]&]] := NDMutate["", {lhs}, {a, b}];


PackageExport["NDArraySummary"]

NDArraySummary[e_NDArray] := Scope[
	If[!ManagedLibraryExpressionQ[e],
		Return["(array no longer valid)"];
	];
	dims = mxNDArrayDimensions[MLEID@e];
	totalcount = Times @@ dims;
	data = Normal[e];
	depth = ArrayDepth[data];
	If[totalcount < 128 && depth < 4 && Max[dims] < 16,
		table = TableForm[data, 
			TableDirections -> If[depth == 1, Row, Automatic],
			TableSpacing -> ConstantArray[1, depth]];
		table = Style[NumberForm[table, 3], FontSize -> 8, FontFamily -> "Courier"];
		table = Framed[table, FrameStyle -> LightGray],
		table = None
	];
	If[4 < totalcount < 16384 && Length[dims] == 2,
		size = Max[Min[Floor[512 / First[dims]], Floor[256 / Last[dims]], 8], 2];
		plot = MatrixPlot[data, PixelConstrained -> size, FrameTicks -> None, Frame -> None],
		plot = None
	];
	{min, max} = MinMax[data];
	flat = Flatten @ data;
	grid = Grid[{
		{"count", Length[flat]},
		{"min", min},
		{"max", max},
		{"mean", Mean[flat]},
		{"s.d.", StandardDeviation[flat]},
		If[plot =!= None, {"plot", plot}, Nothing],
		If[table =!= None, {"table", table}, Nothing]
		}, Alignment -> Left,
		ItemStyle -> {{{}, {FontFamily -> "Courier"}}}
	];
	Framed[grid, FrameStyle -> None]
];


PackageExport["$EnableNDArrayTooltip"]

$EnableNDArrayTooltip = True;

DefineCustomBoxes[NDArray, 
	e_NDArray :> Block[
		{interior, dims, info, device, extra, type},
		dims = mxNDArrayDimensions[MLEID@e];
		device = deviceInfoToString @ mxNDArrayGetContext[MLEID@e];
		type = NDArrayType[e];
		interior = RowBox[Riffle[dims, StyleBox["\[Times]", Gray]]];
		interior = StyleBox[interior, FontFamily -> "Courier", FontSize -> 11];
		extra = {};
		If[device =!= "CPU", AppendTo[extra, device]];
		If[type =!= "Real32", AppendTo[extra, type]];
		If[extra =!= {}, 
			extra = RowBox[{"(", RowBox[Riffle[extra, ", "]], ")"}];
			interior = RowBox[{interior, "   ", StyleBox[extra, Gray]}];
		];
		interior = PanelBox[interior, ContentPadding -> False, BaselinePosition -> Baseline];
		If[$EnableNDArrayTooltip && (Times @@ dims) < 2^20,
			interior = TooltipBox[
				interior, 
				DynamicBox[ToBoxes @ NDArraySummary[e], TrackedSymbols :> {}],
				TooltipDelay -> 0.2
			];		
		];
		TagBox[
			RowBox[{"NDArray", "[", interior, "]"}], 
			False, Selectable -> False, Editable -> False, SelectWithContents -> True
		]
	]
];

deviceInfoString[json_String] := deviceInfoString[json] = 
	StringRiffle[
		Values @ Developer`ReadRawJSONString[json],
		":"
	];

$toTensorName = {
	0 -> "scalar",
	1 -> "vector",
	2 -> "matrix",
	_ -> "tensor"
};

(******************************************************************************)
PackageExport["NDArraySlice"]

MXSetUsage @ "
NDArraySlice[NDArray[$$], start$, stop$] returns a sub-array using elements start$ throught stop$.
The sub-array is a window onto the original NDArray.
"

NDArraySlice[ndarray_, a_Integer, b_Integer] := CatchFailure @ Scope[
	(* NOTE: we don't want to allocate anything, so we don't call NDArrayCreateEmpty *)
	outputHandle = CreateManagedLibraryExpression["NDArray", NDArray];
	dims = MXNetInvoke[mxNDArrayDimensions, MLEID@ndarray]; 
	len = First[dims];
	Which[
		a < 0, a = Max[a + 1 + len, 1], 
		a == 0, Panic["ZeroArraySlice"],
		a > len, a = len;
	];
	Which[
		b < 0, b = Max[b + 1 + len, 1], 
		b == 0, Panic["ZeroArraySlice"],
		b > len, b = len;
	];
	If[b < a, b = a-1];
	result = MXNetInvoke[mxNDArraySlice, MLEID@ndarray, a - 1, b, MLEID@outputHandle];
	System`Private`SetNoEntry @ outputHandle
];

PackageExport["NDArraySliceFast"]

MXSetUsage @ "
NDArraySliceFast[NDArray[$$], start$, stop$] returns a sub-array using elements start$ throught stop$.
The sub-array is a window onto the original NDArray. Minimal error checking is done.
"

NDArraySliceFast[ndarray_, a_Integer, b_Integer] := CatchFailure @ Scope[
	outputHandle = CreateManagedLibraryExpression["NDArray", NDArray];
	result = MXNetInvoke[mxNDArraySlice, MLEID@ndarray, a - 1, b, MLEID@outputHandle];
	System`Private`SetNoEntry @ outputHandle
];

(******************************************************************************)
PackageExport["NDArraySetSlice"]

MXSetUsage @ "
NDArraySetSlice[dst$, src$, start$, stop$] sets dst$ to be src$[[start;;stop]].
"

NDArraySetSlice[dst_NDArray, src_NDArray, a_Integer, b_Integer] := CatchFailure @ Scope[
	MXNetInvoke[mxNDArraySlice, MLEID@src, a - 1, b, MLEID@dst];
];

(******************************************************************************)

NDArray /: Take[nd_NDArray, {m_, n_} | Span[m_, n_]] := NDArraySlice[nd, Replace[m, All -> 1], Replace[n, All -> -1]];
NDArray /: Part[x_NDArray, range1_ ;; range2_] := NDArraySlice[x, range1, range2];
NDArray /: Part[x_NDArray, {indices__}] := Join@Apply[Sequence, x[[# ;; #]]& /@ {indices}]

(******************************************************************************)
PackageExport["NDArrayReshape"]
PackageScope["iNDArrayReshape"]

MXSetUsage @ "
NDArrayReshape[NDArray[$$], dims$] returns a new NDArray sharing the memory of NDArray[$$], but \
with different dimensions. 
NDArrayReshape[None, dims$] returns None.
"

NDArrayReshape[ndarray_NDArray, newDims_List] := CatchFailure @ iNDArrayReshape[ndarray, newDims]

iNDArrayReshape[ndarray_NDArray, newDims_List] := Scope[
	(* NOTE: we don't want to allocate anything, so we don't call NDArrayCreateEmpty *)
	outputHandle = CreateManagedLibraryExpression["NDArray", NDArray];
	dims = MXNetInvoke[
		mxNDArrayReshape, 
		MLEID@ndarray, 
		MLEID@outputHandle, 
		ToPackedArray@newDims
	]; 
	System`Private`SetNoEntry @ outputHandle
];

NDArrayReshape[None, _] := None
iNDArrayReshape[None, _] := None

(******************************************************************************)

PackageExport["NDArrayPartThread"]

MXSetUsage @ "
NDArrayPartThread[matrix$, indices$] gives an vector NDArray whose i$th element is arr$(ind$i).
";

NDArrayPartThread[array_NDArray, indices_NDArray] := CatchFailure @ Scope[
	dim = Dimensions@array;
	dimOut = Drop[dim, {2}];
	outputArray = NDArrayCreateEmpty[dimOut, Sequence@@iNDArrayMetadata@array, "DelayAllocation" -> True];
	MXNetInvoke[mxFuncInvoke, "choose_element_0index", {MLEID@array, MLEID@indices}, {}, {MLEID@outputArray}];
	outputArray
]

(******************************************************************************)
PackageExport["NDArrayJoin"]

MXSetUsage @ "
NDArrayJoin[NDArray[$$], $$] joins a sequence of arrays into a single NDArray. 
NDArrayJoin[{NDArray[$$], $$}] joins a list of arrays into a single NDArray. 
";

NDArrayJoin[arrays:{__NDArray}] := CatchFailure @ Scope[
	If[!AllSameBy[arrays, Rest@Dimensions@#&], ThrowFailure["invalidDims"]];
	If[!AllSameBy[arrays, NDArrayQ], ThrowFailure["ndarray"]];
	lengths = Length /@ arrays;
	(* Must always be at least one array. Inherit type etc from this array. *)
	firstArray = First@arrays;
	outputDim = Join[{Total@lengths}, Rest@Dimensions@firstArray];
	(* Create empty output array *)
	outputArray = NDArrayCreateEmpty[outputDim, Sequence@@iNDArrayMetadata@firstArray];
	(* Use Part to copy into output *)
	MapThread[
		NDArraySet[NDArraySlice[outputArray, First@#2, Last@#2], #1]&
		, 
		{arrays, generateRanges@lengths}
	];
	outputArray
]

NDArrayJoin[arrays__NDArray] := NDArrayJoin[{arrays}]

(* Generate appropriate slice ranges given lengths of join arrays *)
generateRanges[lengths_] := Scope[
	ranges = Partition[Accumulate@Prepend[lengths, 0], 2, 1];
	ranges[[;;, 1]] += 1;
	ranges
]

NDArray /: Join[nd__NDArray] := NDArrayJoin@nd

(******************************************************************************)
(* Many to one functions: deal with functions like total *)
PackageExport["NDArrayAggregate"]

MXSetUsage[NDArrayAggregate, "
NDArrayAggregate[NDArray[$$], 'fname$'] creates a new NDArray of unit size by aggregating the elements of the array via the function fname$.
"]

NDArrayAggregate[array_NDArray, funcName_String] := CatchFailure @ Scope[
	out = NDArrayCreateEmpty[{1}, Sequence@@iNDArrayMetadata@array];
	MXNetInvoke[mxFuncInvoke, funcName, {MLEID@array}, {}, {MLEID@out}];
	out
]

PackageExport["NDArrayAggregateImmediate"]
MXSetUsage[NDArrayAggregateImmediate, "
NDArrayAggregateImmediate[NDArray[$$], 'fname$'] aggregates like NDArrayAggregate,.
"]

NDArrayAggregateImmediate[array_NDArray, funcName_String] := CatchFailure @ Scope[
	out = NDArrayCreateEmpty[{1}, Sequence@@iNDArrayMetadata@array];
	MXNetInvoke[mxFuncInvoke, funcName, {MLEID@array}, {}, {MLEID@out}];
	mxNDArrayWaitAll[];
	First[Normal[out], $Failed]
]

(* Define upvalues for all many-to-one functions *)
Function[{sym, name}, 
	NDArray /: sym[x_NDArray] := NDArrayAggregateImmediate[x, name];
] @@@ {Min -> "min", Max -> "max", Total -> "sum", Norm -> "norm"};


PackageExport["NDArrayTotal"]
PackageExport["NDArrayMax"]
PackageExport["NDArrayMin"]
PackageExport["NDArrayNorm"]

Function[{sym, name}, 
	NDArray /: sym[x_NDArray] := NDArrayAggregate[x, name];
] @@@ {NDArrayMin -> "min", NDArrayMax -> "max", NDArrayTotal -> "sum", NDArrayNorm -> "norm"};


(******************************************************************************)
(* Randomizers *)

PackageExport["NDArrayRandomGaussian"]
MXSetUsage[NDArrayRandomGaussian, "
NDArrayRandomGaussian[NDArray[$$], mean$, stddev$] fills an NDArray[$$] with values randomly \
from a Gaussian with mean $mean and standard deviation stddev$.
"]

NDArrayRandomGaussian[nd_NDArray, mean_, stddev_] := CatchFailure @ Scope[
	MXNetInvoke[mxFuncInvoke, "_random_gaussian", {MLEID@nd}, {mean, stddev}, {MLEID@nd}];
]

PackageExport["NDArrayRandomUniform"]
MXSetUsage[NDArrayRandomUniform, "
NDArrayRandomUniform[NDArray[$$], {low$, high$}] fills an NDArray[$$] with values randomly \
from a uniform distribution in the range {low$, high$}.
"]

NDArrayRandomUniform[nd_NDArray, {low_, high_}] := CatchFailure @ Scope[
	MXNetInvoke[mxFuncInvoke, "_random_uniform", {MLEID@nd}, {low, high}, {MLEID@nd}];
]

(******************************************************************************)
(* Random Choice *)
NDArray /: RandomChoice[x_NDArray, int_] := Part[x, RandomChoice[Range@Length@x, int]]
NDArray /: RandomChoice[x_NDArray] := RandomChoice[x, 1]

(******************************************************************************)
PackageExport["NDArrayRowIndexMax"]

MXSetUsage @ "
NDArrayRowIndexMax[NDArray[$$]] returns the position of the maximum element of each row of a matrix.
"

NDArrayRowIndexMax[x_NDArray] := CatchFailure @ Scope[
	dim = Dimensions@x;
	If[Length@dim =!= 2, ThrowFailure["invdim"]];
	output = NDArrayCreateEmpty[{First@dim}, Sequence@@iNDArrayMetadata@x];
	MXNetInvoke[mxFuncInvoke, "argmax_channel", {MLEID@x}, {}, {MLEID@output}];
	System`Private`SetNoEntry@output
]

NDArrayRowMaximumIndex::invdim = "Input must be a matrix."

(******************************************************************************)
PackageExport["NDArrayCopy"]

MXSetUsage  @ "
NDArrayCopy[NDArray[$$], opts$] copes the given array into a new NDArray whose data type and device is specified via opts$.
"
		
Options[NDArrayCopy] =
{
	"Context" -> Automatic,
	"DataType" -> Automatic
};

NDArrayCopy[data_, opts:OptionsPattern[]] := CatchFailure @ Scope[
	UnpackOptions[context, dataType];
	(* Inherit options from data NDArray if not specified *)
	If[MemberQ[{context, dataType}, Automatic],
		metadata = Association@iNDArrayMetadata@data;
		If[context === Automatic, context = metadata["Context"]];
		If[dataType === Automatic, dataType = metadata["DataType"]];
	];	

	dim = Dimensions@data;
	If[FailureQ@dim, Return@dim];
	outputArray = NDArrayCreateEmpty[
		dim, 
		"Context" -> context,
		"DataType" -> dataType,
		"DelayAllocation" -> True
		];
	If[FailureQ@handle, Return@handle];
	res = NDArraySet[outputArray, data];
	If[FailureQ@res, Return@res];
	outputArray
]


(******************************************************************************)
PackageExport["NDArrayBatchSlice"]

MXSetUsage[NDArrayBatchSlice, "
NDArrayBatchSlice[NDArray[$$], batchSize$]: slices NDArray[$$] into a list of non-overlapping \ 
slices of length batchSize$ (except for the endpoints). No copies are made. 
"]

NDArrayBatchSlice[nd_NDArray, batchSize_] := Scope[
	len = Length@nd;
	arrayLengths = ConstantArray[batchSize, Floor[len/batchSize]];
	rem = Mod[len, batchSize];
	If[rem =!= 0, AppendTo[arrayLengths, rem]];
	lengthPairs = generateRanges@arrayLengths;
	NDArraySlice[nd, First@#, Last@#]& /@ lengthPairs
]

(******************************************************************************)
PackageExport["NDArrayEquality"]

MXSetUsage[NDArrayEquality, "
NDArrayEquality[assoc1$, assoc2$]: checks whether two associations of NDArray's \
are numerically equivalent, up to some default Tolerance (controlled via Options). If \
one association has different keys, False is returned.
"]

Options[NDArrayEquality] =
{
	Tolerance -> 0.0001
};

NDArrayEquality[nd1_List, nd2_List, opts:OptionsPattern[]] := Scope[
	dims1 = Dimensions /@ nd1;
	dims2 = Dimensions /@ nd2;

	If[dims1 =!= dims2, Return@False];
	(* Third check: numerical similarity *)
	tolerance = OptionValue@Tolerance;
	nd1Normal = Normal /@ nd1;
	nd2Normal = Normal /@ nd2;
	diff = Max@Abs@#& /@ (nd1Normal - nd2Normal);
	trues = (# < tolerance)& /@ diff;
	AllTrue[trues, TrueQ]
]

NDArrayEquality[assoc1_Association, assoc2_Association, opts:OptionsPattern[]] := Scope[
	(* Put into canonical order *)
	assoc1Sort = KeySort@assoc1;
	assoc2Sort = KeySort@assoc2;
	(* First check: key equality *) 
	If[Keys@assoc1 =!= Keys@assoc2, Return@False];
	(* otherwise, call list version *)
	NDArrayEquality[Values@assoc1Sort, Values@assoc2Sort, opts]
]

NDArrayEquality[nd1_NDArray, nd2_, opts:OptionsPattern[]] := 
	NDArrayEquality[{nd1}, {nd2}, opts]


