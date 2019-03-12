Package["MXNetLink`"]

PackageImport["GeneralUtilities`"]

PackageExport["MXBatchIterator"]

(******************************************************************************)
(* In Memory Iterators *)

MXSetUsage @ "
MXBatchIterator[<|key$1 -> list$1, key$2 -> list$, $$|>, size$] gives an iterator that produces \
associations of the form <|'Arrays' -> <|key$1 -> slice$1, $$|>, 'Padding' -> pos$|>, where 'Arrays' \
contain windows slice$i on the list$i, each of the given size, and 'Padding' is False when no padding \
was used, and an integer giving the padding position after which padding was used. 
If the remaining data is less than the batch size, padding can be used. The following options are avaialable:

'Padding' -> 'RandomChoice': one of {None, 'RandomChoice'}. If None is selected, no padding is used, and \
no all of the batches are guaranteed to be the same size. 
"

Options[MXBatchIterator] = {
	"PaddingMethod" -> "RandomChoice"
}

MXBatchIterator[assoc_Association, batchSize2_Integer, OptionsPattern[]] := CatchFailure @ Scope[
	UnpackOptions[paddingMethod];
	{keys, values} = KeysValues[assoc];	
	If[!AllSameBy[values, Length], Panic["DifferentLengths"]];
	len = Length @ First @ values;
	NewIterator[MXBatchIterator, 
		{pos = 1, len = len, keys = keys, values = values, batchSize = batchSize2, paddingMethod = paddingMethod},
		If[pos > len, IteratorExhausted,
		Block[{res, mask},
			{res, mask} = batchProvider[values, pos, batchSize, len, paddingMethod];
			res = AssociationThread[keys, res];
			(* Increment *)
			pos += batchSize; 
			{res, mask}
		]
	]]
];

(* Slices the NDArray, and pads when necessary. Returns {slicedvalsList, maskPos}  *)

batchProvider[values_, pos_, batchSize_, length_, paddingMethod_] := Scope[
	maxPos = pos + batchSize - 1;
	mask = False; (* False means no masking *)
	slicedVals = NDArraySliceFast[#, pos, Min[maxPos, length]]& /@ values;
	(* if maxPos > length, we need padding *)
	If[maxPos > length,
		Return@batchPadder[paddingMethod, slicedVals, values, batchSize]
	];
	{slicedVals, mask}
]

(* RandomChoice padder: pad with random choice from entire batch *)
batchPadder["RandomChoice", slicedVals_, vals_, batchSize_] := Scope[
	padSize = batchSize - Length@First@slicedVals;
	newSlicedVals = MapThread[Join[#1, RandomChoice[#2, padSize]]&, {slicedVals, vals}];
	{newSlicedVals, batchSize - padSize}
]

batchPadder[None, slicedVals_, vals_, batchSize_] := {slicedVals, False}

(******************************************************************************)

PackageExport["MXDataRandomizer"]

MXSetUsage @ "
MXDataRandomizer[<|key$1 -> array$1, $$|>] takes an association of normal WL arrays, \
randomizes them all with a common randomizer, and returns an association of \
NDArrays.
"

Options[MXDataRandomizer] = {
	"Context" -> {"CPU", 0},
	"DataType" -> "Real32"
}

MXDataRandomizer[assoc_Association, OptionsPattern[]] := CatchFailure @ Scope[
	UnpackOptions[context, dataType];
	If[!AllSameBy[Values@assoc, Length], Panic["DifferentLengths"]];
	randIndices = RandomSample@Range@Length@First@Values@assoc;
	randomizedAssoc = #[[randIndices]]& /@ assoc;
	NDArrayCreate[#, "Context" -> context, "DataType" -> dataType]& /@ randomizedAssoc
];



PackageExport["NDArrayEpochIterator"]

PackageExport["NDArrayIterator"]

NDArrayEpochIterator[nd_NDArray, Identity, inputData_] := With[
	{stepSize = NDArrayLength[nd]},
	{randomizedInput = NDArrayRandomizeAndPad[inputData, stepSize]},
	NewIterator[NDArrayEpochIterator, {
		bulk = randomizedInput,
		tmp = NDArrayCreateEmpty[NDArrayDimensions[nd], "Context" -> {"CPU", 0}],
		pos = 1, step = stepSize, last = NDArrayLength[randomizedInput]-stepSize+1
		},
		If[pos > last, IteratorExhausted,
			NDArraySetSlice[tmp, bulk, pos, pos + step-1];
			NDArrayCopyTo[nd, tmp];
			pos += step;
		]
	]&
];	

NDArrayIterator[nd_NDArray, Identity, inputData_] := With[
	{stepSize = NDArrayLength[nd]},
	NewIterator[NDArrayIterator, {
		bulk = NDArrayCreate[inputData, "Context" -> {"CPU", 0}],
		tmp = NDArrayCreateEmpty[NDArrayDimensions[nd], "Context" -> {"CPU", 0}],
		pos = 1, step = stepSize, last = Length[inputData]-stepSize+1
		},
		If[pos > last, IteratorExhausted,
			NDArraySetSlice[tmp, bulk, pos, pos + step-1];
			NDArrayCopyTo[nd, tmp];
			pos += step;
		]
	]
];

NDArrayEpochIterator[nd_NDArray, encoder_, inputData_] := With[
	{stepSize = NDArrayLength[nd]},
	{randomizedInput = RandomizeAndPad[inputData, stepSize]},
	NewIterator[NDArrayEpochIterator, {
		input = randomizedInput,
		pos = 1, step = stepSize, last = Length[randomizedInput]-stepSize+1},
		If[pos > last, IteratorExhausted,
			NDArraySet[nd, encoder @ Take[randomizedInput, {pos, pos+step-1}]];
			pos += step;
		]
	]&
];

NDArrayIterator[nd_NDArray, encoder_, inputData_] := With[
	{stepSize = NDArrayLength[nd]},
	NewIterator[NDArrayIterator, {
		input = inputData,
		pos = 1, step = stepSize, last = Length[inputData]-stepSize+1},
		If[pos > last, IteratorExhausted,
			NDArraySet[nd, encoder @ Take[inputData, {pos, pos+step-1}]];
			pos += step;
		]
	]
];

PackageExport["$IteratorRandomSeed"]

$IteratorRandomSeed = 1;

NDArrayRandomizeAndPad[input_, batchsize_] := 
	NDArrayCreateSampled[input, makeRandomIndices[Length[input], batchsize]]

RandomizeAndPad[input_, batchsize_] := 
	Part[input, makeRandomIndices[Length[input], batchsize]];

makeRandomIndices[len_, batch_] := BlockRandom[
	SeedRandom[$IteratorRandomSeed];
	If[Divisible[len, batch],
		RandomSample @ Range[len],
		Join[
			RandomSample @ Range[len], 
			RandomInteger[{1,len}, batch - Mod[len, batch]]
		]
	]
];


PackageEnd[]