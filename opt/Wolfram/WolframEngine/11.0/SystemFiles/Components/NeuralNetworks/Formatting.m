Package["NeuralNetworks`"]

PackageImport["GeneralUtilities`"]


$genericIcon := $genericIcon = Uncompress @ "
1:eJxTTMoPSmNiYGAo5gAS7kWJBRmZycUQERYg4ZNZXILGYwTx2IGEf0FicmZJZZExGFy2R1PHjKkL
wstLxWd+JpBmABPYVIFcGZRYkpmfl5iT+R8IMkHCmSDTaWIFyEwaW8GMagW5wcZI+2Aj3gqyg414Kw
gHmyCQcEwqzs8pLUkNyM/MKwnOrErNZIU7CMQCi5MTkUguRTOMGbdhhCODxIAlMZDgyoNKc1KLOYEM
z9zE9FRwuCgAeQBsxr7m"


PackageScope["MakeLayerBoxes"]

SetHoldFirst[MakeLayerBoxes];

MakeLayerBoxes[layer:head_Symbol[assoc_Association]] := Scope[
	icon = If[InitializedNetQ[layer], 
		$genericIcon, 
		Labeled[
			Append[$genericIcon, BaseStyle -> GrayLevel[0.65]],
			Style["uninitialized", Gray, FontSize -> 8]
		]
	];
	BoxForm`ArrangeSummaryBox[
		head, None, icon,
		Sequence @@ infoItems[assoc],
		StandardForm,
		"Interpretable" -> FreeQ[assoc, _RawArray]
	]
];

MakeLayerBoxes[_] := Fail;


PackageScope["infoItems"]

infoItems[assoc_] := Scope[
	type = assoc["Type"];
	hypers = fmtSection[assoc["Parameters"], "Parameters", False];
	If[Length[hypers] === 1, AppendTo[hypers, {Style["none", Gray]}]];
	params = fmtSection[assoc["Arrays"], "Arrays", True];
	ports = fmtSection[Join[$in /@ assoc["Inputs"], $out /@ assoc["Outputs"]], "Ports", True];
	hidden = Developer`ToList[params, ports];
	{hypers, hidden}
];

fmtSection[_Missing | <||>, title_, divider_] := {};

fmtSection[assoc_, title_, divider_] := Scope[
	list = fmtEntries @ assoc;
	If[divider, 
		frameStyle = Sequence[Frame -> {{False, False}, {False, True}}, FrameStyle -> LightGray],
		frameStyle = Sequence[];
	];
	PrependTo[list, {
		Item[Style[title <> ":", Bold], frameStyle],
		Item[If[divider, Spacer[{1,11}], ""], frameStyle]
	}];
	list
];

PackageScope["fmtEntries"]

fmtEntries[assoc_] := KeyValueMap[fmtEntry, assoc];

fmtEntry[k_, v_] /; StringStartsQ[k, "$"] := Nothing;

fmtEntry[k_, v_] := {
	Style[Row[{k, ":"}], "SummaryItemAnnotation"], 
	Style[fmtItem[v], "SummaryItem"]
};


tensorName[SizeT|NaturalT] := "tensor";
tensorName[0] = "scalar";
tensorName[1] = "vector";
tensorName[2] = "matrix";
tensorName[n_Integer] := "tensor";

genTensorName[n_Integer /; n > 2] := 
	Row[{"tensor", " ", fmtNote["rank", n]}];
genTensorName[n_] := tensorName[n];


PackageScope["fmtItem"]

courierBold[e_] := Style[e, FontFamily -> "Courier", FontWeight -> Bold, FontSize -> Larger];

fmtItem[$in[e_]] := fmtItem[e];
fmtItem[$out[e_]] := fmtItem[e]; (* couldn't find a nice icon *)
fmtItem[e_List] := If[Developer`StringVectorQ[e], Short[e, 0.5], fmtItem /@ e];
fmtItem[EncodedType[d_, t_]] := Row[{"encoded", " ", fmtItem[t]}];
fmtItem[DecodedType[d_, t_]] := Row[{"decoded", " ", fmtItem[t]}];

fmtItem[TensorT[n_, _ListT]] := genTensorName[n];
fmtItem[_EnumT] := $PlaceholderIcon;
fmtItem[EitherT[e_]] := Alternatives @@ Map[fmtItem, e];
fmtItem[TensorT[0, {}]] := "scalar";
fmtItem[TensorT[n_, list_List]] := fmtTensorDims[list];
fmtItem[c_ChannelT] := fmtItem[ExpandChannels[c]];
fmtItem[r_RawArray] := fmtTensorDims[Dimensions[r]];
fmtItem[NaturalT] := "non-negative integer"
fmtItem[SizeT | PosIntegerT] := "positive integer";
fmtItem[IndexIntegerT[max_Integer]] := Row[{"index", " ", fmtNote["range", Row[{1, "..", max}]]}];
fmtItem[IndexIntegerT[_]] := "index";
fmtItem[t:True|False] := t;
fmtItem[IntegerT] := "integer";
fmtItem[Nullable[t_]] := Row[{"optional", " ", fmtItem[t]}];

fmtItem[ListT[n_Integer, t_]] := ConstantArray[fmtItem[t], n];
fmtItem[ListT[_, t_]] := Row[{"list of ", fmtItem[t], "s"}];
fmtItem[e_ /; !ValidTypeQ[e]] := e;

fmtItem[img_Image] := Thumbnail[img, If[$VersionNumber >= 11, UpTo[32], 32], Padding -> None];


PackageScope["$PlaceholderIcon"]

$PlaceholderIcon = "\[DottedSquare]";
fmtItem[e_] := If[ConcreteParameterQ[e], e, $PlaceholderIcon];

fmtTensorDims[e_List] := Row[{
	tensorName[Length[e]], " ",
	fmtNote["size", Row[fmtDim /@ e, "\[Times]"]]
}];

fmtNote[prop_, value_] := 
	Row[{"(", "\[VeryThinSpace]", prop, ":", " ", value,  "\[VeryThinSpace]", ")"}, BaseStyle -> Gray];


PackageScope["fmtDim"]

fmtDim[n_Integer] := IntegerString[n];
fmtDim[_] := $PlaceholderIcon;

tensorName[n_] := Switch[n, 1, "vector", 2, "matrix", _, IntegerString[n] <> "-tensor"];


PackageScope["fmtDims"]

fmtDims[_] := "";
fmtDims[TensorT[n_, _ListT]] := $PlaceholderIcon;(*tensorName[n];*)
fmtDims[TensorT[n_, list_List]] := StringJoin[Riffle[fmtDim /@ list, "\[Cross]"]];
fmtDims[c_ChannelT] := fmtDims[ExpandChannels[c]];
fmtDims[r_RawArray] := fmtDims[Dimensions[r]];
fmtDims[EncodedType[_, t_]] := fmtDims[t];
fmtDims[DecodedType[_, t_]] := fmtDims[t];
fmtDims[ScalarT] := "scalar";


PackageScope["typeInfo"]

typeInfo[key_ -> type_] := 
	infoGrid["Port", "Port", fmtKey @ key, fmtEntries @ <|"Form" -> type|>];

typeInfo[key_ -> (EncodedType|DecodedType)[_[ename_String, assoc_], type_]] := 
	infoGrid["Port", "Port", fmtKey @ key,
		fmtEntries @ Prepend[
			assoc["Parameters"],
			{"Form" -> type, "Type" -> ename}
		]
	];

PackageScope["itemInfo"]

itemInfo[key_ -> x_] := x;
itemInfo[key_ -> assoc_Association] := 
	infoGrid[
		$TypeToSymbol @ assoc["Type"], 
		"Layer", Row[{"(", fmtKey[key], ")"}],
		showInfo[assoc]
	];

showInfo[assoc_Association] := Switch[
	assoc["Type"], 
	"Graph", List @ List @ netGraphPlot[assoc],
	"Chain", List @ List @ netChainGrid[assoc],
	_, Apply[Join] @ infoItems[assoc]
];

infoGrid[header_, type_, key_, grid_] := Panel[
	Framed[Grid[
		Prepend[{Style[Row[{header, " ", key}], Bold, 12], SpanFromLeft}] @ grid,
		Alignment -> {Left, Automatic}, 
		Spacings -> {1, {0., 0.65, {Automatic}}}
	], FrameStyle -> None, FrameMargins -> {{0,0},{0,5}}]
];

fmtKey[key_] := If[StringMatchQ[key, DigitCharacter..], key,
	Style[key, ShowStringCharacters -> True]
];