(* Mathematica package *)
Begin["Tools`MathematicalFunctionData`Private`"]

(*-------------- clear variables --------------------------------------------------------------------*)
(* Initialization *)
$ProtectedSymbols = {
	System`MathematicalFunctionData
};
Unprotect@@$ProtectedSymbols;
Clear@@$ProtectedSymbols;
$tag = "MathematicalFunctionDataCatchThrowTag";

ClearAll[MathematicalFunctionData, iMathematicalFunctionData]
ClearAll[MFDEntPattern, MFDEntListPattern, MFDEntClassPattern, MFDPropPattern, MFDPropListPattern, MFDPropClassPattern]
ClearAll[MFDEntGroupPattern, MFDPropGroupPattern, MFDStringReplace, MFDQualReplace, MFDArgReplace]
ClearAll[MFDEntities, MFDProperties, MFDEntityClasses, MFDPropertyClasses]
ClearAll[MFDAssociationStrings, MFDAssociationStringPattern, MFDAnnotationStrings, MFDAnnotationStringPattern]
ClearAll[MFDNonMissingEntStrings, MFDNonMissingEntStringPattern, MFDNonMissingPropStrings, MFDNonMissingPropStringPattern]
ClearAll[MFDGeneralInfoStrings, MFDAllowedStrings, MFDAllowedStringPattern] 

(*-------------- helper definitions ---------------------------------------------------*)
(* Define some useful lists *)
MFDEntities := MFDEntities = EntityValue["MathematicalFunction", "Entities"]
MFDProperties := MFDProperties = EntityValue["MathematicalFunction", "Properties"]
MFDEntityClasses := MFDEntityClasses = EntityValue["MathematicalFunction", "EntityClasses"]
MFDPropertyClasses := MFDPropertyClasses = EntityValue["MathematicalFunction", "PropertyClasses"]
MFDAssociationStrings = {"EntityAssociation", "PropertyAssociation", "EntityPropertyAssociation", "PropertyEntityAssociation", "DataSet"}
MFDAssociationStringPattern = Alternatives @@ MFDAssociationStrings
MFDAnnotationStrings = {"Qualifiers", "QualifierValues", "Description", "Definition"}
MFDAnnotationStringPattern = Alternatives @@ MFDAnnotationStrings
MFDNonMissingEntStrings = {"NonMissingEntities", "NonMissingEntityAssociation"}
MFDNonMissingPropStrings = {"NonMissingProperties", "NonMissingPropertyAssociation"}
MFDNonMissingEntStringPattern = Alternatives @@ MFDNonMissingEntStrings
MFDNonMissingPropStringPattern = Alternatives @@ MFDNonMissingPropStrings
MFDGeneralInfoStrings = {
	"Entities", "EntityCount", "SampleEntities", "EntityCanonicalNames",
	"EntityClasses", "EntityClassCount", "SampleEntityClasses", "EntityClassCanonicalNames",
	"Properties", "PropertyCount", "PropertyCanonicalNames", "PropertyClasses", "PropertyClassCount",
	"PropertyClassCanonicalNames", "RandomEntity", "RandomEntities", "RandomEntityClass", "RandomEntityClasses"
}
MFDAllowedStrings = Join[MFDGeneralInfoStrings, MFDNonMissingEntStrings, MFDNonMissingPropStrings, MFDAnnotationStrings, MFDAssociationStrings]
MFDAllowedStringPattern = Alternatives @@ MFDAllowedStrings

(* Set up some entity, property, and qualifier (and groupings thereof) patterns *)
MFDEntPattern = Entity["MathematicalFunction", _String]
MFDEntListPattern = {MFDEntPattern ..}
MFDEntClassPattern = EntityClass["MathematicalFunction", _String]
MFDPropPattern = EntityProperty["MathematicalFunction", _String] | EntityProperty["MathematicalFunction", _, {(_Rule | _RuleDelayed) ..}]
MFDPropListPattern = {MFDPropPattern ..}
MFDPropClassPattern = EntityPropertyClass["MathematicalFunction", _String]
MFDEntGroupPattern = MFDEntPattern | MFDEntListPattern | MFDEntClassPattern
MFDPropGroupPattern = MFDPropPattern | MFDPropListPattern | MFDPropClassPattern
MFDQualGroupPattern = {(_Rule | _RuleDelayed) ...} | (_Rule | _RuleDelayed) ..

(* Set up replacement rules to canonicalize Entity/Property groupings *)
MFDStringReplace[input_String, messageTag_String] := Module[
	{
		result
	},
	If[MatchQ[input, MFDAllowedStringPattern], Return[input, Module]];
	result = Join[
		Cases[MFDEntities, _?(ToLowerCase[#[[2]]] === ToLowerCase[input] &), {1}],
		Cases[MFDProperties, _?(ToLowerCase[#[[2]]] === ToLowerCase[input] &), {1}],
		Cases[MFDEntityClasses, _?(ToLowerCase[#[[2]]] === ToLowerCase[input] &), {1}],
		Cases[MFDPropertyClasses, _?(ToLowerCase[#[[2]]] === ToLowerCase[input] &), {1}],
		Cases[MFDAllowedStrings, _?(ToLowerCase[#] === ToLowerCase[input] &), {1}]
	];
	If[ result === {}, Message[MessageName[MathematicalFunctionData, messageTag], input // FullForm, MathematicalFunctionData]; Throw[$Failed, $tag] ];
	result[[1]]
]
MFDStringReplace[input_List, messageTag_String] :=
	input //. {{first___, n_String, rest___} :> {first, MFDStringReplace[n, messageTag], rest}}
MFDStringReplace[input_String] := MFDStringReplace[input, "notent"]
MFDStringReplace[input_List] := MFDStringReplace[input, "notent"]
MFDStringReplace[input_, _] := input
MFDClassResolve[input_]:= 
	input //. {ec_EntityClass :> EntityValue[ec, "Entities"], pc_EntityPropertyClass :> EntityValue[pc, "Properties"]}

(* Code to combine properties with qualifiers *)
MFDQualReplace[prop : MFDPropPattern, quals : MFDQualGroupPattern] := Switch[prop,
	EntityProperty["MathematicalFunction", _String], Append[prop, {quals} // Flatten],
	EntityProperty["MathematicalFunction", _String, MFDQualGroupPattern], prop /. n : {(_Rule | _RuleDelayed) ...} -> Join[n, Flatten[{quals}]],
	_String, prop
]
MFDQualReplace[pg : MFDPropListPattern | MFDPropClassPattern, quals : MFDQualGroupPattern] := MFDQualReplace[#, quals] & /@ MFDClassResolve[pg]

(* Canonicalize initial argument structure *)
MathematicalFunctionData::"unrecog" = "Unrecognized argument `1` in evaluation of `2`."
MFDArgReplace[args___]:= Module[
	{
		input, output, attemptToEntity
	},
	attemptToEntity[expr_] := Module[
		{toEntAttempt},
		toEntAttempt = Quiet[ToEntity[expr, "MathematicalFunction"]]; 
		If[Head[toEntAttempt] === Entity, toEntAttempt, expr]
	];
	attemptToEntity[exprList_List]:=attemptToEntity /@ exprList;
	If[ Length[{args}] === 0, input = {args}, input = {attemptToEntity[First[{args}]]}~Join~Rest[{args}] ];
	output = Switch[ input,
		{}, Return[Null],
		{_}, output = MFDStringReplace[input, "notent"],
		{_, _}, output = MapThread[MFDStringReplace, {input, {"notent", "notprop"}}],
		{_, _, __}, output = MapThread[MFDStringReplace, {input, {"notent", "notprop"}~Join~Table["unrecog", {Length[{args}]-2}]}]
	];
	output = Replace[output, {first___, pg: MFDPropGroupPattern, qg: MFDQualGroupPattern} :> {first, MFDQualReplace[pg, qg]}];
	output = Replace[output, {first___, pg: MFDPropGroupPattern, assoc: MFDAllowedStringPattern, qg: MFDQualGroupPattern} :> {first, MFDQualReplace[pg, qg], assoc}];
	output = Replace[output, {first___, pg: MFDPropGroupPattern, qg: MFDQualGroupPattern, assoc: MFDAllowedStringPattern} :> {first, MFDQualReplace[pg, qg], assoc}];
	Sequence @@ output
]

(*-------------- define main function downvalues ---------------------------------------------------*)
(* Error-handling *)
MathematicalFunctionData[args___] := With[{res = Catch[iMathematicalFunctionData[MFDArgReplace[args]], $tag]}, res /; res =!= $Failed]
iMathematicalFunctionData[args___] := CompoundExpression[
	System`Private`Arguments[MathematicalFunctionData[args],{0,4}],
	checkArgsAndIssueMessages[args],
	Throw[$Failed, $tag]
]

checkArgsAndIssueMessages[ent_] := checkEntityAndIssueMessages[ent]
checkArgsAndIssueMessages[ent_, prop_] := CompoundExpression[
	checkEntityAndIssueMessages[ent],
	checkPropertyAndIssueMessages[prop]
]
checkArgsAndIssueMessages[ent_, prop_, qual_] := CompoundExpression[
	checkEntityAndIssueMessages[ent],
	checkPropertyAndIssueMessages[prop],
	checkQualifiersAndIssueMessages[qual]
]
checkArgsAndIssueMessages[ent_, prop_, ann_, qual_] := CompoundExpression[
	checkEntityAndIssueMessages[ent],
	checkPropertyAndIssueMessages[prop],
	checkAnnotationAndIssueMessages[ann],
	checkQualifiersAndIssueMessages[qual]
]

checkEntityAndIssueMessages[ent_] := If[
	!MatchQ[ent, Alternatives[MFDEntGroupPattern, MFDPropGroupPattern, All, Alternatives@@MFDGeneralInfoStrings, {"RandomEntities"|"RandomEntityClasses", _Integer}
		]],
	Message[MathematicalFunctionData::notent, ent, MathematicalFunctionData]
]

checkPropertyAndIssueMessages[prop_] := If[
	!MatchQ[prop, Alternatives[MFDAnnotationStringPattern, 
		"Entities", "EntityCount", "EntityCanonicalNames", 
		"Properties", "PropertyCount","PropertyCanonicalNames",
		MFDAssociationStringPattern, MFDNonMissingPropStringPattern, MFDNonMissingEntStringPattern,
		MFDPropGroupPattern, All
		]],
	Message[MathematicalFunctionData::notprop, prop, MathematicalFunctionData]
]

MathematicalFunctionData::notqual = "`1` is not a valid qualifier specification.";
MathematicalFunctionData::notann = "`1` is not a valid annotation specification.";
checkQualifiersAndIssueMessages[qual_] := If[
	!MatchQ[qual, Alternatives[
		MFDQualGroupPattern, MFDAssociationStringPattern, 
		MFDNonMissingEntStringPattern, MFDNonMissingPropStringPattern
		]],
	Message[MathematicalFunctionData::notqual, qual]
]

checkAnnotationAndIssueMessages[ann_] := Message[MathematicalFunctionData::notann, ann]


(* General domain info *)
iMathematicalFunctionData[Null] :=  EntityValue["MathematicalFunction", "Entities"]
(iMathematicalFunctionData[#] := EntityValue["MathematicalFunction", #])& /@ MFDGeneralInfoStrings

(* Annotation stuff *)
iMathematicalFunctionData[pg: MFDPropGroupPattern, annot: MFDAnnotationStringPattern]:= EntityValue["MathematicalFunction", pg, annot]

(* Single-argument stuff *)
iMathematicalFunctionData[eg: MFDEntPattern | MFDEntListPattern]:= eg
iMathematicalFunctionData[ec: MFDEntClassPattern]:= EntityValue[ec, "Entities"]
iMathematicalFunctionData[prop: MFDPropPattern]:= EntityValue[MFDEntities, prop, "EntityAssociation"]
iMathematicalFunctionData[props: MFDPropListPattern]:= EntityValue[MFDEntities, props, "PropertyEntityAssociation"]
iMathematicalFunctionData[pc: MFDPropClassPattern]:= EntityValue[pc, "Properties"]
(* specified-number random entity/class stuff... unspecified case handled under GeneralInfo *)
iMathematicalFunctionData[input: {"RandomEntities", n_Integer}]:= EntityValue["MathematicalFunction", input]
iMathematicalFunctionData[input: {"RandomEntityClasses", n_Integer}]:= EntityValue["MathematicalFunction", input]

(* Single-arg plus GeneralInfo stuff *)
iMathematicalFunctionData[ents : MFDEntGroupPattern, "Entities"] := ents // MFDClassResolve
iMathematicalFunctionData[ents : MFDEntGroupPattern, "EntityCount"] := MFDClassResolve[ents] // If[MatchQ[#, _List], Length[#], 1] &
iMathematicalFunctionData[ents : MFDEntGroupPattern, "EntityCanonicalNames"] := MFDClassResolve[ents] // If[MatchQ[#, _List], Part[#, All, 2], Part[#, 2]] &
iMathematicalFunctionData[props : MFDPropGroupPattern, "Properties"] := props // MFDClassResolve
iMathematicalFunctionData[props : MFDPropGroupPattern,  "PropertyCount"] := MFDClassResolve[props] // If[MatchQ[#, _List], Length[#], 1] &
iMathematicalFunctionData[props : MFDPropGroupPattern, "PropertyCanonicalNames"] := MFDClassResolve[props] // If[MatchQ[#, _List], Part[#, All, 2], Part[#, 2]] &

(* Single-argument plus association stuff *)
iMathematicalFunctionData[eg: MFDEntGroupPattern, stringArg: MFDAssociationStringPattern]:= EntityValue[eg // MFDClassResolve, MFDProperties, stringArg]
iMathematicalFunctionData[ent: MFDEntPattern, stringArg: MFDNonMissingPropStringPattern]:= EntityValue[ent, stringArg]
iMathematicalFunctionData[eg: MFDEntGroupPattern, stringArg: MFDAssociationStringPattern, quals: MFDQualGroupPattern]:= 
	EntityValue[eg // MFDClassResolve, MFDQualReplace[MFDProperties, quals], stringArg]
iMathematicalFunctionData[ent: MFDEntPattern, stringArg: MFDNonMissingPropStringPattern, quals: MFDQualGroupPattern]:= 
	EntityValue[ent, MFDQualReplace[MFDProperties, quals], stringArg]
iMathematicalFunctionData[pg: MFDPropGroupPattern, stringArg: MFDAssociationStringPattern]:= EntityValue[MFDEntities, pg // MFDClassResolve, stringArg]
iMathematicalFunctionData[pg: MFDPropPattern, stringArg: MFDNonMissingEntStringPattern]:= EntityValue[MFDEntities, pg, stringArg]

(* Entity/Property stuff *)
iMathematicalFunctionData[ents: MFDEntGroupPattern, "Properties"] := EntityValue[ents, "Properties"]
iMathematicalFunctionData[ents: MFDEntGroupPattern, "PropertyCount"] := EntityValue[ents, "PropertyCount"]
iMathematicalFunctionData[ents: MFDEntGroupPattern, "PropertyCanonicalNames"] := EntityValue[ents, "PropertyCanonicalNames"]
iMathematicalFunctionData[ents: MFDEntGroupPattern, props: MFDPropGroupPattern] := EntityValue[ents, props]
iMathematicalFunctionData[ents: MFDEntGroupPattern, props: MFDPropGroupPattern, assoc: MFDAssociationStringPattern] := EntityValue[ents, props, assoc]
iMathematicalFunctionData[ents: MFDEntListPattern | MFDEntClassPattern, props: MFDPropPattern, assoc: MFDNonMissingEntStringPattern] := EntityValue[ents, props, assoc]
iMathematicalFunctionData[ents: MFDEntPattern, props: MFDPropListPattern | MFDPropClassPattern, assoc: MFDNonMissingPropStringPattern] := EntityValue[ents, props, assoc]

(* NonMissing with qualifier stuff *)
iMathematicalFunctionData[ent: MFDEntPattern, "NonMissingProperties", qg: MFDQualGroupPattern]:= iMathematicalFunctionData[ent, "NonMissingProperties"]
iMathematicalFunctionData[ent: MFDEntPattern, "NonMissingPropertyAssociation", qg: MFDQualGroupPattern]:= 
	iMathematicalFunctionData[ent, MFDQualReplace[iMathematicalFunctionData[ent, "NonMissingProperties"], qg], "PropertyAssociation"]

(* "All" stuff *)
iMathematicalFunctionData[All, args___]:= iMathematicalFunctionData[MFDEntities, args]
iMathematicalFunctionData[ents: MFDEntGroupPattern, All, rest___]:= iMathematicalFunctionData[ents, MFDProperties, rest]

(* Postscript *)
With[{symbols = $ProtectedSymbols},(*SetAttributes is HoldFirst*)
	SetAttributes[symbols, {ReadProtected}]
];
Protect@@$ProtectedSymbols;

End[];