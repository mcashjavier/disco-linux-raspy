
(Unprotect[#]; Clear[#])& /@ {}

BeginPackage["DataResource`"]

Begin["`Private`"] (* Begin Private Context *) 

ResourceSystemClient`Private`resourceexecute0[$DataResourceType,id_,info_,rest___]:=
    readDataResource[id,info,rest]

readDataResource[id_,info_,rest___]:=Which[
	MemberQ[$localResources,id],readDataResourceLocal[id, info, rest],
	MemberQ[$cloudResources,id],readDataResourceCloud[id, info, rest],
	True,readDataResourceSystem[id, info, rest]
]

readDataResourceCloud[id_, info_, rest___]:=CloudEvaluate[readDataResourceLocal[id, info, rest]]

readDataResourceLocal[id_, info_, elem_,rest___]:=If[(Lookup[info,"DownloadedVersion",None]=!=None||localStoredQ[info, elem]),
		readdataResourceLocal[id, info,elem, rest],
		readDataResourceSystem[id, info, elem,rest]
	]

readdataResourceLocal[id_, info_, elem_,rest___]:=Block[{copyinfo, func},
    func=readDataResourcePostProcessFunction[rest];
    func@readdataresourceLocal[id,info,elem,rest]
]

readdataresourceLocal[id_,info_,l_List, rest___]:=With[{elems=Lookup[info,"ContentElements",{}]},
	If[Complement[l,elems]=!={},
		Message[ResourceData::invelem];Throw[$Failed]
	];
	AssociationMap[readdataresourceLocal[id,info,#,rest]&,l]
]

readdataresourceLocal[id_,info_,All, rest___]:=With[{elems=Lookup[info,"ContentElements",{}]},
	readdataresourceLocal[id,info,elems, rest]
]

readdataresourceLocal[id_,info_,Automatic, rest___]:=readdataresourcelocal[getElementFormat[id,Automatic],info["ContentElementLocations"]]

readdataresourceLocal[id_,info_,elem_, ___]:=(Message[ResourceData::invelem,elem];Throw[$Failed])/;!MemberQ[info["ContentElements"],elem]

readdataresourceLocal[id_,info_,elem_, ___]:=info["InformationElements",elem]/;KeyExistsQ[Lookup[info,"InformationElements",Association[]], elem]

readdataresourceLocal[id_,info_,elem_String,rest___]:=readdataresourcelocal[
	getElementFormat[id,elem],info["ContentElementLocations",elem]
	]/;Quiet[TrueQ[KeyExistsQ[Lookup[info,"ContentElementLocations",Association[]],elem]]]

$reaquiredResource=False;

readdataresourceLocal[id_,info_,elem_String,rest___]:=With[{contentfunction=getElementFunction[id, elem]},
	If[contentfunction=!=$Failed,
		produceDataResourceElement[id, info, elem, contentfunction, rest]
		,
		If[$reaquiredResource,
			(Message[ResourceData::invelem1,elem];Throw[$Failed])
			,
			ResourceSystemClient`Private`resourceAcquire[id, True];
			Block[{$reaquiredResource=True},
				readdataresourceLocal[id,info,elem,rest]
			]
		]
		
	]
	
]

readdataresourcelocal[format_, as_Association]:=readdataresourcelocal[format, as["Content"]]/;KeyExistsQ[as,"Content"]

readdataresourcelocal["WDF",location_]:=Get[location]

readdataresourcelocal["MX"|"PNG",location_]:=Import[location]

readdataresourcelocal[Automatic,location_]:=importlocal[location]

produceDataResourceElement[id_, info_, elem_, contentfunction_, rest___]:=Block[{reqelems, default, cf=contentfunction},
	reqelems=Intersection[Cases[contentfunction, Slot[e_] :> e, Infinity, Heads -> True], Append[Lookup[info,"ContentElements",{}],"Content"]];
	If[MemberQ[reqelems,"Content"]&&!MemberQ[info["ContentElements"],"Content"],
		default=info["DefaultContentElement"];
		If[StringQ[default],
			reqelems=reqelems/."Content"->default;
			cf=ReplaceAll[cf,Slot["Content"]->Slot[default]]
			,
			Return[$Failed]
		]	
	];
	If[Length[reqelems]>0,
		If[reqelems==={"Content"},
			cf[Association["Content"->resourcedatawithProgress[{id,info}, "Content"]]]
			,
			cf[resourcedatawithProgress[{id,info}, reqelems]]
		]
		,
		cf[Association[]]
	]
	
]


readDataResourceSystem[id_, info_,elem_, rest___]:=Block[{res},
	If[cacheResourceQ[info],
		ResourceSystemClient`ResourceDownload[id, Association["Element"->elem,"ResponseMethod"->"Download"]];
		readdataResourceLocal[id, resourceInfo[id],elem,rest]
		,
		res=ResourceSystemClient`Private`readresource[id, Join[takeReadRequestParameters[info],Association["Element"->elem]]];
		If[KeyExistsQ[res,"Result"],
			readDataResourcePostProcessFunction[rest]@res["Result"]
			,
			Throw[$Failed]
		]
	]
]

$ReadRequestParameters={"Start","End","Count","Function"};

takeReadRequestParameters[info_, fun:(_Function|_Symbol)]:=takeReadRequestParameters[info, Association["Function"->fun]]

takeReadRequestParameters[info_, as_Association]:=KeyTake[as,$ReadRequestParameters]

takeReadRequestParameters[info_, rest___]:=With[{as=Quiet[Association[rest]]},
	If[AssociationQ[as],
		takeReadRequestParameters[info,as],
		Throw[$Failed]
	]
]

readDataResourcePostProcessFunction[{}]=Identity;
readDataResourcePostProcessFunction[f:(_Function|_Composition|_RightComposition)]:=f
readDataResourcePostProcessFunction[sym_Symbol]:=sym
readDataResourcePostProcessFunction[___]:=Identity


$AutoCacheLimit=10^7;

cacheResourceQ[info_]:=cacheresourceQ[Lookup[info,"Caching",Automatic], QuantityMagnitude@Lookup[info,"ContentSize",Quantity[Infinity, "Bytes"]]]
cacheresourceQ[Automatic, size_]:=True/;size<$AutoCacheLimit
cacheresourceQ[True, _]=True
cacheresourceQ[False, _]=True
cacheresourceQ[_, _]=False

End[] (* End Private Context *)

EndPackage[]