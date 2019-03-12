(* Wolfram Language Package *)

(Unprotect[#]; Clear[#])& /@ {System`ResourceSearch}

BeginPackage["ResourceSystemClient`"]
(* Exported symbols added here with SymbolName::usage *)  

System`ResourceSearch
ResourceSystemClient`RandomResources
ResourceSystemClient`ResourceSearchData

Begin["`Private`"] (* Begin Private Context *) 

Options[resourcesearch]=Options[System`ResourceSearch]={"SearchLocations"->All, Method->Automatic, "UpdateCloudIndex"->Automatic};

System`ResourceSearch[args___]:=Catch[resourceSearch[args]]

ResourceSystemClient`ResourceSearchData[args___]:=Catch[resourceSearchData[args]]

resourceSearch[str_String, rest___]:=createResourceObjects[resourcesearch[str, rest]]

resourceSearchData[str_String, rest___]:=createResourceDataset[resourcesearch[str, rest]]

resourceSearch[]:=(Message[ResourceSearch::argt, ResourceSearch, 0, 1, 2];$Failed)
resourceSearchData[]:=(Message[ResourceSearch::argt, ResourceSearch, 0, 1, 2];$Failed)

resourceSearch[expr_,___]:=(Message[ResourceSearch::invquery,expr];$Failed)

resourceSearch[___]:=$Failed
resourceSearchData[___]:=$Failed

resourcesearch[str_String, opts:OptionsPattern[]]:=resourcesearch[str, Automatic, opts]

resourcesearch[str_, n_, OptionsPattern[]]:=Block[{list,progress=0, count,locations, res},
	count=Switch[n,
		Automatic|Infinity, Infinity,
		0,Return[{}],
		_Integer, If[n>0, n, Message[ResourceSearch::invcount,n];Throw[$Failed]],
		_,Message[ResourceSearch::invcount,n];Throw[$Failed]
	];	
    PrintTemporary[ProgressIndicator[Dynamic[progress], {0,3}]];
    locations=OptionValue["SearchLocations"];
    If[locations=!=All,locations=Flatten[{locations}]];
    If[locations===All||MemberQ[locations, "Local"],
		list=resourcesearchLocal[str, n,OptionValue[Method]];
    ];
    progress=1;
	If[Length[list]>=count,	Return[minimalSearchInfo/@Take[list,UpTo[count]]]];
    If[locations===All||MemberQ[locations, "Cloud"],
		list=DeleteDuplicates[Join[list,resourceSearchCloud[str, count, OptionValue["UpdateCloudIndex"]]],#1["UUID"]==#2["UUID"]&];
    ];
    progress=2;
	If[Length[list]>=count,	Return[minimalSearchInfo/@Take[list,UpTo[count]]]];
    If[locations===All||MemberQ[locations, "Repository"],	    
		list=Take[DeleteDuplicates[Join[list,resourcesearchResourceSystem[str, n,OptionValue[Method]]],#1["UUID"]==#2["UUID"]&],UpTo[count]];
    ];
	res=minimalSearchInfo/@list
]

minimalSearchInfo[info_]:=If[MemberQ[$loadedResources,info["UUID"]],usableResourceInfo[info],info]

resourceSearchCloud[str_, n_, _]:=resourcesearchLocal[str, n, Automatic]/;$CloudEvaluation

resourceSearchCloud[str_, count_, True]:=Block[{cloudindex=updateCloudResourceIndex[], info},
	If[Head[cloudindex]===Dataset,
		$cloudResourceIndex=cloudindex;
		resourceSearchDataset[str,count,cloudindex]
		,
		{}
	]
]

resourceSearchCloud[str_, count_, update_]:=resourcesearchCloud[str,count, update]

resourcesearchCloud[str_,count_, Automatic]:=resourceSearchDataset[str, count,$cloudResourceIndex]/;Head[$cloudResourceIndex]===Dataset
resourcesearchCloud[str_,count_, Automatic]:=resourceSearchCloud[str, count, True]

resourceSearchDataset[str_,count_, ds_Dataset]:=With[{info=Normal[createResourceQuery[str][ds]]},
	If[ListQ[info],
		Normal[autoloadResource[DeleteMissing[#]]]&/@Take[info, UpTo[count]]
		,
		{}
	]
]

createResourceQuery[q_String]:=Query[Select[MemberQ[#Keywords, q] ||MemberQ[#Categories, q]||MemberQ[#ContentTypes, q]|| 
	(!StringFreeQ[#Name, q, IgnoreCase->True])|| (!StringFreeQ[#Description, q, IgnoreCase->True])  &]]

$cloudResourceIndex=None;

updateCloudResourceIndex[]:=Block[{info=cloudResourceSearchInfo[]},
	If[MatchQ[info,{_Association...}],
		Dataset[KeyUnion[info,defaultsearchvalue]]
		,
		None
	]
]

defaultsearchvalue["Categories"|"ContentTypes"|"Keywords"]:={}
defaultsearchvalue["Name"|"Description"]:=""
defaultsearchvalue[_]:=Missing[]
	
addToCloudResourceIndex[info_]:=addToCloudResourceIndex[info,$cloudResourceIndex]

addToCloudResourceIndex[info_,None]:=With[{index=addToCloudResourceIndex[]},
	If[Head[index]===Dataset,
		addToCloudResourceIndex[info,index]
		,
		$Failed
	]
]

addToCloudResourceIndex[info_Association,index_Dataset]:=With[{id=info["UUID"]},
	If[StringQ[id],
		If[MemberQ[index[All,"UUID"],id],
			dropFromCloudResourceIndex[id]
		];
		addtoCloudResourceIndex[info, index]
		,
		$Failed
	]
]

addtoCloudResourceIndex[info_, index_]:=Block[{keys, first, newindex, indexinfo},
	If[Length[index]>0,
		first=Normal[First[index]];
		indexinfo=Last[KeyUnion[{first,KeyTake[info, Keys[first]]},defaultsearchvalue]];
		newindex=Check[Append[$cloudResourceIndex,indexinfo],$Failed];
		If[newindex=!=$Failed,
			$cloudResourceIndex=newindex
			,
			$Failed
		]
	]
]

addToCloudResourceIndex[___]:=$Failed

dropFromCloudResourceIndex[id_]:=dropFromCloudResourceIndex[id,$cloudResourceIndex]

dropFromCloudResourceIndex[id_String,index_Dataset]:=Block[{newindex},
	newindex=Check[DeleteCases[index, _?(StringMatchQ[#UID, id] &)],$Failed];
	If[newindex=!=$Failed,
		$cloudResourceIndex=newindex
		,
		$Failed
	]
]

dropFromCloudResourceIndex[___]:=Null

cloudResourceSearchInfo[]:=Quiet[
	    CloudEvaluate[importLocalResourceInfo[]]]

resourcesearchLocal[str_, n_, method_Association]:=resourcesearchLocal[str, n, Lookup[method,"Local",Automatic]]

resourcesearchLocal[str_, n_, "BruteForce"]:=Block[{ids, count=n/.Automatic->Infinity},
	ids=Select[$localResources,resourceKeywordMatchQ[str,#]&,UpTo[count]];
	getResourceInfo/@ids
]


resourcesearchLocal[str_, n_, "TextSearch"]:=Block[{ids},
	ids=textSearchResources[str];
	If[ListQ[ids],
		getResourceInfo/@ids
		,
		$Failed
	]
]

resourcesearchLocal[str_, n_, Automatic]:=Block[{res=None},
	If[$AllowResourceTextSearch,
		res=Quiet[resourcesearchLocal[str, n, "TextSearch"]]
	];
	If[ListQ[res],
		res
		,
		resourcesearchLocal[str, n, "BruteForce"]
	]
]

resourcesearchLocalName[str_, n_]:=Block[{ids, count=n/.Automatic->Infinity},
	ids=Select[$localResources,resourceNameMatchQ[str,#]&,UpTo[count]];
	getResourceInfo/@ids
]

resourceKeywordMatchQ[str_,id_]:=Block[{info=Quiet[getResourceInfo[id,{"Name","Keywords"}]]},
	TrueQ[!FreeQ[info,str]]
]

resourceNameMatchQ[str_,id_]:=Block[{info=Quiet[getResourceInfo[id,{"Name"}]]},
	TrueQ[!FreeQ[info,str]]
]

$resourceSystemSearchMethods={"Automatic",Automatic,"TextSearch","Table"};

resourcesearchResourceSystem[str_String, n_, method_]:=Block[{res, opts=Sequence[]},
	If[AssociationQ[method],
		If[KeyExistsQ[method,"ResourceSystem"],
			If[MemberQ[$resourceSystemSearchMethods,method["ResourceSystem"]],
				opts="SearchMethod"->method["ResourceSystem"]
			]
		]
	];
	res=apifun["SearchResources",{"Query"->str,"Count"->n,opts},System`ResourceSearch];
	If[KeyExistsQ[res,"Resources"],
		res=Lookup[res,"Resources",{}];
		res=standardizeResourceInfo/@res;
		res=fillResourceMetadata[#, Association["RepositoryLocation"->URL[$resourceSystemRequestBase]]]&/@res;
		cacheresourceInfo[res];
		res
		,
		$Failed
	]
]

createResourceObjects[l_List]:=System`ResourceObject/@Select[l,AssociationQ]
createResourceObjects[expr_]:=expr

createResourceDataset[l_List]:=Dataset[KeyUnion[l,$SearchDataKeys]]
createResourceDataset[expr_]:=expr

ResourceSystemClient`RandomResources[args___]:=Catch[randomResources[args]]

randomResources[]:=randomResources[1]

randomResources[n_Integer]:=Block[{res},
    res=apifun["SearchResources",{"Query"->"","Count"->n,"Method"->"RandomSample"},System`ResourceSearch];
    If[KeyExistsQ[res,"Resources"],
        res=Lookup[res,"Resources",{}];
		res=fillResourceMetadata[#, Association["RepositoryLocation"->URL[$resourceSystemRequestBase]]]&/@res;
		cacheresourceInfo[res];
        System`ResourceObject[usableResourceInfo[#]]&/@res
        ,
        $Failed
    ]
]

randomResources[___]:=$Failed
End[] (* End Private Context *)

EndPackage[]

SetAttributes[{ResourceSearch},
   {ReadProtected, Protected}
];