(* Wolfram Language Package *)

(Unprotect[#]; Clear[#])& /@ {ResourceSystemClient`ResourceExecute}


BeginPackage["ResourceSystemClient`"]
(* Exported symbols added here with SymbolName::usage *)  
ResourceSystemClient`ResourceExecute
ResourceSystemClient`ResourceInformation

Begin["`Private`"] (* Begin Private Context *) 

ResourceSystemClient`ResourceExecute[args___]:=Catch[resourceExecute[args]]

resourceExecute[resource:rpat,rest___]:=resourceExecute[resourceObjectID[resource],rest]

resourceExecute[id_String,rest___]:=resourceexecute[id,resourceInfo[id],rest]/;MemberQ[$loadedResources, id]

resourceExecute[id_String,rest___]:=With[{loaded=loadResource[id]},
		If[AssociationQ[loaded],
			resourceexecute[id,loaded,rest]
			,
			Throw[$Failed]
		]
	]

resourceexecute[id_String,info_Association,rest___]:=With[{rtype=Lookup[info, "ResourceType"]},
	loadResourceType[rtype];
	resourceexecute0[rtype,id,info,rest]
]

resourceExecute[expr_,___]:=(Message[ResourceSystemClient`ResourceExecute::invapp,expr];Throw[$Failed])



ResourceSystemClient`ResourceInformation[args___]:=Catch[resourceInformation[args]]

resourceInformation[resource:rpat,rest___]:=resourceInformation[resourceObjectID[resource],rest]

resourceInformation[id_String,rest___]:=resourceinformation[id,resourceInfo[id],rest]/;MemberQ[$loadedResources, id]

resourceInformation[id_String,rest___]:=With[{loaded=loadResource[id]},
		If[AssociationQ[loaded],
			resourceinformation[id,loaded,rest]
			,
			Throw[$Failed]
		]
	]

resourceInformation[expr_,___]:=(Message[ResourceExecute::invapp,expr];Throw[$Failed])

resourceinformation[id_String,info_Association,rest___]:=resourceinformation0[Lookup[info, "ResourceType"],id,info,rest]

$ReservedProperties={"Properties"};

resourceinformation0[type_,id_,info_,as_Association,rest___]:=With[{prop=as["Property"]},
	If[KeyExistsQ[info, prop]||MemberQ[$ReservedProperties,prop],
		resourceMetadataLookup[type, id, info, prop,rest]
		,
		Message[ResourceObject::unkpar,prop];Throw[$Failed]
	]
]/;KeyExistsQ[as,"Property"]

resourceMetadataLookup[type_, id_, info_, "Properties",___]:=Keys[info]

resourceMetadataLookup[type_, id_, info_, str_,rest___]:=formatResourceMetadata[Lookup[info,str], str, rest]/;KeyExistsQ[info, str]

formatResourceMetadata[elems_,"ContentElements",___]:=DeleteCases[elems,Automatic]

formatResourceMetadata[expr_,___]:=expr

readresource[id_, params_]:=apifun["ReadResource",Join[params,Association["UUID"->id]],System`ResourceObject]


End[] (* End Private Context *)

EndPackage[]

SetAttributes[{ResourceSystemClient`ResourceExecute},
   {ReadProtected, Protected}
];