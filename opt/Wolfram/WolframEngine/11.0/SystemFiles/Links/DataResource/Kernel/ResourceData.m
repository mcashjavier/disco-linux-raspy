(* Wolfram Language Package *)

(Unprotect[#]; Clear[#])& /@ {"System`ResourceData"}

BeginPackage["DataResource`"]
(* Exported symbols added here with SymbolName::usage *)  

System`ResourceData

Begin["`Private`"] (* Begin Private Context *) 
System`ResourceData[args___]:=Catch[resourceData[args]]

resourceData[ro_System`ResourceObject,rest___]:=resourcedata[resourceObjectID[ro],rest]

resourceData[id_String,rest___]:=resourcedata[id,rest]/;uuidQ[id]

resourceData[name_String,rest___]:=resourcedata[Lookup[localResourceNameMap,name,$Failed],rest]/;KeyExistsQ[localResourceNameMap,name]

resourceData[name_String,rest___]:=resourcedataName[name,rest]

resourceData[___]:=$Failed

resourcedata[id_String,rest___]:=resourcedatawithProgress[{id, ResourceSystemClient`Private`getResourceInfo[id]},rest]/;MemberQ[$localResources,id]

resourcedata[id_String,rest___]:=resourcedatawithProgress[{id, resourceInfo[id]},rest]/;MemberQ[$loadedResources,id]

resourcedata[id_String,rest___]:=With[{info=ResourceSystemClient`Private`loadResource[id]},
	If[AssociationQ[info],
		resourcedatawithProgress[{id, info},rest]
	]	
]

resourcedata[info_Association,rest___]:=resourcedatawithProgress[{info["UUID"],info},rest]

$resourceProgressIndicator=True;

resourcedatawithProgress[{id_String,info_Association}, rest___]:=Block[{$resourceProgressIndicator=False},
	resourcedata[{id, info},rest]
]


resourcedata[{id_String,info_Association}]:=resourcedata[{id,info},Automatic]

resourcedata[{id_String,info_Association},elem_String]:=info["InformationElements",elem]/;KeyExistsQ[Lookup[info,"InformationElements",Association[]],elem]

resourcedata[{id_String,info_Association},rest___]:=Block[{saved=ResourceSystemClient`Private`saveresourceObject[info], newinfo},
	newinfo=resourceInfo[id];
	If[!KeyExistsQ[newinfo,"Content"],
		resourcedata[{id,newinfo},rest]
		,
		Throw[$Failed]
	]		
]/;userdefinedResourceQ[info]&&!MemberQ[$localResources,id]

resourcedata[{id_String,info_Association},Automatic,rest___]:=Block[{},
	If[KeyExistsQ[info,"DefaultContentElement"],
		resourcedata[{id,info},info["DefaultContentElement"],rest]
		,
		resourcedata[{id,info},All,rest]
	]
]/;multipartResourceQ[info]


resourcedata[{id_String,info_Association}, elems0_,rest___]:=Block[{allelems=Lookup[info,"ContentElements",{}], elems},
	elems=If[elems0===All,
		allelems
		,
		If[Complement[elems0, allelems]=!={},
			Message[ResourceData::invelem];Throw[$Failed]
		];
		elems0
	];
	AssociationMap[resourcedata[{id,info}, #,rest]&,elems]
]/;ListQ[elems0]||elems0===All

resourcedata[{id_String,info_Association},elem_,rest___]:=If[
	localStoredQ[info,elem]||!marketplacebasedResourceQ[info],
	If[localStoredQ[info,elem],
		resourcedataLocal[info,elem,rest],
		If[cloudStoredQ[info,elem],
			resourcedataCloud[info,elem, rest],
			$Failed
		]
	]
	,
	resourcedataResourceSystem[id,elem,rest]	
]/;contentElementQ[info,elem]

resourcedata[_,elem_,___]:=(Message[ResourceData::invelem,elem];Throw[$Failed])

resourcedata[___]:=Throw[$Failed]

resourcedataName[name_,rest___]:=Block[{local, res},
	local=resourcesearchLocalName[name, 1];
	If[Length[local]>0,
		resourcedata[First[local],rest]
		,
		resourcedataResourceSystem[name,rest]
	]
]

resourcedataResourceSystem[str_]:=resourcedataResourceSystem[str,Automatic]

resourcedataResourceSystem[str_,elem_,rest___]:=Block[{resource},
	If[!TrueQ[$resourceProgressIndicator],
		$resourceProgressIndicator=True;
		PrintTemporary[ProgressIndicator[Appearance -> "Indeterminate"]]
	];
	resource=ResourceSystemClient`ResourceDownload[str, Association["Element"->elem,"ResponseMethod"->"Download"]];
	If[resourceObjectQ[resource],
		resourcedataLocal[resource,elem,rest]
	]
]

resourcedataLocal[ro_System`ResourceObject,rest___]:=resourcedataLocal[resourceObjectID[ro],rest]

resourcedataLocal[id_String,rest___]:=resourcedataLocal[resourceInfo[id],rest]/;MemberQ[$loadedResources,id]

resourcedataLocal[id_String,rest___]:=resourcedataLocal[ResourceSystemClient`Private`getResourceInfo[id],rest]

resourcedataLocal[info_]:=resourcedataLocal[info, Automatic]
elempattern0=(_String|Automatic|All);
elempattern=(elempattern0|{elempattern0...});

resourcedataLocal[info_Association,elem:elempattern]:=resourcedatalocal[info,elem]

resourcedataLocal[___]:=Throw[$Failed]

resourcedatalocal[info_,elem_]:=resourcedatalocal[Lookup[info,"ResourceType",Throw[$Failed]],info, elem]

resourcedatalocal[$DataResourceType,info_, elem_]:=readDataResourceLocal[Lookup[info,"UUID",Throw[$Failed]], info, elem]

resourcedataCloud[info_Association,elem_,rest___]:=With[{newinfo=cloudresourceDownload[info,Lookup[info,"ContentElementLocations",Throw[$Failed]], elem]},
	If[localStoredQ[newinfo, elem],
		resourcedataLocal[newinfo,elem, rest]
	]
]


End[] (* End Private Context *)

EndPackage[]

SetAttributes[{ResourceData},
   {ReadProtected, Protected}
];