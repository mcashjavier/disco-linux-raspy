(* Wolfram Language Package *)

(Unprotect[#]; Clear[#])& /@ {}

BeginPackage["DataResource`"]

Begin["`Private`"] (* Begin Private Context *) 


$ResourceElementStorageSizeLimit=10^4;(* Bytes *)

ResourceSystemClient`Private`repositorystandardizeContentMetadata[$DataResourceType,id_, info_]:=standardizeContentMetadata[id, info]

standardizeContentMetadata[_, info_]:=(Message[ResourceObject::twodef];Throw[$Failed])/;KeyExistsQ[info,"Content"]&&KeyExistsQ[info,"ContentLocation"]

standardizeContentMetadata[id_, info_]:=Block[{elements, locations,
	functions,moreinfo, contentelements, default=Lookup[info,"DefaultContentElement",Automatic],storedelements},
	locations=Lookup[info,"ContentElementLocations",Association[]];
	contentelements=Lookup[info,"ContentElements",Association[]];
	If[ListQ[contentelements],contentelements=Association[]];
	If[!AssociationQ[contentelements],
		Message[ResourceObject::invas,"ContentElements"]; Throw[$Failed]
	];
	If[!AssociationQ[locations],
		Message[ResourceObject::invas,"ContentElementLocations"]; Throw[$Failed]
	];
	If[KeyExistsQ[info,"Content"],
		If[default=!=Automatic,
			Message[ResourceObject::twodef];Throw[$Failed]
		];
		If[KeyExistsQ[contentelements,"Content"],
			Message[ResourceObject::twocont];Throw[$Failed]
		];
		default="Content";
		contentelements=Prepend[contentelements,"Content"->info["Content"]]			
	];
	If[KeyExistsQ[info,"ContentLocation"],
		If[default=!=Automatic,
			Message[ResourceObject::twodef];Throw[$Failed]
		];
		If[KeyExistsQ[locations,"Content"],
			Message[ResourceObject::twocont];Throw[$Failed]
		];
		default="Content";
		locations=Prepend[locations,"Content"->info["ContentLocation"]]			
	];
	functions=Lookup[info,"ContentElementFunctions",Association[]];
	If[!AssociationQ[functions],
		Message[ResourceObject::invas,"ContentElementFunctions"]; Throw[$Failed]
	];
	
	If[KeyExistsQ[info,"InformationElements"],
		moreinfo=Lookup[info,"InformationElements",Association[]];
		If[!AssociationQ[moreinfo],
			Message[ResourceObject::invas,"InformationElements"]; Throw[$Failed]
		]
		,
		moreinfo=Select[contentelements,(ByteCount[#]<$ResourceElementStorageSizeLimit)&];
		contentelements=KeyComplement[{contentelements,moreinfo}];
	];
	
	elements=Flatten[Keys/@{contentelements,locations,functions,moreinfo}];
	If[!DuplicateFreeQ[elements],
		Message[ResourceObject::elemcon,Cases[Tally[elements], {el_, _?(# > 1 &)} :> el, {1}]];Throw[$Failed]
	];
	elements=Sort[elements];
	storedelements=Sort[Flatten[Keys/@{contentelements,locations}]];
	If[!MemberQ[Append[elements, Automatic],default],
		Message[ResourceObject::invdefa,default]; Throw[$Failed]
	];
	Association[
		KeyDrop[info,{"ContentElements","ContentElementLocations","Content","ContentLocation","InformationElements"}]
		,
		Association[
			"ContentElements"->elements,
			"ContentElementLocations" -> AssociationMap[Lookup[locations,#,None]&,storedelements],
			"RepositoryLocation"->None,
			"InformationElements"->moreinfo,
			"ContentValues"->contentelements
		]
		,
		If[default===Automatic,
			Association[
					"ContentElementAccessType"->"Multipart",
					"ContentSize"->If[Keys[locations]==={},
						ByteCount[{contentelements,moreinfo}]
						,
						Missing["NotAvailable"]
					]
				]
			,
			{
			If[KeyExistsQ[contentelements,default],
				Association[
					"ContentElementAccessType"->ToString[Head[contentelements[default]]],
					"ContentSize"->bytecountQuantity[ByteCount[contentelements[default]]]
				]
				,
				If[KeyExistsQ[moreinfo,default],
					Association[
						"ContentElementAccessType"->ToString[Head[moreinfo[default]]],
						"ContentSize"->bytecountQuantity[ByteCount[moreinfo[default]]]
					],
					Association[
						"ContentElementAccessType"->Missing["NotAvailable"],
						"ContentSize"->Missing["NotAvailable"]
					]
				]
			]
			,
			"DefaultContentElement"->default
			}
		]
	]
]

standardizeContentMetadata[___]:=(Message[ResourceObject::nocont]; Throw[$Failed])


ResourceSystemClient`Private`repositorysaveresourceobject[$DataResourceType,info_]:=saveresourceobject[info]

saveresourceobject[info_]:=saveMultipartResource[info]/;AssociationQ[Lookup[info,"ContentElementLocations"]]

saveresourceobject[info_]:=saveresourceobjectwithcontent[info]/;Lookup[info,"ContentElementLocations"]==None

saveresourceobject[info_]:=saveResourceObjectWithLocation[info]/;KeyExistsQ[info,"ContentElementLocations"]

saveresourceobject[info_]:=saveresourceobjectwithcontent[info]/;KeyExistsQ[info,"Content"]

saveresourceobject[info_]:=(Message[ResourceObject::nocont];Throw[$Failed])


saveresourceobjectwithcontent[info0_]:=Block[{content, id=Lookup[info0,"UUID",Throw[$Failed]], 
	info=KeyDrop[info0,"Content"]},
	content=info0["Content"];
	info["RepositoryLocation"]=localObject[resourceCacheDirectory[]];
	cacheresourceinfo[info];
	
	resourcedownload0[Lookup[info,"UUID"],Join[
		KeyTake[info,{"UUID","ContentElementFunctions","ResourceType","Version"}],
		Association["Content"->content,"ContentFormat"->"WDF"]]];
	id
	]


saveResourceObjectWithLocation[info0_]:=Block[{id=Lookup[info0,"UUID",Throw[$Failed]],format, 
	info=info0,copyinfo, lo=Lookup[info0,"ContentElementLocations",Throw[$Failed]]},
	If[!MatchQ[lo,(_CloudObject|_LocalObject)],Message[ResourceObject::invloc];Throw[$Failed]];
	info["RepositoryLocation"]=localObject[resourceCacheDirectory[]];
	cacheresourceinfo[info];
	saveresourceObjectWithLocation[id, info, lo];
	id
]
	
	
saveresourceObjectWithLocation[id_, info_, lo_, elem_:Automatic]:=Block[{copyinfo},
	copyinfo=resourcedownloadInfo[id, info,lo];
	If[AssociationQ[copyinfo],
        Put[copyinfo,dataresourceCopyInfoFile[id,Automatic,elem]]
        ,
        Throw[$Failed]
	];
	storecontentFunctions[id,Lookup[info,"ContentElementFunctions"]];
	ResourceSystemClient`Private`storeDownloadVersion[id,Association["Version"->None],{lo},{Automatic}, Association["Element"->elem]];
	]

saveMultipartResource[info0_]:=Block[{
	id=Lookup[info0,"UUID",Throw[$Failed]],format, 
	info=KeyDrop[info0,"ContentValues"],copyinfo, 
	locations=Lookup[info0,"ContentElementLocations",Throw[$Failed]], 
	rawelements=Lookup[info0,"ContentValues",Throw[$Failed]]
	},
	info["RepositoryLocation"]=localObject[resourceCacheDirectory[]];
	cacheresourceinfo[info];
	
	AssociationMap[savemultipartResource[info,#]&, rawelements];
	AssociationMap[savemultipartResourceLocation[id,info, #]&, DeleteCases[locations, None]];

	storecontentFunctions[id,Lookup[info,"ContentElementFunctions"]];
	id
	]
	

savemultipartResource[info_,_[elem_,value_]]:=resourcedownload0[
	Lookup[info,"UUID"],
	Join[
		KeyTake[info,{"UUID","ResourceType","Version"}],
		Association["Format"->"WDF","Content"->value,"ContentFormat"->"WDF","Element"->elem]]
	]

savemultipartResourceLocation[id_,info_,_[elem_,value_]]:=saveresourceObjectWithLocation[id, info, value, elem]

End[] (* End Private Context *)

EndPackage[]



SetAttributes[{},
   {ReadProtected, Protected}
];