(* Wolfram Language Package *)

(Unprotect[#]; Clear[#])& /@ {System`ResourceAcquire}

BeginPackage["ResourceSystemClient`"]

System`ResourceAcquire
System`ResourceRemove

Begin["`Private`"] (* Begin Private Context *) 

$ResourceSystemSyncedQ=True/;$CloudEvaluation
$ResourceSystemSyncedQ=False


(*** ResourceAcquire ***)

System`ResourceAcquire[args___]:=Catch[resourceAcquire[args]]

resourceAcquire[resource:rpat]:=resourceAcquire[resourceObjectID[resource]]

resourceAcquire[id_]:=(loadResource[id];resourceAcquire[id])/;MemberQ[$localResources,id]&&!MemberQ[$loadedResources,id]
resourceAcquire[id_]:=System`ResourceObject[id]/;MemberQ[$localResources, id]&&myAccountQ[id]
resourceAcquire[id_]:=With[{info=loadResource[id]},
	If[AssociationQ[info],
        cacheresourceinfo[info];
		resourceAcquire[id]
		,
		$Failed
	]]/;MemberQ[$cloudResources,id]||MemberQ[$myResourceSystemResources,id]

resourceAcquire[str_,addToAccount_:True]:=Block[{info, id},
	info=importresourceInfo[str,addToAccount];
	id=info["UUID"];
	System`ResourceObject[id]
]

importresourceInfo[str_,addToAccount_]:=Block[{params, info, id},
	params={If[uuidQ[str],
        "UUID"->str,
        "Name"->str
    ],"RecordUserAccess"->addToAccount,"Elements"->"True","ContentElementFunctions"->"True"};
	info=importresourceinfo[params];
	If[Quiet[TrueQ[KeyExistsQ[info,"UUID"]]],
		info=fillResourceMetadata[info, Association["RepositoryLocation"->URL[$resourceSystemRequestBase],"MyAccount"->addToAccount]];
		id=info["UUID"];
		cacheresourceinfo[info]
		,
		Message[ResourceObject::notf];
		Throw[$Failed]
	]
]


importresourceinfo[params_]:=With[{res=apifun["AcquireResource",params, System`ResourceAcquire]},
	standardizeResourceInfo[res]
]


myAccountQ[id_]:=myaccountQ[resourceinfo[id]]
myaccountQ[as_Association]:=Lookup[as,"MyAccount",False]
myaccountQ[_]:=False




(*** ResourceRemove ***)

System`ResourceRemove[args___]:=Catch[resourceRemove[args]]

resourceRemove[resource:rpat]:=resourceRemove[resourceObjectID[resource]]

resourceRemove[id_]:=Block[{info},
	info=resourceInfo[id];
	If[AssociationQ[info],
		deleteresourcecache[info];
		If[myaccountQ[info],
			info=removeResourceFromAccount[If[uuidQ[id],
		        {"UUID"->id},
		        {"Name"->id}
		    ]]
		];
	    If[KeyExistsQ[info,"UUID"],
			id
			,
			Throw[$Failed]
		]
		,
		deleteresourcecache[id, Missing[]];
		id
	]
	
]

removeResourceFromAccount[params_]:=With[{res=apifun["RemoveResource",params, System`ResourceAcquire]},
	res
]


End[] (* End Private Context *)

EndPackage[]

SetAttributes[{ResourceAcquire},
   {ReadProtected, Protected}
];