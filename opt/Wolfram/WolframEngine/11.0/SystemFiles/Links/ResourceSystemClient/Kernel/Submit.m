(* Wolfram Language Package *)

(Unprotect[#]; Clear[#])& /@ {System`ResourceSubmit}

BeginPackage["ResourceSystemClient`"]
(* Exported symbols added here with SymbolName::usage *)  

System`ResourceSubmit

Begin["`Private`"] (* Begin Private Context *) 

$SubmissionSizeLimit=10^6;

System`ResourceSubmit[args___]:=Catch[resourceSubmitCloudConnect[args]]

resourceSubmitCloudConnect[args___]:=resourceSubmit[args]/;$CloudConnected

resourceSubmitCloudConnect[args___]:=(CloudConnect[];
	If[$CloudConnected,
		resourceSubmit[args],
		Throw[$Failed]
	])

resourceSubmit[ro_System`ResourceObject]:=resourceSubmitRO[ro]

resourceSubmit[___]:=(Message[ResourceSubmit::noro];$Failed)

resourcesubmit[as_Association]:=With[{fullparams=completeResourceSubmission[as]},
	submitresourceToSystem[fullparams]
]

resourceSubmitRO[ro:HoldPattern[System`ResourceObject][as_Association]]:=With[{id=Lookup[as,"UUID",Throw[$Failed]]},
	If[MemberQ[$loadedResources,id],
		resourceSubmitRO[id,resourceInfo[id]]
		,
		resourceSubmitRO[id,as]
	]
]

resourceSubmitRO[id_,info_]:=(Message[System`ResourceSubmit::exists];$Failed)/;!userdefinedResourceQ[info]

resourceSubmitRO[id_,info_]:=Block[{saved=saveresourceObject[info], newinfo},
	newinfo=resourceInfo[id];
	If[!KeyExistsQ[newinfo,"Content"],
		resourceSubmitRO[id, newinfo]
		,
		Throw[$Failed]
	]		
]/;!MemberQ[$localResources,id]


resourceSubmitRO[id_, info_]:=With[{rtype=Lookup[info,"ResourceType"]},
	If[!StringQ[rtype],
		Message[ResourceSubmit::invrt,rtype]
	];
	loadResourceType[rtype];
	resourcesubmit[
		repositoryValidateSubmission[rtype,id, info]
	]
	
]

repositoryValidateSubmission[_,id_, info_]:=info

resourceSubmitRO[___]:=$Failed

completeResourceSubmission[as_Association]:=With[{rtype=as["ResourceType"]},
	loadResourceType[rtype];
	repositorycompleteResourceSubmission[rtype,Lookup[as,"UUID"],KeyDrop[as,$nonsubmittedParameters]]]/;KeyExistsQ[as,"ResourceType"]

	
completeResourceSubmission[as_Association]:=With[{rtype=promptForResourceType[]},
    If[!StringQ[rtype],Throw[$Failed]];
	completeResourceSubmission[Append[as,"ResourceType"->rtype]]]

$nonsubmittedParameters={"UUID","Version","ContentSize","RepositoryLocation","ResourceLocations","DownloadedVersion"}

repositorycompleteResourceSubmission[rtype_,_,as0_]:=Block[{missingKeys, as, values},
	loadResourceType[rtype];
	as=DeleteMissing[AssociationMap[validateParameter[rtype,#]&,as0]];
	If[!FreeQ[as,_Failure],Message[ResourceSubmit::invprop];Throw[$Failed]];
	missingKeys=Complement[Keys[requiredparameters[rtype]],Keys[as]];
	values=promptForMissingKeys[rtype, KeyTake[requiredparameters[rtype],missingKeys]];
	If[!AssociationQ[values],Throw[$Failed]];
	Join[as, values]
]

promptForResourceType[]:=FormFunction[{"ResourceType"->{"DataResource","App"}},#ResourceType&][]

promptForMissingKeys[rtype_, {}|Association[]]:=Association[]
promptForMissingKeys[rtype_, signature_]:=FormFunction[signature,#&][]

submitresourceToSystem[as_]:=Block[{res},
	res=apifun["SubmitResource",as, System`ResourceSubmit];
	If[Quiet[KeyExistsQ[res,"SubmissionID"]],
		ResourceSubmissionObject[res]
		,
		$Failed
	]
	
]

End[] (* End Private Context *)

EndPackage[]

SetAttributes[{ResourceSubmit},
   {ReadProtected, Protected}
];