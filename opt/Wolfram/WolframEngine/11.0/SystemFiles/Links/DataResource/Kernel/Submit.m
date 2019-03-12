(* Wolfram Language Package *)

(Unprotect[#]; Clear[#])& /@ {}

BeginPackage["DataResource`"]

Begin["`Private`"] (* Begin Private Context *) 


$CommonDataResourceDeployPattern=(_Dataset|_Image|_Sound|_List);

ResourceSystemClient`Private`repositoryValidateSubmission[$DataResourceType,id_, info_]:=validateSubmission[id, info]

validateSubmission[id_, info_]:=info/;KeyExistsQ[info,"Content"]

validateSubmission[id_, info0_]:=Block[{location=info0["ContentElementLocations"], content, info=KeyDrop[info0,"ContentElementLocations"]},
	Switch[Head[location],
		System`File|LocalObject,
		content=Import[location];
		If[content=!=$Failed,
			Join[info,Association["Content"->importlocal[location],"Asynchronous"->False]]
			,
			Throw[$Failed]
		]
		,
		CloudObject,
		info0,
		String,
		If[FileExistsQ[location],
			Join[info,Association["Content"->importlocal[location],"Asynchronous"->False]]
			,
			If[!StringFreeQ[location,$CloudBase],
				Append[info,"ContentElementLocations"->CloudObject[location]]
				,
				Message[ResourceSubmit::invcon];Throw[$Failed]
			]
		],
		Association,
		multipartValidateSubmission[id, info0]
	]
]/;KeyExistsQ[info0,"ContentElementLocations"]

validateSubmission[___]:=$Failed

multipartValidateSubmission[id_, info_]:=Block[{locations, content, local, cloud},
	If[Lookup[info,"Content",{}]=!={},Message[ResourceSubmit::invcon];Throw[$Failed]];
	locations=Lookup[info,"ContentElementLocations"];
	If[!AssociationQ[locations],Message[ResourceSubmit::invcon];Throw[$Failed]];
	cloud=Select[locations,(MatchQ[Head[#], CloudObject] &)];
	local=Select[locations,((MatchQ[Head[#], System`File | LocalObject])||Quiet[stringFileExistsQ[#]])&];
	If[Complement[Keys[locations],Keys[cloud],Keys[local]]=!={},Message[ResourceSubmit::invcon];Throw[$Failed]];
	content=importlocal/@local;
	locations=Join[cloud,Automatic&/@local];
	Join[info,Association["ContentElementLocations"->locations,"Content"->content],
		Association@If[Length[local]>0,"Asynchronous"->Automatic,{}]]
]


stringFileExistsQ[str_String]:=FileExistsQ[str]
stringFileExistsQ[___]:=False


ResourceSystemClient`Private`repositorycompleteResourceSubmission[$DataResourceType, id_,as0_]:=Block[{as, allelems,funcs},
	as=DeleteMissing[AssociationMap[validateParameter["DataResource",#]&,as0]];
	funcs=getAllElementFunction[id,Lookup[as0,"ContentElements"]];
	If[Keys[funcs]=!={},
		as["ContentElementFunctions"]=funcs
	];
	If[!(KeyExistsQ[as,"Content"]||KeyExistsQ[as,"ContentElementLocations"]),Message[ResourceSubmit::noncont];Throw[$Failed]];
	as
]

End[] (* End Private Context *)

EndPackage[]

SetAttributes[{},
   {ReadProtected, Protected}
];