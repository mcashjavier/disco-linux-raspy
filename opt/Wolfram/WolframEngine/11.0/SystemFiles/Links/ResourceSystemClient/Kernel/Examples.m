(* Wolfram Language Package *)

BeginPackage["ResourceSystemClient`"]

ResourceSystemClient`ExampleNotebook

Begin["`Private`"] (* Begin Private Context *) 

exampleNotebookLocation[id_]:=examplenotebookLocation[resourceDirectory[id]]

examplenotebookLocation[dir_]:=examplenotebooklocation[dir]/;DirectoryQ[dir]

examplenotebookLocation[_]:=None

examplenotebooklocation[dir_]:=FileNameJoin[{dir,"examples.nb"}]

blankExampleNotebook[_,id_]:=Notebook[
	{
	Cell[CellGroupData[{Cell["Basic Examples","Subsection"],
		Cell[CellGroupData[{Cell["First Example","Subsubsection"],
			Cell["Caption 1.","Text"],
			Cell[BoxData[RowBox[{"ResourceObject","[","...","]"}]],"Input"]},System`Open]],
		Cell[CellGroupData[{Cell["Second Example","Subsubsection"],
	Cell["Caption 2.","Text"],Cell[BoxData[RowBox[{"fun","[",RowBox[{RowBox[{"ResourceObject","[","...","]"}],",","arg"}],"]"}]],"Input"]},System`Open]]},System`Open]],
	Cell["Features","Subsection"],
	Cell["Visualization Examples","Subsection"],
	Cell["Analysis Examples","Subsection"],
	Cell["Intercactive Examples","Subsection"],
	Cell["Possible Issues","Subsection"],
	Cell[CellGroupData[{Cell["Save","Subsection"],
		If[StringQ[exampleNotebookLocation[id]],
			Cell[BoxData[ButtonBox["\"Save Example Notebook\"",Appearance->Automatic,
				ButtonFunction:>(ResourceSystemClient`Private`saveExampleNotebook[id,EvaluationNotebook[]]),Evaluator->Automatic,Method->"Preemptive"]],"Output"],
			Cell[BoxData[ButtonBox["\"Save Example Notebook\"",Appearance->Automatic,
				ButtonFunction:>(ResourceSystemClient`Private`saveresourceObject[id,Association[]];
				ResourceSystemClient`Private`saveExampleNotebook[id,EvaluationNotebook[]]),Evaluator->Automatic,Method->"Preemptive"]],"Output"]
		]
				},System`Open]]
	}
	,StyleDefinitions->"Default.nb"]
  

saveExampleNotebook[id_, nb_]:=With[{file=exampleNotebookLocation[id]},
	NotebookSave[nb, file];
	setResourceInfo[id, Association["ExampleNotebook"->file]];
	file
]

createBlankExampleNotebook[rtype_,id_,name_]:=Block[{nb},
	loadResourceType[rtype];
	nb=NotebookPut[blankExampleNotebook[rtype,id]];
	SetOptions[nb,"WindowTitle" -> "Examples for "<>ToString[name]];
	nb
]  

deployExampleNotebook[nb_]:=CloudDeploy[nb,IconRules->{},Permissions->{"marketplace-admin@wolfram.com"->"Read"}]


ResourceSystemClient`ExampleNotebook[args___]:=Catch[exampleNotebook[args]]

exampleNotebook[ro_System`ResourceObject]:=exampleNotebook[resourceObjectID[ro]]

exampleNotebook[id_String]:=examplenotebook[id]/;uuidQ[id]

exampleNotebook[name_String]:=examplenotebook[Lookup[localResourceNameMap,name,$Failed]]/;KeyExistsQ[localResourceNameMap,name]

exampleNotebook[___]:=$Failed

examplenotebook[id_String]:=examplenotebook[{id, getResourceInfo[id]}]/;MemberQ[$localResources,id]

examplenotebook[id_String]:=examplenotebook[{id, resourceInfo[id]}]/;MemberQ[$loadedResources,id]

examplenotebook[id_String]:=With[{info=ResourceSystemClient`Private`loadResource[id]},
	If[AssociationQ[info],
		examplenotebook[{id, info}]
		,
		$Failed
	]	
]

examplenotebook[info_Association]:=examplenotebook[{info["UUID"],info}]

examplenotebook[{id_String, info_Association}]:=customExampleNotebook[id, info]/;userdefinedResourceQ[info]
examplenotebook[{id_String, info_Association}]:=resourcesystemExampleNotebook[id, info]/;marketplacebasedResourceQ[info]

customExampleNotebook[id_, info_]:=Block[{file=exampleNotebookLocation[id]},
	If[TrueQ[Quiet[FileExistsQ[file]]],
		NotebookOpen[file],
		createBlankExampleNotebook[Lookup[info,"ResourceType"],id,Lookup[info,"Name"]]
	]	
]

resourcesystemExampleNotebook[id_, info_]:=With[{nb=info["ExampleNotebook"]},
	If[FileExistsQ[nb],
		NotebookOpen[nb]
		,
		resourcesystemExampleNotebook[id, KeyDrop[info,"ExampleNotebook"]]
	]	
]/;KeyExistsQ[info,"ExampleNotebook"]

resourcesystemExampleNotebook[id_, info_]:=Block[{res, nb},
	res=apifun["ExampleNotebook",{"UUID"->id}, ResourceSystemClient`ExampleNotebook];
	If[KeyExistsQ[res,"ExampleNotebook"],
		nb=NotebookOpen[res["ExampleNotebook"]];
		If[Head[nb]===NotebookObject,
			setResourceInfo[id, Association["ExampleNotebook"->res["ExampleNotebook"]]];
			nb
			,
			$Failed
		]
	]
]


validateexamplenotebook[co_CloudObject]:=With[{res=setReviewerPermissions[co]},
    If[!ListQ[res],
        Message[ResourceSubmit::appperms,co];
        Throw[$Failed]
    ];
    co
]

validateexamplenotebook[nb:(_NotebookObject|_Notebook)]:=With[{res=deployExampleNotebook[nb]},
    If[Head[res]=!=CloudObject,
        Message[ResourceSubmit::enbdf];
        Throw[$Failed]
    ];
    res
]

validateexamplenotebook[file_String]:=With[{nb=Get[file]},
    If[Head[nb]=!=Notebook,
        Message[ResourceSubmit::enbdf];
        Throw[$Failed]
    ];
    validateexamplenotebook[nb]
]/;FileExistsQ[file]






End[] (* End Private Context *)

EndPackage[]