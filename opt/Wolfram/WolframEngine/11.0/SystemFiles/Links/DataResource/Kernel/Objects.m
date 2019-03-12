(* Wolfram Language Package *)

(Unprotect[#]; Clear[#])& /@ {}

BeginPackage["DataResource`"]

Begin["`Private`"] (* Begin Private Context *) 

ResourceSystemClient`Private`repositoryBelowFoldItems[drt:$DataResourceType,id_, info_]:={
    BoxForm`SummaryItem[{"Categories: ", Short[Row[Lookup[info,"Categories",{}],","]]}],
    BoxForm`SummaryItem[{"ContentTypes: ", Short[Row[Lookup[info,"ContentTypes",{}],","]]}],
	BoxForm`SummaryItem[{"Keywords: ", Short[Row[Lookup[info,"Keywords",{}],","]]}],
    BoxForm`SummaryItem[{"Data Location: ", typesetStorageLocation[drt,id]}],
    BoxForm`SummaryItem[{"UUID: ", id}],
	BoxForm`SummaryItem[{"Version: ", Lookup[info,"Version",None]}],
	BoxForm`SummaryItem[{"Size: ", typesetSize[info]}],
	BoxForm`SummaryItem[{"Elements: ", Short[Row[Lookup[info,"ContentElements",{}],","],2]}]
    }


typesetStorageLocation[$DataResourceType,id_]:=DynamicModule[{typesetstorage=typesetstorageLocation, info},
	Dynamic[info=resourceInfo[id];
		If[AssociationQ[info],
			typesetstorage[Lookup[info,"ContentElementLocations",None]]
			,
			Missing["NotAvailable"]
		]
	]
]

typesetstorageLocation[as_Association]:=Row[Riffle[DeleteDuplicates[typesetstorageLocation/@Values[as]]," "]]
typesetstorageLocation[l_System`LocalObject]="Local"
typesetstorageLocation[c_CloudObject]="Cloud"
typesetstorageLocation[{_CloudObject..}]="Cloud"
typesetstorageLocation[ResourceSystemClient`$ResourceSystemRequestBase]=Text[Style["Repository", 8, Black], Background -> Red]
typesetstorageLocation[str_String]=str
typesetstorageLocation[m_Missing]=m
typesetstorageLocation[None]=Style[None,Gray]
typesetstorageLocation[expr_]=Null

$drDirectory=DirectoryName[System`Private`$InputFileName];

ResourceSystemClient`Private`resourceIcon[$DataResourceType]:=ResourceSystemClient`Private`resourceicon[
	FileNameJoin[{$drDirectory,"Images","dataResourceIcon.pdf"}]]

typesetSize[info_]:=bytecountQuantity[Lookup[info,"ContentSize",Missing["NotAvailable"]]]


End[] (* End Private Context *)

EndPackage[]



SetAttributes[{},
   {ReadProtected, Protected}
];