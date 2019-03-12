Package["TextSearch`"]

PackageImport["Macros`"]

SetArgumentCount[DeleteSearchIndex, 1];

DeleteSearchIndex[expr_] := If[!FreeQ[iDeleteSearchIndex[expr], $Failed], $Failed];
DeleteSearchIndex[All] :=
	Block[{},
		Function[{directory},
			If[FileExistsQ[directory],
				DeleteAllHandles[];
				Quiet @ DeleteDirectory[directory, DeleteContents -> True]
			]
		] /@ $SearchIndicesDirectories;
	]

iDeleteSearchIndex[list_List] := 
	Map[iDeleteSearchIndex, list];

iDeleteSearchIndex[obj:SearchIndexObject[File[path_String]]] := (  
    DeleteHandle[obj];
    Quiet @ Check[DeleteDirectory[path, DeleteContents -> True], $Failed];
)


iDeleteSearchIndex[name_String] := 
	iDeleteSearchIndex[SearchIndexObject[name]];

iDeleteSearchIndex[obj_] := 
	(Message[DeleteSearchIndex::invind, obj]; $Failed);

iDeleteSearchIndex[$Failed] := $Failed;
