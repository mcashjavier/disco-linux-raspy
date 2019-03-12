Package["TextSearch`"]

PackageImport["Macros`"]

SetArgumentCount[UpdateSearchIndex, 1];

UpdateSearchIndex[e_] := iUpdateSearchIndex[e];

iUpdateSearchIndex[name_String] := 
	iUpdateSearchIndex[SearchIndexObject[name]];

iUpdateSearchIndex[e_] := (
	Message[UpdateSearchIndex::invind, e]; 
	$Failed
);

iUpdateSearchIndex[obj_SearchIndexObject] := Module[
	{handle, notstale, docs, sources},
	If[!ValidSearchIndexObjectQ[obj],
		Message[UpdateSearchIndex::invind, obj];
		Return[$Failed];
	];
	handle = CreateHandle[obj];
	If[FailureQ[handle],
		Message[UpdateSearchIndex::badind, obj];
		Return[$Failed]];
	notstale = Catch[DRemoveStaleDocumentsFromIndex[handle]];
	If[!Developer`StringVectorQ[notstale], 
		Message[UpdateSearchIndex::interr];
		Return[$Failed];
	];
	sources = NormalizeSources[obj, UpdateSearchIndex];
	If[sources === {},
		(* For example, if an index was built from a specific list of files,
		   and all of those files are now gone. *)
		Return[obj]
	];
	docs = EnumerateFilesToIndex[sources];
	docs = Complement[docs, notstale];
	docs = DeleteCases[docs, $ExplicitFile[s_] /; MemberQ[notstale, s]];
	$IndexedFileCount = 0;
	DAddToIndex[handle, docs];
	obj
];
