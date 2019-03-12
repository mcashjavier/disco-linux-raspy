Package["TextSearch`"]


PackageScope["$CurrentVersion"]

$CurrentVersion = 1;

PackageScope["$MinimumSupportedVersion"]

$MinimumSupportedVersion = 1;

PackageScope["$HandleCache"]

$HandleCache = Data`UnorderedAssociation[];

PackageScope["ResetHandleCache"]

PackageScope["DeleteHandle"]

DeleteHandle[obj_SearchIndexObject] := Block[{handle},
	handle = Lookup[$HandleCache, obj, Return[$Failed]];
	If [handle === $Failed, Return[$Failed]];
	$HandleCache[obj] =.;
	Unprotect[IndexMetadata];
	IndexMetadata[handle] =.;
	Protect[IndexMetadata];
	DDeleteHandle[handle]
];


PackageScope["DeleteAllHandles"]

DeleteAllHandles[] := (
	Scan[DDeleteHandle, Values[$HandleCache]];
	$HandleCache = Data`UnorderedAssociation[];
	Unprotect[IndexMetadata];
	Clear[IndexMetadata];
	IndexMetadata[_] := Missing[];
	Protect[IndexMetadata];
);


PackageScope["CreateHandle"]

(* TODO: Use an ExpressionTable to associate the cache with the 
   SearchIndexObject, so that the handle will be closed as soon
   as the SearchIndexObject gets garbage collected. Then we can
   use ManagedLibraryExpression to close the link on the CLucene
   side *)

CreateHandle[obj_SearchIndexObject] :=
	Lookup[$HandleCache, obj, With[{tmp = iCreateHandle[obj]}, If [!FailureQ[tmp], $HandleCache[obj] = tmp, tmp]]];

CreateHandle[_] := $Failed;

PackageScope["IndexMetadata"]

IndexMetadata[_] := Missing[]

(* If this function returns $Failed it should also issue a message explaining
 * the reason this index couldn't be used to the user.
 *)
iCreateHandle[obj_] := Block[
	{meta, path, driver, version, handle},
	meta = GetIndexMetadata[obj];
	checkCorrupt[meta, obj];
	path = Lookup[meta, "IndexLocation", obj[[1,1]]];
	driver = Lookup[meta, "Driver", Lookup[meta, "IndexType", $Failed]];
	version = Lookup[meta, "Version", Infinity];
	If[version < $MinimumSupportedVersion,
		Message[SearchIndexObject::versionexp];
		Return[$Failed]
	];
	If[version > $CurrentVersion, 
		Message[SearchIndexObject::version];
		Return[$Failed]
	];
	checkCorrupt[driver, obj];
	result = DCreateHandle[path, driver, version];
	checkCorrupt[result, obj];
	Unprotect[IndexMetadata];
	IndexMetadata[result] = meta;
	Protect[IndexMetadata];
	result
];

SearchIndexObject::version = "The given SearchIndexObject was created by a later version of Wolfram Language and cannot be used.";
SearchIndexObject::versionexp = "The experimental format used for this index is no longer supported; create a new index from the original source.";

checkCorrupt[thing_, obj_] := If[FailureQ[thing],
	Message[SearchIndexObject::badind, InputForm[obj]];
	Return[$Failed, Block]
];