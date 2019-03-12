Package["TextSearch`"]

PackageImport["GeneralUtilities`"]

PackageScope["ToObjectPath"]

ToObjectPath[File[name_String]] := ExpandFileName @ name;
ToObjectPath[name_String] :=
	Block[{file},
		Function[{directory},
			file = FileNameJoin[{directory, name}];
			If [FileExistsQ[file],
				Return[file, Block];
			];
		] /@ $SearchIndicesDirectories;
		
		FileNameJoin[{$SearchIndicesDirectory, name}]
	];
ToObjectPath[_] := $Failed;

ToObjectPathShort[name_String] := Block[{path},
	path = ToObjectPath[name];
	If[!DirectoryQ[path] && StringMatchQ[name, Repeated[HexadecimalCharacter, 8]],
		Function[{directory},
			path = First[FileNames[name <> "-*", directory], $Failed];
			If [path =!= $Failed,
				Return[path, Block];
			];
		] /@ $SearchIndicesDirectories;
	];
	path
];
	

PackageExport["SearchIndexObject"]

PackageScope["ValidSearchIndexObjectNameQ"]

ValidSearchIndexObjectNameQ[File[str_String]] := True;
ValidSearchIndexObjectNameQ[str_String] := StringMatchQ[str, (WordCharacter | "-" | "_") ..];
ValidSearchIndexObjectNameQ[_] := False;

General::neind = "There is no SearchIndexObject with name ``.";
General::invindnm = "`` is not a valid name for a SearchIndexObject.";

SearchIndexObject[name_String] := Catch[
	name // FailUnless[ValidSearchIndexObjectNameQ, SearchIndexObject::invindnm, name] // 
	ToObjectPathShort // FailUnless[DirectoryQ, SearchIndexObject::neind, name] //
	File // SearchIndexObject
];


PackageExport["$SearchIndicesDirectory"]

EnsureDirectory[dir_] := If[DirectoryQ[dir], dir, CreateDirectory[dir]];

$SearchIndicesDirectory := $SearchIndicesDirectory =
	EnsureDirectory @ FileNameJoin[{$UserBaseDirectory, "SearchIndices"}]

(* Previous location. Now only used for backwards compatibility in case there's
   an old index here. *)
$OldSearchIndicesDirectory := $OldSearchIndicesDirectory = Switch[$OperatingSystem,
	"Unix", FileNameJoin[{$HomeDirectory, ".Wolfram", "SearchIndices"}],
	"MacOSX", FileNameJoin[{$HomeDirectory, "Library", "Wolfram", "SearchIndices"}],
	"Windows", FileNameJoin[{$WindowsAppData, "Wolfram", "SearchIndices"}]]
$WindowsAppData := $WindowsAppData = "APPDATA" // 
	Replace[Developer`ToList[GetEnvironment["APPDATA"], _ -> None]] //
	Replace[None :> FileNameJoin[{$HomeDirectory, "AppData", "Local"}]];

PackageScope["$SearchIndicesDirectories"]
$SearchIndicesDirectories = {$SearchIndicesDirectory, $OldSearchIndicesDirectory};

PackageExport["SearchIndices"]

SetArgumentCount[SearchIndices, 0];

SearchIndices[] :=
	Join @@
		Function[{dir},
			SearchIndexObject[File[#]]& /@ Select[FileNames["*", dir], DirectoryQ]
		] /@ $SearchIndicesDirectories


PackageScope["ValidSearchIndexDirectoryQ"]

ValidSearchIndexDirectoryQ[path_String] := 
	DirectoryQ[path] && FileType[FileNameJoin[{path, "IndexMetadata.wl"}]] === File;


PackageExport["ValidSearchIndexObjectQ"]

SearchIndexObject /: SystemOpen[SearchIndexObject[File[path_String]]] := SystemOpen[path];

ValidSearchIndexObjectQ[HoldPattern[SearchIndexObject[File[path_String]]]] := 
	ValidSearchIndexDirectoryQ[path];

ValidSearchIndexObjectQ[_] := False;

$uuidRegex = 
	StringExpression @@ Riffle[Thread @ Repeated[HexadecimalCharacter, {8, 4, 4, 4, 12}], "-"];

totalByteCount[path_] := Switch[FileType[path],
	Directory,
		Total[totalByteCount /@ FileNames[All, path]],
	File,
		FileByteCount[path],
	_, 
		0
];

SearchIndexObject /: MakeBoxes[obj:SearchIndexObject[File[path2_String]] ? ValidSearchIndexObjectQ, StandardForm] := Module[
	{path, name, shortname, indexedPaths, creationDate, indexLoc, indexSize = None, fileCounts, metadata, res, handle,
	 invalidIndexQ},
	path = ExpandFileName[path2];
	name = FileNameTake[path];
	If[StringMatchQ[name, $uuidRegex], shortname = StringTake[name, 8]]; 
	metadata = GetIndexMetadata[obj, {"IndexedPaths", "CreationDate", "IndexLocation"}];
	If[FailureQ[metadata], res = $Failed, 
	{indexedPaths, creationDate, indexLoc} = metadata;
	If[MissingQ[indexLoc], indexLoc = path];
	If[StringQ[indexLoc] && FileExistsQ[indexLoc], 
		indexSize = totalByteCount[indexLoc];
		handle = CreateHandle[obj];
		invalidIndexQ = FailureQ[handle];
		fileCounts = DGetDocumentCount[handle];
		If[!IntegerQ[fileCounts], fileCounts = "unknown"];
	];
	res = BoxForm`ArrangeSummaryBox[
		SearchIndexObject, obj, 
		$searchIndexIcon,
		{
			If[StringQ[shortname], BoxForm`SummaryItem[{"Short name: ", shortname}], BoxForm`SummaryItem[{"Name: ", name}]],
			BoxForm`SummaryItem[{"Creation date: ", DateString[creationDate, "DateTimeShort"]}],
			If[!TrueQ[invalidIndexQ] && IntegerQ[indexSize],
				BoxForm`SummaryItem[{"Index size: ", Row[{fileCounts, " ", "documents", " ", "(", bytesToSize[indexSize], ")"}]}],
				Nothing
			],
			If [TrueQ[invalidIndexQ],
				Row[{Style["Invalid index", Red]}]
				,
				Nothing
			]
		},
		{
			If[StringQ[shortname], BoxForm`SummaryItem[{"Name: ", name}], Nothing],
			BoxForm`SummaryItem[{"Location: ", path}],
			If[indexedPaths =!= None, 
				BoxForm`SummaryItem[{"Indexed paths: ", Column[indexedPaths, BaselinePosition -> 1]}],
				Nothing
			]
		},
		StandardForm
	]];
	res /; res =!= $Failed
];

(* undocumented convenience... *)
$qpattern = All | _String | _Alternatives | _List | _Except | _Not;
(so_SearchIndexObject)[str:$qpattern] := TextSearch[so, str];
(so_SearchIndexObject)[str:$qpattern, prop_String] := TextSearch[so, str, prop];

PackageExport["GetDocumentCount"]

GetDocumentCount[obj_SearchIndexObject] := DGetDocumentCount[GetHandle @ obj];


PackageExport["GetIndexMetadata"]

GetIndexMetadata[SearchIndexObject[File[path_String]]] := 
	Quiet @ Check[Get @ FileNameJoin[{path, "IndexMetadata.wl"}], $Failed];
	
GetIndexMetadata[dir_String] :=
	GetIndexMetadata[SearchIndexObject[File[dir]]]

GetIndexMetadata[obj_, prop_] := 
	Block[{assoc = GetIndexMetadata[obj]},
	If[AssociationQ[assoc], Lookup[assoc, prop], $Failed]];

GetIndexMetadata[___] := $Failed;

SearchIndexObject /: Normal[so_SearchIndexObject] := 
	GetIndexMetadata[so];


PackageExport["PutIndexMetadata"]

PutIndexMetadata[SearchIndexObject[File[path_String]], meta_] := 
	Put[meta, FileNameJoin[{path, "IndexMetadata.wl"}]]; 


PackageExport["GetIndexedFiles"]

GetIndexedFiles[ts_TextSearchObject] := DGetIndexedFiles[GetHandle @ ts];
