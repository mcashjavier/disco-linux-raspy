Package["TextSearch`"]

PackageScope["NormalizeSources"]
PackageScope["IndexHandle"]

(*Note: NormalizeSources will also show hidden files, so
when it's used in the SummaryBox, such files will show up,
even if they are not indexed*)
NormalizeSources[spec_, head_] := Block[
	{$head = head, $expand = (head =!= TextSearch)},
	setupHardcodedPaths;
	DeleteCases[$Failed] @ Flatten @ List @ normalize @ spec
];

General::nesrc = "The search path `` does not exist.";
General::invsrc = "`` is not a valid file, directory, ContentObject or SearchIndexObject.";
General::invind = "`` is not a valid SearchIndexObject.";
General::badind = "Index `` is corrupt.";
General::nosrcs = "One or more valid directories, files, or SearchIndexObject values are required.";
General::cosupp = "CloudObject is not supported in this version of the Wolfram Language.";

General::csupp = "`` is not supported in this version of the Wolfram Cloud."

$expand = False;

PackageImport["GeneralUtilities`"]

checkPath[x_] := If[FileExistsQ[x], x, msg["nesrc", x]];

normalize[File[path_String]]  := normalizePath[path];
normalize[path_String]  := normalizePath[path];

normalizePath[path_String] := promoteSearchIndexDirectories @ checkPath @ ExpandFileName @ path;

PackageScope["promoteSearchIndexDirectories"]

(* this means that if a source is given that happens to be the raw directory
of a search index, it'll be searched as an index rather than searched as an
ordinary directory *)
promoteSearchIndexDirectories[s_] := s;
promoteSearchIndexDirectories[s_String ? ValidSearchIndexDirectoryQ] :=
	Block[{tmp = CreateHandle @ SearchIndexObject @ File @ s}, 
		IndexHandle[tmp] /; !FailureQ[tmp]
	];


msg[str_String, args___] := With[{head = $head}, Message[MessageName[head, str], args]; $Failed];

cloudns[] := msg["csupp", $head];

normalize[obj_SearchIndexObject] := Block[{tmp},
	If[!ValidSearchIndexObjectQ[obj], 
		msg["invind", obj],
	If[$expand,
		If[!Developer`StringVectorQ[tmp = GetIndexMetadata[obj, "IndexedPaths"]], 
			msg["badind", obj],
			(* Only complain if one of the indexed directories is missing.
			   If an explicitly indexed file is missing, we won't complain.
			   (but one might argue that we should complain in either case) *)
			Join[
				Select[tmp, !DirectoryQ[#] &],
				checkPath /@ Select[tmp, DirectoryQ[#] &]
			]
		]
	,
		If[FailureQ[tmp = CreateHandle @ obj],
			(* We assume that CreateHandle have already issued messages explaining this failure. *)
			tmp,
			IndexHandle @ tmp
		]
	]]
];

normalize[HoldPattern[_CloudObject]] := msg["cosupp"];

normalize[co_ContentObject ? ValidContentObjectQ] := Module[{loc},
	If[StringQ[loc = co["Location"]] && FileExistsQ[loc = ExpandFileName[loc]],
		loc, msg["invsrc", co]]];
		
normalize[assoc_Association] := assoc

normalize[list_List] := Map[normalize, list];

normalize[other_] := msg["invsrc", other];

setupHardcodedPaths := (
	normalizePath["ExampleData/Text"] := 
		PacletManager`PacletResource["TextSearch", "ExampleData"];
	Scan[
		(normalizePath["ExampleData/Text/" <> FileNameTake[#]] = #)&,
		FileNames["*", PacletManager`PacletResource["TextSearch", "ExampleData"], Infinity]
	];
	Clear[setupHardcodedPaths];
);

PackageScope["$ExplicitFile"]
PackageExport["EnumerateFilesToIndex"]

PackageImport["GeneralUtilities`"]

$IgnoredExt := $IgnoredExt = RegularExpression @ 
	StringInsert[
		ToRegularExpression["." ~~ Alternatives @@ $IgnoredFileExtensions ~~ EndOfString],
		"i", 3];

If[$OperatingSystem === "MacOSX", 
	$IgnoredPath = "/." | RegularExpression["^/Users/\\w*/Library/?$"],
	$IgnoredPath = $PathnameSeparator <> "."
];

EnumerateFilesToIndex[path_] :=  Block[{$pathbag = Internal`Bag[]},
	Scan[iEnumerateFilesToIndex0, If [!AssociationQ[#], ExpandFileName[#], #] & /@ Developer`ToList[path]]; 
	Internal`BagPart[$pathbag, All]
];

TextSearch::badext = "Cannot index file ``.";
TextSearch::badpath = "Cannot index path ``.";

iEnumerateFilesToIndex0[path_] := 
	Switch[FileType[path],
		File,
			If[MemberQ[$IgnoredFileExtensions, FileExtension[path]],
				Message[TextSearch::badext, path],
				Internal`StuffBag[$pathbag, $ExplicitFile[path]]
			], 
		Directory,
			If[!StringFreeQ[path, $IgnoredPath],
				Message[TextSearch::badpath, path],
				iEnumerateFilesToIndex1[path]
			]
	];

(* If there are plain associations mixed in with file names, then
   leave the associations alone. *)
iEnumerateFilesToIndex0[assoc_Association] := Internal`StuffBag[$pathbag, assoc]

iEnumerateFilesToIndex1[path_] := If[StringFreeQ[path, $IgnoredPath],
    Switch[FileType[path],
        Directory, 
            Scan[iEnumerateFilesToIndex1, FileNames[All, path]],
        File, 
        	If[StringFreeQ[path, $IgnoredExt], Internal`StuffBag[$pathbag, path]]
    ]];  