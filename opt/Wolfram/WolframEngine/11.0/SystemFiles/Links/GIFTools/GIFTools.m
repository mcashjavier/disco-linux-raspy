BeginPackage["GIFTools`"]


Begin["`Private`"]

$InitGIFTools = False;

$ThisDirectory = FileNameDrop[$InputFileName, -1]
$BaseLibraryDirectory = FileNameJoin[{$ThisDirectory, "LibraryResources", $SystemID}];
$GIFToolsLibrary = "GIFTools";
dlls["Linux"|"Linux-x86-64"|"Linux-ARM"|"MacOSX-x86-64"] = {"libgif"};
dlls["Windows"|"Windows-x86-64"] = {"giflib5"};
dlls[___] := $Failed;

safeLibraryLoad[debug_, lib_] :=
	Quiet[
		Check[
			LibraryLoad[lib],
			If[TrueQ[debug],
				Print["Failed to load ", lib]
			];
			Throw[$InitGIFTools = $Failed]
		]
	]
safeLibraryFunctionLoad[debug_, args___] :=
	Quiet[
		Check[
			LibraryFunctionLoad[$GIFToolsLibrary, args],
			If[TrueQ[debug],
				Print["Failed to load the function ", First[{args}], " from ", $GIFToolsLibrary]
			];
			Throw[$InitGIFTools = $Failed]
		]
	]
  
InitGIFTools[debug_:False] := If[TrueQ[$InitGIFTools],
	$InitGIFTools,
	$InitGIFTools = Catch[
	  If[dlls[$SystemID] === $Failed,
	  	Message[GIFTools::sys, "Incompatible SystemID"];
	  	Throw[$Failed]
	  ];
	  Block[{$LibraryPath = Prepend[$LibraryPath, $BaseLibraryDirectory]},
		  safeLibraryLoad[debug, #]& /@ Flatten[{dlls[$SystemID], $GIFToolsLibrary}];
		  $ExportGIF = safeLibraryFunctionLoad[debug, "ExportGIF", {"UTF8String",{"Image","Constant"}},"UTF8String"];
		  $ReadOneFrame = safeLibraryFunctionLoad[debug,"ReadOneFrame",{{"UTF8String"},Integer}, "Image"];
		  $ReadAllFrames = safeLibraryFunctionLoad[debug,"ReadAllFrames",{"UTF8String"}, LibraryDataType[Image|Image3D]];
		  $ReadGlobalPalette = safeLibraryFunctionLoad[debug,"ReadGlobalPalette",{"UTF8String"}, {_Integer, _}];
		  $ReadPalettes = safeLibraryFunctionLoad[debug,"ReadPalettes", LinkObject, LinkObject];
		  $ReadRasterBits = safeLibraryFunctionLoad[debug,"ReadRasterBits", LinkObject, LinkObject];
		  $ReadFileMetadata = safeLibraryFunctionLoad[debug,"ReadFileMetadata", LinkObject, LinkObject];
		  $ReadFrameMetadata = safeLibraryFunctionLoad[debug,"ReadFrameMetadata", LinkObject, LinkObject];
		  $ClearCache = safeLibraryFunctionLoad[debug,"ClearCache", {"UTF8String"}, True|False];
	  ];
	  True
	]
]

End[]
EndPackage[]
