BeginPackage["TesseractTools`"]
Begin["`Private`"]

$InitTesseractTools = False;

$ThisDirectory = FileNameDrop[$InputFileName, -1]
$BaseLibraryDirectory = FileNameJoin[{$ThisDirectory, "LibraryResources", $SystemID}];
$TesseractToolsLibrary = "TesseractTools";

(*****************************)
(**Getting path to traindata**)
(*****************************)
baseDir = $BaseLibraryDirectory;
TrainDataPath = $BaseLibraryDirectory;
LanguageName = "eng";

safeLibraryLoad[debug_, lib_] :=
	Quiet[
		Check[
			LibraryLoad[lib],
			If[TrueQ[debug],
				Print["Failed to load ", lib]
			];
			Throw[$InitTesseractTools = $Failed]
		]
	]
safeLibraryFunctionLoad[debug_, args___] :=
	Quiet[
		Check[
			LibraryFunctionLoad[$TesseractToolsLibrary, args],
			If[TrueQ[debug], (*LibraryFunctionError[]*)
				Print["Failed to load the function ", First[{args}], " from ", $TesseractToolsLibrary]
			];
			Throw[$InitTesseractTools = $Failed]
		]
	]
  
InitTesseractTools[debug_:False] := If[TrueQ[$InitTesseractTools],
	$InitTesseractTools,
	$InitTesseractTools = Catch[
	  Block[{$LibraryPath = Prepend[$LibraryPath, $BaseLibraryDirectory]},
		  safeLibraryLoad[debug, $TesseractToolsLibrary];
		  $RecognizeTextImage = safeLibraryFunctionLoad[debug, "RecognizeTextImage", {{"Image", "Constant"}, {"UTF8String"}, {"UTF8String"}, {"UTF8String"}, {"UTF8String"}, {"UTF8String"}, {"UTF8String"}, {"Boolean"}}, "UTF8String"];
		  $RecognizeTextFile = safeLibraryFunctionLoad[debug, "RecognizeTextFile", {{"UTF8String"}, {"UTF8String"}, {"UTF8String"}, {"UTF8String"}, {"UTF8String"}, {"UTF8String"}, {"UTF8String"}}, "UTF8String"];
		  $GetImageComponentsImage = safeLibraryFunctionLoad[debug, "GetImageComponentsImage", {{"Image", "Constant"}, {"UTF8String"}, {"UTF8String"}, {"UTF8String"}, {"UTF8String"}, {"UTF8String"}, {"UTF8String"}, {"UTF8String"},  _Integer, {"Boolean"}}, "UTF8String"];
		  $GetImageComponentsFile = safeLibraryFunctionLoad[debug, "GetImageComponentsFile", {{"UTF8String"}, {"UTF8String"}, {"UTF8String"}, {"UTF8String"}, {"UTF8String"}, {"UTF8String"}, {"UTF8String"}, {"UTF8String"},  _Integer}, "UTF8String"];
		  $GetCharactersConfidenceImage = safeLibraryFunctionLoad[debug, "GetCharactersConfidenceImage", {{"Image", "Constant"}, {"UTF8String"}, {"UTF8String"}, {"UTF8String"}, {"UTF8String"}, {"UTF8String"}, {"UTF8String"}, {"Boolean"}}, "UTF8String"];
		  $GetCharactersConfidenceFile = safeLibraryFunctionLoad[debug, "GetCharactersConfidenceFile", {{"UTF8String"}, {"UTF8String"}, {"UTF8String"}, {"UTF8String"}, {"UTF8String"}, {"UTF8String"}, {"UTF8String"}}, "UTF8String"];
		  $GetOrientationImage = safeLibraryFunctionLoad[debug, "GetOrientationImage", {{"Image", "Constant"}, {"UTF8String"}, {"UTF8String"}, {"UTF8String"}, {"UTF8String"}, {"UTF8String"}, {"UTF8String"}, _Integer, {"Boolean"}}, "UTF8String"];
		  $GetOrientationFile = safeLibraryFunctionLoad[debug, "GetOrientationFile", {{"UTF8String"}, {"UTF8String"}, {"UTF8String"}, {"UTF8String"}, {"UTF8String"}, {"UTF8String"}, {"UTF8String"}, _Integer}, "UTF8String"];		  

	  ];
	  True
	]
]

InitializeLanguageFromFilePath[file_]:=Block[{lan, path},
	                                      If[!Quiet[FileExistsQ[FindFile[file]]], Return[$Failed]];
	                                      lan = Quiet[First[StringSplit[FileNameTake[file, -1], "."]]];
	                                      If[StringQ[lan], LanguageName = lan, Return[$Failed]];
	                                      path = Quiet[FileNameTake[file, {1, -2}]];
	                                      If[StringQ[path], TrainDataPath = path, Return[$Failed]];
                                        ]

(*****************************)
(**Assisting functions**)
(*****************************)
formatString[asc_]:= If[StringMatchQ[ToString@asc, "N/A", IgnoreCase -> True], "n/a",
						If[
 							StringContainsQ[ToString@asc, "{"] 
 							,             
 							StringTake[StringReplace[ToString@asc, {"->" -> "-", WhitespaceCharacter -> ""}], {2, -2}]
 							,            
 							StringReplace[ToString@asc, {"->" -> "-", WhitespaceCharacter -> "" , "True" -> "T", "False" -> "F"}]]]

processRegion[rect_]:= If[StringMatchQ[ToString@rect, "N/A", IgnoreCase -> True], "n/a", 
	                       With[{tmp = Join[rect[[1]], {rect[[2, 1]] - rect[[1, 1]], rect[[2, 2]] - rect[[1, 2]]}]}, 
 									StringTake[StringJoin[{ToString@tmp[[#]], ","} & /@ Range[4]], {1, -2}]]
                    ]

(*****************************)
(**Overridden functions**)
(*****************************)
RecognizeText[FI_, rect_: "N/A", opts_: "N/A", seg_: "N/A", engine_: "N/A", convLept_: True] :=
    If[Quiet@StringQ[FI],
    	$RecognizeTextFile [FI, ToLowerCase[LanguageName], TrainDataPath, ToLowerCase[seg], ToLowerCase[engine], formatString[opts], processRegion[rect]],
        $RecognizeTextImage[FI, ToLowerCase[LanguageName], TrainDataPath, ToLowerCase[seg], ToLowerCase[engine], formatString[opts], processRegion[rect], convLept]
    ]
     
     

GetImageComponents[FI_, lvl_: "word", mode_: 0 , rect_: "N/A", opts_: "N/A", seg_: "N/A", engine_: "N/A", convLept_: True] := Module[{res}, 
    res = If[Quiet@StringQ[FI],
    	  		$GetImageComponentsFile[FI, ToLowerCase[LanguageName], TrainDataPath, ToLowerCase[seg], ToLowerCase[engine], formatString[opts], processRegion[rect], ToLowerCase[lvl], mode],
    			$GetImageComponentsImage[FI, ToLowerCase[LanguageName], TrainDataPath, ToLowerCase[seg], ToLowerCase[engine], formatString[opts], processRegion[rect], ToLowerCase[lvl], mode, convLept]
    		];
    
    res = Quiet@ToExpression@StringReplace[res, {"\n" | "\n\n" -> "", "\\"-> ""}];
    If[Quiet@AssociationQ[res], res, $Failed]
]

GetCharactersConfidence[FI_, rect_: "N/A", opts_: "N/A", seg_: "N/A", engine_: "N/A", convLept_:True] := Module[{res, tmp, l}, 
	res = If[Quiet@StringQ[FI],
				$GetCharactersConfidenceFile [FI, ToLowerCase[LanguageName], TrainDataPath, ToLowerCase[seg], ToLowerCase[engine], formatString[opts], processRegion[rect]],
				$GetCharactersConfidenceImage[FI, ToLowerCase[LanguageName], TrainDataPath, ToLowerCase[seg], ToLowerCase[engine], formatString[opts], processRegion[rect], convLept]
			];
	tmp = StringTake[StringReplace[res, {"<||>|>" -> "<||>", ", |>|>" -> "|>|>,"}], {1, -8}];
	l = Length[StringPosition[StringTake[tmp, -6], ">"]];
    res = Quiet@ToExpression[StringJoin[tmp, Which[l ===0, "|>|>|>", l===1, "|>|>", l===2, "|>", True, ""]]];
    If[Quiet@AssociationQ[res],res, $Failed]
]
 	                                              
GetOrientation[image_, rect_: "N/A", opts_: "N/A", seg_: "N/A", engine_: "N/A", convLept_:True]                          := GetOrientationInfo[image, rect, opts, seg, engine, 1, convLept]
GetDirection[image_, rect_: "N/A", opts_: "N/A", seg_: "N/A", engine_: "N/A", convLept_:True]                            := GetOrientationInfo[image, rect, opts, seg, engine, 2, convLept]
GetOrder[image_, rect_: "N/A", opts_: "N/A", seg_: "N/A", engine_: "N/A", convLept_:True]                                := GetOrientationInfo[image, rect, opts, seg, engine, 3, convLept]
GetDeskewAngle[image_, rect_: "N/A", opts_: "N/A", seg_: "N/A", engine_: "N/A", convLept_:True]                          := GetOrientationInfo[image, rect, opts, seg, engine, 4, convLept]                                             
GetOrientationInfo[FI_, rect_: "N/A", opts_: "N/A", seg_: "N/A", engine_: "N/A", mode_:0, convLept_:True]                := Module[{res}, 
                                             	                                                                  res = If[Quiet@StringQ[FI],
                                             	                                                                  $GetOrientationFile [FI, ToLowerCase[LanguageName], TrainDataPath, ToLowerCase[seg], ToLowerCase[engine], formatString[opts], processRegion[rect], mode],
                                             	                                                                  $GetOrientationImage[FI, ToLowerCase[LanguageName], TrainDataPath, ToLowerCase[seg], ToLowerCase[engine], formatString[opts], processRegion[rect], mode, convLept]
                                             	                                                                  ];
                                             	                                                                  ToExpression[res]
                                                                                                              ]
                                                     

End[]
EndPackage[]