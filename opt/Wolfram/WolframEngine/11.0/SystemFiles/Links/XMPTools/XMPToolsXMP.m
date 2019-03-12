BeginPackage["XMPTools`"]


Begin["`Private`"]

$InitXMPTools = False;

$ThisDirectory = FileNameDrop[$InputFileName, -1]
$BaseLibraryDirectory = FileNameJoin[{$ThisDirectory, "LibraryResources", $SystemID}];
$XMPToolsLibrary = "XMPTools";
dlls["Windows"|"Windows-x86-64"|"Linux"|"Linux-x86-64"|"Linux-ARM"|"MacOSX-x86-64"] = {"XMPCore", "XMPFiles"};
dlls[___] := $Failed;

safeLibraryLoad[debug_, lib_] :=
	Quiet[
		Check[
			LibraryLoad[lib],
			If[TrueQ[debug],
				Print["Failed to load ", lib]
			];
			Throw[$InitXMPTools = $Failed]
		]
	]
safeLibraryFunctionLoad[debug_, args___] :=
	Quiet[
		Check[
			LibraryFunctionLoad[$XMPToolsLibrary, args],
			If[TrueQ[debug],
				Print["Failed to load the function ", First[{args}], " from ", $XMPToolsLibrary]
			];
			Throw[$InitXMPTools = $Failed]
		]
	]
  
InitXMPTools[debug_:False] := If[TrueQ[$InitXMPTools],
	$InitXMPTools,
	$InitXMPTools = Catch[
	  If[dlls[$SystemID] === $Failed,
	  	Message[XMPTools::sys, "Incompatible SystemID"];
	  	Throw[$Failed]
	  ];
	  Block[{$LibraryPath = Prepend[$LibraryPath, $BaseLibraryDirectory]},
		  safeLibraryLoad[debug, #]& /@ Flatten[{dlls[$SystemID], $XMPToolsLibrary}];
		  $ErrorDescription = safeLibraryFunctionLoad[debug, "ErrorDescription", {}, "UTF8String"];
		  $ProfileLog = safeLibraryFunctionLoad[debug, "ProfileLog", {}, "UTF8String"];
		  $ReadXMPString = safeLibraryFunctionLoad[debug, "ReadXMPString", {{"UTF8String"}, {"UTF8String"}}, "UTF8String"];
		  $WriteXMPString = safeLibraryFunctionLoad[debug, "WriteXMPString", {{"UTF8String"}, {"UTF8String"}, {"UTF8String"}}, "UTF8String"];
		  $WriteXMPBool = safeLibraryFunctionLoad[debug, "WriteXMPBool", {{"UTF8String"}, {"UTF8String"}, {"Boolean"}}, "Boolean"];
		  $ReadEXIFString = safeLibraryFunctionLoad[debug, "ReadEXIFString", {{"UTF8String"}, {"UTF8String"}, {"UTF8String"}}, "UTF8String"];
		  $WriteEXIFString = safeLibraryFunctionLoad[debug, "WriteEXIFString", {{"UTF8String"}, {"UTF8String"}, {"UTF8String"}}, "UTF8String"];
		  $WriteEXIFIntVector = safeLibraryFunctionLoad[debug, "WriteEXIFIntVector", {{"UTF8String"}, {"UTF8String"}, {_Integer, 1}}, {_Integer, 1}];
		  $WriteEXIFStringProperty = safeLibraryFunctionLoad[debug, "WriteEXIFStringProperty", {{"UTF8String"}, {"UTF8String"}, {"UTF8String"}, {"UTF8String"}}, "UTF8String"];
		  $WriteEXIFBoolProperty = safeLibraryFunctionLoad[debug, "WriteEXIFBoolProperty", {{"UTF8String"}, {"UTF8String"}, {"UTF8String"}, {"Boolean"}}, "Boolean"];
		  $ReadEXIFInt = safeLibraryFunctionLoad[debug, "ReadEXIFInt", {{"UTF8String"}, {"UTF8String"}}, _Integer];
		  $ReadEXIFRealExpression = safeLibraryFunctionLoad[debug, "ReadEXIFRealExpression", {{"UTF8String"}, {"UTF8String"}}, "UTF8String"];
		  $ReadEXIFIntProperty = safeLibraryFunctionLoad[debug, "ReadEXIFIntProperty", {{"UTF8String"}, {"UTF8String"}, {"UTF8String"}}, _Integer];
		  $WriteEXIFInt = safeLibraryFunctionLoad[debug, "WriteEXIFInt", {{"UTF8String"}, {"UTF8String"}, {_Integer}}, _Integer];
		  $WriteEXIFReal = safeLibraryFunctionLoad[debug, "WriteEXIFReal", {{"UTF8String"}, {"UTF8String"}, {_Real}}, _Real];
		  $WriteEXIFIntProperty = safeLibraryFunctionLoad[debug, "WriteEXIFIntProperty", {{"UTF8String"}, {"UTF8String"}, {"UTF8String"}, {_Integer}}, _Integer];
		  $XMPInitialize = safeLibraryFunctionLoad[debug, "XMPInitialize", {{"UTF8String"}}, _Integer];
		  $XMPUnInitialize = safeLibraryFunctionLoad[debug, "XMPUnInitialize", {}, _Integer];
	  ];
	  True
	]
]

StingTags = {
	"Make",
	"Model",
	"ComponentsConfiguration",
	"ExifVersion",
	"DateTimeOriginal",
	"DateTimeDigitized",
	"UserComment",
	"FlashpixVersion",
	"DateTime"
}

IntegerTags = {
	"Orientation",
	"ResolutionUnit",
	"YCbCrPositioning",
	"Compression",
	"MeteringMode",
	"ColorSpace",
	"PixelXDimension",
	"PixelYDimension",
	"FocalPlaneResolutionUnit",
	"SensingMethod",
	"FileSource",
	"CustomRendered",
	"ExposureMode",
	"WhiteBalance",
	"SceneCaptureType",
	"CompressedBitsPerPixel",
	"XResolution",
	"YResolution",
	"DigitalZoomRatio",
	"ExposureBiasValue"
}

RealTags = {
	"ExposureTime",
	"FNumber",
	"ShutterSpeedValue",
	"ApertureValue",
	"MaxApertureValue",
	"FocalLength",
	"FocalPlaneXResolution",
	"FocalPlaneYResolution"
}	

(*GetExifInformation[file_] := Join[		
   Rule[#, $ReadEXIFRealExpression[file, #]] &/@ RealTags,
   Rule[#, $ReadEXIFInt[file, #]] &/@ IntegerTags,
   Rule[#, $ReadEXIFString[file, #, ""]] &/@ StingTags
]*)

GetExifInformation[file_] := 
 Join[Rule[#, 
     ToExpression[
      XMPTools`Private`$ReadEXIFRealExpression[file, #]]] & /@ 
   RealTags, 
  Rule[#, XMPTools`Private`$ReadEXIFInt[file, #]] & /@ IntegerTags, 
  Rule[#, XMPTools`Private`$ReadEXIFString[file, #, ""]] & /@ 
   StingTags]

End[]
EndPackage[]
