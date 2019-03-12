BeginPackage["SoundFileTools`"]
Begin["`Private`"]

InitSoundFileTools[] := 
  Module[{dir, dlls, SoundFileToolsDll, DLLTable}, 
   DLLTable = {"MacOSX-x86-64" -> {FileNameJoin[{$InstallationDirectory, "SystemFiles", "Links", 
         "SoundFileTools", "LibraryResources", "MacOSX-x86-64"}], {"libsndfile-tools.dylib"}}, 
     "Linux" -> {FileNameJoin[{$InstallationDirectory, "SystemFiles", "Links", 
     	"SoundFileTools", "LibraryResources", "Linux"}], {"libsndfile.so"}}, 
     "Linux-x86-64" -> {FileNameJoin[{$InstallationDirectory, "SystemFiles", "Links", 
     	"SoundFileTools", "LibraryResources", "Linux-x86-64"}], {"libsndfile.so"}}, 
     "Linux-ARM" -> {FileNameJoin[{$InstallationDirectory, "SystemFiles", "Links",
		"SoundFileTools", "LibraryResources", "Linux-ARM"}], {}}, 
     "Windows" -> {FileNameJoin[{$InstallationDirectory, "SystemFiles", "Links", 
     	"SoundFileTools", "LibraryResources", "Windows"}], {"libsndfile.dll"}}, 
     "Windows-x86-64" -> {FileNameJoin[{$InstallationDirectory, "SystemFiles", "Links", 
     	"SoundFileTools", "LibraryResources", 
         "Windows-x86-64"}], {"libsndfile.dll"}}};
   dlls = Cases[DLLTable, ($SystemID -> l_) :> l];
   If[dlls === {}, Message[SoundFileTools::sys, "Incompatible SystemID"];
    $InitSoundFileTools = $Failed;
    Return[$Failed]];
   {dir, dlls} = First@dlls;
   Needs["CCompilerDriver`"];
   LibraryLoad[FileNameJoin[{dir, #}]] & /@ dlls;
   SoundFileToolsDll = FileNameJoin[{dir, "SoundFileTools"}];
   $ErrorDescription = LibraryFunctionLoad[SoundFileToolsDll, "ErrorDescription", {}, "UTF8String"];
   $ExportSoundFile = LibraryFunctionLoad[SoundFileToolsDll,"ExportSoundFile", {"UTF8String", {"RawArray", "Shared"}, Integer, Integer, Integer, {_Integer, 1}, "UTF8String", Real}, "Void"];
   $ExportSoundMetadata = LibraryFunctionLoad[SoundFileToolsDll,"ExportSoundMetadata", {"UTF8String", {_Integer, 1}, "UTF8String"}, "Void"];
   $ImportSoundFile = LibraryFunctionLoad[SoundFileToolsDll,"ImportSoundFile", {"UTF8String"}, "RawArray"];
   $ImportSoundIntegerMetadata = LibraryFunctionLoad[SoundFileToolsDll, "ImportSoundIntegerMetadata", {"UTF8String"}, {_Integer,_}];
   $ImportSoundStringMetadata = LibraryFunctionLoad[SoundFileToolsDll, "ImportSoundStringMetadata", {"UTF8String"}, "UTF8String"];
   $ImportSoundSingleStringMetadata = LibraryFunctionLoad[SoundFileToolsDll, "ImportSoundSingleStringMetadata", {"UTF8String", Integer}, "UTF8String"];
   $CheckFormatEncoding = LibraryFunctionLoad[SoundFileToolsDll, "CheckFormatEncoding", {Integer, Integer, Integer, Integer}, Integer];
   $InitSoundFileTools = If[$ErrorDescription === $Failed || $ExportSoundFile === $Failed || $ExportSoundMetadata === $Failed || $ImportSoundFile === $Failed || $ImportSoundIntegerMetadata === $Failed || $ImportSoundStringMetadata === $Failed || $ImportSoundSingleStringMetadata === $Failed || $CheckFormatEncoding === $Failed, $Failed, True];
   $InitSoundFileTools
];

End[]
EndPackage[]
