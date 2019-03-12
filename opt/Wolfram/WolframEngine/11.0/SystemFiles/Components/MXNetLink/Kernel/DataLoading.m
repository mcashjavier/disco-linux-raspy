Package["MXNetLink`"]

PackageImport["GeneralUtilities`"]
PackageImport["Developer`"]

(******************************************************************************)
(****** Images ******)
(******************************************************************************)


(******************************************************************************)
PackageExport["FastImageImport"]

FastImageImport[filename_] := CatchFailure @ Scope[
	(* infer from filename *)
	ext = ToLowerCase@FileExtension[filename];
	image = Which[
		ext === "jpg" || ext === "jpeg",
			First@Image`ImportExportDump`ImageReadJPEG[filename],
		ext === "png",
			First@Image`ImportExportDump`ImageReadPNG[filename],
		ext === "tiff",
			First@Image`ImportExportDump`ImageReadTIFF[filename]
	];
	(* If not an image yet, try Import *)
	If[Not@ImageQ@image, image = Import@filename];
	If[Not@ImageQ@image, ThrowFailure["invimage", filename]];
	(* return image *)
	image
]

FastImageImport::invimage = "Cannot import `` as an image.";

