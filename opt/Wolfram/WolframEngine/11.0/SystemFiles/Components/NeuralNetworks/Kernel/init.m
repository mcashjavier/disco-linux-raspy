BeginPackage["NeuralNetworks`"]

Begin["NeuralNetworks`Bootstrap`Private`"];

(* protect against this autoloading ML, PacletManager needs a way
of declaring non-autoloading system symbols *)

Unprotect[ValidationSet];
Clear[ValidationSet];

Quiet @ Needs["GeneralUtilities`"];
Quiet @ Needs["MXNetLink`"];

syms = PacletExportedSymbols["NeuralNetworks"];

Unprotect @@ syms;
ClearAll @@ syms;

$basePath = DirectoryName[$InputFileName, 2];

Block[{
	$ContextPath = {"GeneralUtilities`", "Developer`", "System`"}, 
	Developer`$CurrentPackage = "NeuralNetworks`", $Context = $Context},
	$outcome = $Aborted;
	CheckAbort[
		If[FailureQ[$outcome = CatchFailure[Get, Get[FileNameJoin[{$basePath, "Utils.m"}]]]], Abort[]]; $loaded = True;
		If[FailureQ[$outcome = NeuralNetworks`LoadLayerDefinitions[]], Abort[]];
		If[FailureQ[$outcome = NeuralNetworks`LoadEncoderDefinitions[]], Abort[]];
		If[FailureQ[$outcome = NeuralNetworks`LoadDecoderDefinitions[]], Abort[]];,
		Null
	];
];

Protect @@ syms;

End[];

EndPackage[];

NeuralNetworks`Bootstrap`Private`$outcome