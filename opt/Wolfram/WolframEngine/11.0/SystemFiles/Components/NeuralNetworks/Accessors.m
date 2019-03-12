Package["NeuralNetworks`"]

PackageImport["GeneralUtilities`"]


PackageExport["$TypeToSymbol"]

$TypeToSymbol = Data`UnorderedAssociation[];

(* 
The NetP here probably slows things down a bit, especially for things like InputPorts, which
don't particularly get called on top-level entities. Still, it's nice to be general.
*)


PackageExport["LayerProp"]

LayerProp[net_NetP, prop_] := $LayerData[net["Type"], prop];


PackageScope["Params"]
PackageScope["Hypers"]

Params[net_NetP] := net["Params"];
Hypers[net_NetP] := net["Hypers"];


PackageExport["Inputs"]
PackageExport["Outputs"]

Inputs[net_NetP] := net["Inputs"];
Outputs[net_NetP] := net["Outputs"];


PackageExport["InputNames"]
PackageExport["OutputNames"]

InputNames[net_NetP] := Keys @ net["Inputs"];
OutputNames[net_NetP] := Keys @ net["Outputs"];


PackageExport["InputTypes"]
PackageExport["OutputTypes"]

InputTypes[net_NetP] := Values @ net["Inputs"];
OutputTypes[net_NetP] := Values @ net["Outputs"];


PackageExport["PortTypes"]

PortTypes[net_NetP] := {
	KeyValueMap[NetPort["Inputs", #1] -> #2&, net["Inputs"]],
	KeyValueMap[NetPort["Outputs", #1] -> #2&, net["Outputs"]]
};


PackageScope["InputPorts"]
PackageScope["OutputPorts"]

InputPorts[net_NetP] := Thread @ Join[$path, NetPort["Inputs", Keys @ net["Inputs"]]];
OutputPorts[net_NetP] := Thread @ Join[$path, NetPort["Outputs", Keys @ net["Outputs"]]];


PackageScope["SetupGenericDispatch"]

SetupGenericDispatch[sym_Symbol] := (
	DeclareSymbolicHead[sym];
	ValidNetQ[HoldPattern[sym[assoc_Association]] ? System`Private`NoEntryQ] := True;
	sym /: Normal[HoldPattern[sym[assoc_Association] ? System`Private`NoEntryQ]] := assoc;
	sym /: Derivative[1][HoldPattern[layer:sym[_Association] ? System`Private`NoEntryQ]] := NetDerivative[layer];
	(s_sym ? System`Private`NoEntryQ)[args___] := CatchFailure[sym, NetApply[s, args]];
);
