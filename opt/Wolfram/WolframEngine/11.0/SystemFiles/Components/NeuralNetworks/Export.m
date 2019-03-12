Package["NeuralNetworks`"]

PackageImport["GeneralUtilities`"]


PackageExport["WLNetExport"]

WLNetExport[filename_String, net_NetP] := Scope[
	CollectTo[{$tensors},
		result = Call[net, PrepareForExport];
	];
	json = Block[{$ContextPath = {"System`", "NeuralNetworks`"}, $Context = "Dummy`"}, ToString[result, InputForm]];	
	ExportStructuredHDF5[filename,  <|"Version" -> $VersionNumber, "Network" -> json, "Arrays" -> $tensors|>];
	filename
];

PackageExport["ExportedArray"]

PackageScope["PrepareForExport"]

PrepareForExport[_] := MapAtFields["Arrays", ExportTensor]

ExportTensor[raw_RawArray] := (
	BagInsert[$tensors, raw]; 
	ExportedArray[Internal`BagLength[$tensors]]
);

ExportTensor[e_] := e;



PackageExport["WLNetImport"]

Import::wlnetver = "Cannot load network produced using version `` in kernel of version ``."

WLNetImport[file_String] := Scope[
	result = ImportStructuredHDF5[file, "ImportAsRawArray" -> True];
	If[!MatchQ[result, KeyValuePattern[{"Version" -> _Real, "Network" -> _String, "Arrays" -> _List}]],
		Return[$Failed];
	];
	UnpackAssociation[result, network, arrays, version];
	If[version > $VersionNumber && $VersionNumber < 300., 
		Message[Import::wlnetver, ToString @ version, ToString @ $VersionNumber];
		Return[$Failed]
	];
	network = Block[{$ContextPath = {"System`", "NeuralNetworks`"}, $Context = "Dummy`"}, ToExpression[network]];
	network = network /. ExportedArray[id_] :> RuleCondition @ arrays[[id]];
	ConstructLayer[network]
];

