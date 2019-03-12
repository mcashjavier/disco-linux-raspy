Package["NeuralNetworks`"]

PackageImport["GeneralUtilities`"]

Clear[$Cache];
$Cache = Language`NewExpressionStore["NeuralNetworkCache"];


PackageScope["Cached"]

ClearAll[Cached];

Cached[func_, net_, args__] := 
	Replace[$Cache["get"[net, {func, args}]], Null :>
		Replace[func[net, args],
			res:Except[_ ? FailureQ] :> 
			($Cache["put"[net, {func, args}, res]]; res)
		]
	];

Cached[func_, net_] := 
	Replace[$Cache["get"[net, func]], Null :>
		Replace[func[net],
			res:Except[_ ? FailureQ] :> 
			($Cache["put"[net, func, res]]; res)
		]
	];


PackageScope["CachedIf"]
	
CachedIf[True, args___] := Cached[args];
CachedIf[_, args___] := Developer`Construct[args];

PackageScope["CachedValues"]

CachedValues[] := $Cache["listTable"[]];


PackageExport["ClearCache"]

ClearCache[] := Scope[
	keys = CachedValues[][[All, 1]];
	Scan[$Cache["remove"[#]]&, keys];
];