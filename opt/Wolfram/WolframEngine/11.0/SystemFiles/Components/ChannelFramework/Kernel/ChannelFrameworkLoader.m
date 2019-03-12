If[Internal`$PrototypeBuild,
	ChannelFramework`debug`$Server = "dev";
	PacletUpdate["ChannelFramework", 
		"Site" -> "http://paclet-int.wolfram.com:8080/PacletServerInternal"]
	,
	PacletManager`Package`getPacletWithProgress["ChannelFramework"]
];

Get["ChannelFramework`"]
