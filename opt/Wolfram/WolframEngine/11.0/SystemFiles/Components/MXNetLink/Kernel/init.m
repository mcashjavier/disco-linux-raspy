BeginPackage["MXNetLink`"]

Begin["Private`"]


Block[{Developer`$CurrentPackage = "MXNetLink`"},
	Get @ FileNameJoin[{DirectoryName @ $InputFileName, "Common.m"}];
];

LoadLibraries[];
InitialiazeSymbols[];

End[]
EndPackage[]