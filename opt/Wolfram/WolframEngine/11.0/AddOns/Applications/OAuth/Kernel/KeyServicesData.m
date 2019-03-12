(* 
This package loads individual Services that authenticate using a simple API key in the url query
*)

Begin["KeyClient`"] 

KeyClient`$predefinedKeyservicelist;
KeyClient`KeyServicesData;
KeyClient`keycookeddata;
KeyClient`keysendmessage;
KeyClient`addKeyservice;

Begin["`Private`"]

(Unprotect[#]; Clear[#])& /@ {KeyClient`KeyServicesData,KeyClient`keycookeddata,KeyClient`keysendmessage,KeyClient`addKeyservice}
Unprotect[KeyClient`$predefinedKeyservicelist];

defaultKeyParams={
					(* defaults *)
					"ServiceName"       -> Null,
				    "Information"		-> "",
				    "URLFetchFun"		-> URLFetch
				    };

defaultKeyLabels=First/@defaultKeyParams;		    
(*************************** KeyServices *************************************)
(* A simple function for retrieving data from below *)
KeyClient`$predefinedKeyservicelist={}

KeyClient`KeyServicesData[args___]:=With[{res=keyservices[args]},
	res/;!FailureQ[res]&&Head[res]=!=keyservicedata]

keyservices[name_,prop___]:=Module[{data=keyservicedata[name],availableproperties},
	availableproperties=First/@data;
	Switch[{prop},
		{},	data,
		{"Requests"},availableproperties,
		{"Authentication"},
			Thread[defaultKeyLabels->(defaultKeyLabels/.Join[data,defaultKeyParams])]
		,
		{Alternatives@@availableproperties},
		prop/.data,
		_,
		keyservicedata[name,prop]		
	]
]

keyservices[___]:=$Failed
KeyClient`KeyServicesData[___]:=$Failed

$packagedirectory=FileNameJoin[{DirectoryName[System`Private`$InputFileName],"Services"}];

KeyClient`addKeyservice[name_, dir_:$packagedirectory]:=Module[{funs, file},
	Unprotect[KeyClient`$predefinedKeyservicelist,keyservicedata,KeyClient`keycookeddata,KeyClient`keysendmessage];
	KeyClient`$predefinedKeyservicelist=Union[Append[KeyClient`$predefinedKeyservicelist,name]];
	ServiceConnections`Private`appendservicelist[name,"APIKey"];
	file=FileNameJoin[{dir, "APIKey",name<>"API.m"}];
	If[!FileExistsQ[file],
		(* alternate *)
		file=FileNameJoin[{dir,name<>".m"}]
	];
	If[!FileExistsQ[file],Return[$Failed]];
	funs=Get[file];
	keyservicedata[name,args___]:=funs[[1]][args];
	KeyClient`keycookeddata[name,args___]:=funs[[2]][args];
	KeyClient`keysendmessage[name,args___]:=funs[[3]][args];
	Protect[KeyClient`$predefinedKeyservicelist,keyservicedata,KeyClient`keycookeddata,KeyClient`keysendmessage];
]

Unprotect[KeyClient`keycookeddata,KeyClient`keysendmessage,keyservicedata];

(**** error handling ***)
keyservicedata[___]:=$Failed
KeyClient`keycookeddata[___]:=Throw[$Failed]
KeyClient`keysendmessage[___]:=Throw[$Failed]

SetAttributes[{KeyClient`$predefinedKeyservicelist,KeyClient`KeyServicesData,KeyClient`keycookeddata,KeyClient`keysendmessage,KeyClient`addKeyservice},{ReadProtected, Protected}];

End[];
End[];

{}