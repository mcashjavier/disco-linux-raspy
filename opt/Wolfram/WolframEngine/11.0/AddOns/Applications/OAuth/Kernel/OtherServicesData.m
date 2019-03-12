(* 
This package loads individual Services which do not use the built in authentication frameworks
*)


Begin["OtherClient`"] 

OtherClient`$predefinedOtherservicelist;
OtherClient`OtherServicesData;
OtherClient`othercookeddata;
OtherClient`otherrawdata;
OtherClient`othersendmessage;
OtherClient`addOtherservice;

Begin["`Private`"]

(Unprotect[#]; Clear[#])& /@ {OtherClient`OtherServicesData,OtherClient`othercookeddata,
	OtherClient`otherrawdata,OtherClient`othersendmessage,OtherClient`addOtherservice}
Unprotect[OtherClient`$predefinedOtherservicelist];

defaultOtherParams={
					(* defaults *)
					"ServiceName"       -> Null,
				    "Information"		-> ""
				    };

defaultOtherLabels=First/@defaultOtherParams;		    
(*************************** OtherServices *************************************)
(* A simple function for retrieving data from below *)
OtherClient`$predefinedOtherservicelist={}

OtherClient`OtherServicesData[args___]:=With[{res=otherservices[args]},
	res/;!FailureQ[res]&&Head[res]=!=otherservicedata]

otherservices[name_,prop___]:=Module[{data=otherservicedata[name],availableproperties},
	availableproperties=First/@data;
	Switch[{prop},
		{},	data,
		{"Requests"},availableproperties,
		{"Authentication"},{},
		{Alternatives@@availableproperties},
		prop/.data,
		_,
		otherservicedata[name,prop]		
	]
]

otherservices[___]:=$Failed
OtherClient`OtherServicesData[___]:=$Failed

$packagedirectory=FileNameJoin[{DirectoryName[System`Private`$InputFileName],"Services"}];

OtherClient`addOtherservice[name_, dir_:$packagedirectory]:=Module[{funs, file},
	Unprotect[OtherClient`$predefinedOtherservicelist,otherservicedata,OtherClient`othercookeddata,OtherClient`otherrawdata,OtherClient`othersendmessage];
	OtherClient`$predefinedOtherservicelist=Union[Append[OtherClient`$predefinedOtherservicelist,name]];
	ServiceConnections`Private`appendservicelist[name,"Other"];
	file=FileNameJoin[{dir,"Other", name<>".m"}];
	If[!FileExistsQ[file],
		(* alternate *)
		file=FileNameJoin[{dir,name<>".m"}]
	];
	If[!FileExistsQ[file],Return[$Failed]];
	funs=Get[file];
	otherservicedata[name,args___]:=funs[[1]][args];
	OtherClient`othercookeddata[name,args___]:=funs[[2]][args];
	OtherClient`othersendmessage[name,args___]:=funs[[3]][args];
	OtherClient`otherrawdata[name,args___]:=funs[[4]][args];
	Protect[OtherClient`$predefinedOtherservicelist,otherservicedata,OtherClient`othercookeddata,OtherClient`otherrawdata,OtherClient`othersendmessage];
]

Unprotect[OtherClient`othercookeddata,OtherClient`otherrawdata,OtherClient`othersendmessage,otherservicedata];

otherservicedata[___]:=$Failed

(**** error handling ***)
OtherClient`othercookeddata[args___]:=Throw[$Failed]
OtherClient`otherrawdata[args___]:=Throw[$Failed]
OtherClient`othersendmessage[___]:=Throw[$Failed]

SetAttributes[{OtherClient`$predefinedOtherservicelist,OtherClient`OtherServicesData,OtherClient`otherrawdata,OtherClient`othercookeddata,OtherClient`othersendmessage,OtherClient`addOtherservice},{ReadProtected, Protected}];

End[];
End[];


{}