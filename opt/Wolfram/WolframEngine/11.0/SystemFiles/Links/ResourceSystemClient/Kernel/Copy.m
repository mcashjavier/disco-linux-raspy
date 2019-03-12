(* Wolfram Language Package *)

(Unprotect[#]; Clear[#])& /@ {}


BeginPackage["ResourceSystemClient`"]
(* Exported symbols added here with SymbolName::usage *)  
ResourceSystemClient`ResourceDownload
ResourceSystemClient`ResourceUpdate

Begin["`Private`"] (* Begin Private Context *) 


ResourceSystemClient`ResourceDownload[args___]:=Catch[resourceDownload[args]]

resourceDownload[resource:rpat, rest___]:=resourceDownload[resourceObjectID[resource], rest]

resourceDownload[str_String]:=resourceDownload[str,Association[]]

resourceDownload[str_String, params_Association]:=Block[{res,resource,downloadedversion, id=str},
	If[!MemberQ[$localResources,str],
		resource=resourceAcquire[str, False];
		If[MatchQ[resource,rpat],
			id=resourceObjectID[resource];
			,
			Return[$Failed]
		]
	];
	res=apifun["CopyResource",Join[params,Association["UUID"->id]],ResourceSystemClient`ResourceDownload];
	resourcedownload[res];
	System`ResourceObject[id]
]

resourcedownload[res_]:=With[{rtype=Lookup[res,"ResourceType",Throw[$Failed]]},
	loadResourceType[rtype];
	repositoryresourcedownload[rtype,
	Lookup[res,"UUID",Throw[$Failed]],res]/;KeyExistsQ[res,"UUID"]
]

resourcedownload[___]:=$Failed

repositoryresourcedownload[___]:=Null

storeDownloadVersion[id_,res_, locations_, formats_, as_:Association[]]:=Block[{dv=Lookup[res,"Version"],infofile, info},
	If[!MatchQ[dv,None|_String],Return[$Failed]];
	infofile=resourceInfoFile[id];
	info=Get[infofile];
	info["DownloadedVersion"]=dv;
	
	info=updateRepositoryResourceInfo[Lookup[info,"ResourceType"],id, info, locations, formats, as];
	
	Put[info,infofile];
	resourceInfo[id]=info
	
	
]

updateRepositoryResourceInfo[_,_, info_, __]:=info



cloudResourceDownload[info_, as_]:=With[{rtype=Lookup[info,"ResourceType",Throw[$Failed]]},
	loadResourceType[rtype];
	repositorycloudResourceDownload[rtype, info, as]	
]

repositorycloudResourceDownload[___]:=$Failed


End[] (* End Private Context *)

EndPackage[]



SetAttributes[{},
   {ReadProtected, Protected}
];