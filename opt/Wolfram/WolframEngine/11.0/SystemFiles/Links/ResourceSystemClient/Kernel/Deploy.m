(* Wolfram Language Package *)

(Unprotect[#]; Clear[#])& /@ {}



BeginPackage["ResourceSystemClient`"]
(* Exported symbols added here with SymbolName::usage *)  

ResourceSystemClient`$DeployResourceContent=True;
ResourceSystemClient`$CreateCloudResourceCache=True;

Begin["`Private`"] (* Begin Private Context *) 

cloudDeployResource[ro_System`ResourceObject, rest___]:=clouddeployResourceObject[resourceObjectID[ro],ro,rest]

cloudDeployResource[___]:=$Failed

clouddeployResourceObject[id_, _,rest___]:=clouddeployresourceObject[id, getResourceInfo[id], rest]/;MemberQ[$localResources, id]
clouddeployResourceObject[id_, _,rest___]:=(
	importandclouddeployresourceObject[id, resourceInfo[id], rest])/;MemberQ[$loadedResources, id]
clouddeployResourceObject[id_, ro_,rest___]:=loadandclouddeployresourceObject[id, ro, rest]

clouddeployResourceObject[___]:=Throw[$Failed]

importandclouddeployresourceObject[id_, info_,rest___]:=Block[{acquired},
	acquired=resourceAcquire[id,False];
	If[Head[acquired]===System`ResourceObject&&MemberQ[$localResources, id],
		clouddeployresourceObject[id, resourceInfo[id], rest]
	]
]/;marketplacebasedResourceQ[info]


importandclouddeployresourceObject[id_, info_,rest___]:=Block[{saved},
	saved=saveresourceObject[info];
	If[Head[saved]===System`ResourceObject&&MemberQ[$localResources, id],
		clouddeployresourceObject[id, resourceInfo[id], rest]
	]
]/;userdefinedResourceQ[info]

importandclouddeployresourceObject[___]:=Throw[$Failed]

loadandclouddeployresourceObject[id_, ro_, rest___]:=Block[{loaded},
	loaded=loadResource[id];
	If[AssociationQ[loaded]&&MemberQ[$loadedResources, id],
		clouddeployResourceObject[id, ro, rest]
	]
]

loadandclouddeployresourceObject[___]:=Throw[$Failed]


clouddeployresourceObject[id_, info_, rest___]:=clouddeployresourceobject[id,info, rest]
	
clouddeployresourceobject[id_,localinfo_, rest___]:=Block[{type,newinfo=Association[],cloudinfo, res, fullinfo},
	type=Lookup[localinfo,"ResourceType",Throw[$Failed]];
	loadResourceType[type];
	fullinfo=repositoryBundleResourceObject[type,id, localinfo];
	If[TrueQ[ResourceSystemClient`$DeployResourceContent],
		newinfo=repositoryclouddeployResourceContent[type, id, fullinfo];
		,
		If[containsLocalFileContentQ[localinfo],
			Message[ResourceObject::nocdep]
		]
	];
	If[TrueQ[ResourceSystemClient`$CreateCloudResourceCache],
		cloudinfo=repositoryclouddeployResourceInfo[type,id,  fullinfo,newinfo];
		addToCloudResourceIndex[cloudinfo];
		AppendTo[$cloudResources,id];
		,
		cloudinfo=Join[fullinfo, newinfo];
		cloudinfo["RepositoryLocation"]=None;
		cloudinfo["ResourceLocations"]={};
	];
	cloudinfo["Autoload"]=True;
	Block[{System`ResourceObject},
		res=CloudDeploy[ExportForm[System`ResourceObject[cloudinfo],"WL"], rest];
		If[Head[res]===CloudObject,
			res
			,
			$Failed
		]
	]
]

repositoryclouddeployResourceInfo[_String,id_,  localinfo_, newinfo_]:=Block[{cloudinfo=localinfo,infoco},
	cloudinfo["ResourceLocations"]={CloudObject[cloudpath[resourceDirectory[id]]]};
	infoco=cloudResourceDirectoryObject[FileNameJoin[{StringTake[id,3], id,"metadata"}]];
	cloudinfo=Join[cloudinfo,newinfo];
	CloudPut[cloudinfo,infoco];
	cloudinfo
]

repositoryclouddeployResourceContent[___]:=Association[]
repositoryclouddeployResourceInfo[___]:=Throw[$Failed]



repositoryBundleResourceObject[_,_, localinfo_]:=localinfo

containsLocalFileContentQ[localinfo_]:=containslocalFileContentQ[Lookup[localinfo,{"ContentElementLocations","ContentLocation"}]]

containslocalFileContentQ[locations_]:=containslocalfileContentQ[Flatten[locations /. as_Association :> Values[as]]]

containslocalfileContentQ[locations_]:=(!FreeQ[locations,LocalObject|System`File])||AnyTrue[Select[locations,StringQ],FileExistsQ]

createResourceShingle[ro:HoldPattern[System`ResourceObject][as_Association]]:=With[{id=Lookup[as,"UUID",Throw[$Failed]]},
	If[MemberQ[$loadedResources,id],
		createresourceShingle[resourceInfo[id]]
		,
		createresourceShingle[as]
	]
]
	
	
createresourceShingle[info_Association]:=Block[{template, rtype},
	rtype=Lookup[info,"ResourceType",Throw[$Failed]];
	loadResourceType[rtype];
	template=shingletemplate[rtype];
	If[Head[template]===TemplateObject,
		createresourceshingle[template, info]
	]	
]

createresourceshingle[template_, info_]:=With[{shingleinfo=formatShingleInfo[info]},
	TemplateApply[template, shingleinfo]	
]

shingletemplate[rtype_]:=(shingletemplate[rtype]=fileTemplate[shingletemplatefile[rtype]])

fileTemplate[file_]:=FileTemplate[file]/;FileExistsQ[file]
fileTemplate[_]:=Throw[$Failed]

shingletemplatefile[_]:=FileNameJoin[{$rscDirectory,"templates","shingle.xml"}]

formatShingleInfo[info_]:=info


End[] (* End Private Context *)

EndPackage[]



SetAttributes[{},
   {ReadProtected, Protected}
];