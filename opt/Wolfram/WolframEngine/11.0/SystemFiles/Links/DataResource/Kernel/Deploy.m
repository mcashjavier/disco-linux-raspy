(* Wolfram Language Package *)

(Unprotect[#]; Clear[#])& /@ {}

BeginPackage["DataResource`"]

Begin["`Private`"] (* Begin Private Context *) 

ResourceSystemClient`Private`repositoryclouddeployResourceContent[$DataResourceType,id_,  info_]:=cloudDeployDataResourceContent[id, info]

$DataResourceDeployIndicator=0;

$DeployDownloadInfo=False;

cloudDeployDataResourceContent[id_, info_]:=Block[{$DataResourceDeployIndicator, elems=Lookup[info,"ContentElements"]},
	PrintTemporary[ProgressIndicator[Dynamic[$DataResourceDeployIndicator], {0,Length[elems]+1}]];
	Association["ContentElementLocations"->
		DeleteMissing[Association[clouddeployDataResourceContent[id, info,#]&/@Lookup[info,"ContentElements"]]]
	]
]/;userdefinedResourceQ[info]
cloudDeployDataResourceContent[id_,  info_]:=Association[]/;marketplacebasedResourceQ[info]

clouddeployDataResourceContent[id_,info_,elem_]:=Block[{
	cloudcontentlocation=clouddeploydataResourceContent[id,info,elem, dataresourceElementDirectory[id, elem]],
	eleminfo=getElementInfo[id, elem], format,copyinfo},
	format=Lookup[cloudcontentlocation,"Format",None];
	(* element info *)
	If[AssociationQ[eleminfo],
		If[format=!=None,eleminfo["Formats"]={format}];
		CloudPut[eleminfo,cloudpath[dataresourceElementInfoFile[id, elem]]]
	];
	(* copy info *)
	If[format=!=None&&TrueQ[$DeployDownloadInfo],
		copyinfo=getElementCopyInfo[id, format, elem];
		copyinfo["Location"]=cloudcontentlocation["Location"];
		CloudPut[copyinfo,cloudpath[dataresourceCopyInfoFile[id,format, elem]]]
	];
	$DataResourceDeployIndicator++;
	elem->Lookup[cloudcontentlocation,"Location"]
]

clouddeploydataResourceContent[id_,info_,elem_,dir_]:=clouddeploydataresourceContent[id, info, elem, dir, Quiet[Lookup[info["ContentElementLocations"],elem]]]

clouddeploydataresourceContent[id_, info_, elem_, dir_, elemlocation:(_LocalObject|_String)]:=
	clouddeploydataresourcecontent[id, info, elem, dir, elemlocation]/;fileExistsQ[elemlocation]

clouddeploydataresourceContent[id_, info_, elem_, dir_, elemlocation_CloudObject]:=
	Association[{"Location"-> elemlocation}]

clouddeploydataresourcecontent[id_, info_, elem_, dir_, elemlocation_]:=Block[{targetCO,format=getElementFormat[id, elem],res},
	targetCO=CloudObject[cloudpath[FileNameJoin[{dataresourceCopyDirectory[id,format,elem],"data"}]]];
	res=CopyFile[elemlocation,targetCO];
	If[res===$Failed,
		Message[ResourceObject::depcf, elem];
		Throw[$Failed]
	];
	Association[{"Format"->format,"Location"-> targetCO}]
]

clouddeploydataresourceContent[___]:=Association[]

ResourceSystemClient`Private`repositoryclouddeployResourceInfo[$DataResourceType,id_,  localinfo_, newinfo_]:=Block[{cloudinfo=localinfo,infoco},
	cloudinfo["ResourceLocations"]={CloudObject[cloudpath[resourceDirectory[id]]]};
	cloudinfo["ContentElementLocations"]=Join[Lookup[cloudinfo,"ContentElementLocations",Association[]],Lookup[newinfo,"ContentElementLocations",Association[]]];
	cloudinfo["RepositoryLocation"]=None;
	infoco=cloudResourceDirectoryObject[FileNameJoin[{StringTake[id,3], id,"metadata"}]];
	CloudPut[Join[cloudinfo,newinfo],infoco];
	cloudinfo
]


ResourceSystemClient`Private`repositoryBundleResourceObject[$DataResourceType,id_, localinfo_Association]:=Block[{fullinfo=localinfo},
	fullinfo["ContentElementFunctions"]=getAllElementFunction[localinfo];	
	fullinfo
]


End[] (* End Private Context *)

EndPackage[]



SetAttributes[{},
   {ReadProtected, Protected}
];