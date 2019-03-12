(* Wolfram Language Package *)


BeginPackage["ResourceSystemClient`"]

Begin["`Private`"] (* Begin Private Context *) 

$ResourceBase="Resources";
$CloudResourceBase="Resources";

$rscDirectory=DirectoryName[System`Private`$InputFileName];

localObject:=If[$CloudEvaluation||$CloudResourcePathQ,
	CloudObject[toRelativePath[#],$CloudResourceBase]&,
	System`LocalObject[toRelativePath[#],resourceCacheDirectory[]]&]

$CloudResourcePathQ=False;

resourceCacheDirectory[]:=If[$CloudEvaluation||$CloudResourcePathQ,
	$CloudResourceBase
	,
	FileNameJoin[{LocalObjects`PathName[LocalObject[$LocalBase]], $ResourceBase}]
];

$ResourceSystemClientDebug=False;

$loadedResources={};

marketPlaceReviewerGroup:=marketPlaceReviewerGroup=PermissionsGroup["marketplace-admin@wolfram.com","Reviewers"];

(* Directories *)
toRelativePath[co_CloudObject]:=toRelativePath[First[co]]
toRelativePath[path_]:=path/;StringFreeQ[path,$ResourceBase]
toRelativePath[path_]:=FileNameJoin[Replace[FileNameSplit[path],{___,$ResourceBase,rest___}:>{rest},{0}],OperatingSystem->"Unix"]

createDirectory[dir_]:=CreateDirectory[dir, CreateIntermediateDirectories->True]/;!DirectoryQ[dir]

resourceDirectory[id_]:=FileNameJoin[{resourceCacheDirectory[],StringTake[id,3], id}]

resourceInfoFile[id_]:=localObject[FileNameJoin[{resourceDirectory[id],"metadata"}]]
resourceinfoFile[dir_]:=localObject[FileNameJoin[{dir,"metadata"}]]

cloudResourceDirectoryObject[path_]:=FileNameJoin[Flatten[{$CloudRootDirectory,$CloudResourceBase, FileNameSplit[path]}]]

Attributes[cloudpath]={HoldFirst};
cloudpath[expr_]:=Block[{$CloudResourcePathQ=True, os=Options[FileNameJoin,OperatingSystem],
	localObject=(CloudObject[toRelativePath[#],$CloudResourceBase]&), res},
	SetOptions[FileNameJoin,OperatingSystem->"Unix"];
	res=expr;
	SetOptions[FileNameJoin,os];
	res
]

(* common utility functions *)

rpat=(_System`ResourceObject);

resourceObjectQ[ro_System`ResourceObject]:=AssociationQ[First[ro]]
resourceObjectQ[___]:=False

getResourceInfo[id_,keys_]:=With[{info=getResourceInfo[id]},
	If[AssociationQ[info],
		KeyTake[info,keys],
		Missing["NotAvailable"]
	]
]


getResourceInfo[id_]:=With[{info=resourceInfo[id]},
	If[AssociationQ[info],info,getresourceInfo[id]]
]

$usableResourceInfoKeys={
	"Name","UUID","ResourceType",
	"Version","Description"
};

usableResourceInfo[as_]:=usableresourceInfo[as,Lookup[as,"ResourceType"]]
usableresourceInfo[as_, rtype_]:=KeyTake[as,usableResourceInfoKeys[rtype]]

usableResourceInfoKeys[rtype_]:=DeleteDuplicates[Flatten[{$usableResourceInfoKeys,usableresourceInfoKeys[rtype]}]]

usableresourceInfoKeys[rtype_String]:=(loadResourceType[rtype];usableresourceinfoKeys[rtype])

usableresourceinfoKeys[_]:={}

getresourceInfo[id_]:=Block[{lo=resourceInfoFile[id], info},
	If[fileExistsQ[lo],
		info=Quiet[Get[lo]];
		If[AssociationQ[info],
			info=standardizeResourceInfo[info];
			resourceInfo[id]=info;
			info
			,
			importresourceInfo[id,False]
		]
		,
		Missing["NotAvailable"]
	]
]

standardizeResourceInfo[info_Association]:=$Failed/;!KeyExistsQ[info,"UUID"]


standardizeResourceInfo[info_Association]:=With[{rtype=Lookup[info,"ResourceType"]},
	loadResourceType[rtype];
	sortBasicInfo[repositorystandardizeResourceInfo[rtype,info]]
]
standardizeResourceInfo[l_List]:=standardizeResourceInfo[Association[l]]
standardizeResourceInfo[expr_]:=expr

repositorystandardizeResourceInfo[_,info_Association]:=info

setResourceInfo[id_, as_Association]:=Block[{lo=resourceInfoFile[id], info},
	If[fileExistsQ[lo],
		info=Get[lo];
		If[AssociationQ[info],
			info=Join[info,as],
			info=as
		];
		resourceInfo[id]=info;
		Put[info, lo];
		info
		,
		Missing["NotAvailable"]
	]
]

setResourceInfo[id_, opts__?OptionQ]:=setResourceInfo[id, Association[opts]]
setResourceInfo[___]:=$Failed

cloudConnect[head_]:=(
    CloudConnect[];
    If[!$CloudConnected,
        Message[head::cloudc];
        Throw[$Failed]
    ]
)


resourceObjectID[object_]:=Lookup[First[object],"UUID",$Failed]
resourceObjectName[object_]:=Lookup[First[object],"Name",
	Lookup[getResourceInfo[resourceObjectID[object]],"Name",Missing["NotAvailable"]]]
	
uuidQ[str_String]:=StringMatchQ[str, RegularExpression["\\w{8}-\\w{4}-\\w{4}-\\w{4}-\\w{12}$"]]
uuidQ[_]:=False

newerversionQ[{x_, x_}]:=False
newerversionQ[{new_String, old_String}]:=newerversionQ[ToExpression[StringSplit[new,"."]],ToExpression[StringSplit[old,"."]]]
newerversionQ[{new_String, old_}]:=True
newerversionQ[{new_, old_String}]:=False
newerversionQ[_]:=False

newerversionQ[new:{__Integer},old:{__Integer}]:=Catch[
    (If[new[[#]]>old[[#]],Throw[True,"newer"],If[old[[#]]>new[[#]],Throw[False,"newer"]]
    ]&/@Range[Min[Length/@{new,old}]];False),"newer"]
     
fillResourceMetadata[info_,more_]:=Join[more, info]

fileExistsQ[lo_System`LocalObject]:=FileExistsQ[LocalObjects`PathName[lo]]
fileExistsQ[expr_]:=FileExistsQ[expr]

cacheresourceInfo[l_]:=cacheresourceinfo/@l

cacheresourceinfo[info_]:=Block[{dir, id=Lookup[info,"UUID",Throw[$Failed]], newinfo=info,rtype},
	dir=resourceDirectory[id];
	newinfo["ResourceLocations"]=DeleteDuplicates[Flatten[{Lookup[newinfo,"ResourceLocations",{}],localObject[dir]}]];
	rtype=Lookup[info,"ResourceType"];
	loadResourceType[rtype];
	newinfo=repositoryCacheResourceInfo[rtype,id, newinfo, dir];
	If[$AllowResourceTextSearch,
		addToLocalSearchIndex[id, newinfo,dir]
	];
	$localResources=Cases[DeleteDuplicates[Append[$localResources,id]],_String];
	loadresource[newinfo];
	newinfo

]

repositoryCacheResourceInfo[_,id_, info_, dir_]:=Block[{newinfo},
	If[DirectoryQ[dir],
		newinfo=updatecachedresourceinfo[dir, id, info]
		,
		createDirectory[dir];
		newinfo=Append[info,"DownloadedVersion"->None];
		Put[newinfo,resourceinfoFile[dir]];
	];
	newinfo
]



deleteresourcecache[info_Association]:=With[{id=Lookup[info,"UUID",Throw[$Failed]], name=Lookup[info,"Name"]},
	deleteresourcecache[id,name]
]

deleteresourcecache[id_String,name_:Missing[]]:=With[{dir=FileNameJoin[{resourceCacheDirectory[],StringTake[id,3], id}]},
	If[DirectoryQ[dir],
		DeleteDirectory[dir, DeleteContents->True]
	];
	$localResources=Cases[DeleteCases[$localResources,id],_String];
	clearresource[id, name]
]

updatecachedresourceinfo[dir_, id_, info_]:=Block[{infofile,oldinfo, olddownloadversion, newinfo},
	infofile=resourceinfoFile[dir];
	If[fileExistsQ[infofile],
		oldinfo=Quiet[Get[infofile]];
		If[!AssociationQ[oldinfo],
			oldinfo=Association[]
		];
		,
		oldinfo=Association[]
	];
	If[newerversionQ[Lookup[{info,oldinfo},"Version"]],
		olddownloadversion=If[fileExistsQ[infofile],
			Lookup[oldinfo,"DownloadedVersion",None],
			None
		];
		newinfo=Append[info,"DownloadedVersion"->olddownloadversion];
		Put[newinfo,infofile];
		newinfo
		,
		newinfo=mergeResourceInfo[info, oldinfo];
		Put[newinfo,infofile];
		newinfo
	]
]

mergeResourceInfo[info_Association, oldinfo_Association]:=Merge[{info, oldinfo},mergeinfofun]
mergeResourceInfo[info_Association, oldinfo_]:=info
mergeResourceInfo[info_, oldinfo_Association]:=oldinfo
mergeResourceInfo[__]:=Association[]


mergeinfofun[l:{_List..}]:=DeleteDuplicates[Flatten[l]]
mergeinfofun[l_]:=Last[l]



$CategoryListCloudObject:=($CategoryListCloudObject=CloudObject[URLBuild[MapAt[Append[Drop[#, -2], "CategoryList"] &, URLParse[$resourceSystemRequestBase], "Path"]]])
$ContentTypeListCloudObject:=($ContentTypeListCloudObject=CloudObject[URLBuild[MapAt[Append[Drop[#, -2], "ContentTypeList"] &, URLParse[$resourceSystemRequestBase], "Path"]]])

$resourceCategories:=($resourceCategories=getResourceCategories[])
$resourceContentTypes:=($resourceContentTypes=getResourceContentTypes[])

resourceCategories[All]:=DeleteDuplicates[Flatten[Values[$resourceCategories]]]
resourceContentTypes[All]:=DeleteDuplicates[Flatten[Values[$resourceContentTypes]]]

resourceCategories[rtype_]:=Lookup[$resourceCategories,rtype,{}]
resourceContentTypes[rtype_]:=Lookup[$resourceContentTypes,rtype,{}]

getResourceCategories[]:=With[{res=CloudGet[$CategoryListCloudObject]},
	If[AssociationQ[res],
		res
		,
		Association[]
	]
]

getResourceContentTypes[]:=With[{res=CloudGet[$ContentTypeListCloudObject]},
	If[AssociationQ[res],
		res
		,
		Association[]
	]
]



bytecountQuantity[n_?NumberQ]:=Which[
	n<2000,
	Quantity[n,"Bytes"],
	n<2*10^6,
	Quantity[N[n]/10^3,"Kilobytes"],
	True,
	Quantity[N[n]/10^6,"Megabytes"]
]
bytecountQuantity[expr_]:=expr


fileByteCount[lo_LocalObject]:=filebyteCount[lo,LocalObjects`PathName@lo]
fileByteCount[expr_]:=FileByteCount[expr]
filebyteCount[lo_,dir_]:=filebyteCount[lo,LocalObjects`BundlePathName[lo]]/;DirectoryQ[dir]
filebyteCount[_,file_]:=FileByteCount[file]/;FileExistsQ[file]
filebyteCount[___]:=Missing["NotAvailable"]

marketplacebasedResourceQ[info_]:=Head[Lookup[info,"RepositoryLocation"]]===System`URL
userdefinedResourceQ[info_]:=MatchQ[Lookup[info,"RepositoryLocation", None],(_LocalObject|None)]

setReviewerPermissions[co_CloudObject]:=SetPermissions[co,"marketplace-admin@wolfram.com"->"Read"]

importlocal[lo_LocalObject]:=With[{objectinfo=LocalObjects`getLocalObject[lo]},
	Switch[Lookup[objectinfo,"Type"],
		"Export",
		Import[lo],
		"Expression"|Expression,
		Get[lo],
		_,
		Import[lo]
	]
]
importlocal[file_]:=Import[file]

End[] (* End Private Context *)

EndPackage[]