(* Wolfram Language Package *)

(Unprotect[#]; Clear[#])& /@ {}

BeginPackage["DataResource`"]

Begin["`Private`"] (* Begin Private Context *) 

localObject:=ResourceSystemClient`Private`localObject;
resourceDirectory=ResourceSystemClient`Private`resourceDirectory;
resourceInfoFile=ResourceSystemClient`Private`resourceInfoFile;
resourceInfo=ResourceSystemClient`Private`resourceInfo;
resourceObjectID=ResourceSystemClient`Private`resourceObjectID;
fileExistsQ=ResourceSystemClient`Private`fileExistsQ;
createDirectory=ResourceSystemClient`Private`createDirectory;
bytecountQuantity=ResourceSystemClient`Private`bytecountQuantity;
fileByteCount=ResourceSystemClient`Private`fileByteCount;
cacheresourceinfo=ResourceSystemClient`Private`cacheresourceinfo;
cloudpath=ResourceSystemClient`Private`cloudpath;
cloudResourceDirectoryObject=ResourceSystemClient`Private`cloudResourceDirectoryObject;
userdefinedResourceQ=ResourceSystemClient`Private`userdefinedResourceQ;
marketplacebasedResourceQ=ResourceSystemClient`Private`marketplacebasedResourceQ;
importlocal=ResourceSystemClient`Private`importlocal;
$localResources:=ResourceSystemClient`Private`$localResources;
$cloudResources:=ResourceSystemClient`Private`$cloudResources;
$loadedResources:=ResourceSystemClient`Private`$loadedResources;
uuidQ=ResourceSystemClient`Private`uuidQ;
localResourceNameMap:=ResourceSystemClient`Private`localResourceNameMap;
resourcesearchLocalName=ResourceSystemClient`Private`resourcesearchLocalName;
resourceObjectQ=ResourceSystemClient`Private`resourceObjectQ;
validateParameter=ResourceSystemClient`Private`validateParameter;
resourceCacheDirectory=ResourceSystemClient`Private`resourceCacheDirectory;
setReviewerPermissions=ResourceSystemClient`Private`setReviewerPermissions;

ResourceSystemClient`Private`getresourceType["Data"]="DataResource";

$DataResourceType=("DataResource"|"Data"|"data");

dataresourceCopyDirectory[id_,rest___]:=dataresourcecopyDirectory[resourceDirectory[id],rest]
dataresourcecopyDirectory[dir_,format_]:=dataresourcecopyDirectory[dir,format, Automatic]
dataresourcecopyDirectory[dir_,format_, Automatic]:=FileNameJoin[{dir,"download","Automatic",IntegerString[Hash[format],16]}]
dataresourcecopyDirectory[dir_,format_, elem_]:=FileNameJoin[{dir,"download",elem,IntegerString[Hash[format],16]}]

dataresourceCopyInfoFile[id_,format_]:=dataresourceCopyInfoFile[id,format, Automatic]
dataresourceCopyInfoFile[id_,format_, Automatic]:=localObject@FileNameJoin[{resourceDirectory[id],"download","Automatic",IntegerString[Hash[format],16],"metadata"}]
dataresourceCopyInfoFile[id_,format_, elem_]:=localObject@FileNameJoin[{resourceDirectory[id],"download",elem,IntegerString[Hash[format],16],"metadata"}]
dataresourcecopyInfoFile[copydir_,format_]:=localObject@FileNameJoin[{copydir,"metadata"}]

getElementCopyInfo[id_, format_, elem_]:=getelementCopyInfo[dataresourceCopyInfoFile[id,format, elem]]
getelementCopyInfo[file_]:=Get[file]/;FileExistsQ[file]
getelementCopyInfo[_]:=None

dataresourceElementDirectory[id_, elem_]:=FileNameJoin[{resourceDirectory[id],"download",ToString[elem]}]
dataresourceElementInfoFile[id_, elem_]:=localObject@FileNameJoin[{resourceDirectory[id],"download",ToString[elem],"metadata"}]

getAllElementInfo[info_]:=getAllElementInfo[Lookup[info,"UUID"],Lookup[info,"ContentElements"]]
getAllElementInfo[id_String,elems_List]:=With[{as=AssociationMap[getElementInfo[id, #]&, elems]},
	DeleteCases[as,$Failed|None]
	]
getAllElementInfo[__]:=Association[]


getElementInfo[id_, elem_]:=getelementInfo[dataresourceElementInfoFile[id, elem]]
getelementInfo[file_]:=Get[file]/;FileExistsQ[file]
getelementinfo[_]:=None

getElementFormat[id_,elem_]:=getelementFormat[dataresourceElementInfoFile[id, elem]]
getelementFormat[file_]:=chooseformat[Lookup[Get[file],"Formats"]]/;fileExistsQ[file]
getelementFormat[_]:=Automatic

storeElementFormats[id_, elem_, formats_]:=storeelementFormats[dataresourceElementInfoFile[id, elem],formats]
storeelementFormats[file_, formats_]:=storeelementformats[Get[file], file,formats]/;fileExistsQ[file]
storeelementformats[as_Association, file_,formats_]:=Block[{newas=as},
	newas["Formats"]=DeleteDuplicates[Flatten[{formats,newas["Formats"]}]];
	Put[newas,file]	
]
storeelementFormats[file_, formats_]:=Put[Association["Formats"->formats],file]
storeelementformats[_,file_, formats_]:=Put[Association["Formats"->formats],file]

getAllElementFunction[info_]:=getAllElementFunction[Lookup[info,"UUID"],Lookup[info,"ContentElements"]]
getAllElementFunction[id_String,elems_List]:=With[{as=AssociationMap[getElementFunction[id, #]&, elems]},
	DeleteCases[as,$Failed]
	]
getAllElementFunction[__]:=Association[]

getElementFunction[id_, elem_]:=getelementFunction[dataresourceElementInfoFile[id, elem]]
getelementFunction[file_]:=getelementfunction[Get[file]]/;fileExistsQ[file]
getelementFunction[_]:=$Failed
getelementfunction[as_Association]:=Lookup[as,"ContentFunction",$Failed]
getelementfunction[_]:=$Failed

storeElementFunction[id_, elem_, func_]:=storeelementFunction[dataresourceElementInfoFile[id, elem],func]
storeelementFunction[file_, func_]:=storeelementfunction[Get[file], file,func]/;fileExistsQ[file]
storeelementfunction[as_Association, file_,func_]:=Block[{newas=as},
	newas["ContentFunction"]=func;
	Put[newas,file]	
]
storeelementFunction[file_, func_]:=Put[Association["ContentFunction"->func],file]
storeelementfunction[_,file_, func_]:=Put[Association["ContentFunction"->func],file]

chooseformat[formats_List]:=First[formats];
chooseformat[_]:=Automatic;

ResourceSystemClient`Private`usableresourceinfoKeys[$DataResourceType]={"ContentSize","ContentElements"};



multipartResourceQ[info_]:=AssociationQ[Lookup[info,"ContentElementLocations", None]]||AssociationQ[Lookup[info,"InformationElements", None]]
cloudStoredQ[info_]:=MatchQ[Lookup[info,"ContentElementLocations", None],_CloudObject]/;!multipartResourceQ[info]
cloudStoredQ[info_]:=MatchQ[Values[Lookup[info,"ContentElementLocations", None]],{(_CloudObject)...}]
cloudStoredQ[info_]:=False
cloudStoredQ[info_,Automatic]:=cloudStoredQ[info]
cloudStoredQ[info_,elems_List]:=And@@(cloudStoredQ[info,#]&/@elems)
cloudStoredQ[info_,elem_]:=With[{locations=Lookup[info,"ContentElementLocations"]},
	If[AssociationQ[locations],
		If[MatchQ[Lookup[locations, elem],(_CloudObject)],
			True,False]
		,
		False
	]]

localStoredQ[info_]:=MatchQ[Lookup[info,"ContentElementLocations", None],_LocalObject|_File]/;!multipartResourceQ[info]
localStoredQ[info_]:=MatchQ[Values[Lookup[info,"ContentElementLocations", None]],{(_LocalObject|_File)...}]
localStoredQ[info_,Automatic|All]:=localStoredQ[info]
localStoredQ[info_,elems_List]:=And@@(localStoredQ[info,#]&/@elems)
localStoredQ[info_, elem_]:=localstoredQ[info,Lookup[info,"ContentElementLocations"],elem]

localstoredQ[info_,locations_Association,elem_]:=MatchQ[Lookup[locations, elem],(_LocalObject|None)]/;KeyExistsQ[locations,elem]
localstoredQ[info_,locations_Association,elem_]:=MemberQ[Lookup[info,"ContentElements",{}],elem]&&!MemberQ[Keys[locations],elem]
localstoredQ[info_,_Missing|_CloudObject,elem_]:=False/;elem===Lookup[info,"DefaultContentElement"]
localstoredQ[info_,_,elem_]:=MemberQ[Lookup[info,"ContentElements",{}],elem]
localstoredQ[___]:=False

contentElementQ[_,Automatic]=True;
contentElementQ[info_,l_List]:=And@@(MemberQ[Lookup[info,"ContentElements",{}],#]&/@l)
contentElementQ[info_,elem_]:=MemberQ[Lookup[info,"ContentElements",{}],elem]


ResourceSystemClient`Private`repositorystandardizeResourceInfo[$DataResourceType,info_Association]:=mapAt[
		Join[KeyDrop[info,"ElementInformation"],Lookup[info,"ElementInformation",Association[]]],	
			{
			"ContentSize"->bytecountQuantity,
			"ContentElements"->(DeleteCases[#,Automatic]&)			
			}]

mapAt[as_, rules : {_Rule ..}] := Fold[mapAt[#2[[2]], #1, #2[[1]]] &, as, rules]
mapAt[f_,as_,key_]:=MapAt[f,as,key]/;KeyExistsQ[as,key]
mapAt[_,as_,_]:=as

ResourceSystemClient`Private`resourceInfoOrder[_]:={
	"Name","UUID",
	"Content","ContentElementLocations",
	"RepositoryLocation","ResourceLocations",
	"ResourceType","ContentSize","ContentElements",
	"Version","Description",
	"ContentTypes",
	"Categories","Keywords",
	"MyAccount","Attributes",
	"LatestUpdate","DownloadedVersion",
	"Formats","DefaultReturnFormat","Caching"
}

ResourceSystemClient`Private`repositoryCacheResourceInfo[$DataResourceType,id_, info_, dir_]:=Block[{cfs, newinfo=info},
	If[KeyExistsQ[newinfo,"ContentElementFunctions"],
		cfs=newinfo["ContentElementFunctions"];
		newinfo=KeyDrop[newinfo,"ContentElementFunctions"]
	];
	If[DirectoryQ[dir],
		newinfo=ResourceSystemClient`Private`updatecachedresourceinfo[dir, id, newinfo]
		,
		createDirectory[dir];
		newinfo=Append[newinfo,"DownloadedVersion"->None];
		Put[Append[newinfo,"DownloadedVersion"->None],resourceInfoFile[id]];
	];
	storecontentFunctions[id, cfs];
	newinfo
]

attributeCheck[info_,att_]:=MemberQ[info["Attributes"],att]/;KeyExistsQ[info, "Attributes"]
attributeCheck[__]:=False

ResourceSystemClient`Private`addToResourceTypes["DataResource"]

getContentElementAccessType[as_]:=Lookup[as,"ContentElementAccessType",Lookup[as,"ContentType"]]

End[] (* End Private Context *)

EndPackage[]



SetAttributes[{},
   {ReadProtected, Protected}
];