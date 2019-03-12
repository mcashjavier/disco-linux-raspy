(* Wolfram Language Package *)

(Unprotect[#]; Clear[#])& /@ {}

BeginPackage["DataResource`"]

Begin["`Private`"] (* Begin Private Context *) 

ResourceSystemClient`Private`repositoryresourcedownload[$DataResourceType,args___]:=resourcedownload0[args]

resourcedownload0[id_String,res_]:=Block[{formats, locations, elem=Lookup[res,"Element",Automatic]},
	{locations, formats}=If[KeyExistsQ[res,"ContentFormat"],
        dataresourceexport[id,Lookup[res,"ContentFormat"],elem,res]
        ,
        If[KeyExistsQ[res,"DownloadInfo"],
            dataresourcedownload[id,Lookup[res,"DownloadInfo",Throw[$Failed]],elem]
            ,
            $Failed
        ]
    ];
    storeContentFunctions[id, res];
    ResourceSystemClient`Private`storeDownloadVersion[id,res,locations,formats, Association["Element"->elem]]
]

resourcedownload0[___]:=$Failed

storeContentFunctions[id_, res_]:=storecontentFunctions[id,Lookup[res,"ContentElementFunctions"]]
storecontentFunctions[id_,as_Association]:=AssociationMap[storeElementFunction[id,#[[1]],#[[2]]]&,as]
storecontentFunctions[id_,str_String]:=With[{unc=Quiet[Uncompress[str]]},
	If[AssociationQ[str],storeContentFunctions[id, unc]]
]


dataresourceexport[id_,format_,elem_,res_]:=Block[{content, file, dir,lo},
	content=Lookup[res,"Content",Throw[$Failed]];
	content=Switch[format,
		"Compressed",Uncompress,
		"PlainText",ToExpression,
		_,Identity][content];
	dir=dataresourceCopyDirectory[id,"MX",elem];
	createDirectory[dir];
	lo=localObject[FileNameJoin[{dir,"data"}]];
	Export[lo, content,"MX"];
	Put[Association[
		{
			"Location"->lo,
			"DownloadDate"->DateObject[],
			"Format"->"MX",
			"Size"->bytecountQuantity[ByteCount[content]]
		}
	   ],
	   dataresourcecopyInfoFile[dir,"MX"]
	];
	{{lo},{"MX"}}
]

dataresourcedownload[id_,downloadinfo_List,elem_]:=Transpose[dataresourcefileDownload[id,#, elem]&/@downloadinfo]
dataresourcedownload[id_,downloadinfo_,elem_]:=dataresourcedownload[id,{downloadinfo},elem]
dataresourcedownload[__]:=$Failed

dataresourcefileDownload[id_,fileinfo_,elem_]:=Block[{format, cos},
	{format,cos}=Lookup[fileinfo,{"Format","ContentElementLocations"}];
	dataresourcefiledownload[id,format,cos,elem]
]

dataresourcefiledownload[id_,fmt:("MXChunks"|"CompressedChunks"),cos_,elem_]:=Block[{n=Length[cos], progress=0, 
	dir=dataresourceCopyDirectory[id,fmt,elem],lo,los={},size},
	If[!ListQ[cos],Throw[$Failed]];
	If[Length[cos]>0,
		PrintTemporary[ProgressIndicator[Dynamic[progress], {0,n+1}]];
		createDirectory[dir];
		({progress,lo}=dataresourcechunkdownload[id,fmt,dir,#,progress];
			AppendTo[los,lo])&/@cos;
		{lo,size}=mergeChunks[id,fmt,los];
		
		DeleteDirectory[dir,DeleteContents->True];
		Put[Association[
        	{
            "Location"->lo,
            "DownloadDate"->DateObject[],
            "Format"->"MX",
            "Size"->bytecountQuantity[size]
            }
        ],
        dataresourceCopyInfoFile[id,"MX",elem]
        ];
		
		{lo,"MX"}
		,
		Throw[$Failed]
	]
	
]

dataresourcechunkdownload[id_,fmt_,dir_,co_CloudObject,i_]:=With[{
	raw=URLFetch[co, {"StatusCode","ContentData"},"VerifyPeer" -> False,"CredentialsProvider" -> None],
	lo=localObject[FileNameJoin[{dir,"data",ToString[i]}]]},
    If[raw[[1]]===200,
    	Switch[fmt,
    		"MXChunks",
    		dataresourcelocalrawexport[lo,raw[[2]],"MX"],
    		"CompressedChunks",
    		Export[lo,raw,"String"],
    		_,
    		Throw[$Failed]
    	]
        ,
        Throw[$Failed]
    ];
    {i+1,lo}
]

mergeChunks[id_,"MXChunks",los_]:=Block[{dir=dataresourceCopyDirectory[id,"MX"],
	lo=localObject[FileNameJoin[{dataresourceCopyDirectory[id,"MX"],"data"}]]},
	(Export[lo,#,"MX"];{lo,ByteCount[#]})&@(Join@@(Import/@los))
]

mergeChunks[id_,"CompressedChunks",los_]:=Block[{dir=dataresourceCopyDirectory[id,"MX"],
	lo=localObject[FileNameJoin[{dataresourceCopyDirectory[id,"MX"],"data"}]]},
	(Export[lo,#,"MX"];{lo,ByteCount[#]})&@(Join@@(Uncompress[Import[#,"String"]]&/@los))
]

cloudexportfmts=("MX"|"PNG");
dataresourcefiledownload[id_,fmt:cloudexportfmts,co_CloudObject, elem_]:=With[{
	raw=URLFetch[co, {"StatusCode","ContentData"},"VerifyPeer" -> False,"CredentialsProvider" -> None],
	dir=dataresourceCopyDirectory[id,fmt,elem],
	lo=localObject[FileNameJoin[{dataresourceCopyDirectory[id,fmt,elem],"data"}]]},
	createDirectory[dir];
    If[raw[[1]]===200,
        dataresourcelocalrawexport[lo,raw[[2]],fmt];
        Put[Association[
        	{
            "Location"->lo,
            "DownloadDate"->DateObject[],
            "Format"->fmt,
            "Size"->bytecountQuantity[Length[raw[[2]]]]
            }
        ],
        dataresourcecopyInfoFile[dir,fmt]
        ];
        {lo,fmt}
        ,
        Throw[$Failed]
    ]
]

dataresourcefiledownload[id_,"Compressed",co_CloudObject, elem_]:=Block[{
	raw=URLFetch[co, {"StatusCode","ContentData"},"VerifyPeer" -> False,"CredentialsProvider" -> None],
	dir=dataresourceCopyDirectory[id,"MX",elem],
	lo=localObject[FileNameJoin[{dataresourceCopyDirectory[id,"MX",elem],"data"}]], content},
	createDirectory[dir];
    If[raw[[1]]===200,
		content=Uncompress[FromCharacterCode[raw[[2]]]];
		If[content=!=$Failed,
	        Export[lo,content,"MX"];
	        Put[Association[
	        	{
	            "Location"->lo,
	            "DownloadDate"->DateObject[],
	            "Format"->"MX",
	            "Size"->bytecountQuantity[ByteCount[content]]
	            }
	        ],
	        dataresourcecopyInfoFile[dir,"MX"]
	        ];
	        {lo,"MX"}
	        ,
	        Throw[$Failed]
		]
        ,
        Throw[$Failed]
    ]
]

dataresourcefiledownload[id_,_Missing,co_CloudObject, elem_]:=With[{
	wdf=CloudImport[co],
	dir=dataresourceCopyDirectory[id,"MX",elem],
	lo=localObject[FileNameJoin[{dataresourceCopyDirectory[id,"MX",elem],"data"}]]},
	createDirectory[dir];
    Export[lo,wdf,"MX"];
    Put[Association[
    	{
        "Location"->lo,
        "DownloadDate"->DateObject[],
        "Format"->"MX",
        "Size"->bytecountQuantity[ByteCount[wdf]]
        }
    ],
    dataresourcecopyInfoFile[dir,"MX"]
    ];
    {lo,"MX"}
]

dataresourcelocalrawexport[co_CloudObject,bytes_,fmt_]:=CloudDeploy[ExportForm[bytes,{"Byte","MX"}],co]
dataresourcelocalrawexport[lo_LocalObject,bytes_,fmt_]:=Export[lo,rawResourceBytes[fmt,bytes],"Byte"]

rawHandler[fmt_]:= Association[
   "Export" -> exportRaw,
   "Import" -> importRaw,
   "Get" -> importRaw,
   "Format" -> fmt
   ];
   
rawResourceBytes /: LocalObjects`GetHandler[rawResourceBytes[fmt_,_]] := rawHandler[fmt]

exportRaw[rawResourceBytes[fileformat_,bytes_], dataformat_, obj_LocalObject, h0_] :=Module[
	{h = h0, file},
	LocalObject;
	h["Format"] = fileformat;
  	h["Type"] = "Export";
  	h["DirectoryBundle"] = True;
  	LocalObjects`WithLocalBundleDirectory[obj,
   		file = "data."<>fileformat;
   	If[FileType[file] =!= None, DeleteFile[file]];
   		Export[file, bytes, dataformat];
   	];
  	h["ExternalData"] = file;
  	KeyDropFrom[h, {"CopyOut", "Import", "Destructor"}];
  	h]

importRaw[assoc_,obj_]:=If[
	assoc["DirectoryBundle"],
	With[{file=LocalObjects`AuxFileName[assoc]},
		LocalObjects`WithLocalBundleDirectory[obj,
			If[FileType[file]===File,
				Import[file,Lookup[assoc,"Format",FileFormat[file]]],
				$Failed]]],
	With[{data=Lookup[assoc,"Data",$Failed]},If[StringQ[data],ImportString[data,Lookup[assoc,Format,Automatic]],$Failed]]
	]

dataresourcefiledownload[___]:=$Failed
  


ResourceSystemClient`Private`updateRepositoryResourceInfo[$DataResourceType,id_,info0_, locations_, formats_, as_]:=Block[{info=info0, elem=Lookup[as,"Element",Automatic]},
	If[elem===Automatic,
		info["ContentElementLocations"]=First[locations]
		,
		If[AssociationQ[info["ContentElementLocations"]],
			info["ContentElementLocations",elem]=First[locations],
			info["ContentElementLocations"]=Association[elem->First[locations]]
		]
	];
	If[ListQ[formats],
        storeElementFormats[id, elem, formats]
	];
	info
]


ResourceSystemClient`Private`repositorycloudResourceDownload[$DataResourceType, info_, as_]:=
	cloudresourceDownload[info,info["ContentElementLocations"], Lookup[as,"Element"]]/;KeyExistsQ[info,"ContentElementLocations"]

cloudresourceDownload[info_,locations_, elem_]:=
	Switch[locations,
		_CloudObject,
		cloudresourcedownload[Lookup[info,"UUID",Throw[$Failed]],info,elem, locations],
		_Association,
		If[KeyExistsQ[locations,elem],
			cloudresourcedownload[Lookup[info,"UUID",Throw[$Failed]],info,elem, locations[elem]]
			,
			Throw[$Failed]
		],
		_,
		Throw[$Failed]	
	]

cloudResourceDownload[_]:=Throw[$Failed]
cloudresourceDownload[_]:=Throw[$Failed]

cloudresourcedownload[id_,info_,elem_, co_]:=Block[{res,copyinfo,
	dir=dataresourceCopyDirectory[id,"MX", elem],lo=localObject[FileNameJoin[{dataresourceCopyDirectory[id,"MX", elem],"data"}]]},
	createDirectory[dir];
	res=CopyFile[co,lo];
    If[Head[res]===LocalObject,
		copyinfo=resourcedownloadInfo[id, info, res];
		If[AssociationQ[copyinfo],
	        Put[copyinfo,dataresourcecopyInfoFile[dir,"MX"]]
	        ,
	        Throw[$Failed]
		];
		
    	ResourceSystemClient`Private`storeDownloadVersion[id,Association["Version"->None],{lo},{"MX"},Association["Element"->elem]]
		
        ,
        Throw[$Failed]
    ]
]

cloudresourcedownload[___]:=$Failed


resourcedownloadInfo[id_, info_, co_CloudObject]:=Association[
        	{
            "Location"->co,
            "DownloadDate"->DateObject[],
            "Format"->"MX",
            "Size"->Missing["NotAvailable"]
            }
        ]
        
resourcedownloadInfo[id_, info_, lo_]:=Association[
        	{
            "Location"->lo,
            "DownloadDate"->DateObject[],
            "Format"->"MX",
            "Size"->fileByteCount[lo]
            }
        ]
End[] (* End Private Context *)

EndPackage[]



SetAttributes[{},
   {ReadProtected, Protected}
];