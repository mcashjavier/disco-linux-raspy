(* Mathematica Package *)

BeginPackage["DataDropClient`"]

Begin["`Private`"] (* Begin Private Context *) 

(*** API Function ***)
apifun[Except[_String],_]:=(Message[Databin::invreq];$Failed)


apifun["Add", as_] := Block[{request,callback},
	{request,callback}=databinAddRequestAndCallback[as];
	With[{
		raw=If[$CloudConnected,CloudObject`Private`authenticatedURLFetch,URLFetch][#1,{"StatusCode","ContentData"},##2]&@@request},
		callback[raw]
	]
]


asynchapifun["Add",as_,outercallback_]:=Block[{request,innercallback},
	{request,innercallback}=databinAddRequestAndCallback[as];
	With[{fun=Composition[outercallback,innercallback]},
		If[$CloudConnected,CloudObject`Private`authenticatedURLFetchAsynchronous,URLFetchAsynchronous][#1,
			(callbackfun[fun, {##}]&),
			##2]&@@request
	]
]



uuidQ[str_]:=StringLength[str]>20

apifun["Upload", as_] := Block[{raw, mpdata, strkeys, imgkeys},
    {strkeys, imgkeys, mpdata} = makeMPData[Normal[as]];
    raw = CloudObject`Private`authenticatedURLFetch[gatewayapi,{"StatusCode", "ContentData"},
        "MultipartData" -> {
            {"API", "text/plain", {85, 112, 108, 111, 97, 100}},
            {"InputStrings", "text/plain", ToCharacterCode@strkeys},
            {"CompressedWDFImages", "text/plain", ToCharacterCode@imgkeys},
            {"SourceType", "text/plain", {67, 111, 110, 110, 101, 99, 116, 101, 100,
                32, 87, 111, 108, 102, 114, 97, 109, 32, 76, 97, 110, 103, 117, 97, 103, 101}},
            {"ClientVersion", "text/plain",ToCharacterCode@$datadropclientversion},
            Sequence @@ mpdata
        },  "Method" -> "POST", "VerifyPeer" -> False, "CredentialsProvider" -> None]; 
    importResults["Add"][checkAvailable[raw, "Add"]]
    ]/;$CloudConnected&&KeyExistsQ[as,"DataDropReferences"]
    
apifun["Upload",as_]:=
With[{
    raw=CloudObject`Private`authenticatedURLFetch[gatewayapi,{"StatusCode","ContentData"},
        "Parameters"->Join[{"API"->"Upload","SourceType"->"\"Connected Wolfram Language\"","InputStrings"->"True",
            "ClientVersion"->ToString[$datadropclientversion,InputForm]},
            preparedata[Normal[as]]],"Method"->"POST", 
            "VerifyPeer" -> False,"CredentialsProvider" -> None]},
    importResults["Upload"][checkAvailable[raw,"Add"]]
]/;$CloudConnected

apifun[name_,as_]:=
With[{
	raw=CloudObject`Private`authenticatedURLFetch[gatewayapi,{"StatusCode","ContentData"},
		"Parameters"->Join[{"API"->name,"SourceType"->"\"Connected Wolfram Language\"","InputStrings"->"True",
            "ClientVersion"->ToString[$datadropclientversion,InputForm]},
			preparedata[Normal[as]]], 
			"VerifyPeer" -> False,"CredentialsProvider" -> None]},
	importResults[name][checkAvailable[raw,name]]
]/;$CloudConnected

apifun[name_,as_]:=
With[{
	raw=URLFetch[gatewayapi,{"StatusCode","ContentData"},
		"Parameters"->Join[{"API"->name,"SourceType"->"\"Unconnected Wolfram Language\"","InputStrings"->"True",
			"ClientVersion"->ToString[$datadropclientversion,InputForm]},
			preparedata[Normal[as]]],"VerifyPeer" -> False,"CredentialsProvider" -> None]},
	importResults[name][checkAvailable[raw,name]]
]/;MemberQ[Join[$nonauthenticatedrequests,$nonauthenticatedapinames],name]


apifun[name_,as_]:=(
	CloudConnect[];
	If[$CloudConnected,
		apifun[name,as]
		,
		Message[Databin::cloudc];
		Throw[$Failed]
	]
)


$DataDropHTTPCodes=_;
checkAvailable[{501,_},_]:=(Message[Databin::notav];Throw[$Failed])
checkAvailable[{504,_},"Read"|"Dashboard"]:=(Message[Databin::timeout1];Throw[$Failed])
checkAvailable[{504,_},_]:=(Message[Databin::timeout2];Throw[$Failed])
checkAvailable[$Failed,_]:=$Failed
checkAvailable[{code_,bytes_},_]:={code,Quiet[importBytes[bytes]]}
checkAvailable[___]:=$Failed

importBytes[bytes_]:=FromCharacterCode[bytes]

readrequestpattern=("Read"|"Recent"|"Entries"|"Latest"|"Values");

importResults[readrequestpattern]:=(With[{data=datadropMXRead[Quiet[ToExpression[#[[2]]]]]},
     If[$ImportDataDropReferences,
         importDataDropReferences[checkWarnings[data]],
         checkWarnings[data]
     ]
]&)


importResults[name_]:=(
	If[MatchQ[#[[1]],$DataDropHTTPCodes],
		importresults[name][#]
		,
		errorcheck[importResults[name][{200, #[[2]]}], name]
	]&)

(*
HoldPattern[importResults][name_][{code_,raw_}]:=errorcheck[importResults[name][{200, 
raw}], name]/;!MatchQ[code,$DataDropHTTPCodes]
*)

importresults["JSON"]:=(ImportString[#[[2]],"RawJSON"]&)
importresults["Raw"]:=Last
importresults[name_]:=(Quiet[ToExpression[#[[2]]]]&)

makeMPData[rules_] := Block[{toexpkeys = {}, mpdata},
  mpdata = makempdata /@ rules;
  toexpkeys = Flatten[First /@ mpdata];
  {StringJoin[Riffle[toexpkeys, ","]], 
   StringJoin[Riffle[Complement[First /@ rules, toexpkeys], ","]], 
   Last /@ mpdata}
  ]
makempdata[_[key_, img_Image]] := {{}, {key, "image/png", 
   ToCharacterCode[ExportString[img, "PNG"]]}}
makempdata[_[key_, value_]] := {{key}, {key, "text/plain", 
   ToCharacterCode[ToString[value, InputForm]]}}


$JavaAddSizeLimit=25000; 
(* No images *)
javaCompatibleQ[as_]:=False/;!FreeQ[as,_Image] 

(* Put a size limit on the request *)
javaCompatibleQ[as_]:=False/;ByteCount[as]>$JavaAddSizeLimit

DataDropClient`$JavaCompatibleQ=False;
javaCompatibleQ[_Association]:=True/;DataDropClient`$JavaCompatibleQ
javaCompatibleQ[_]:=False


databinAddRequestAndCallback[as_]:=Block[{raw, mpdata, strkeys, imgkeys},
	{strkeys, imgkeys, mpdata} = makeMPData[Normal[as]];
  	{
  		{gatewayapi,"MultipartData" -> {
      		{"API", "text/plain", {65, 100, 100}},
      		{"InputStrings", "text/plain", ToCharacterCode@strkeys},
      		{"CompressedWDFImages", "text/plain", ToCharacterCode@imgkeys},
      		{"SourceType", "text/plain", {67, 111, 110, 110, 101, 99, 116, 101, 100,
      			32, 87, 111, 108, 102, 114, 97, 109, 32, 76, 97, 110, 103, 117, 97, 103, 101}},
            {"ClientVersion", "text/plain",ToCharacterCode@$datadropclientversion},
      		Sequence @@ mpdata
      	},  "Method" -> "POST", "VerifyPeer" -> False, "CredentialsProvider" -> None}
      	,
      	(importResults["Add"][checkAvailable[#,"Add"]]&)
  	}
    ]/;$CloudConnected&&!FreeQ[as,_Image]&&ByteCount[as]>$UncompressedImageLimit

databinAddRequestAndCallback[as_]:=(
	CloudConnect[];
	If[$CloudConnected,
		databinAddRequestAndCallback[as]
		,
		Message[Databin::cloudc];
		Throw[$Failed]
	]
)/;!$CloudConnected&&!FreeQ[as,_Image]&&ByteCount[as]>$UncompressedImageLimit


databinAddRequestAndCallback[as_]:={
	{
		URLBuild[{$CloudBase, "/app/databins", Lookup[as, "Bin"], "entries"}],
		"Parameters"->Join[{"SourceType"->"Connected Wolfram Language","ClientVersion"->$datadropclientversion},
			preparedata[Normal[as]]],"Method"->"POST", 
			"VerifyPeer" -> False,"CredentialsProvider" -> None}
		,
		(importResults["JSON"][checkAvailable[#,"Add"]]&)
}/;javaCompatibleQ[as]


databinAddRequestAndCallback[as_]:={
	{gatewayapi,
		"Parameters"->Join[{"API"->"Add","SourceType"->"\"Connected Wolfram Language\"","InputStrings"->"True",
			"ClientVersion"->ToString[$datadropclientversion,InputForm]},
			preparedata[Normal[as]]],"Method"->"POST", 
			"VerifyPeer" -> False,"CredentialsProvider" -> None}
		,
      	(importResults["Add"][checkAvailable[#,"Add"]]&)
}/;ByteCount[as]<$UncompressedImageLimit

callbackfun[fun_,{_,"data",{bytes_}}]:=(fun[{200,bytes}])
callbackfun[_,code:{_,"statuscode",Except[{200}|200]}]:=Message[Databin::asyncf]



End[] (* End Private Context *)

EndPackage[]