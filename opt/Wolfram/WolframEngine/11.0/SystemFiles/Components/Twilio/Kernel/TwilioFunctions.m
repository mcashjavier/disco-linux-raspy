(* Created with the Wolfram Language : www.wolfram.com *)

BeginPackage["TwilioFunctions`"]

TImport::usage = "";
TGetCloudXML::usage = "";
TFormatRule::usage = "";

Begin["`Private`"]
TFormatRule[rule_Rule] :=
    StringJoin[
      Function[word, 
        StringReplacePart[word, ToUpperCase[StringTake[word, 1]], 1]] /@
        StringSplit[
        StringReplace[
         rule[[1]], {
        "img" -> "Image", 
        "url" -> "URL",
        "uri" -> "URI",
          "api" -> "API",
        "sms" -> "SMS", 
              "mms" -> "MMS"
      }], "_"]] -> rule[[2]];
TImport[json_String] :=
    Module[ {res = ImportString[json, "JSON"],status},
        If[ res===$Failed,
            Throw[$Failed]
        ];
        status = "status"/.res;
        If[ MatchQ[status,400],
            (Message[ServiceExecute::apierr,"message"/.res];
             Throw[$Failed]),
            Map[If[ Length[#] > 0 && Head[#[[1]]] === Rule,
                    Association[
                     TFormatRule /@ #],
                    #
                ] &,
            Association[TFormatRule /@ (res /. {
              ("date_updated" -> val_ /; val =!= Null) :> ("date_updated" -> DateObject[First[StringSplit[val, " +"]]]),
              ("date_created" -> val_ /; val =!= Null) :> ("date_created" -> DateObject[First[StringSplit[val, " +"]]]),
              ("date_sent" -> val_ /; val =!= Null) :> ("date_sent" -> DateObject[First[StringSplit[val, " +"]]])
              })],
            -2]
        ]
    ];
TGetCloudXML[url_, body_: ""] :=
    CloudDeploy[
        ExportForm[
            ImportString[StringTemplate["<?xml version=\"1.0\" encoding=\"UTF-8\" ?><Response><Say>``</Say><Play>``</Play></Response>"][body,url],"XML"],"XML"
        ],Permissions->"Public"
    ][[1]];

End[]

EndPackage[]