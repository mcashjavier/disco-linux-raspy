(* Created with the Wolfram Language : www.wolfram.com *)

BeginPackage["BingSearchFunctions`"];

BSSearchParameter::usage = "";
BSGetType::usage = "";
(*BSSearchMarket::usage = "";
BSSearchContentFiltering::usage = "";
BSSearchFileType::usage = "";*)

BSBooleanParse::usage = "";
BSFileTypeParse::usage = "";
BSSiteParse::usage = "";

BSadultMap::usage = "";
BSfiletypeMap::usage = "";
BSmarketMap::usage = "";

BSParseHTMLErrors::usage = "";

Begin["`Private`"];

BSGetType[name_] :=
    Switch[name,
        "Image"|"Images"|"Picture"|"Pictures","Image",
        "Video"|"Videos"|"Clips","Video",
        "News"|"RelatedSearch"|"SpellingSuggestions"|"Web",name,
        __, Missing["NotAvailable"]    
    ]

BSBooleanParse[e_] :=
    e //. {Verbatim[Alternatives][x_] :> x, 
      Verbatim[Alternatives][x_, y__] :> 
       "" ~~ x ~~ " OR " ~~ Alternatives[y] ~~ "", 
      Verbatim[Except][x_] :> "-" ~~ x, List[x_] :> x, 
      List[x_, y__] :> "" ~~ x ~~ " AND " ~~ List[y] ~~ ""}

BSSiteParse[e_] :=
    Module[ {tmp},
        (
        tmp = e /. {Verbatim[Except][Verbatim[Alternatives][x___]] :> 
        List[Sequence @@ Except /@ List[x]], 
        Verbatim[Except][List[x___]] :> 
        Alternatives[Sequence @@ Except /@ List[x]]};
        tmp = BSBooleanParse[tmp];
        tmp = StringJoin[
          Riffle[(If[ ! MemberQ[{"AND", "OR", "("}, #],
                      If[ StringMatchQ[#, "-" ~~ ___],
                          "-site:" ~~ StringDrop[#, 1],
                          "site:" ~~ #
                      ],
                      #
                  ]) & /@ 
            Flatten[(If[ StringMatchQ[#, "(" ~~ ___],
                         {"(", 
                         StringDrop[#, 1]},
                         #
                     ] & /@ StringSplit[tmp])], " "]];
        StringReplace[tmp, {"( " -> "", ")" -> ""}]
        )
    ]
   
BSFileTypeParse[e_] :=
    Module[ {tmp},
        (
        tmp = e /. {Verbatim[Except][Verbatim[Alternatives][x___]] :> 
        List[Sequence @@ Except /@ List[x]], 
        Verbatim[Except][List[x___]] :> 
        Alternatives[Sequence @@ Except /@ List[x]]};
        tmp = tmp /. Entity["FileFormat", x_] :> StringReplace[EntityValue[Entity["FileFormat", x], "Extension"], "." -> ""];
        tmp = BSBooleanParse[tmp];
        tmp = StringReplace[tmp, "." -> ""];
        tmp = StringJoin[
          Riffle[(If[ ! MemberQ[{"AND", "OR", "("}, #],
                      If[ StringMatchQ[#, "-" ~~ ___],
                          "-filetype:" ~~ StringDrop[#, 1],
                          "filetype:" ~~ #
                      ],
                      #
                  ]) & /@ 
            Flatten[(If[ StringMatchQ[#, "(" ~~ ___],
                         {"(", 
                         StringDrop[#, 1]},
                         #
                     ] & /@ StringSplit[tmp])], " "]];
        StringReplace[tmp, {"( " -> "", ")" -> ""}]
        )
    ]
    
BSSearchParameter[parameter_,typeMap_] :=
    (If[ KeyExistsQ[typeMap, parameter],
         parameter /. typeMap,
         Missing["NotAvailable"]
     ])
         
BSadultMap = {
    "Off" -> "'Off'", 
    "off" -> "'Off'", 
    "Medium" -> "'Moderate'", 
    "medium" -> "'Moderate'", 
    "High" -> "'Strict'", 
    "high" -> "'Strict'"
    };
    
BSfiletypeMap =
    {
         Entity["FileFormat", "DOC-1"] -> "'DOC'", 
         Entity["FileFormat", "DWF-1"] -> "'DWF'", 
         Entity["FileFormat", "RSS-1"] -> "'FEED'", 
         Entity["FileFormat", "HTML-1"] -> "'HTM'", 
         Entity["FileFormat", "HTML-1"] -> "'HTML'", 
         Entity["FileFormat", "PDF-1"] -> "'PDF'", 
         Entity["FileFormat", "PPT-1"] -> "'PPT'", 
         Entity["FileFormat", "RTF-1"] -> "'RTF'", 
         Entity["FileFormat", "TXT-1"] -> "'TEXT'", 
         Entity["FileFormat", "TXT-1"] -> "'TXT'", 
         Entity["FileFormat", "XLS-1"] -> "'XLS'"
     }
     
BSmarketMap = 
    {
      {Entity["Language", "Arabic"], Entity["Country", "SaudiArabia"]} -> "'ar-XA'", 
      {Entity["Language", "Bulgarian"], Entity["Country", "Bulgaria"]} -> "'bg-BG'", 
      {Entity["Language", "Czech"], Entity["Country", "CzechRepublic"]} -> "'cs-CZ'", 
      {Entity["Language", "Danish"], Entity["Country", "Denmark"]} -> "'da-DK'", 
      {Entity["Language", "German"], Entity["Country", "Austria"]} -> "'de-AT'", 
      {Entity["Language", "German"], Entity["Country", "Switzerland"]} -> "'de-CH'", 
      {Entity["Language", "German"], Entity["Country", "Germany"]} -> "'de-DE'", 
      {Entity["Language", "Greek"], Entity["Country", "Greece"]} -> "'el-GR'", 
      {Entity["Language", "English"], Entity["Country", "Australia"]} -> "'en-AU'", 
      {Entity["Language", "English"], Entity["Country", "Canada"]} -> "'en-CA'", 
      {Entity["Language", "English"], Entity["Country", "UnitedKingdom"]} -> "'en-GB'", 
      {Entity["Language", "English"], Entity["Country", "Indonesia"]} -> "'en-ID'",
      {Entity["Language", "English"], Entity["Country", "Ireland"]} -> "'en-IE'", 
      {Entity["Language", "English"], Entity["Country", "India"]} -> "'en-IN'",
      {Entity["Language", "English"], Entity["Country", "Malaysia"]} -> "'en-MY'",
      {Entity["Language", "English"], Entity["Country", "NewZealand"]} -> "'en-NZ'", 
      {Entity["Language", "English"], Entity["Country", "Philippines"]} -> "'en-PH'", 
      {Entity["Language", "English"], Entity["Country", "Singapore"]} -> "'en-SG'", 
      {Entity["Language", "English"], Entity["Country", "UnitedStates"]} -> "'en-US'",
      {Entity["Language", "English"], Entity["Country", "SaudiArabia"]} -> "'en-XA'", 
      {Entity["Language", "English"], Entity["Country", "SouthAfrica"]} -> "'en-ZA'", 
      {Entity["Language", "Spanish"], Entity["Country", "Argentina"]} -> "'es-AR'", 
      {Entity["Language", "Spanish"], Entity["Country", "Chile"]} -> "'es-CL'", 
      {Entity["Language", "Spanish"], Entity["Country", "Spain"]} -> "'es-ES'", 
      {Entity["Language", "Spanish"], Entity["Country", "Mexico"]} -> "'es-MX'", 
      {Entity["Language", "Spanish"], Entity["Country", "UnitedStates"]} -> "'es-US'", 
      {Entity["Language", "Estonian"], Entity["Country", "Estonia"]} -> "'et-EE'", 
      {Entity["Language", "Finnish"], Entity["Country", "Finland"]} -> "'fi-FI'", 
      {Entity["Language", "French"], Entity["Country", "Belgium"]} -> "'fr-BE'", 
      {Entity["Language", "French"], Entity["Country", "Canada"]} -> "'fr-CA'", 
      {Entity["Language", "French"], Entity["Country", "Switzerland"]} -> "'fr-CH'", 
      {Entity["Language", "French"], Entity["Country", "France"]} -> "'fr-FR'", 
      {Entity["Language", "Hebrew"], Entity["Country", "Israel"]} -> "'he-IL'", 
      {Entity["Language", "Croatian"], Entity["Country", "Croatia"]} -> "'hr-HR'", 
      {Entity["Language", "Hungarian"], Entity["Country", "Hungary"]} -> "'hu-HU'", 
      {Entity["Language", "Italian"], Entity["Country", "Italy"]} -> "'it-IT'", 
      {Entity["Language", "Japanese"], Entity["Country", "Japan"]} -> "'ja-JP'", 
      {Entity["Language", "Korean"], Entity["Country", "SouthKorea"]} -> "'ko-KR'", 
      {Entity["Language", "Lithuanian"], Entity["Country", "Lithuania"]} -> "'lt-LT'", 
      {Entity["Language", "Latvian"], Entity["Country", "Latvia"]} -> "'lv-LV'", 
      {Entity["Language", "Norwegian"], Entity["Country", "Norway"]} -> "'nb-NO'", 
      {Entity["Language", "Dutch"], Entity["Country", "Belgium"]} -> "'nl-BE'", 
      {Entity["Language", "Dutch"], Entity["Country", "Netherlands"]} -> "'nl-NL'", 
      {Entity["Language", "Polish"], Entity["Country", "Poland"]} -> "'pl-PL'", 
      {Entity["Language", "Portuguese"], Entity["Country", "Brazil"]} -> "'pt-BR'", 
      {Entity["Language", "Portuguese"], Entity["Country", "Portugal"]} -> "'pt-PT'", 
      {Entity["Language", "Romanian"], Entity["Country", "Romania"]} -> "'ro-RO'", 
      {Entity["Language", "Russian"], Entity["Country", "Russia"]} -> "'ru-RU'", 
      {Entity["Language", "Slovak"], Entity["Country", "Slovakia"]} -> "'sk-SK'", 
      {Entity["Language", "Slovenian"], Entity["Country", "Slovenia"]} -> "'sl-SL'", 
      {Entity["Language", "Swedish"], Entity["Country", "Sweden"]} -> "'sv-SE'", 
      {Entity["Language", "Thai"], Entity["Country", "Thailand"]} -> "'th-TH'", 
      {Entity["Language", "Turkish"], Entity["Country", "Turkey"]} -> "'tr-TR'", 
      {Entity["Language", "Ukrainian"], Entity["Country", "Ukraine"]} -> "'uk-UA'", 
      {Entity["Language", "ChineseMandarin"], Entity["Country", "China"]} -> "'zh-CN'", 
      {Entity["Language", "ChineseMandarin"], Entity["Country", "HongKong"]} -> "'zh-HK'", 
      {Entity["Language", "ChineseMandarin"], Entity["Country", "Taiwan"]} -> "'zh-TW'"}

BSParseHTMLErrors[stringHTML_String] :=
    First[StringCases[
    StringTrim[
    StringDelete[ImportString[stringHTML, "HTML"], {"\n", "\\n"}]], ___ ~~ 
    "Server Error  " ~~ x__ :> x]]
     
End[];
  
EndPackage[];