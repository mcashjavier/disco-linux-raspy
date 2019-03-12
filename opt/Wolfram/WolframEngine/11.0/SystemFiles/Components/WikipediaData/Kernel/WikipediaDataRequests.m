(* ::Package:: *)

(* Mathematica Package *)

BeginPackage["WikipediaData`"]
(* Exported symbols added here with SymbolName::usage *)  

System`WikipediaData

Begin["`Private`"] (* Begin Private Context *) 

Unprotect[WikipediaData]

Options[WikipediaData] = {Language -> Automatic, "TargetLanguage" -> Automatic, "MaxLevelItems" -> Automatic, 
	"MaxItems" -> Automatic, MaxItems -> Automatic, "MaxLevel" -> Automatic, "Section" -> Automatic, "StartDate" -> Automatic, "EndDate" -> Automatic, "Geodisk"->Automatic}

supportedlangrules = {"abkhazian" -> "ab", "acehnese" -> "ace", 
   "afrikaans" -> "af", "akan" -> "ak", "albanian" -> "sq", 
   "alemannic" -> "als", "amharic" -> "am", "anglo-saxon" -> "ang", 
   "arabic" -> "ar", "aragonese" -> "an", "aramaic" -> "arc", 
   "armenian" -> "hy", "aromanian" -> "roa-rup", "assamese" -> "as", 
   "asturian" -> "ast", "avar" -> "av", "aymara" -> "ay", 
   "azerbaijani" -> "az", "bambara" -> "bm", "banjar" -> "bjn", 
   "banyumasan" -> "map-bms", "bashkir" -> "ba", "basque" -> "eu", 
   "bavarian" -> "bar", "belarusian" -> "be", 
   "belarusian (tara\[SHacek]kievica)" -> "be-x-old", "bengali" -> "bn", 
   "bihari" -> "bh", "bishnupriya manipuri" -> "bpy", 
   "bislama" -> "bi", "bosnian" -> "bs", "breton" -> "br", 
   "buginese" -> "bug", "bulgarian" -> "bg", "burmese" -> "my", 
   "buryat" -> "bxr", "cantonese" -> "zh-yue", "catalan" -> "ca", 
   "cebuano" -> "ceb", "central bicolano" -> "bcl", 
   "chamorro" -> "ch", "chavacano" -> "cbk-zam", "chechen" -> "ce", 
   "cherokee" -> "chr", "cheyenne" -> "chy", "chichewa" -> "ny", 
   "chinese" -> "zh", "chuvash" -> "cv", 
   "classical chinese" -> "zh-classical", "cornish" -> "kw", 
   "corsican" -> "co", "cree" -> "cr", "crimean tatar" -> "crh", 
   "croatian" -> "hr", "czech" -> "cs", "danish" -> "da", 
   "divehi" -> "dv", "dutch" -> "nl", "dutch low saxon" -> "nds-nl", 
   "dzongkha" -> "dz", "egyptian arabic" -> "arz", 
   "emilian-romagnol" -> "eml", "english" -> "en", "erzya" -> "myv", 
   "esperanto" -> "eo", "estonian" -> "et", "ewe" -> "ee", 
   "extremaduran" -> "ext", "faroese" -> "fo", "fijian" -> "fj", 
   "fiji hindi" -> "hif", "finnish" -> "fi", 
   "franco-proven\[CCedilla]al" -> "frp", "french" -> "fr", "friulian" -> "fur",
   "fula" -> "ff", "gagauz" -> "gag", "galician" -> "gl", 
   "gan" -> "gan", "georgian" -> "ka", "german" -> "de", 
   "gilaki" -> "glk", "goan konkani" -> "gom", "gothic" -> "got", 
   "greek" -> "el", "greenlandic" -> "kl", "guarani" -> "gn", 
   "gujarati" -> "gu", "haitian" -> "ht", "hakka" -> "hak", 
   "hausa" -> "ha", "hawaiian" -> "haw", "hebrew" -> "he", 
   "hill mari" -> "mrj", "hindi" -> "hi", "hungarian" -> "hu", 
   "icelandic" -> "is", "ido" -> "io", "igbo" -> "ig", 
   "ilokano" -> "ilo", "indonesian" -> "id", "interlingua" -> "ia", 
   "interlingue" -> "ie", "inuktitut" -> "iu", "inupiak" -> "ik", 
   "irish" -> "ga", "italian" -> "it", "japanese" -> "ja", 
   "javanese" -> "jv", "kabardian" -> "kbd", "kabyle" -> "kab", 
   "kalmyk" -> "xal", "kannada" -> "kn", "kapampangan" -> "pam", 
   "karachay-balkar" -> "krc", "karakalpak" -> "kaa", 
   "kashmiri" -> "ks", "kashubian" -> "csb", "kazakh" -> "kk", 
   "khmer" -> "km", "kikuyu" -> "ki", "kinyarwanda" -> "rw", 
   "kirghiz" -> "ky", "kirundi" -> "rn", "komi" -> "kv", 
   "komi-permyak" -> "koi", "kongo" -> "kg", "korean" -> "ko", 
   "kurdish" -> "ku", "ladino" -> "lad", "lak" -> "lbe", 
   "lao" -> "lo", "latgalian" -> "ltg", "latin" -> "la", 
   "latvian" -> "lv", "lezgian" -> "lez", "ligurian" -> "lij", 
   "limburgish" -> "li", "lingala" -> "ln", "lithuanian" -> "lt", 
   "lojban" -> "jbo", "lombard" -> "lmo", "lower sorbian" -> "dsb", 
   "low saxon" -> "nds", "luganda" -> "lg", "luxembourgish" -> "lb", 
   "macedonian" -> "mk", "maithili" -> "mai", "malagasy" -> "mg", 
   "malay" -> "ms", "malayalam" -> "ml", "maltese" -> "mt", 
   "manx" -> "gv", "maori" -> "mi", "marathi" -> "mr", 
   "mazandarani" -> "mzn", "meadow mari" -> "mhr", 
   "minangkabau" -> "min", "min dong" -> "cdo", "mingrelian" -> "xmf",
   "min nan" -> "zh-min-nan", "mirandese" -> "mwl", 
   "moksha" -> "mdf", "mongolian" -> "mn", "nahuatl" -> "nah", 
   "nauruan" -> "na", "navajo" -> "nv", "neapolitan" -> "nap", 
   "nepali" -> "ne", "newar" -> "new", "norfolk" -> "pih", 
   "norman" -> "nrm", "northern luri" -> "lrc", 
   "northern sami" -> "se", "northern sotho" -> "nso", 
   "north frisian" -> "frr", "norwegian (bokm\[ARing]l)" -> "no", 
   "norwegian (nynorsk)" -> "nn", "novial" -> "nov", 
   "occitan" -> "oc", "old church slavonic" -> "cu", "oriya" -> "or", 
   "oromo" -> "om", "ossetian" -> "os", "palatinate german" -> "pfl", 
   "pali" -> "pi", "pangasinan" -> "pag", "papiamentu" -> "pap", 
   "pashto" -> "ps", "pennsylvania german" -> "pdc", 
   "persian" -> "fa", "picard" -> "pcd", "piedmontese" -> "pms", 
   "polish" -> "pl", "pontic" -> "pnt", "portuguese" -> "pt", 
   "punjabi" -> "pa", "quechua" -> "qu", "ripuarian" -> "ksh", 
   "romani" -> "rmy", "romanian" -> "ro", "romansh" -> "rm", 
   "russian" -> "ru", "rusyn" -> "rue", "sakha" -> "sah", 
   "samoan" -> "sm", "samogitian" -> "bat-smg", "sango" -> "sg", 
   "sanskrit" -> "sa", "sardinian" -> "sc", 
   "saterland frisian" -> "stq", "scots" -> "sco", 
   "scottish gaelic" -> "gd", "serbian" -> "sr", 
   "serbo-croatian" -> "sh", "sesotho" -> "st", "shona" -> "sn", 
   "sicilian" -> "scn", "silesian" -> "szl", 
   "simple english" -> "simple", "sindhi" -> "sd", 
   "sinhalese" -> "si", "slovak" -> "sk", "slovenian" -> "sl", 
   "somali" -> "so", "sorani" -> "ckb", 
   "southern azerbaijani" -> "azb", "spanish" -> "es", 
   "sranan" -> "srn", "sundanese" -> "su", "swahili" -> "sw", 
   "swati" -> "ss", "swedish" -> "sv", "tagalog" -> "tl", 
   "tahitian" -> "ty", "tajik" -> "tg", "tamil" -> "ta", 
   "tarantino" -> "roa-tara", "tatar" -> "tt", "telugu" -> "te", 
   "tetum" -> "tet", "thai" -> "th", "tibetan" -> "bo", 
   "tigrinya" -> "ti", "tok pisin" -> "tpi", "tongan" -> "to", 
   "tsonga" -> "ts", "tswana" -> "tn", "tumbuka" -> "tum", 
   "turkish" -> "tr", "turkmen" -> "tk", "tuvan" -> "tyv", 
   "twi" -> "tw", "udmurt" -> "udm", "ukrainian" -> "uk", 
   "upper sorbian" -> "hsb", "urdu" -> "ur", "uyghur" -> "ug", 
   "uzbek" -> "uz", "venda" -> "ve", "venetian" -> "vec", 
   "vepsian" -> "vep", "vietnamese" -> "vi", "volap\[UDoubleDot]k" -> "vo", 
   "v\[OTilde]ro" -> "fiu-vro",
   "walloon" -> "wa", "waray-waray" -> "war", "welsh" -> "cy", 
   "western punjabi" -> "pnb", "west flemish" -> "vls", 
   "west frisian" -> "fy", "wolof" -> "wo", "wu" -> "wuu", 
   "xhosa" -> "xh", "yiddish" -> "yi", "yoruba" -> "yo", 
   "zazaki" -> "diq", "zeelandic" -> "zea", "zhuang" -> "za", 
   "zulu" -> "zu"};

(***********************************************************************************)

WikipediaData[HoldPattern[Rule["PageID", pageid_]], "SummaryPlaintext", (opt:OptionsPattern[]|{opt:OptionsPattern[]})] := Module[{language, targetLanguage},
	language = OptionValue[Language] /. Automatic -> "English";
	targetLanguage = OptionValue["TargetLanguage"] /. Automatic -> language;
	wikipedia["SummaryPlaintext", {"PageID" -> pageid, Language -> language, "TargetLanguage" -> targetLanguage}]
];

WikipediaData[title_/; !MatchQ[title, Rule["PageID", _]], "SummaryPlaintext", (opt:OptionsPattern[]|{opt:OptionsPattern[]})] := Module[{language, targetLanguage},
	language = OptionValue[Language] /. Automatic -> "English";
	targetLanguage = OptionValue["TargetLanguage"] /. Automatic -> language;
	wikipedia["SummaryPlaintext", {"Title" -> title, Language -> language, "TargetLanguage" -> targetLanguage}]
];

WikipediaData[title_/; !MatchQ[title, Rule["PageID", _]], "ArticlePlaintext", (opt:OptionsPattern[]|{opt:OptionsPattern[]})] := Module[{language, targetLanguage},
	language = OptionValue[Language] /. Automatic -> "English";
	targetLanguage = OptionValue["TargetLanguage"] /. Automatic -> language;
	wikipedia["ArticlePlaintext", {"Title" -> title, Language -> language, "TargetLanguage" -> targetLanguage}]
];

WikipediaData[HoldPattern[Rule["Title", title_]], "ArticlePlaintext", opt:OptionsPattern[]] := WikipediaData[title, "ArticlePlaintext", opt];

WikipediaData[HoldPattern[Rule["PageID", pageid_]], "ArticlePlaintext", opt:OptionsPattern[]] := Module[{language, targetLanguage},
	language = OptionValue[Language] /. Automatic -> "English";
	targetLanguage = OptionValue["TargetLanguage"] /. Automatic -> language;
	wikipedia["ArticlePlaintext", {"PageID" -> pageid, Language -> language, "TargetLanguage" -> targetLanguage}]
];

WikipediaData[title_/; !MatchQ[title, "GeoNearbyArticles"], opt:OptionsPattern[]] := WikipediaData[title, "ArticlePlaintext", opt];

WikipediaData[HoldPattern[Rule["PageID", pageid_]], opt:OptionsPattern[]] := WikipediaData[Rule["PageID", pageid], "ArticlePlaintext", opt];

WikipediaData[HoldPattern[Rule["Category", category_String]]] := wikipedia["CategoryArticles", {"Category" -> category}];

(***********************************************************************************)

WikipediaData[arg_,"ArticleContributors",opt:OptionsPattern[]] := Module[{result,limit,title,pageid},
	limit=OptionValue["MaxItems"]/.Automatic->100;
	title=arg;
	pageid=If[MatchQ[arg,Rule["PageID",_]],"PageID"/.arg,Missing["NotAvailable"]];

	Which[
		MatchQ[pageid,_Missing],
		result=wikipedia["ArticleContributors",{"Title"->title,"MaxItems"->limit}],
		!MatchQ[pageid,_Missing],
		result=wikipedia["ArticleContributors",{"PageID"->pageid,"MaxItems"->limit}],
		True,
		result=Missing["BadInput"]
	];

	result
];

WikipediaData[search_,"ArticleOpenSearch",opt:OptionsPattern[]] := Module[{result,limit},
	limit=OptionValue["MaxItems"]/.Automatic->100;
	If[MatchQ[search,Rule["Search",_String]],
		result=wikipedia["ArticleOpenSearch",{search,"MaxItems"->limit}],
		result=Missing["BadInput"]
	];
	result
];

(***************************************************************************)

(***************************************************************************)

WikipediaData[arg_,"ArticleWikicode"] := Module[{result,title,pageid},
	title=arg;
	pageid=If[MatchQ[arg,Rule["PageID",_]],"PageID"/.arg,Missing["NotAvailable"]];

	Which[
		MatchQ[pageid,_Missing],
		result=wikipedia["ArticleWikicode",{"Title"->title}],
		!MatchQ[pageid,_Missing],
		result=wikipedia["ArticleWikicode",{"PageID"->pageid}],
		True,
		result=Missing["BadInput"]
	];

	result
];

WikipediaData[arg_,"BacklinksRules",opt:OptionsPattern[]] := Module[{result,limit,level,title,pageid},
	limit=OptionValue["MaxLevelItems"]/.Automatic->Missing["NotAvailable"];
	level=OptionValue["MaxLevel"]/.Automatic->1;

	title=arg;
	pageid=If[MatchQ[arg,Rule["PageID",_]],"PageID"/.arg,Missing["NotAvailable"]];

	Which[
		MatchQ[pageid,_Missing],
		result=wikipedia["BacklinksRules",{"Title"->title,"MaxLevelItems"->limit,"MaxLevel"->level}],
		!MatchQ[pageid,_Missing],
		result=wikipedia["BacklinksRules",{"PageID"->pageid,"MaxLevelItems"->limit,"MaxLevel"->level}],
		True,
		result=Missing["BadInput"]
	];

	result
];

WikipediaData[arg_,"BacklinksList",opt:OptionsPattern[]] := Module[{result,limit,level,title,pageid},
	limit=OptionValue["MaxLevelItems"]/.Automatic->Missing["NotAvailable"];
	level=OptionValue["MaxLevel"]/.Automatic->1;

	title=arg;
	pageid=If[MatchQ[arg,Rule["PageID",_]],"PageID"/.arg,Missing["NotAvailable"]];

	Which[
		MatchQ[pageid,_Missing],
		result=wikipedia["BacklinksList",{"Title"->title,"MaxLevelItems"->limit,"MaxLevel"->level}],
		!MatchQ[pageid,_Missing],
		result=wikipedia["BacklinksLists",{"PageID"->pageid,"MaxLevelItems"->limit,"MaxLevel"->level}],
		True,
		result=Missing["BadInput"]
	];

	result
];

WikipediaData[arg_,"CategoryArticles",opt:OptionsPattern[]] := Module[{result,limit},
	If[MatchQ[arg,Rule["Category",_String]]||MatchQ[arg,Rule["Category",{__String}]],
		limit=OptionValue["MaxItems"]/.Automatic->100;
		result=wikipedia["CategoryArticles",{arg,"MaxItems"->limit}],
		result=Missing["BadInput"]
	];

	result
];

WikipediaData[arg_,"CategoryArticleIDs",opt:OptionsPattern[]] := Module[{result,limit},
	If[MatchQ[arg,Rule["Category",_String]]||MatchQ[arg,Rule["Category",{__String}]],
		limit=OptionValue["MaxItems"]/.Automatic->All;
		result=wikipedia["CategoryArticleIDs",{arg,"MaxItems"->limit}],
		result=Missing["BadInput"]
	];

	result
];

WikipediaData[arg_,"CategoryLinks",opt:OptionsPattern[]] := Module[{result,level},
	If[MatchQ[arg,Rule["Category",_String]]||MatchQ[arg,Rule["Category",{__String}]],
		level=OptionValue["MaxLevel"]/.Automatic->2;
		result=wikipedia["CategoryLinks",{arg,"MaxLevel"->level}],
		result=Missing["BadInput"]
	];

	result
];

WikipediaData[arg_,"CategoryMembers",opt:OptionsPattern[]] := Module[{result,limit},
	If[MatchQ[arg,Rule["Category",_String]]||MatchQ[arg,Rule["Category",{__String}]],
		limit=OptionValue["MaxItems"]/.Automatic->100;
		result=wikipedia["CategoryMembers",{arg,"MaxItems"->limit}],
		result=Missing["BadInput"]
	];

	result
];

WikipediaData[arg_,"CategoryMemberIDs",opt:OptionsPattern[]] := Module[{result,limit},
	If[MatchQ[arg,Rule["Category",_String]]||MatchQ[arg,Rule["Category",{__String}]],
		limit=OptionValue["MaxItems"]/.Automatic->100;
		result=wikipedia["CategoryMemberIDs",{arg,"MaxItems"->limit}],
		result=Missing["BadInput"]
	];

	result
];

WikipediaData[arg_,"CategorySearch",opt:OptionsPattern[]] := Module[{result,limit},
	If[MatchQ[arg,Rule["Search",_String]],
		limit=OptionValue["MaxItems"]/.Automatic->100;
		result=wikipedia["CategorySearch",{arg,"MaxItems"->limit}],
		result=Missing["BadInput"]
	];

	result
];

WikipediaData[arg_,"ContentSearch",opt:OptionsPattern[]] := Module[{result,limit},
	If[MatchQ[arg,Rule["Content",_String]]||MatchQ[arg,Rule["Content",{__String}]],
		limit=OptionValue["MaxItems"]/.Automatic->100;
		result=wikipedia["ContentSearch",{arg,"MaxItems"->limit}],
		result=Missing["BadInput"]
	];

	result
];

WikipediaData[arg_,"ContributorArticles",opt:OptionsPattern[]] := Module[{result,limit},
	If[MatchQ[arg,Rule["Contributor",_String]],
		limit=OptionValue["MaxItems"]/.Automatic->100;
		result=wikipedia["ContributorArticles",{arg,"MaxItems"->limit}],
		result=Missing["BadInput"]
	];

	result
];

WikipediaData[arg_,"ExternalLinks"] := Module[{result,title,pageid},
	title=arg;
	pageid=If[MatchQ[arg,Rule["PageID",_]],"PageID"/.arg,Missing["NotAvailable"]];

	Which[
		MatchQ[pageid,_Missing],
		result=wikipedia["ExternalLinks",{"Title"->title}],
		!MatchQ[pageid,_Missing],
		result=wikipedia["ExternalLinks",{"PageID"->pageid}],
		True,
		result=Missing["BadInput"]
	];

	result
];

(***********************************************************************************)

WikipediaData[arg_,"GeoPosition"] := Module[{result,title,pageid},
	title=arg;
	pageid=If[MatchQ[arg,Rule["PageID",_]],"PageID"/.arg,Missing["NotAvailable"]];

	Which[
		MatchQ[pageid,_Missing],
		result=wikipedia["GeoPosition",{"Title"->title}],
		!MatchQ[pageid,_Missing],
		result=wikipedia["GeoPosition",{"PageID"->pageid}],
		True,
		result=Missing["BadInput"]
	];

	result
];

WikipediaData[title_String,"GeoNearbyArticles",opt:OptionsPattern[]] := Module[{},
	wikipedia["GeoNearbyArticles",{"Title"->title,opt}]
];

WikipediaData["GeoNearbyArticles",opt_List] := Module[{},
	wikipedia["GeoNearbyArticles",opt]
];

WikipediaData["GeoNearbyArticles",opt__Rule] := Module[{},
	wikipedia["GeoNearbyArticles",{opt}]
];

WikipediaData[title_String,"GeoNearbyDataset",{opt__Rule}] := wikipedia["GeoNearbyDataset",{"Title"->title,opt}];

WikipediaData[title_String,"GeoNearbyDataset",opt:OptionsPattern[]] := Module[{},
	wikipedia["GeoNearbyDataset",{"Title"->title,opt}]
];

WikipediaData["GeoNearbyDataset",opt__] := Module[{},
	wikipedia["GeoNearbyDataset",{opt}]
];

(*
WikipediaData[title_String,"GeoNearbyDataset",opt_List] := Module[{options,result},
	result={};
	options={"Title"->title};
	options=Join[options,opt];
	result=wikipedia["GeoNearbyArticles",options];
	If[MatchQ[result,{__}],
		result=Dataset[Association@@@result]
	];
	result
];

WikipediaData["GeoNearbyDataset",opt_List] := Module[{result},
	result=wikipedia["GeoNearbyArticles",opt];
	If[MatchQ[result,{__}],
		result=Dataset[Association@@@result]
	];
	result
];
*)

(***********************************************************************************)

WikipediaData[arg_,"ImageDataset",opt:OptionsPattern[]] := Module[{result,title,pageid},
	title=arg;
	pageid=If[MatchQ[arg,Rule["PageID",_]],"PageID"/.arg,Missing["NotAvailable"]];

	Which[
		MatchQ[pageid,_Missing],
		result=wikipedia["ImageDataset",{"Title"->title,opt}],
		!MatchQ[pageid,_Missing],
		result=wikipedia["ImageDataset",{"PageID"->pageid,opt}],
		True,
		result=Missing["BadInput"]
	];

	result
];

WikipediaData[arg_,"ImageList"] := Module[{result,title,pageid},
	title=arg;
	pageid=If[MatchQ[arg,Rule["PageID",_]],"PageID"/.arg,Missing["NotAvailable"]];

	Which[
		MatchQ[pageid,_Missing],
		result=wikipedia["ImageList",{"Title"->title}],
		!MatchQ[pageid,_Missing],
		result=wikipedia["ImageList",{"PageID"->pageid}],
		True,
		result=Missing["BadInput"]
	];

	result
];

WikipediaData[arg_,"ImageURLs"] := Module[{result,title,pageid},
	title=arg;
	pageid=If[MatchQ[arg,Rule["PageID",_]],"PageID"/.arg,Missing["NotAvailable"]];

	Which[
		MatchQ[pageid,_Missing],
		result=wikipedia["ImageURLs",{"Title"->title}],
		!MatchQ[pageid,_Missing],
		result=wikipedia["ImageURLs",{"PageID"->pageid}],
		True,
		result=Missing["BadInput"]
	];

	result
];

(***********************************************************************************)

WikipediaData[arg_,"LanguagesURLRules"] := Module[{result,title,pageid},
	title=arg;
	pageid=If[MatchQ[arg,Rule["PageID",_]],"PageID"/.arg,Missing["NotAvailable"]];

	Which[
		MatchQ[pageid,_Missing],
		result=wikipedia["LanguagesURLRules",{"Title"->title}],
		!MatchQ[pageid,_Missing],
		result=wikipedia["LanguagesURLRules",{"PageID"->pageid}],
		True,
		result=Missing["BadInput"]
	];

	result
];

WikipediaData[arg_,"LanguagesList"] := Module[{result,title,pageid},
	title=arg;
	pageid=If[MatchQ[arg,Rule["PageID",_]],"PageID"/.arg,Missing["NotAvailable"]];

	Which[
		MatchQ[pageid,_Missing],
		result=wikipedia["LanguagesList",{"Title"->title}],
		!MatchQ[pageid,_Missing],
		result=wikipedia["LanguagesList",{"PageID"->pageid}],
		True,
		result=Missing["BadInput"]
	];

	result
];

WikipediaData[arg_,"LanguagesURLs"] := Module[{result,title,pageid},
	title=arg;
	pageid=If[MatchQ[arg,Rule["PageID",_]],"PageID"/.arg,Missing["NotAvailable"]];

	Which[
		MatchQ[pageid,_Missing],
		result=wikipedia["LanguagesURLs",{"Title"->title}],
		!MatchQ[pageid,_Missing],
		result=wikipedia["LanguagesURLs",{"PageID"->pageid}],
		True,
		result=Missing["BadInput"]
	];

	result
];

(***********************************************************************************)

WikipediaData[arg_,"LinksRules",opt:OptionsPattern[]] := Module[{result,limit,level,title,pageid},
	limit=OptionValue["MaxLevelItems"]/.Automatic->All;
	level=OptionValue["MaxLevel"]/.Automatic->1;

	title=arg;
	pageid=If[MatchQ[arg,Rule["PageID",_]],"PageID"/.arg,Missing["NotAvailable"]];

	Which[
		MatchQ[pageid,_Missing],
		result=wikipedia["LinksRules",{"Title"->title,"MaxLevelItems"->limit,"MaxLevel"->level}],
		!MatchQ[pageid,_Missing],
		result=wikipedia["LinksRules",{"PageID"->pageid,"MaxLevelItems"->limit,"MaxLevel"->level}],
		True,
		result=Missing["BadInput"]
	];

	result
];

WikipediaData[arg_,"LinksList",opt:OptionsPattern[]] := Module[{result,limit,level,title,pageid},
	limit=OptionValue["MaxLevelItems"]/.Automatic->All;
	level=OptionValue["MaxLevel"]/.Automatic->1;

	title=arg;
	pageid=If[MatchQ[arg,Rule["PageID",_]],"PageID"/.arg,Missing["NotAvailable"]];

	Which[
		MatchQ[pageid,_Missing],
		result=wikipedia["LinksList",{"Title"->title,"MaxLevelItems"->limit,"MaxLevel"->level}],
		!MatchQ[pageid,_Missing],
		result=wikipedia["LinksList",{"PageID"->pageid,"MaxLevelItems"->limit,"MaxLevel"->level}],
		True,
		result=Missing["BadInput"]
	];

	result
];

(***********************************************************************************)

WikipediaData[arg_,"PageID"] := Module[{result,title,pageid},
	title=arg;
	pageid=If[MatchQ[arg,Rule["PageID",_]],"PageID"/.arg,Missing["NotAvailable"]];
	
	Which[
		MatchQ[pageid,_Missing],
		result=wikipedia["PageID",{"Title"->title}],
		!MatchQ[pageid,_Missing],
		result=wikipedia["PageID",{"PageID"->pageid}],
		True,
		result=Missing["BadInput"]
	];
	
	result
];

(***********************************************************************************)

WikipediaData[arg_,"ParentCategories"] := Module[{result,title,pageid},
	title=arg;
	pageid=If[MatchQ[arg,Rule["PageID",_]],"PageID"/.arg,Missing["NotAvailable"]];

	Which[
		MatchQ[pageid,_Missing],
		result=wikipedia["ParentCategories",{"Title"->title}],
		!MatchQ[pageid,_Missing],
		result=wikipedia["ParentCategories",{"PageID"->pageid}],
		True,
		result=Missing["BadInput"]
	];

	result
];

(***********************************************************************************)

WikipediaData[arg_,"Revisions",opt:OptionsPattern[]] := Module[{result,limit,startDate,endDate,title,pageid},
	limit=OptionValue["MaxItems"]/.Automatic->10;
	startDate=OptionValue["StartDate"]/.Automatic->Now;
	If[!MatchQ[startDate,_DateObject],Throw[$Failed]];
	endDate=OptionValue["EndDate"]/.Automatic->DateObject[{2000, 1, 1}];
	If[!MatchQ[endDate,_DateObject],Throw[$Failed]];

	title=arg;
	pageid=If[MatchQ[arg,Rule["PageID",_]],"PageID"/.arg,Missing["NotAvailable"]];

	Which[
		MatchQ[pageid,_Missing],
		result=wikipedia["Revisions",{"Title"->title,"MaxItems"->limit,"StartDate"->startDate,"EndDate"->endDate}],
		!MatchQ[pageid,_Missing],
		result=wikipedia["Revisions",{"PageID"->pageid,"MaxItems"->limit,"StartDate"->startDate,"EndDate"->endDate}],
		True,
		result=Missing["BadInput"]
	];

	result
];

(***********************************************************************************)

WikipediaData[arg_,"SeeAlsoList"] := Module[{result,title,pageid},
	title=arg;
	pageid=If[MatchQ[arg,Rule["PageID",_]],"PageID"/.arg,Missing["NotAvailable"]];

	Which[
		MatchQ[pageid,_Missing],
		result=wikipedia["SeeAlsoList",{"Title"->title}],
		!MatchQ[pageid,_Missing],
		result=wikipedia["SeeAlsoList",{"PageID"->pageid}],
		True,
		result=Missing["BadInput"]
	];

	result
];

WikipediaData[arg_,"SeeAlsoRules"] := Module[{result,title,pageid},
	title=arg;
	pageid=If[MatchQ[arg,Rule["PageID",_]],"PageID"/.arg,Missing["NotAvailable"]];

	Which[
		MatchQ[pageid,_Missing],
		result=wikipedia["SeeAlsoRules",{"Title"->title}],
		!MatchQ[pageid,_Missing],
		result=wikipedia["SeeAlsoRules",{"PageID"->pageid}],
		True,
		result=Missing["BadInput"]
	];

	result
];

(***********************************************************************************)

WikipediaData[arg_,"SummaryWikicode",opt:OptionsPattern[]] := Module[{result,section,title,pageid},
	section=OptionValue["Section"]/.Automatic->0;

	title=arg;
	pageid=If[MatchQ[arg,Rule["PageID",_]],"PageID"/.arg,Missing["NotAvailable"]];

	Which[
		MatchQ[pageid,_Missing],
		result=wikipedia["SummaryWikicode",{"Title"->title,"Section"->section}],
		!MatchQ[pageid,_Missing],
		result=wikipedia["SummaryWikicode",{"PageID"->pageid,"Section"->section}],
		True,
		result=Missing["BadInput"]
	];

	result
];

(***********************************************************************************)

WikipediaData[arg_,"Tables"] := Module[{result,title,pageid},

	title=arg;
	pageid=If[MatchQ[arg,Rule["PageID",_]],"PageID"/.arg,Missing["NotAvailable"]];

	Which[
		MatchQ[pageid,_Missing],
		result=wikipedia["Tables",{"Title"->title}],
		!MatchQ[pageid,_Missing],
		result=wikipedia["Tables",{"PageID"->pageid}],
		True,
		result=Missing["BadInput"]
	];

	result
];

(***********************************************************************************)

WikipediaData[arg_,"Title"] := Module[{result,title,pageid},
	title=arg;
	pageid=If[MatchQ[arg,Rule["PageID",_]],"PageID"/.arg,Missing["NotAvailable"]];
	
	Which[
		MatchQ[pageid,_Missing],
		result=wikipedia["Title",{"Title"->title}],
		!MatchQ[pageid,_Missing],
		result=wikipedia["Title",{"PageID"->pageid}],
		True,
		result=Missing["BadInput"]
	];
	
	result
];

(***********************************************************************************)

WikipediaData[search_,"TitleSearch",opt:OptionsPattern[]] := Module[{result,limit},
	limit=OptionValue["MaxItems"]/.Automatic->100;
	If[MatchQ[search,Rule["Search",_String]],
		result=wikipedia["TitleSearch",{search,"MaxItems"->limit}],
		result=Missing["BadInput"]
	];
	result
];

(***********************************************************************************)

WikipediaData[arg_,"TitleTranslationRules",opt:OptionsPattern[]] := Module[{result,limit,title,pageid},
	limit=OptionValue["MaxItems"]/.Automatic->All;

	title=arg;
	pageid=If[MatchQ[arg,Rule["PageID",_]],"PageID"/.arg,Missing["NotAvailable"]];

	Which[
		MatchQ[pageid,_Missing],
		result=wikipedia["TitleTranslationRules",{"Title"->title,"MaxItems"->limit}],
		!MatchQ[pageid,_Missing],
		result=wikipedia["TitleTranslationRules",{"PageID"->pageid,"MaxItems"->limit}],
		True,
		result=Missing["BadInput"]
	];

	result
];

WikipediaData[arg_,"TitleTranslations",opt:OptionsPattern[]] := Module[{result,limit,title,pageid},
	limit=OptionValue["MaxItems"]/.Automatic->All;

	title=arg;
	pageid=If[MatchQ[arg,Rule["PageID",_]],"PageID"/.arg,Missing["NotAvailable"]];

	Which[
		MatchQ[pageid,_Missing],
		result=wikipedia["TitleTranslations",{"Title"->title,"MaxItems"->limit}],
		!MatchQ[pageid,_Missing],
		result=wikipedia["TitleTranslations",{"PageID"->pageid,"MaxItems"->limit}],
		True,
		result=Missing["BadInput"]
	];

	result
];

(***********************************************************************************)

WikipediaData["WikipediaRecentChanges"] := wikipedia["WikipediaRecentChanges"];

WikipediaData["WikipediaRecentChanges",opt:OptionsPattern[]] := Module[{result,limit},
	limit=OptionValue["MaxItems"]/.Automatic->100;
	result=wikipedia["WikipediaRecentChanges",{"MaxItems"->limit}]
];

(***********************************************************************************)

Protect[WikipediaData]
End[] (* End Private Context *)

EndPackage[]
