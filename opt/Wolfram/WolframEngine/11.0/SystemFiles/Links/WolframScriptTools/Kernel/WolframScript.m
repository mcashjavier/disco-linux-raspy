(* ::Package:: *)

BeginPackage["WolframScript`"]

Unprotect[System`$ScriptInputString];
System`$ScriptInputString = None;
Protect[System`$ScriptInputString];
CharacterSet::Unicode="Unicode is not yet available in wolframscript"
WolframScriptExecution::BadArgs="Invalid argument provided to WolframScriptExecute"
WolframScriptInitialize::BadArgs="Invalid argument provided to WolframScript initialization"
WolframScriptEvaluate::BadArgs="Invalid argument provided to WolframScript evaluation"


Begin["`Private`"]

Output::ExitCode="`1`";

Options[WolframScriptExecute]={TimeConstraint->0, "format"->ScriptForm, "timedout"->$TimedOut, "charset"->$CharacterEncoding, "print"->0, "line"->0, "formatoptions"->"{}"};

(*evaltypes, 1=code, 2=file, 3=function*)
(*setuptype, 1=local, 2=local verbose, 3=cloud, 4=cloud verbose *)
WolframScriptExecute[code_String, scriptArgs_String, evalType_Integer, setupType_Integer, version_String, OptionsPattern[]]:=Block[{parentLinkStore=$ParentLink},
	(*Message if character set is Unicode*)
	If[OptionValue["charset"] === "Unicode", Message[CharacterSet::Unicode];Return[""]];
	TimeConstrained[
		initialize[
			scriptArgs,
			evalType,
			setupType,
			Sequence@@Map[
				If[StringQ[#], #, ""]&,
				{
					OptionValue["format"],
					OptionValue["formatoptions"],
					OptionValue["charset"]
				}
			]
		];
		evaluate[code, evalType, setupType, OptionValue["print"], OptionValue["line"]]
	,
		If[OptionValue[TimeConstraint] == 0, Infinity, OptionValue[TimeConstraint]]
	,
		$ParentLink=parentLinkStore;
		OptionValue["timedout"]
	]
];

WolframScriptExecute[___]/;Message[WolframScriptExecution::BadArgs]:=$Failed


characterSetMapping = <|"adobestandard"->"AdobeStandard", "ascii"->"ASCII", "cp936"->"CP936","cp949"->"CP949","eucjp"->"EUC-JP",
    "euc"->"EUC","ibm850"->"IBM-850","iso106461"->"ISO10646-1","iso88591"->"ISO8859-1","iso88592"->"ISO8859-2",
    "iso88593"->"ISO8859-3","iso88594"->"ISO8859-4","iso88595"->"ISO8859-5","iso88596"->"ISO8859-6","iso88597"->"ISO8859-7",
    "iso88598"->"ISO8859-8","iso88599"->"ISO8859-9","iso885910"->"ISO8859-10","iso885911"->"ISO8859-11","iso885913"->"ISO8859-13",
    "iso885914"->"ISO8859-14","iso885915"->"ISO8859-15","iso885916"->"ISO8859-16","isolatin1"->"ISOLatin1","isolatin2"->"ISOLatin2",
    "isolatin3"->"ISOLatin3","isolatin4"->"ISOLatin4","isolatincyrillic"->"ISOLatinCyrillic","koi8r"->"koi8-r","macintosharabic"->"MacintoshArabic",
    "macintoshchinesesimplified"->"MacintoshChineseSimplified","macintoshchinesetraditional"->"MacintoshChineseTraditional",
    "macintoshcroatian"->"MacintoshCroatian","macintoshcyrillic"->"MacintoshCyrillic","macintoshgreek"->"MacintoshGreek",
    "macintoshhebrew"->"MacintoshHebrew","macintoshicelandic"->"MacintoshIcelandic","macintoshkorean"->"MacintoshKorean",
    "euckr"->"MacintoshKorean","macintoshnoncyrillicslavic"->"MacintoshNonCyrillicSlavic","macintoshromanian"->"MacintoshRomanian",
    "macintoshroman"->"MacintoshRoman","macintoshromanpdfexport"->"MacintoshRomanPDFExport","macintoshthai"->"MacintoshThai",
    "macintoshturkish"->"MacintoshTurkish","macintoshuktrainian"->"MacintoshUkrainian","printableascii"->"PrintableASCII",
    "shiftjis"->"ShiftJIS","symbol"->"Symbol","math1"->"Math1","math2"->"Math2","math3"->"Math3","math4"->"Math4",
    "math5"->"Math5","mathematica1"->"Mathematica1","mathematica2"->"Mathematica2","mathematica3"->"Mathematica3",
    "mathematica4"->"Mathematica4","mathematica5"->"Mathematica5","mathematica6"->"Mathematica6","mathematica7"->"Mathematica7",
    "utf8"->"UTF8","windowsansi"->"WindowsANSI","windowbaltic"->"WindowsBaltic","windowscyrillic"->"WindowsCyrillic",
    "windowseasteurope"->"WindowsEastEurope","windowsgreek"->"WindowsGreek","windowsthai"->"WindowsThai",
    "windowsturkish"->"WindowsTurkish", "ansix31101983"->"ASCII", "ansix341968"->"ASCII", "usascii"->"ASCII", "armscii8"->"ISO8859-5", 
    "asmo449"->"ISO8859-6", "cp100007"->"MacintoshCyrillic", "cp1250"->"ISO8859-2", "cp1251"->"WindowsCyrillic",
    "cp1252"->"WindowsANSI", "cp1253"->"WindowsGreek", "cp1254"->"WindowsTurkish", "cp1255"->"ISO8859-8",
    "cp1256"->"ISO8859-6", "cp1257"->"WindowsBaltic", "cp1258"->"WindowsGreek", "gb2312"->"MacintoshChineseSimplified",
    "hz"->"MacintoshChineseSimplified",  "gbk"->"MacintoshChineseSimplified",  "koi8ru"->"koi8-r", "koi8t"->"koi8-r", "koi8u"->"koi8-r",
    "big5"->"MacintoshChineseTraditional", "big5hkscs"->"MacintoshChineseTraditional", "cns"->"MacintoshChineseTraditional",
    "eten"->"MacintoshChineseTraditional","euccn"->"MacintoshChineseSimplified", "936"->"CP936", "949"->"CP949", "20932"->"EUC-JP",
    "51950"->"EUC", "850"->"IBM-850", "1200"->"ISO10646-1", "28591"->"ISO8859-1", "28592"->"ISO8859-2", "28593"->"ISO8859-3",
    "28594"->"ISO8859-4", "28595"->"ISO8859-5", "28596"->"ISO8859-6", "28597"->"ISO8859-7", "28598"->"ISO8859-8", "28599"->"ISO8859-9",
    "28600"->"ISO8859-10","874"->"ISO8859-11","28603"->"ISO8859-13","28604"->"ISO8859-14","28605"->"ISO8859-15", 
    "28606"->"ISO8859-16", "10002"->"MacintoshChineseTraditional", "10004"->"MacintoshArabic", "10008"->"MacintoshChineseSimplified", 
    "10082"->"MacintoshCroatian", "10007"->"MacintoshCyrillic", "10006"->"MacintoshGreek", "10005"->"MacintosHebrew",
    "10079"->"MacintoshIcelandic", "10003"->"MacintoshKorean", "10029"->"MacintoshNonCyrillicSlavic", "10010"->"MacintoshRomanian",
    "10000"->"MacintoshRoman","10021"->"MacintoshThai", "10081"->"MacintoshTurkish", "10017"->"MacintoshUkrainian", "932"->"ShiftJIS",
    "65001"->"UTF8","1252"->"WindowsANSI", "1257"->"WindowsBaltic", "1250"->"WindowsEastEurope", "1253"->"WindowsGreek",
    "874"->"WindowsThai", "1254"->"WindowsTurkish", "he"->"ISO8859-8"|>;


initialize[scriptArgs_String, evalType_Integer, setupType_Integer, stringFormat_String, formatOptions_String, stringCharset_String]:=Block[{format=stringFormat, charset},
	Unprotect[$EvaluationEnvironment, Exit, Quit];
	$EvaluationEnvironment="Script";

	(*Finding character encoding*)
	charset = characterSetMapping[ToLowerCase[StringReplace[stringCharset,RegularExpression["[^A-Za-z0-9]"]->""]]];
	
	(*Setting character encoding and format*)		
	If[MemberQ[$CharacterEncodings,charset],$CharacterEncoding=charset, $CharacterEncoding="ISO8859-1"];
	If[!MemberQ[$ExportFormats, format],
		If[MemberQ[Map[Hold,$PrintForms],
			ToExpression[format,InputForm,Hold]]
		,
			format=ToExpression[format]
		]
	];

	(*Redefine Exit and Quit*)
	Exit[]:=Throw[wsExitCode[0], "WolframScriptExitCode"];
	Exit[code_Integer]:=Throw[wsExitCode[code], "WolframScriptExitCode"];
	Quit=Exit;
	
	(*Open a new stream to direct output at. This catches print statements*)
	outputsave=$Output;
	outputstream=OpenWrite[CharacterEncoding->$CharacterEncoding, FormatType->If[!StringQ[format], format, ScriptForm],
		BinaryFormat->If[MemberQ[$ExportFormats, format],True,False], PageWidth->Infinity];
	$Output={outputstream};
	Protect[$EvaluationEnvironment,Exit,Quit];

	(*Set variables available in the script and redefine Input*)
	Unprotect[System`$ScriptInputString, Input, InputString];
	If[setupType >= 3,
		Clear[Input];Input[args___]:=Missing["NotAvailable"];
		Clear[InputString];InputString[args___]:=Missing["NotAvailable"]
	,
		Quiet[System`$ScriptInputString=ReadString[System`$ScriptInputString]];
		If[!StringQ[System`$ScriptInputString], System`$ScriptInputString=""]
	];
	Protect[System`$ScriptInputString, Input, InputString];
	If[!MemberQ[Attributes[$ScriptCommandLine],Locked],
		Unprotect[$ScriptCommandLine];
		Set[$ScriptCommandLine,ToExpression[scriptArgs]];
		Protect[$ScriptCommandLine]
	];
	
	(*Set up formatter which applys ExportString to output*)
	Unprotect[Internal`$PrintFormatter];
	If[MemberQ[$ExportFormats, format],
		Internal`$PrintFormatter=SequenceForm@@Map[ExportString[#, format,Sequence[ToExpression[formatOptions]]]&,#]&
		,If[format==="numeric",Internal`$PrintFormatter=SequenceForm@@Map[If[NumericQ[#], #, "NaN"]&,#]&]
	];
	Protect[Internal`$PrintFormatter];
];

WolframScriptInitialize[___]/;Message[WolframScriptInitialize::BadArgs]:=$Failed;


evaluate[expr_String,evalType_Integer,setupType_Integer, print_Integer, line_]:=Block[{output, pipedList, exitCode=0},
	(*Check if linewise version needs to be run*)
	Block[{$ParentLink=Null},
		exitCode = Catch[
			If[line===True, 
				pipedList = StringSplit[$ScriptInputString, {"\r\n","\n"}];
				Map[Block[{$ScriptInputString=#},
					If[print===2,ReleaseHold[If[#=!=Null,Print[#]]&/@ToExpression[expr, InputForm, Hold]]];
					If[print < 2,output=ToExpression[expr]];
					If[print===1 || evalType=!=2,Print[output]];
				]&, pipedList]
			,
				If[print===2,ReleaseHold[If[#=!=Null,Print[#]]&/@ToExpression[expr, InputForm, Hold]]];
				If[print < 2,output=ToExpression[expr]];
				If[print===1 || evalType=!=2,Print[output]]
			]
		, "WolframScriptExitCode"]
	];
	
	(*Read collected print statements. Then clean up and leave*)
	output = ReadString[First@outputstream];
	If[!StringQ[output], output=""];
	Close[outputstream];
	$Output=outputsave;
	If[Head[exitCode] === wsExitCode, exitCode = exitCode[[1]], exitCode=0];
	If[exitCode =!= 0,
		If[setupType >=3, Print["ExitCode=", exitCode], Message[Output::ExitCode, exitCode]]
	];
	If[setupType === 3, output=ExportString[output, "Base64"]];
	output
];

WolframScriptEvaluate[___]/;Message[WolframScriptEvaluate::BadArgs]:=$Failed;


End[]

EndPackage[]
