(* ::Package:: *)

Begin["System`Convert`JSONDump`"]

readExpressionJSON[filename_String, opts___] := Developer`ReadExpressionJSONFile[filename, "IssueMessagesAs" -> Import];
readExpressionJSON[stream_InputStream, opts___] := Developer`ReadExpressionJSONStream[stream, "IssueMessagesAs" -> Import];

ImportExport`RegisterImport["ExpressionJSON",
	readExpressionJSON,
	"FunctionChannels" -> {"FileNames", "Streams"}
]

End[]
