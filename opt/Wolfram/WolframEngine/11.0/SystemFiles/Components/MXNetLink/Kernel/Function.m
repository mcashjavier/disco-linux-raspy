Package["MXNetLink`"]

PackageImport["GeneralUtilities`"]


(****** Symbol Exports ******)

PackageExport["MXFunctionEnumerate"]
PackageExport["MXFunctionEvaluate"]


(* Cache for speed *)
$MXFunctionEnumerate := $MXFunctionEnumerate = MXFunctionEnumerate[];

(******************************************************************************)
(****** Load Library Functions ******)

DeclareLibraryFunction[mxFunctionInfoEnumerate, "MXFunctionInfoEnumerate", 
	{}
	, 
	"UTF8String"	(* JSON *)					
]	

PackageScope["mxFuncInvoke"]

DeclareLibraryFunction[mxFuncInvoke, "WL_MXFuncInvoke", 
	{
		"UTF8String",					(* function name *)
		{Integer, 1, "Constant"}, 		(* use_vars *)
		{Real, 1, "Constant"}, 			(* scalar_args *)
		{Integer, 1, "Constant"} 		(* mutate_vars *)
	}
	, 
	"Void"					
]	

(******************************************************************************)

MXSetUsage @
"MXFunctionEnumerate[] returns an association whose keys are the available MXNet functions and values are associations providing further information."


MXFunctionEnumerate[] := Scope[
	result = MXNetInvoke[mxFunctionInfoEnumerate];
	(* Return associaiton *)
	Developer`ReadRawJSONString@result
]

	
