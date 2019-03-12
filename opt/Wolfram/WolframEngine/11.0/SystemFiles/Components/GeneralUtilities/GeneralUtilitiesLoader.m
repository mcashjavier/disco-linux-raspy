(* All loading of the paclet's Wolfram Language code should go through this file. *)

(* Developer maintains this list of symbols.
   autoloadSymbols must agree with the symbols listed in the Kernel extension in the PacletInfo.m file.
*)

GeneralUtilities`Private`autoloadSymbols = {
            "System`DeleteMissing",
            "System`DisjointQ", 
            "System`IntersectingQ", 
            "System`SubsetQ",
            "System`Failure",
            "System`PowerRange",
            "System`CountDistinct",
            "System`CountDistinctBy",
            "System`DeleteDuplicatesBy",
            "System`TextString",
            "System`AssociationMap",
            "System`InsertLinebreaks",
			"System`StringPadLeft",
			"System`StringPadRight",
			"System`StringExtract",
			"System`Capitalize",
			"System`Decapitalize",
			"System`StringRepeat",
			"System`StringRiffle",
			"System`PrintableASCIIQ",  
			"System`AssociationFormat","System`ListFormat",
			"System`BooleanStrings", "System`MissingString",
			"System`TimeFormat", "System`ElidedForms" 
}

General::notassoc = "First argument `` is not a symbol whose value is an association.";
General::nomon = "Progress of function `` cannot be automatically monitored.";

General::noreturnscope = "There is no valid outermost head to Return to after macro expansion: ``.";
General::badreturnscope = "The outermost head `` in `` is not a valid target for a Return.";
General::nomessagehead = "There is no head with which to associate the message in ``.";
General::invmacro = "Encountered invalid invocation of `` macro during expansion of ``.";
General::holdmacro = "`` macro cannot be used with symbol ``, which has Hold-type attributes.";
General::unmatched = "The case `` was unmatched by ``.";

General::interr = "An internal error occurred. Please contact Wolfram Research.";
General::interr1 = "An internal exception was generated. Please contact Wolfram Research.";
General::interr2 = "An internal exception was generated. Please contact Wolfram Research.";

General::invsrcarg = "Argument `` is not a paclet name, file, or directory.";
General::invsrcdir = "Could not find an entry point within the directory ``.";
General::invsrcpac = "Could not find a location on disk for the context ``.";
General::invcont = "`` is not a valid context or list of contexts."
General::invsrcfile = "Cannot load `` in isolation, because it is a new-style package fragment."
General::nosyms = "No symbols match ``."

General::invspatt = "The argument `1` is not a valid string pattern."
General::lists = "List of lists expected at position `2` in `1`."
General::stringnz = "String of non-zero length expected at position `2` in `1`."
General::strlist = "List of strings expected at position `2` in `1`."

General::asrtf = "Assertion `1` failed.";

Block[{GeneralUtilities`$MacroDebugMode = False, Developer`$CurrentPackage = "GeneralUtilities`"},
	Quiet[
	PacletManager`Package`loadWolframLanguageCode["GeneralUtilities", "GeneralUtilities`", DirectoryName[$InputFileName], "Code.m",
				"AutoUpdate" -> False, "ForceMX" -> False, "Lock" -> False, 
				"AutoloadSymbols" -> GeneralUtilities`Private`autoloadSymbols,
		"HiddenImports" -> {"Macros`"}
	],
	{General::shdw, RuleDelayed::rhs}
	];
];

If[!ValueQ[Developer`$CurrentPackage], Developer`$CurrentPackage = None];
