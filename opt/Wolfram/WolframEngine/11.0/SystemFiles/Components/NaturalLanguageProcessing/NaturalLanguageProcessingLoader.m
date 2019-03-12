(* All loading of the paclet's Wolfram Language code should go through this file. *)

(* Developer maintains this list of symbols.
   autoloadSymbols must agree with the symbols listed in the Kernel extension in the PacletInfo.m file.
*)
NaturalLanguageProcessing`Private`autoloadSymbols = {          
				"System`WordStem",
				"System`LanguageIdentify",
				
				"System`TextPosition",
				"System`TextCases",
				
				"System`Containing",
				
				"System`TextElement",
				"System`TextStructure"
         }
    
NaturalLanguageProcessing`Private`symsToProtect = {};


PacletManager`Package`loadWolframLanguageCode["NaturalLanguageProcessing", "NaturalLanguageProcessing`", DirectoryName[$InputFileName], "NLPTools.m",
         "AutoUpdate" -> True,
         "ForceMX" -> TrueQ[NaturalLanguageProcessing`$ForceMX], 
         "Lock" -> False,
         "AutoloadSymbols" -> NaturalLanguageProcessing`Private`autoloadSymbols,
         "HiddenImports" -> {"PacletManager`", "Developer`", "GeneralUtilities`", "Macros`"},
         "SymbolsToProtect" -> NaturalLanguageProcessing`Private`symsToProtect
]