(* Paclet Info File *)

(* created 2016/02/18*)

Paclet[
    Name -> "NaturalLanguageProcessing",
    Version -> "1.2",
    MathematicaVersion -> "10+",
    Description -> "Natural Language Processing Utilities",
    Creator -> "Gopal Sarma <gopals@wolfram.com>",
    Loading -> Automatic,
    Extensions -> 
        {
            {"Resource", Root -> "Resources", Resources -> 
                {"Misc", "SentenceBoundaries", "WLTagger"}
            }, 
            {"Kernel", Symbols -> 
                {"System`WordStem", "System`LanguageIdentify", "System`TextPosition", "System`TextCases", "System`Containing", "System`TextElement", "System`TextStructure"}
            , Context -> 
                {"NaturalLanguageProcessingLoader`", "NaturalLanguageProcessing`"}
            }
        }
]


