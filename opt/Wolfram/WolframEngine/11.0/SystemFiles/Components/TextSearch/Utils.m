Package["TextSearch`"]

PackageImport["GeneralUtilities`"]

PackageScope["hasFE"]

PackageExport["CreateSearchIndexFromStrings"]

PackageScope["$linuxQ"]
PackageScope["$windowsQ"]

$linuxQ = !StringFreeQ[$SystemID, "Linux"];
$windowsQ = !StringFreeQ[$SystemID, "Windows"];

CreateSearchIndexFromStrings[strs_List] := With[
	{tmp = Quiet[CreateDirectory[FileNameJoin[{$TemporaryDirectory, "TextSearch"}]], CreateDirectory::filex]},
    CreateSearchIndex[
    	Export[FileNameJoin[{tmp, ToString[Hash @ #] <> ".txt"}], #, "Text"]& /@ strs
   	]
];


PackageScope["ReadStringASCII"]

(* this is what Import[..., "String"] does anyway. 
it's not UTF8 safe, though. unfortunately reading UTF8 is incredibly slow 
(see KERN/Kernel/StartUp/Convert/Text.m for how gross it is).
we should solve this at the kernel level.
 *)
ReadStringASCII[file_] := Module[{res, stream}, 
	Internal`WithLocalSettings[
		stream = OpenRead[file],
		res = Read[stream, Record, RecordSeparators -> {}],
		Close[stream]
	];
	Replace[res, EndOfFile -> ""]
];


PackageScope["FailUnless"]

SetAttributes[{FailUnless, makeMessage}, HoldAll];
FailUnless[test_, rest___][arg_] := If[test[arg], arg, makeMessage[rest]; Throw[$Failed]];

makeMessage[{head_, str_String}, args___] := With[{sym = head}, Message[MessageName[sym, str], args]];
makeMessage[] := Null;
makeMessage[args___] := Message[args];

(*!
	\function DiffListOfAssociations
	
	\calltable
		DiffListOfAssociations[list1, list2, idKey] '' given two lists of associations, and a key that can be used to uniquely identify an association, produce a diff.

	Examples:
    
    DiffListOfAssociations[
        {Association["Key" -> 1, "a" -> 2, "b" -> 3], Association["Key" -> 2, "a" -> 2, "b" -> 3]},
        {
            Association["Key" -> 1, "a" -> 9, "b" -> 3],
            Association["Key" -> 2, "a" -> 2, "b" -> 9],
            Association["Key" -> 3, "a" -> 7, "b" -> 11]
        },
        "Key"
    ]

    ===

    {
        <|"Key" -> 1, "a" -> Diff[2, 9], "b" -> 3|>,
        <|"Key" -> 2, "a" -> 2, "b" -> Diff[3, 9]|>,
        Diff[Missing[], <|"Key" -> 3, "a" -> 7, "b" -> 11|>]
    }

    Unit tests: DiffListOfAssociations.mt

    \maintainer danielb
*)
PackageScope["DiffListOfAssociations"]
DiffListOfAssociations[list1_List, list2_List, idKey_] :=
	Scope[
		keysUnion =
			DeleteDuplicates[Join[
				list1[[All, idKey]],
				list2[[All, idKey]]
			]];
			
		list1Assoc = ListOfAssociationsToAssociation[list1, idKey];
		list2Assoc = ListOfAssociationsToAssociation[list2, idKey];
			
		Function[{key},
			assoc1 = Lookup[list1Assoc, key, Missing[]];
			assoc2 = Lookup[list2Assoc, key, Missing[]];
			
			DiffIfNecessary[assoc1, assoc2]
		] /@ keysUnion
	];

(*!
	\function ListOfAssociationsToAssociation
	
	\calltable
		ListOfAssociationsToAssociation[list, idKey] '' given a list of associations, and a key that can be used to uniquely identify an association, produce a new association that uses those id key values as its keys.

	Examples:
    
    ListOfAssociationsToAssociation[
        {Association["Key" -> 1, "a" -> 2, "b" -> 3], Association["Key" -> 2, "a" -> 2, "b" -> 3]},
        "Key"
    ]

    ===

    <|1 -> <|"Key" -> 1, "a" -> 2, "b" -> 3|>, 2 -> <|"Key" -> 2, "a" -> 2, "b" -> 3|>|>

    Unit tests: ListOfAssociationsToAssociation.mt

    \maintainer danielb
*)
PackageScope["ListOfAssociationsToAssociation"]
ListOfAssociationsToAssociation[list_, idKey_] :=
	Block[{},
		Association @@
			Function[{assoc},
				assoc[idKey] -> assoc
			] /@ list
	];
	
(*!
	\function ProduceDiff
	
	\calltable
		ProduceDiff[a, b] '' given two expressions, produce a diff.

	Examples:

    ProduceDiff[
        Association["Key" -> 1, "a" -> 2, "b" -> 3],
        Association["Key" -> 1, "a" -> 2, "b" -> 4, "c" -> 5]
    ]

    ===

    <|"Key" -> 1, "a" -> 2, "b" -> Diff[3, 4], "c" -> Diff[Missing[], 5]|>

    Unit tests: ProduceDiff.mt

    \maintainer danielb
*)
PackageScope["ProduceDiff"]
PackageScope["Diff"]

Options[ProduceDiff] =
{
    "UseIdiomThatFirstKeyIsAssumedToBeUniqueId" -> True		(* If we're diffing a list of associations we'd ideally like to determine the correspondance between associations in the two lists, but we don't know which key (if any) can serve to uniquely identify an association. If we set this option to True, then we'll assume the first key/value pair can serve as a unique ID. *)
};

ProduceDiff[a_, b_, opts:OptionsPattern[]] := Diff[a, b]

ProduceDiff[a_Association, b_Association, opts:OptionsPattern[]] :=
	Scope[
		(* Union but with order preserved. *)
		keyUnion = DeleteDuplicates[Join[Keys[a], Keys[b]]];
		
		Association @@
		Function[{key},
			key ->
                DiffIfNecessary[
                    Lookup[a, key, Missing[]],
                    Lookup[b, key, Missing[]],
                    opts
                ]
		] /@ keyUnion
	]

ProduceDiff[a:{__Association}, b:{__Association}, opts:OptionsPattern[]] :=
	Scope[
		If [TrueQ[OptionValue["UseIdiomThatFirstKeyIsAssumedToBeUniqueId"]],
			defaultKey =
				Normal[
					If [Length[a] > 0, a, b][[1]]
				][[1, 1]];
			
			DiffListOfAssociations[a, b, defaultKey]
			,
			Diff[a, b]
		]
	]

(*!
    \function DiffIfNecessary
	
	\calltable
		DiffIfNecessary[a, b] '' If two things are the same, then just echo the common item. Otherwise, wrap the differing things in Diff.

	Examples:
    
    DiffIfNecessary[1, 2] === Diff[1, 2]
    
    DiffIfNecessary[1, 1] === 1

    Unit tests:

    Unit tests: DiffIfNecessary.mt

    \maintainer danielb
*)
PackageScope["DiffIfNecessary"]

PackageScope["ReplaceValuesOfKey"]

PackageScope["FormatNumericalDiff"]
Options[DiffIfNecessary] =
{
    "UseIdiomThatFirstKeyIsAssumedToBeUniqueId" -> True		(* If we're diffing a list of associations we'd ideally like to determine the correspondance between associations in the two lists, but we don't know which key (if any) can serve to uniquely identify an association. If we set this option to True, then we'll assume the first key/value pair can serve as a unique ID. *)
};
DiffIfNecessary[a_, b_, opts:OptionsPattern[]] :=
	If [a === b,
		a
		,
		ProduceDiff[a, b, opts]
	];

(*!
	\function ReplaceValuesOfKey
	
	\calltable
		ReplaceValuesOfKey[assoc, key, func] '' given an association and a key that might occur somewhere within its nested structure, replace values of that key by applying the given function to them.

	Examples:
    
    ReplaceValuesOfKey[
        Association[
            "a" -> 1,
            "b" -> {Association["c" -> {"a", "b", "c"}], Association["c" -> {"b", "d"}]}
        ],
        "c",
        DeleteCases[#1, "b"] & 
    ]

    ===

    <|"a" -> 1, "b" -> {<|"c" -> {"a", "c"}|>, <|"c" -> {"d"}|>}|>

    Unit tests: ReplaceValuesOfKey.mt

    \maintainer danielb
*)
ReplaceValuesOfKey[assoc_, key_, func_] :=
	Replace[
		assoc,
		a:KeyValuePattern[{key -> _}] :>
			With[{tmp =
				Module[{tmp2 = a},
					tmp2[key] = func[tmp2[key]];
					tmp2
				]},
				tmp /; True
			],
		{0, Infinity}
	]

(*!
	\function FormatNumericalDiff
	
	\calltable
		FormatNumericalDiff[diff] '' format a diff of a numerical value to show its new and old value, its absolute difference, and its percentage difference.

	Examples:

    FormatNumericalDiff[Diff[1, 1.3]]

    ===

    Row[
        {
            1.3,
            " vs. ",
            1,
            " (",
            Style[Row[{"+", 0.3}], RGBColor[0, 0.5, 0]],
            Row[{" = ", Style[Row[{"+", "30", "%"}], RGBColor[0, 0.5, 0]]}],
            ")"
        }
    ]

    Unit tests: FormatNumericalDiff.mt

    \maintainer danielb
*)
FormatNumericalDiff[val_Integer] := val
FormatNumericalDiff[val_Real] := val

FormatNumericalDiff[diff_Diff] :=
	Scope[
		
		diffValue = Round[diff[[2]] - diff[[1]], 0.001];
		
		explicitDiffSign = If [diffValue > 0, "+", ""];
		
		color = If [diffValue > 0, RGBColor[0, 0.5, 0], Red];
		
		Row[{
			diff[[2]],
			" vs. ",
			StringReplace[
				ToString[Round[diff[[1]], 0.001]],
				"." ~~ EndOfString :> ""
			],
			" (",
			Style[
				Row[{explicitDiffSign, diffValue}],
				color
			],
			If [diff[[1]] != 0 && diff[[1]] =!= 0.,
				Row[{
					" = ",
					Style[
						Row[{
							explicitDiffSign,
							StringReplace[
								ToString[Round[diffValue / diff[[1]] * 100, 0.1]],
								"." ~~ EndOfString :> ""
							],
							"%"
						}],
						color
					]
				}]
				,
				Sequence @@ {}
			],
			")"
		}]
	]
	
(*!
    \function Indent2
    
    \calltable
        Indent2[e_] '' Given an expression, convert it to a string and use multiple lines / indenting as needed to improve readability.
*)
PackageScope["Indent2"]

PackageScope["MaxLineLength"]
Options[Indent2] =
{
    "FullFormStrings" -> False,                 (*< If True, then strings are FullFormed, so that they can round trip to files properly as ASCII characters. (since we want .m files to be ASCII) *)
    "RemoveContexts" -> True,                   (*< Remove contexts from symbols for improved readability. *)
    "AlwaysIndentToLevel" -> None,              (*< Can be set if you'd like all expressions prior to a certain depth to be indented. *)
    "AlwaysIndentRulesToLevel" -> None,         (*< Can be set if you'd like all rules prior to a certain depth to be indented. *)
    "AlwaysIndentRuleDelayedToLevel" -> None,   (*< Can be set if you'd like all RuleDelayed's prior to a certain depth to be indented. *)
    "AlwaysIndentListsOfRuleToLevel" -> None,   (*< Can be set if you'd like all lists of rules prior to a certain depth to be indented. *)
    "DoNotIndentRuleRhs" -> False,              (*< Can be set to True if you don't want rile RHSs to be indented. *)
    "RuleLeftHandSidesNotToIndent" -> {},       (*< If a rule has one of these LHSs, it won't be indented. *)
    "RemoveHold" -> False                       (*< If a Hold or HoldComplete wraps the input, is it there only to keep the code from evaluating? In that case, we'll remove it for the caller. *)
};
Indent2[e_, initialIndent_Integer:0, spacesPerIndent_Integer:4, opts:OptionsPattern[]] :=
    Module[{res},
        
        With[{e2 = rowBoxFix1[e]},
        With[{eModified = HoldComplete[e2] /. Repeated :> "Indent2:Repeated"},
            
            res =
            StringReplace[
                linesToString[
                    indent[
                        eModified,
                        (* Since we added HoldComplete *)
                        initialIndent,
                        spacesPerIndent,
                        If [TrueQ[OptionValue["RemoveHold"]],
                            (* Since the user added a HoldComplete, we need to adjust all of the
                               "AlwaysIndentToLevel"-like options accordingly. *)
                            Sequence @@ adjustOptionsInvolvingLevel[opts]
                            ,
                            Sequence @@
                               Append[
                                   filterIndentOptions[opts],
                                   "LevelAdjustment" -> 0
                               ]
                        ]
                    ]
                ],
                "\"Indent2:Repeated\"" :> "Repeated"
            ];
            
            res =
                If [TrueQ[OptionValue["RemoveHold"]],
                    RemoveHoldFromIndentedString[res]
                    ,
                    res
                ];
                
            res = rowBoxFix2[res];
            
            res
        ]
        ]
    ]
    
(*!
    \function rowBoxFix1
    
    \calltable
        rowBoxFix1[e] '' replace uses of RowBox so that they don't end up getting handled by InputForm, which turns them into an unwanted form.
    
    Examples:
    
    rowBoxFix1[RowBox[{"a"}]] === "<<RowBox>>"[{"a"}]
    
    \maintainer danielb
*)
rowBoxFix1[e_] :=
    (* Need to use ReplaceRepeated to handle nested RowBoxes properly. *)
    ReplaceRepeated[
        e /.
            (* Encode these if they were actually passed in as part of the input
               so that we don't mistake them as RowBox symbols in rowBoxFix2. *)
            "<<RowBox>>" :> "<<<RowBox>>>",
        RowBox[args___] :> "<<RowBox>>"[args]
    ]

(*!
    \function rowBoxFix2
    
    \calltable
        rowBoxFix2[e] '' replace/undo uses of an encoded RowBox specifier. See also: rowBoxFix1
    
    \related 'rowBoxFix1
    
    \maintainer danielb
*)
rowBoxFix2[e_] :=
    StringReplace[
        e,
        {
            "\"<<RowBox>>\"" :> "RowBox",
            "\"<<<RowBox>>>\"" :> "\"<<RowBox>>\""
        }
    ]
    
(*!
    \function adjustOptionsInvolvingLevel
    
    \calltable
        adjustOptionsInvolvingLevel[opts] '' Because we wrap a HoldComplete around the expression to prevent if from evaluating, we need to adjust all of the level-based options by 1.
    
    Examples:
    
    adjustOptionsInvolvingLevel[
        "AlwaysIndentToLevel" -> 1
    ]
    
    ===
    
    {
        "AlwaysIndentToLevel" -> 2
    }
    
    \related 'Indent2
    
    \maintainer danielb
*)
Clear[adjustOptionsInvolvingLevel];
Options[adjustOptionsInvolvingLevel] = Options[Indent2];
adjustOptionsInvolvingLevel[opts:OptionsPattern[]] :=
    Join[
        DeleteCases[
           filterIndentOptions[opts],
           HoldPattern[Rule][
               "AlwaysIndentToLevel" |
               "AlwaysIndentRulesToLevel" | 
               "AlwaysIndentRuleDelayedToLevel" | 
               "AlwaysIndentListsOfRuleToLevel",
               _
           ]
        ],
        {
            If [OptionValue["AlwaysIndentToLevel"] =!= None, "AlwaysIndentToLevel" -> OptionValue["AlwaysIndentToLevel"] + 1, Sequence @@ {}],
            If [OptionValue["AlwaysIndentRulesToLevel"] =!= None, "AlwaysIndentRulesToLevel" -> OptionValue["AlwaysIndentRulesToLevel"] + 1, Sequence @@ {}],
            If [OptionValue["AlwaysIndentRuleDelayedToLevel"] =!= None, "AlwaysIndentRuleDelayedToLevel" -> OptionValue["AlwaysIndentRuleDelayedToLevel"] + 1, Sequence @@ {}],
            If [OptionValue["AlwaysIndentListsOfRuleToLevel"] =!= None, "AlwaysIndentListsOfRuleToLevel" -> OptionValue["AlwaysIndentListsOfRuleToLevel"] + 1, Sequence @@ {}],         
            "LevelAdjustment" -> 1
        }
    ]

Clear[MaxLineLength];
MaxLineLength[h:Rule] := 90
MaxLineLength[HoldComplete[h:Rule]] := 90
MaxLineLength[_] := 90

(* Turn the so-far-accrued lines into a string. *)
linesToString[lines_] :=
    If [ListQ[lines],
        StringJoin[Riffle[Flatten[lines], "\n"]]
        ,
        lines
    ]

(*!
    \function filterIndentOptions
    
    \calltable
        filterIndentOptions[opts] '' given options passed to one of the indent functions, filters them so that only the options that should be propagated to recursive calls are left.
    
    Examples:
    
    filterIndentOptions[
        "FullFormStrings" -> True,
        "SomeOtherOptions" -> False
    ]
    
    ===
    
    {"FullFormStrings" -> True}

    \maintainer danielb
*)
filterIndentOptions[opts___] :=
    FilterRules[{opts}, Append[Options[Indent2], "LevelAdjustment" -> 0]]

Clear[indent];

Options[indent] =
Join[
    Options[Indent2],
    {
        "ArgumentOfRule" -> False,          (*< True if this expression is an argument of a Rule or RuleDelayed. *)
        "LevelAdjustment" -> 0              (*< The amount added to level-based options because of the hold wrapper around the expression. *)
    }
];

indent[e:HoldComplete[f_[args___]], lvl_, spacesPerIndent_, opts:OptionsPattern[]] :=
  With[{literal = indentLiteral[e, 0, spacesPerIndent, filterIndentOptions[opts]]},
    If [f =!= Pattern &&
        (
            StringLength[literal] > MaxLineLength[HoldComplete[f]] ||
            (IntegerQ[OptionValue["AlwaysIndentToLevel"]] && lvl <= OptionValue["AlwaysIndentToLevel"])
        ),
        {
            indentLiteral[HoldComplete[f], lvl, spacesPerIndent, filterIndentOptions[opts]]<>"[",
            indentArgs[HoldComplete[args], lvl, spacesPerIndent, filterIndentOptions[opts]],
            indentString["]", lvl, spacesPerIndent, filterIndentOptions[opts]]
        }
        ,
        {indentLiteral[e, lvl, spacesPerIndent, filterIndentOptions[opts]]}
    ]
  ]
  
(* Set *)
indent[e:HoldComplete[Set[arg1_, arg2_]], lvl_, spacesPerIndent_, opts:OptionsPattern[]] :=
    Module[{lBracket = Sequence @@ {}, rBracket = Sequence @@ {}},
        
        If [TrueQ[OptionValue["ArgumentOfRule"]],
            (* If we are a Set inside of a Rule or Rule delayed, then we need to use
               brackets for proper precedence. *)
            lBracket = indentString["(", lvl, spacesPerIndent, filterIndentOptions[opts]];
            rBracket = indentString[")", lvl, spacesPerIndent, filterIndentOptions[opts]];
        ];
        
        With[{literal = indentLiteral[e, 0, spacesPerIndent, filterIndentOptions[opts]]},
            If [StringLength[literal] > MaxLineLength[HoldComplete[Set]] ||
                (IntegerQ[OptionValue["AlwaysIndentToLevel"]] && lvl <= OptionValue["AlwaysIndentToLevel"]),
                {lBracket, linesToString[indent[HoldComplete[arg1], lvl, spacesPerIndent, filterIndentOptions[opts]]] <> " =", indent[HoldComplete[arg2], lvl + 1, spacesPerIndent, filterIndentOptions[opts]], rBracket}
                ,
                {lBracket, linesToString[indent[HoldComplete[arg1], lvl, spacesPerIndent, filterIndentOptions[opts]]] <> " = " <> indentLiteral[HoldComplete[arg2], 0, spacesPerIndent, filterIndentOptions[opts]], rBracket}
            ]
        ]
    ]

(* List *)
indent[e:HoldComplete[List[args___]], lvl_, spacesPerIndent_, opts:OptionsPattern[]] :=
  With[{literal = indentLiteral[e, 0, spacesPerIndent, filterIndentOptions[opts]]},
    If [(StringLength[literal] > MaxLineLength[HoldComplete[List]]) ||
        (IntegerQ[OptionValue["AlwaysIndentToLevel"]] && lvl <= OptionValue["AlwaysIndentToLevel"]) ||
        (IntegerQ[OptionValue["AlwaysIndentListsOfRuleToLevel"]] && lvl <= OptionValue["AlwaysIndentListsOfRuleToLevel"] && MatchQ[e, HoldComplete[List[__Rule]]]),
        If [HoldComplete[{args}] === HoldComplete[{}],
            {
                indentString["{", lvl, spacesPerIndent, filterIndentOptions[opts]],
                indentString["}", lvl, spacesPerIndent, filterIndentOptions[opts]]
            }
            ,
            {
                indentString["{", lvl, spacesPerIndent, filterIndentOptions[opts]],
                indentArgs[HoldComplete[args], lvl, spacesPerIndent, filterIndentOptions[opts]],
                indentString["}", lvl, spacesPerIndent, filterIndentOptions[opts]]
            }
        ]
        ,
        {indentLiteral[e, lvl, spacesPerIndent, filterIndentOptions[opts]]}
    ]
  ]

(* For Rule *)
indent[e:HoldComplete[Rule[arg1_, arg2_]], lvl_, spacesPerIndent_, opts:OptionsPattern[]] :=
  With[{literal = indentLiteral[e, 0, spacesPerIndent, filterIndentOptions[opts]]},
    If [(
            (StringLength[literal] > MaxLineLength[HoldComplete[Rule]]) ||
            (IntegerQ[OptionValue["AlwaysIndentRulesToLevel"]] && lvl <= OptionValue["AlwaysIndentRulesToLevel"] && !MatchQ[e, HoldComplete[Rule["Example", _]]]) ||
            (IntegerQ[OptionValue["AlwaysIndentToLevel"]] && lvl <= OptionValue["AlwaysIndentToLevel"])
        )
        &&
        (
            (
                OptionValue["RuleLeftHandSidesNotToIndent"] === {} ||
                !MemberQ[OptionValue["RuleLeftHandSidesNotToIndent"], arg1]
            )
        ),
        {
            appendToLast[indentLiteral[HoldComplete[arg1], lvl, spacesPerIndent, filterIndentOptions[opts]], " ->"],
                If [TrueQ[OptionValue["DoNotIndentRuleRhs"]] ||
                    MatchQ[HoldComplete[arg2], HoldComplete[_List]],
                    
                    indent[HoldComplete[arg2], lvl, spacesPerIndent, "ArgumentOfRule" -> True, filterIndentOptions[opts]]
                    ,
                    indent[HoldComplete[arg2], lvl + 1, spacesPerIndent, "ArgumentOfRule" -> True, filterIndentOptions[opts]]
                ]
        }
        ,
        {indentLiteral[e, lvl, spacesPerIndent, filterIndentOptions[opts]]}
    ]
  ]
  
(* For RuleDelayed *)
indent[e:HoldComplete[RuleDelayed[arg1_, arg2_]], lvl_, spacesPerIndent_, opts:OptionsPattern[]] :=
  With[{literal = indentLiteral[e, 0, spacesPerIndent, filterIndentOptions[opts]]},
    If [(
            StringLength[literal] > MaxLineLength[HoldComplete[Rule]] ||
            (IntegerQ[OptionValue["AlwaysIndentRulesToLevel"]] && lvl <= OptionValue["AlwaysIndentRulesToLevel"] && !MatchQ[e, HoldComplete[Rule["Example", _]]]) ||
            (IntegerQ[OptionValue["AlwaysIndentRuleDelayedToLevel"]] && lvl <= OptionValue["AlwaysIndentRuleDelayedToLevel"]) ||
            (IntegerQ[OptionValue["AlwaysIndentToLevel"]] && lvl <= OptionValue["AlwaysIndentToLevel"])
        ),
        {
            appendToLast[indent[HoldComplete[arg1], lvl, spacesPerIndent, "ArgumentOfRule" -> True, filterIndentOptions[opts]], " :>"],
            indent[HoldComplete[arg2], lvl + 1, spacesPerIndent, "ArgumentOfRule" -> True, filterIndentOptions[opts]]
        }
        ,
        {indentLiteral[e, lvl, spacesPerIndent, filterIndentOptions[opts]]}
    ]
  ]
  
(* If *)
indent[e:HoldComplete[If[arg1_, args___]], lvl_, spacesPerIndent_, opts:OptionsPattern[]] :=
  With[{literal = indentLiteral[e, 0, spacesPerIndent, filterIndentOptions[opts]]},
    If [StringLength[literal] > MaxLineLength[HoldComplete[If]],
        With[{testLiteral = indentLiteral[HoldComplete[arg1], 0, spacesPerIndent, filterIndentOptions[opts]]},
            (* Is the condition being test large enough to warrant its own line? *)
            If [StringLength[testLiteral] > 60 ||
                (IntegerQ[OptionValue["AlwaysIndentToLevel"]] && lvl <= OptionValue["AlwaysIndentToLevel"]),
                {
                    indentLiteral[If, lvl, spacesPerIndent, filterIndentOptions[opts]]<>"[",
                    indentArgs[HoldComplete[arg1, args], lvl, spacesPerIndent, "ArgNum" -> 1, "CommaOnNewLine" -> True, filterIndentOptions[opts]],
                    indentString["]", lvl, spacesPerIndent, filterIndentOptions[opts]]
                }
                ,
                {
                    indentLiteral[If, lvl, spacesPerIndent, filterIndentOptions[opts]]<>"["<>indentLiteral[HoldComplete[arg1], 0, spacesPerIndent, filterIndentOptions[opts]]<>",",
                    indentArgs[HoldComplete[args], lvl, spacesPerIndent, "ArgNum" -> 2, "CommaOnNewLine" -> True, filterIndentOptions[opts]],
                    indentString["]", lvl, spacesPerIndent, filterIndentOptions[opts]]
                }
            ]
        ]
        ,
        {indentLiteral[e, lvl, spacesPerIndent, filterIndentOptions[opts]]}
    ]
  ]
 
 (* CompoundExpression, one arg, with Null at the end. *)
 indent[e:HoldComplete[CompoundExpression[args_, Null]], lvl_, spacesPerIndent_, opts:OptionsPattern[]] :=
    With[{e2 = ReplacePart[e, {1, 0} -> HoldComplete][[1]]},
        (* Drop the Null. *)
        linesToString[indent[Drop[e2, -1], lvl, spacesPerIndent, filterIndentOptions[opts]]] <> ";"
    ]
 
(* CompoundExpression *)
indent[e:HoldComplete[CompoundExpression[args__]], lvl_, spacesPerIndent_, opts:OptionsPattern[]] :=
    Module[{trailingNull = "", lines},
        With[{e2 = ReplacePart[e, {1, 0} -> HoldComplete][[1]] /.
                       (* Get rid of the trailing Null if it's there. *)
                       HoldComplete[args2___, Null] :> (trailingNull = ";"; HoldComplete[args2])},
            With[{tmp =
                    linesToString[
                        lines = 
                        indentArgs[
                            e2,
                            lvl,
                            spacesPerIndent,
                            "ArgNum" -> 1,
                            "CommaOnNewLine" -> False,
                            "Delimiter" -> ";",
                            filterIndentOptions[opts]
                        ]
                    ] <> trailingNull
                 },
                 
                If [lvl > OptionValue["LevelAdjustment"] && Length[lines] > 1,
                    With[{spaces = StringJoin[Flatten[Table[Table[" ", {spacesPerIndent}], {lvl}]]]},
                        Join[
                            {spaces <> "("},
                            {tmp},
                            {spaces <> ")"}
                        ]
                    ]
                    ,
                    tmp
                ]
            ]
        ]
    ]

indent[e:HoldComplete[arg_], lvl_, spacesPerIndent_, opts:OptionsPattern[]] :=
    If [Head[arg] === Association,
        With[{literal = indentLiteral[e, 0, spacesPerIndent, filterIndentOptions[opts]]},
            If [StringLength[literal] > MaxLineLength[HoldComplete[f]] ||
                (IntegerQ[OptionValue["AlwaysIndentToLevel"]] && lvl <= OptionValue["AlwaysIndentToLevel"]),
                {
                    indentString["Association[", lvl, spacesPerIndent, filterIndentOptions[opts]],
                    indentArgs[
                        HoldComplete @@
                            (* If you don't call Normal on the association, HoldComplete @@ args
                               munges the data for some reason. *)
                            Normal[arg], lvl, spacesPerIndent, filterIndentOptions[opts]],
                    indentString["]", lvl, spacesPerIndent, filterIndentOptions[opts]]
                }
                ,
                {indentLiteral[e, lvl, spacesPerIndent, filterIndentOptions[opts]]}
            ]
        ]
        ,
        {indentLiteral[e, lvl, spacesPerIndent, filterIndentOptions[opts]]}
    ]

Clear[indentArgs];
Options[indentArgs] =
Join[
    Options[indent],
    {
        "ArgNum" -> 1,                 (*< argument number. *)
        "CommaOnNewLine" -> False,     (*< should the comma be on its own line? *)
        "Delimiter" -> ","             (*< character to use as the argument delimiter. *)
    }
];
indentArgs[e:HoldComplete[firstArg_, remainingArgs___], lvl_, spacesPerIndent_, opts:OptionsPattern[]] :=
    If [Length[e] > 1,
        If [TrueQ[OptionValue["CommaOnNewLine"]] && OptionValue["ArgNum"] > 1,
            (* The commas that separate the arguments of If should be on their own line. (Except for the first one) *)
            {indent[HoldComplete[firstArg], lvl+1, spacesPerIndent, filterIndentOptions[opts]], indentString[OptionValue["Delimiter"], lvl+1, spacesPerIndent, filterIndentOptions[opts]], indentArgs[HoldComplete[remainingArgs], lvl, spacesPerIndent, "ArgNum" -> OptionValue["ArgNum"] + 1, "CommaOnNewLine" -> OptionValue["CommaOnNewLine"], "Delimiter" -> OptionValue["Delimiter"], filterIndentOptions[opts]]}
            ,
            {appendToLast[indent[HoldComplete[firstArg], lvl+1, spacesPerIndent, filterIndentOptions[opts]], OptionValue["Delimiter"]], indentArgs[HoldComplete[remainingArgs], lvl, spacesPerIndent, "ArgNum" -> OptionValue["ArgNum"] + 1, "CommaOnNewLine" -> OptionValue["CommaOnNewLine"], "Delimiter" -> OptionValue["Delimiter"], filterIndentOptions[opts]]}
        ]
        ,
        indent[HoldComplete[firstArg], lvl+1, spacesPerIndent, filterIndentOptions[opts]]
    ]
    
indent[e_, lvl_, spacesPerIndent_, opts:OptionsPattern[]] := indentLiteral[e, lvl, spacesPerIndent, filterIndentOptions[opts]] 
  
Clear[indentLiteral];
Options[indentLiteral] = Options[indent];
indentLiteral[e : HoldComplete[_], lvl_, spacesPerIndent_, opts:OptionsPattern[]] :=
    Module[{heldSymbol, contexts},
        
        Attributes[heldSymbol] = {HoldAllComplete};
        
        (* We'll strip domain grammar contexts here for readability.
           This global variable can be set to True to avoid this. The reason
           I'm not going to bother with an option right now is that it would
           be too much plumbing passing it around. *)
        If [!TrueQ[OptionValue["RemoveContexts"]],
            Flatten[Table[Table[" ", {spacesPerIndent}], {lvl}]] <> heldLiteralToString[e, opts]
            ,
            contexts =
                Flatten[Reap[
                    Replace[
                        e,
                        s_Symbol :>
                            With[{res =
                                  (
                                      If [Context[s] =!= "System`", Sow[Context[s], "Context"]]
                                  )
                                 },
                                res /; True
                            ],
                        {1, Infinity},
                        Heads -> True
                    ],
                    "Context"
                ][[2]], 1];

            Block[{$ContextPath = Join[contexts, {"System`"}]},
               Flatten[Table[Table[" ", {spacesPerIndent}], {lvl}]] <> heldLiteralToString[e, opts]
            ]
        ]
    ]
    
Options[heldLiteralToString] = Options[indent];
heldLiteralToString[e:HoldComplete[_], OptionsPattern[]] :=
    Module[{},
        (* Remove the "HoldComplete[" and "]" from around the string. *)
        StringTake[
            If [TrueQ[OptionValue["FullFormStrings"]],
                toInputFormWithFullFormStrings[e]
                ,
                ToString[e, InputForm]
            ],
            {14, -2}
        ]
    ]
    
(*!
    \function toInputFormWithFullFormStrings
    
    \calltable
        toInputForm[e] '' converts an expression to InputForm, taking special care to first convert strings to FullForm. This is useful so that things like \[Rule] get rendered as "\[Rule]", and not as a non-ASCII character. Why is that useful? Because if we're going to write the reuslts in Indent2 to a file, then we'd like it to be ASCII so that it can be edited properly in Workbench, etc, and our standards for .m files are that they should be in ASCII. InputForm on its own would convert \[Rule] to "->", which wouldn't round trip properly to and from file.
    
    Examples:
    
    toInputFormWithFullFormStrings[{"a", "b", "\[Rule]"}] === "{a, b, \"\\[Rule]\"}"

    \related 'heldLiteralToString
    
    \maintainer danielb
*)
toInputFormWithFullFormStrings[e_] :=
    Module[{},
        ToString[e /. s_String :> FullForm[s]]
    ]

indentLiteral[e_, lvl_, spacesPerIndent_, OptionsPattern[]] :=
    Flatten[Table[Table[" ", {spacesPerIndent}], {lvl}]] <> ToString[e, InputForm]
indentString[str_, lvl_, spacesPerIndent_, OptionsPattern[]] :=
    Flatten[Table[Table[" ", {spacesPerIndent}], {lvl}]] <> str
    
appendToLast[list_, suffix_String] :=
    MapAt[# <> suffix &, Flatten[{list}], -1]
	
(*!
	\function ReplaceHeldExpressions
	
	\calltable
		ReplaceHeldExpressions[expr, replacementRules, pattern] '' like Replace, this function replaces occurrences in expr of the given replacement rules. However, the left-hand-sides of the replacement rules are to be wrapped in HoldComplete, so that they can be things that would otherwise evaluate. The sub-expressions to consider replacing are specified via a third argument, which specifies a pattern.

	Examples:
	
	ReplaceHeldExpressions[
		HoldComplete[1 + 1, 2 + 2, 3 + 3],
		{HoldComplete[1 + 1] -> "replaced"},
		_Plus
	]

	===

	HoldComplete[HoldComplete["replaced", 2 + 2, 3 + 3]]

	\maintainer danielb
*) 
PackageScope["ReplaceHeldExpressions"]
ReplaceHeldExpressions[expr_, replacementRules_List, pattern_] :=
	Module[{heldExpr = HoldComplete[expr], temporaryHoldComplete},
		
		Attributes[temporaryHoldComplete] = {HoldAllComplete};

		(* Process replacement rules one by one. *)
		Function[{replacementRule},
			   
				replacementRule /. (Rule | RuleDelayed)[HoldComplete[lhs_], rhs_] :>
					(
					(* Make the replacements for the current replacement rule. *)
					heldExpr =
						Replace[
							heldExpr,
							binding:pattern :>
								(
								With[{replaceRes =
										(
										If [HoldComplete[binding] === HoldComplete[lhs],
											rhs
											,
											(* Whoops, this didn't match the current replacement
											   rule. Put it back, wrapped in something that
											   will prevent it from evaluating, and we'll
											   remove the temporary wrapper below. *)
											temporaryHoldComplete[binding]
										]
										)
									 },
									 
									 replaceRes /; True
								]
								),
							Infinity
						];
					)
					
		   ] /@ replacementRules;
		   
		(* Temove any temporary hold wrappers that we added. *)
		ReleaseHold[
			Replace[heldExpr, temporaryHoldComplete[inner_] :> inner, Infinity]
		]
	]
	
(*!
	\function HeldListToListOfHeld
	
	\calltable
		HeldListToListOfHeld[heldList_] '' given a held List (ie. HoldComplete[List[a, b, c, ...]]), convert it into list of held things. (ie. List[HoldComplete[a], HoldComplete[b], HoldComplete[c], ...])
	
	\maintainer danielb
*)
PackageScope["HeldListToListOfHeld"]
HeldListToListOfHeld[heldList_] :=
	(
	Replace[
		List @@ (HoldComplete /@ (HoldComplete @@@ heldList)[[1]])
		,
		(* If one of the items was already surrounded by HoldComplete,
		   remove the extra one we added. *)
		HoldComplete[HoldComplete[e_]] :> HoldComplete[e]
		,
		2
	]
	);

(*!
	\function ListOfHeldToHeldList
	
	\calltable
		ListOfHeldToHeldList[listOfHeld_] '' given a List of eld items (ie. List[HoldComplete[a], HoldComplete[b], HoldComplete[c], ...]), converts it into a held list of things. (ie. HoldComplete[List[a, b, c, ...]])
	
	\maintainer danielb
*)
PackageScope["ListOfHeldToHeldList"]
ListOfHeldToHeldList[listOfHeld_] := Replace[HoldComplete[listOfHeld], HoldComplete[x_] :> x, {2}];

(*!
	\function TemporaryFile
	
	\calltable
		TemporaryFile[] '' returns the name of a new temporary file created in the $TemporaryDirectory.
	
	Examples:
	
	TemporaryFile[] === "E:\\Users\\Daniel\\AppData\\Local\\Temp\\m-1b608483-cdec-4a13-9fbd-0f07a03c856d"
	
	\maintainer danielb
*)
PackageScope["TemporaryFile"]

PackageScope["WriteFileIndented"]

PackageScope["SortAssociations"]

PackageScope["NormalizeTestOutput"]
Options[TemporaryFile] =
{
	"Extension" -> None,					(*< The file extension to use. *)
	"Directory" -> Automatic				(*< The directory in which to create the temporary files. If Automatic, then $TemporaryDirectory is used. *)
};
TemporaryFile[OptionsPattern[]] :=
	Module[{
			tempFile,
			ext =
				If [OptionValue["Extension"] =!= None,
					If [!StringTake[OptionValue["Extension"], 1] === ".",
						".",
						""
					] <> OptionValue["Extension"]
					,
					""
				]
		   },
		
		tempFile = Close[OpenWrite[]];
		
		If [OptionValue["Directory"] =!= Automatic && OptionValue["Directory"] =!= $TemporaryDirectory,
			With[{path = FileNameJoin[{OptionValue["Directory"], FileNameTake[tempFile, -1] <> ext}]},
				RenameFile[
					tempFile,
					path
				];
				path
			]
			,
			If [ext =!= "",
			   With[{path = tempFile <> ext},
				   RenameFile[tempFile, path];
				   path
			   ]
			   ,
			   tempFile
			]
		]
	]

(*!
	\function WriteFileIndented
	
	\calltable
		WriteFileIndented[expr, file] '' writes a file to disk indented for better readability.

	Examples:
    
    WithTemporaryFiles[{a = ""}, WriteFileIndented[Range[1, 40], a]; Import[a, "Text"]]

    ===

    "{\n    1,\n    2,\n    3,\n    4,\n    5,\n    6,\n    7,\n    8,\n    9,\n    10,\n    11,\n    12,\n    13,\n    14,\n    15,\n    16,\n    17,\n    18,\n    19,\n    20,\n    21,\n    22,\n    23,\n    24,\n    25,\n    26,\n    27,\n    28,\n    29,\n    30,\n    31,\n    32,\n    33,\n    34,\n    35,\n    36,\n    37,\n    38,\n    39,\n    40\n}"

    Unit tests: WriteFileIndented.mt

    \maintainer danielb
*)
WriteFileIndented[expr_, file_] := Export[file, Indent2[expr], "Text"]

(*!
	\function SortAssociations
	
	\calltable
		SortAssociations[e] '' sorts the keys of associations so that order is predictable. Useful for tests.

	Examples:
    
    SortAssociations[{Association["b" -> 1, "a" -> 2], Association["d" -> 3, "c" -> 3]}]

    ===

    {<|"a" -> 2, "b" -> 1|>, <|"c" -> 3, "d" -> 3|>}

    Unit tests: SortAssociations.mt

    \maintainer danielb
*)
SortAssociations[e_] :=
	Block[{},
		e /. assoc_Association :>
			Association[Sort[Normal[assoc]]]
	];

(*!
	\function NormalizeTestOutput
	
	\calltable
		NormalizeTestOutput[e] '' modifies an expression in an attempt to normalize it to be the same on different systems. For example, replacing time zones.

	Examples:
    
    NormalizeTestOutput[
        DateObject[{2016, 5, 12}, TimeObject[{0, 0, 0.}, TimeZone -> -4.], TimeZone -> -4.]
    ]

    ===

    DateObject[{2016, 5, 12}, TimeObject[{0, 0, 0.}, TimeZone -> 0.], TimeZone -> 0.]

    Unit tests: NormalizeTestOutput.mt

    \maintainer danielb
*)
NormalizeTestOutput[e_] :=
	Block[{},
		Replace[
			SortAssociations[e],
			{
				(* To avoid expression diff formatting issues *)
				c:ContentObject[args___] :> "ContentObject"[args],
				d:DateObject[{_, _, _, __}, ___] :> TimeZoneConvert[d, 0.],
				d:DateObject[{_, _, _}, _TimeObject, ___] :> TimeZoneConvert[d, 0.]
			},
			{0, Infinity},
			Heads -> True
		]
	];
