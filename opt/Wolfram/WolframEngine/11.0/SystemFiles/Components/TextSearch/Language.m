Package["TextSearch`"]

PackageScope["ClassifyLanguageOfContentObjects"]

PackageScope["LongestContentObjectFieldWithStringValues"]

PackageScope["SampleTextFromAssociations"]
(*!
	\function ClassifyLanguageOfContentObjects
	
	\calltable
		ClassifyLanguageOfContentObjects[files] '' given a list of files, returns the language that they were authored in. If there isn't sufficien certainty, Automatic is returned, which indicates that a specific language wasn't decided upon.
		
	Example:

    ClassifyLanguageOfContentObjects[
        {
            ContentObject[
                Association[
                    "Title" -> "Wolfram Language",
                    "Plaintext" -> "The Wolfram Language attempts to automate as much as possible"
                ]
            ],
            ContentObject[
                Association[
                    "Title" -> "C++",
                    "Plaintext" ->
                        "C++ can be used to create high performance software while allowing good object oriented features"
                ]
            ]
        }
    ]

    ===

    "English"

    Unit tests: ClassifyLanguageOfFiles.mt

    \maintainer danielb
*)
Clear[ClassifyLanguageOfContentObjects];
ClassifyLanguageOfContentObjects[contentObjects:{___ContentObject}] :=
	Block[{sampleText, classifyRes, langEntity, probability, field},
		
		field = LongestContentObjectFieldWithStringValues[contentObjects];
		If [field === None, Return[Automatic]];
		
		sampleText = SampleTextFromAssociations[contentObjects[[All, 1]], field];
		
		classifyRes = Classify["Language", sampleText, {"TopProbabilities", 1}];
		
		If [MatchQ[classifyRes, {_ -> _}],
			langEntity = classifyRes[[1, 1]];
			probability = classifyRes[[1, 2]];
			
			If [probability >= 0.99,
				With[{lang = langEntity[[2]]},
					If [MemberQ[$SupportedLanguages, lang],
						lang
						,
						(* If we can detect the language, but it's not one of the languages
						   supported by Lucene, then just return Automatic. *)
						Automatic
					]
				]
				,
				Automatic
			]
			,
			Automatic
		]
	];
	
ClassifyLanguageOfContentObjects[assocs:{___Association}] :=
	ClassifyLanguageOfContentObjects[ContentObject /@ assocs]

(*!
	\function stringTakeAtMost
	
	\calltable
		stringTakeAtMost[str, n] '' take at most 'n' characters from the string. Put a small amount of effort into making the resultant string end on a word boundary.

	Examples:
    
    stringTakeAtMost["This is a test of the function", 16] === "This is a test"

    Unit tests: stringTakeAtMost.mt

    \maintainer danielb
*)
stringTakeAtMost[str_, n_] :=
	Block[{res, pos},
		pos = n + 1;
		pos = Min[pos, StringLength[str]];
		While[pos > 0 && !StringMatchQ[StringTake[str, {pos}], WhitespaceCharacter],
			--pos;
		];
		(* If we had to eat more than 50 characters to find the previous whitespace,
		   then abandon our simple attempt at finding a word boundary, because this
		   implies that, who knows, maybe we ended up eating most or all of the string
		   looking for a word boundary, etc. *)
		If [pos > n - 50 && pos > 0,
			StringTake[str, pos - 1]
			,
			StringTake[str, Min[n, StringLength[str]]]
		]
	];

(*!
	\function LongestContentObjectFieldWithStringValues
	
	\calltable
		LongestContentObjectFieldWithStringValues[contentObjects] '' given a list of ContentObjects, return the name of the field that has _String values, and for which those strings are longer than other such fields.

	Examples:
    
    LongestContentObjectFieldWithStringValues[
        {
            ContentObject[
                Association[
                    "Title" -> "Wolfram Language",
                    "Plaintext" -> "The Wolfram Language attempts to automate as much as possible"
                ]
            ],
            ContentObject[
                Association[
                    "Title" -> "C++",
                    "Plaintext" ->
                        "C++ can be used to create high performance software while allowing good object oriented features"
                ]
            ]
        }
    ]

    ===

    "Plaintext"

    Unit tests: LongestContentObjectFieldWithStringValues.mt

    \maintainer danielb
*)
Clear[LongestContentObjectFieldWithStringValues];
LongestContentObjectFieldWithStringValues[contentObjects:{__ContentObject}] :=
	BlockRandom[
	Block[{numItemsToConsider = 4, randomSample, stringKeys, averageFieldLengths},
		
		(* So that tests are reproduceable. *)
		SeedRandom[0];
		
		(* When the content is located in files, then favor the "Plaintext" field. *)
		If [!FreeQ[contentObjects, KeyValuePattern["Plaintext" -> Text[File[_String]]]],
			Return["Plaintext"];
		];
		
		(* Rather than considering all content objects, since there
		   might be thousands, just consider a random sample. *)
		randomSample = RandomSample[contentObjects, Min[4, Length[contentObjects]]];
		
		(* Drop ContentObject wrapper *)
		randomSample = randomSample[[All, 1]];
		
		(* The keys that have String values *)
		stringKeys = Keys[Select[Merge[randomSample, Join], MatchQ[#, {___, _String | {___, _String, ___}, ___}] &]];
		
		(* No keys have String values. *)
		If [stringKeys === {}, Return[None]];
		
		(* Compute the average string length for each field. *)
		(* ex. <|"Title" -> 9.5, "Plaintext" -> 78.5|> *)
		averageFieldLengths =
		AssociationThread[
			stringKeys,
			N[
				Mean /@
				(* We flatten because a value might actually be a list of values. *)
				StringLength[Cases[#, _String] & /@ (Flatten /@ Transpose[Lookup[randomSample, stringKeys, ""]])]
			]
		];
		
		(* Return the name of the longest field. *)
		Keys[Select[averageFieldLengths, # === Max[averageFieldLengths] &]][[1]]
	]
	]

LongestContentObjectFieldWithStringValues[{}] := None

(*!
	\function SampleTextFromAssociations
	
	\calltable
		SampleTextFromAssociations[assocs, key] '' given a list of associations and a key of interest that has String values, returns a random sample of text corresponding to that key. Text from different associations are concatenated with commas.

	Examples:
    
    SampleTextFromAssociations[
        {
            Association[
                "Title" -> "Wolfram Language",
                "Plaintext" -> "The Wolfram Language attempts to automate as much as possible"
            ],
            Association[
                "Title" -> "C++",
                "Plaintext" ->
                    "C++ can be used to create high performance software while allowing good object oriented features"
            ]
        },
        "Plaintext"
    ]

    ===

    "C++ can be used to create high performance software while allowing good object oriented features, The Wolfram Language attempts to automate as much as possible"

    Unit tests: SampleTextFromAssociations.mt

    \maintainer danielb
*)
SampleTextFromAssociations[assocs_, key_] :=
	BlockRandom[
	Block[{numFilesToConsider = 4, sample, txtFiles, sampleText},
		
		SeedRandom[0];
		
		If [Length[assocs] === 0, Return[""]];
		
		sample = RandomSample[assocs, Min[numFilesToConsider, Length[assocs]]];
		
		sample = Cases[Lookup[sample, key], _String | Text[_String] | Text[File[_String]]];

		(* If the content objects point to files, then read the files. *)
		sample = Replace[sample, {Text[File[path_String]] :> Import[path, {"Text", "Plaintext"}], Text[str_String] :> str}, {1}];

		StringJoin[
			Riffle[
				Function[{str},
					stringTakeAtMost[str, 1000]
				] /@ sample,
				", "
			]
		]
	]
	]