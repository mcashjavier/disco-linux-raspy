Package["TextSearch`"]

PackageScope["TokenizeQueryString"]

PackageScope["QuotedSpans"]

PackageScope["CutString"]

PackageScope["Tokizer"]

PackageScope["CreateChunks"]

PackageScope["ParseQueryString"]

Attributes[xPrint] = {HoldAllComplete};

(*!
	\function TokenizeQueryString
	
	\calltable
		TokenizeQueryString[str] '' tokenizes the string.

	Examples:
    
    TokenizeQueryString["just testing"] === {"just", "testing"}

    Unit tests: TokenizeQueryString.mt

    \maintainer danielb
*)
TokenizeQueryString[str_] :=
	Block[{chunks},
		
		chunks = {str};
		
		(* Escaped sections. *)
		chunks = CreateChunks[chunks, "\\" ~~ _, StringTake[#, -1] &];
		
		(* Double quoted sections. *)
		chunks = CreateChunks[chunks, "\"" ~~ Shortest[__] ~~ "\"", quoted[StringTake[#, {2, -2}]] &];
		
		(* Fields *)
		(* Must come before numbers. *)
		chunks = CreateChunks[chunks, (LetterCharacter ~~ (LetterCharacter | DigitCharacter)...) ~~ WhitespaceCharacter... ~~ ":", field[StringDrop[#, -1]] &];
		
		(* Numbers *)
		chunks = CreateChunks[chunks, (DigitCharacter.. ~~ "." ~~ DigitCharacter..) | DigitCharacter.., ToExpression];
		
		Flatten[If [StringQ[#], Tokizer[#], #] & /@ chunks]
	];

$allLetters = 
"ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz\:0144\[CapitalAGrave]\
\[CapitalAAcute]\[CapitalAHat]\[CapitalATilde]\[CapitalADoubleDot]\
\[CapitalARing]\[CapitalAE]\[CapitalCCedilla]\[CapitalEGrave]\
\[CapitalEAcute]\[CapitalEHat]\[CapitalEDoubleDot]\[CapitalIGrave]\
\[CapitalIAcute]\[CapitalIHat]\[CapitalIDoubleDot]\[CapitalEth]\
\[CapitalNTilde]\[CapitalOGrave]\[CapitalOAcute]\[CapitalOHat]\
\[CapitalOTilde]\[CapitalODoubleDot]\[CapitalOSlash]\[CapitalUGrave]\
\[CapitalUAcute]\[CapitalUHat]\[CapitalUDoubleDot]\[CapitalYAcute]\
\[CapitalThorn]\[SZ]\[AGrave]\[AAcute]\[AHat]\[ATilde]\[ADoubleDot]\
\[ARing]\[AE]\[CCedilla]\[EGrave]\[EAcute]\[EHat]\[EDoubleDot]\
\[IGrave]\[IAcute]\[IHat]\[IDoubleDot]\[Eth]\[NTilde]\[OGrave]\
\[OAcute]\[OHat]\[OTilde]\[ODoubleDot]\[OSlash]\[UGrave]\[UAcute]\
\[UHat]\[UDoubleDot]\[YAcute]\[Thorn]\[YDoubleDot]\[CapitalABar]\
\[ABar]\[CapitalACup]\[ACup]\[CapitalCAcute]\[CAcute]\[CapitalCHacek]\
\[CHacek]\[CapitalEBar]\[EBar]\[CapitalECup]\[ECup]\[CapitalICup]\
\[ICup]\[DotlessI]\[CapitalLSlash]\[LSlash]\[CapitalODoubleAcute]\
\[ODoubleAcute]\[CapitalSHacek]\[SHacek]\[CapitalUDoubleAcute]\
\[UDoubleAcute]\[CapitalAlpha]\[CapitalBeta]\[CapitalGamma]\
\[CapitalDelta]\[CapitalEpsilon]\[CapitalZeta]\[CapitalEta]\
\[CapitalTheta]\[CapitalIota]\[CapitalKappa]\[CapitalLambda]\
\[CapitalMu]\[CapitalNu]\[CapitalXi]\[CapitalOmicron]\[CapitalPi]\
\[CapitalRho]\[CapitalSigma]\[CapitalTau]\[CapitalUpsilon]\
\[CapitalPhi]\[CapitalChi]\[CapitalPsi]\[CapitalOmega]\[Alpha]\[Beta]\
\[Gamma]\[Delta]\[CurlyEpsilon]\[Zeta]\[Eta]\[Theta]\[Iota]\[Kappa]\
\[Lambda]\[Mu]\[Nu]\[Xi]\[Omicron]\[Pi]\[Rho]\[FinalSigma]\
\[Sigma]\[Tau]\[Upsilon]\[CurlyPhi]\[Chi]\[Psi]\[Omega]\[CurlyTheta]\
\[CurlyCapitalUpsilon]\[Phi]\[CurlyPi]\[CapitalStigma]\[Stigma]\
\[CapitalDigamma]\[Digamma]\[CapitalKoppa]\[Koppa]\[CapitalSampi]\
\[Sampi]\[CurlyKappa]\[CurlyRho]\[Epsilon]\[ScriptG]\[ScriptCapitalH]\
\[GothicCapitalH]\[ScriptCapitalI]\[GothicCapitalI]\[ScriptCapitalL]\
\[ScriptL]\[ScriptCapitalR]\[GothicCapitalR]\[GothicCapitalZ]\
\[ScriptCapitalB]\[GothicCapitalC]\[ScriptE]\[ScriptCapitalE]\
\[ScriptCapitalF]\[ScriptCapitalM]\[ScriptO]\[Aleph]\[Bet]\[Gimel]\
\[Dalet]\[ScriptA]\[ScriptB]\[ScriptC]\[ScriptD]\[ScriptF]\[ScriptH]\
\[ScriptI]\[ScriptJ]\[ScriptK]\[ScriptM]\[ScriptN]\[ScriptP]\
\[ScriptQ]\[ScriptR]\[ScriptS]\[ScriptT]\[ScriptU]\[ScriptV]\
\[ScriptW]\[ScriptX]\[ScriptY]\[ScriptZ]\[GothicA]\[GothicB]\
\[GothicC]\[GothicD]\[GothicE]\[GothicF]\[GothicG]\[GothicH]\
\[GothicI]\[GothicJ]\[GothicK]\[GothicL]\[GothicM]\[GothicN]\
\[GothicO]\[GothicP]\[GothicQ]\[GothicR]\[GothicS]\[GothicT]\
\[GothicU]\[GothicV]\[GothicW]\[GothicX]\[GothicY]\[GothicZ]\
\[DoubleStruckA]\[DoubleStruckB]\[DoubleStruckC]\[DoubleStruckD]\
\[DoubleStruckE]\[DoubleStruckF]\[DoubleStruckG]\[DoubleStruckH]\
\[DoubleStruckI]\[DoubleStruckJ]\[DoubleStruckK]\[DoubleStruckL]\
\[DoubleStruckM]\[DoubleStruckN]\[DoubleStruckO]\[DoubleStruckP]\
\[DoubleStruckQ]\[DoubleStruckR]\[DoubleStruckS]\[DoubleStruckT]\
\[DoubleStruckU]\[DoubleStruckV]\[DoubleStruckW]\[DoubleStruckX]\
\[DoubleStruckY]\[DoubleStruckZ]\[DotlessJ]\[ScriptDotlessI]\
\[ScriptDotlessJ]\[ScriptCapitalA]\[ScriptCapitalC]\[ScriptCapitalD]\
\[ScriptCapitalG]\[ScriptCapitalJ]\[ScriptCapitalK]\[ScriptCapitalN]\
\[ScriptCapitalO]\[ScriptCapitalP]\[ScriptCapitalQ]\[ScriptCapitalS]\
\[ScriptCapitalT]\[ScriptCapitalU]\[ScriptCapitalV]\[ScriptCapitalW]\
\[ScriptCapitalX]\[ScriptCapitalY]\[ScriptCapitalZ]\[GothicCapitalA]\
\[GothicCapitalB]\[GothicCapitalD]\[GothicCapitalE]\[GothicCapitalF]\
\[GothicCapitalG]\[GothicCapitalJ]\[GothicCapitalK]\[GothicCapitalL]\
\[GothicCapitalM]\[GothicCapitalN]\[GothicCapitalO]\[GothicCapitalP]\
\[GothicCapitalQ]\[GothicCapitalS]\[GothicCapitalT]\[GothicCapitalU]\
\[GothicCapitalV]\[GothicCapitalW]\[GothicCapitalX]\[GothicCapitalY]\
\[DoubleStruckCapitalA]\[DoubleStruckCapitalB]\[DoubleStruckCapitalC]\
\[DoubleStruckCapitalD]\[DoubleStruckCapitalE]\[DoubleStruckCapitalF]\
\[DoubleStruckCapitalG]\[DoubleStruckCapitalH]\[DoubleStruckCapitalI]\
\[DoubleStruckCapitalJ]\[DoubleStruckCapitalK]\[DoubleStruckCapitalL]\
\[DoubleStruckCapitalM]\[DoubleStruckCapitalN]\[DoubleStruckCapitalO]\
\[DoubleStruckCapitalP]\[DoubleStruckCapitalQ]\[DoubleStruckCapitalR]\
\[DoubleStruckCapitalS]\[DoubleStruckCapitalT]\[DoubleStruckCapitalU]\
\[DoubleStruckCapitalV]\[DoubleStruckCapitalW]\[DoubleStruckCapitalX]\
\[DoubleStruckCapitalY]\[DoubleStruckCapitalZ]\[FiLigature]\
\[FlLigature]";

$tokenizerPattern =
	RegularExpression[
		"^(?:[" <> $allLetters <>
		(* In Lucene queries, ? and * are wildcard characters that
		   should be treated as parts of words. *)
		"\\?\\*" <>
		"]+|[0-9]+|.)\\s*"
	];

(*!
	\function Tokizer
	
	\calltable
		Tokizer[str] '' splits text into simple tokens.

	Examples:
    
    Tokizer["abc:\"def\""] === {"abc", ":", "\"", "def", "\""}

    Unit tests: Tokizer.mt

    \maintainer danielb
*)
Tokizer[input_] :=
	Block[{remaining, next, word, words},
		
		remaining = input;
		words = {};
		While[True,
			
			If [StringStartsQ[remaining, "\""],
				(* If this is a doubly quoted string, don't split it up. *)
				Return[Append[words, remaining], Block];
			];
			
			next = StringCases[remaining, $tokenizerPattern];
			If [next === {},
				Break[];
				,
				word = next[[1]];
				AppendTo[words, StringTrim[word]];
				remaining = StringDrop[remaining, StringLength[word]];
			];
		];
		Return[words];
	]

(*!
	\function CreateChunks
	
	\calltable
		CreateChunks[chunks, pattern, head] '' splits up the chunks further using the given string pattern, wrapping matches with the given head.

	Examples:
    
    CreateChunks[{"ababa aba def", "ghi"}, "aba", "MATCH"] === {"MATCH"["aba"], "ba", "MATCH"["aba"], "def", "ghi"}

    Unit tests: CreateChunks.mt

    \maintainer danielb
*)
CreateChunks[chunks_, pattern_, head_] :=
	Block[{prevPos},
		Flatten[
			Reap[
				Function[{chunk},
					If [!StringQ[chunk],
						Sow[chunk]
						,
						prevPos = -1;
						Function[{span},
							With[{start = span[[1]], end = span[[2]]},
								If [prevPos != -1 && start =!= prevPos + 1,
									Sow[StringTrim[StringTake[chunk, {prevPos + 1, start - 1}]]]
								];
								If [!MatchQ[start, 0 | StringLength[chunk] + 1],
									Sow[head[StringTrim[StringTake[chunk, {start, end}]]]]
								];
								prevPos = end;
							]
						] /@
							{
								{0, 0},
								Sequence @@ StringPosition[chunk, pattern, Overlaps -> False, IgnoreCase -> True],
								{StringLength[chunk] + 1, StringLength[chunk] + 1}
							}
					]
				] /@ chunks
			][[2]],
			1
		]
	];

(*!
	\function ParseQueryString
	
	\calltable
		ParseQueryString[str] '' parses a lucene query into a symbolic expression.

	Examples:
    
    ParseQueryString["word"] === QString[All, "word"]

    Unit tests: ParseQueryString.mt

    \maintainer danielb
*)
ParseQueryString[queryString_] :=
	Block[{query},
		
		query = TokenizeQueryString[queryString];
		
		xPrint[query // Indent2];
		
		query =	query /.
			{
				{pre___, phrase:quoted[_String], "~", val:(_Integer | _Real), post___} :> {pre, SearchAdjustment[phrase, MaxWordGap -> val], post},
				{pre___, str_String, "~", val:(_Integer | _Real), post___} :> {pre, QFuzzy[str, val], post},
				{pre___, str_String, "~", post___} :> {pre, QFuzzy[str], post}
			};
		
		query =	query /.
			{
				quoted[str_String] :> str
			};
		
		query =
			ReplaceRepeated[
				query
				,
				{
					{pre___, "(", Shortest[inner___], ")", post___} :> {pre, bracketed[{inner}], post},
                    {
                        pre___,
                        "[",
                        a_,
                        (* Case insensitive "TO" *)
                        to_String /; StringMatchQ[to, "TO", IgnoreCase -> True],
                        b_,
                        "]",
                        post___
                    } :>
                        {pre, QRange[a, b, True, True], post},
                    {
                        pre___,
                        "{",
                        a_,
                        (* Case insensitive "TO" *)
                        to_String /; StringMatchQ[to, "TO", IgnoreCase -> True],
                        b_,
                        "}",
                        post___
                    } :>
                        {pre, QRange[a, b, False, False], post}
				}
			];
		
		xPrint[query // Indent2];
		
		query =
			Replace[
				query
				,
				{
					{pre___, f_field, next_, post___} :> {pre, Append[f, next], post}
				}
			];
		
		query =
			ReplaceRepeated[
				query
				,
				{
					{pre___, item_, "^", num_Integer | num_Real, post___} :> {pre, QSearchBoost[item, num], post},
					{pre___, "+", item_, post___} :> {pre, QMust[item], post},
					{pre___, "-" | "NOT", item_, post___} :> {pre, QNot[item], post}
				}
			];
		
		xPrint[query//Indent2];
			
		query =
			ReplaceRepeated[
				query
				,
				{
					{pre___, a_, "AND", b_, post___} :> {pre, QIntersection[a, b], post},
					{pre___, a_, "OR", b_, post___} :> {pre, QUnion[a, b], post}
				}
			];
		
		xPrint[query//Indent2];
		
		query =
			ReplaceRepeated[
				query
				,
				{
					list_List :> If [Length[list] === 1, list[[1]], If [FreeQ[list, _QNot | _QMust, 1], QUnion @@ list, QBooleanQuery @@ list]]
				}
			];
		
		query =
			ReplaceRepeated[
				query
				,
				(* QString *)
				head_[pre___, str_String, post___] /; !MatchQ[head, QString | field] :>
					head[pre, QString[All, str], post]
			];
		
		xPrint[query // Indent2];
		
		query =
			ReplaceRepeated[
				query
				,
				{
					f:field[fieldName_, str_String] :> QString[fieldName, str],
					f:field[fieldName_, range_QRange] :> Prepend[range, fieldName],
					(* Odd. Apparently we turn numeric values into QString? Appears to work, anyway. *)
					f:field[fieldName_, val_Integer | val_Real] :> QString[fieldName, ToString[val]],
					f:field[fieldName_, other:Except[_String | _QRange]] :>
						Replace[other, QString[All, str_] :> QString[fieldName, str], Infinity],
					bracketed[inner_] :> inner,
					(* Don't perform this when there is a single item, since we
					   apparently have a down value that changes QIntersection[a_] :> a. *)
					(QBooleanQuery|QUnion)[items:Repeated[_QMust, {2, Infinity}]] :> QIntersection[Sequence @@ ({items}[[All, 1]])]
				}
			];
		
		xPrint[query // Indent2];
		
		If [StringQ[query],
			query = QString[All, query];
		];
		
		query =
			Replace[
				query
				,
				{
					QString[field_, str_] :>
						If [!StringFreeQ[str, "*" | "?"] && !MatchQ[str, "*" | "?"],
							QWildcard[QString[field, str]]
							,
							QString[field, str]
						]
				}
			];
		
		query
	];