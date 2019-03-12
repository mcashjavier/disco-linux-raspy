BeginPackage["CloudObject`"]

System`GenerateHTTPResponse;
System`ResponseForm;

Begin["`Private`"]

(* Developer riccardod, carlob *)

Unprotect[GenerateHTTPResponse]
SetAttributes[GenerateHTTPResponse, HoldAllComplete]

Unprotect[{AutoRefreshed, Delayed, ExportForm, ResponseForm, HTTPRedirect, HTTPResponse, HTTPErrorResponse}]


(* Delayed export spec parser *)

SetAttributes[export, HoldAllComplete];

export[body_, fmt:Except[_?ListQ]] := export[body, {fmt, Inherited}];
export[body_, {fmt_?StringQ, rules:Repeated[_Rule|_RuleDelayed]}] := export[body, {{fmt, rules}, Inherited}];
export[body_, {None|_Missing|False, rfmt_}] := export[body, {rfmt}];
export[body_, {fmt:Except[_?ListQ], rfmt_}] := export[ExportForm[body, fmt], {rfmt}];
export[body_, {{fmt__}, rfmt_}] := export[ExportForm[body, fmt], {rfmt}];
export[body_, {rfmt:Except[_?ListQ]}] := ResponseForm[body, rfmt];
export[body_, {{rfmt__}}] := ResponseForm[body, rfmt];

(* $Failed should display a nice error page *)

Unprotect[Failure]
Failure /: GenerateHTTPResponse[f:Failure[_String, _?AssociationQ]] := HTTPResponse[
    f["Message"],
    <|"StatusCode" -> 500, "ContentType" -> "text/plain"|>
]
Protect[Failure]

(* Grammar rule special behaviour *)

Unprotect[GrammarRules];
GrammarRules /: GenerateHTTPResponse[g_GrammarRules] /; ! TrueQ[$CloudEvaluation] := (
    Message[GenerateHTTPResponse::onlycloud, GrammarRules];
    GenerateHTTPResponse @ HTTPErrorResponse[500]
)
Protect[GrammarRules];


AutoRefreshed /: GenerateHTTPResponse[AutoRefreshed[body_, tspec:Except[_Rule|_RuleDelayed]:3600, fmt:Except[_Rule|_RuleDelayed]:"WL", ___]] := 
    GenerateHTTPResponse[Delayed[body, fmt]]

Delayed /: GenerateHTTPResponse[Delayed[body_, format:Except[_Rule|_RuleDelayed]:Automatic, opt:OptionsPattern[Delayed]]] :=
    With[
        {response = GenerateHTTPResponse[export[body, format]]},
        Replace[
            {OptionValue[Delayed, {opt}, UpdateInterval], response}, {
                {Infinity|None|Automatic|False|_Missing, expr_} :> 
                    expr,
                {refresh_, res:HTTPResponse[inner_, meta_, rest___]} :> 
                    HTTPResponse[
                        inner,
                        Append[
                            meta, 
                            "Headers" -> Join[
                                res["Headers"], 
                                {"Refresh" -> ToString[refresh]}
                            ]
                        ],
                        rest
                    ],
                _ :> $Failed (* This should never happen*)
            }
        ]
    ]
   
(* HTTPRedirect to HTTPResponse *)

HTTPErrorResponse /: GenerateHTTPResponse[res:HTTPErrorResponse[code_Integer]] := 
    HTTPResponse[
        TemplateApply[
            File["Templates/HTTPErrorResponse.html"], <|
                "StatusCode" -> code, 
                "StatusCodeDescription" -> res["StatusCodeDescription"]
            |>
        ],
        <|"StatusCode" -> code|>
    ]

HTTPRedirect /: GenerateHTTPResponse[HTTPRedirect[url:_CloudObject|_URL, rest___]] :=
    GenerateHTTPResponse @ HTTPRedirect[First[url], rest];

HTTPRedirect /: GenerateHTTPResponse[HTTPRedirect[url_?StringQ, meta:_Rule|_RuleDelayed|{RepeatedNull[_Rule|_RuleDelayed]}]] := 
    GenerateHTTPResponse @ HTTPRedirect[url, <|meta|>];

HTTPRedirect /: GenerateHTTPResponse[HTTPRedirect[url:_?StringQ:"/", meta:_?AssociationQ:<||>]] := 
    HTTPResponse[
        "Redirecting...",
        Append[
            meta, {
                "Headers" -> Join[
                    Replace[
                        Lookup[meta, "Headers", None], {
                            None|Null|Missing|_Missing -> {},
                            a_Association?AssociationQ :> Normal[Delete[a, "Location"]],
                            l_List :> DeleteCases[l, (Rule|RuleDelayed)["Location", _], {1}],
                            (Rule|RuleDelayed)["Location", _] -> Sequence[],
                            any_ :> {any}
                        }
                    ], 
                    {"Location" -> url}
                ],
                "StatusCode" -> Lookup[meta, "StatusCode", 301]
            }
        ]
    ];

(*  A valid HTTPResponse will just stay as is *)

(*  An HTTPResponse with something inside will continue the recursion, 
    but metadata will won and will be preserved.
    Sample Usage: 
        HTTPResponse[ExportForm[<|"Success" -> False|>, "JSON"], "StatusCode" -> 500] 

    It must reset any ExportForm was defined previously
        This is done by Block *)

HTTPResponse /: GenerateHTTPResponse[res:HTTPResponse[_, meta_?AssociationQ, ___]] :=
    Block[
        {$exportFormat = None, $responseForm = None, $responseKeys = All}, 
		res["Encode"]
    ]

(*  ExportForm is using Block.
    Nested ExportForm will override each other until the last iteration is done.
    The inner ExportForm with this system always wins. *)

$exportFormat  = "WL"

ExportForm /: GenerateHTTPResponse[ExportForm[body_, Inherited|{Inherited, ___}, ___]] := 
    GenerateHTTPResponse[body]

ExportForm /: GenerateHTTPResponse[ExportForm[body_, opt__]] := Block[
    {$exportFormat = opt},
    GenerateHTTPResponse[body]
]

makeExportForm[body_] := 
    With[
        {exported = applyExportForm[body, $exportFormat]},
        Switch[
            First[exported],
            $bodyPattern,
            HTTPResponse @@ exported,
            _HTTPResponse|_ExportForm|_ResponseForm,
            GenerateHTTPResponse[HTTPResponse @@ exported], 
            _,
            HTTPResponse[
                StringJoin[
                    "Failed to export to ", 
                    ToString[First[Flatten[{$exportFormat}]]],
                    " for input:\n", 
                    ToString[body, InputForm]
                ], <|
                    "ContentType" -> "text/plain", 
                    "Headers" -> {"Content-Disposition" -> "inline"},
                    "StatusCode" -> 500
                |>
            ]
        ]
    ]

(* Starting response form logic *)

SetAttributes[ResponseForm, HoldFirst];

responseJSON = ReplaceAll[{
    None|_Missing -> Null,
    any:_HoldForm|_Hold|_MessageName :> Block[{}, ToString[Unevaluated[any], InputForm] /; True] 
}]

responseXML[result_] := 
    XMLElement["evaluation-data", 
        Map[
            #1 -> ToString[result[#1]] &, {
            "Success", 
            "FailureType", 
            "StatusCode", 
            "InputString", 
            "Timing", 
            "AbsoluteTiming"
            }
        ], {
            XMLElement[
                "Result", {}, 
                {result["Result"]}
            ],
            XMLElement[
                "OutputLog", {}, 
                Map[XMLElement["OutputLogEvent", {}, {ToString[#, InputForm]}] &, result["OutputLog"]]
            ],
            XMLElement[
                "Messages", {}, 
                Map[XMLElement["Message", {}, {ToString[#, InputForm]}] &, result["Messages"]]
            ],
            XMLElement[
                "MessagesText", {}, 
                Map[XMLElement["MessagesText", {}, {#}] &, result["MessagesText"]]
            ],
            XMLElement[
                "MessagesExpressions", {}, 
                Map[XMLElement["MessageExpression", {}, {ToString[#, InputForm]}] &, result["MessagesExpressions"]]
            ]
        }
    ]

makeResponseForm[evalData_] /; MatchQ[$responseForm, None|Inherited|_Missing|False] := evalData["Result"]
makeResponseForm[evalData_] := GenerateHTTPResponse @ Replace[{  
    (* Uppercasing the format for a quick match in Replace *)
    If[StringQ[$responseForm], ToUpperCase[$responseForm], $responseForm],
    (* Quick and dirty implementation of third arg of ResponseForm *)
    If[$responseKeys === All, Identity, KeyTake[$responseKeys]] @ <|
        "StatusCode" -> evalData["Result"]["StatusCode"],
        "Success" -> evalData["Result"]["StatusCode"] < 400 && evalData["Success"],
        (* Removing extra keys *)
        KeyDrop[evalData, {"Result", "Success", "Messages", "MessagesText", "MessagesExpressions"}],
        (* Joining messages with existing messages *)
        Thread[
            Rule[
                {"Messages", "MessagesText", "MessagesExpressions"},
                MapThread[
                    Join,
                    Lookup[
                        {evalData, CloudObject`$EvaluationParameters}, 
                        {"Messages", "MessagesText", "MessagesExpressions"}, 
                        {}
                    ]
                ]
            ]
        ],
        (* Summing Timings *)
        Thread[
            Rule[
                {"Timing", "AbsoluteTiming"},
                MapThread[
                    Plus,
                    Lookup[
                        {evalData, CloudObject`$EvaluationParameters}, 
                        {"Timing", "AbsoluteTiming"}, 
                        0
                    ]
                ]
            ]
        ],
        (* In case the result is binary then we encode it base64. *)
        "Result" -> If[
            evalData["Result"]["BinaryFormatQ"],
            ExportString[evalData["Result"]["Body"], {"Base64", "String"}],
            evalData["Result"]["Body"]
        ],
        "ResultMeta" -> evalData["Result"]["Meta"]
    |>}, {
        {"XML", result_} :> 
            ExportForm[responseXML[Evaluate[result]], "XML"],
        {"JSON"|"RawJSON", result_} :> 
            ExportForm[responseJSON[Evaluate[result]], "JSON"],
        {None|Null|_Missing|Automatic|"WL"|"STRING"|"TEXT", result_} :> 
            ExportForm[result, "WL"],
        {"HTML"|"HTMLFRAGMENT", result_} :> 
            ExportForm[
                ResponseForm[result["Result"], $responseForm, $responseKeys, <|"EvaluationData" -> result|>], 
                "HTMLFragment", 
                "FullDocument"->True
            ],
        {_?StringQ, result_} :> 
            HTTPResponse[
                "ResponseForm " <> $responseForm <> " is not JSON, WL, XML, or HTML", <|
                    "ContentType" -> "text/plain", 
                    "StatusCode" -> 500,
                    "Headers" -> {"Content-Disposition" -> "inline"}
                |>
            ],
        {any_, result_} :> 
            ExportForm[result, any]
    }
]

(*  ResponseForm is only setting a global variable that the user at some point requested it 
    At the end of the recursion, after exiting from Block it will be applyed *)

$responseForm = None;
$innerCall    = False;
$responseKeys = All;

ResponseForm /: GenerateHTTPResponse[ResponseForm[expr_, format_:"WL", keys_:All]] := (
    $responseForm = Replace[format, Inherited :> $responseForm];
    $responseKeys = Replace[keys, Inherited :> $responseKeys];
    GenerateHTTPResponse[expr]
)

(* this is the very last call *)
GenerateHTTPResponse[obj_] /; TrueQ[$innerCall] := makeExportForm[obj]
GenerateHTTPResponse[held_, req_:Inherited] /; ! TrueQ[$innerCall] := 
    With[
        {expr = held, request = Replace[
            req, {
                Automatic|Inherited :> Replace[$HTTPRequest, {a_?AssociationQ :> a, _ :> <||>}],
                None|_Missing :> <||>,
                any:_String|_URL :> URLParse[any, All],
                any:{RepeatedNull[_Rule|_RuleDelayed]}|_Rule|_RuleDelayed :> <|any|>,
                any_?AssociationQ :> any,
                _ :> (
                    Message[GenerateHTTPResponse::nvldrequest, req];
                    <||>
                )
            }
        ]},
        Block[{
            $EvaluationEnvironment = "WebAPI",
            $innerCall    = True,
            $responseForm = None,
            $responseKeys = All,
            $exportFormDefault = "HTML",
            $exportFormSoundDefault = "HTML",
            $HTTPRequest = request
            },
            Internal`InheritedBlock[
                {GenerateHTTPResponse},
                ClearAttributes[GenerateHTTPResponse, HoldAllComplete];
                makeResponseForm[EvaluationData[GenerateHTTPResponse[expr, request]]]
            ]
        ]
    ]    

(* Two argument version *)

GenerateHTTPResponse[expr_, _] /; $innerCall :=
    GenerateHTTPResponse[expr]

(* no args are creating a blank response *)

GenerateHTTPResponse[] := HTTPResponse[]

GenerateHTTPResponse[args__] := (
    ArgumentCountQ[GenerateHTTPResponse, Length[{args}], 1, 2];
    $Failed
)

Unprotect[{AutoRefreshed, Delayed, ExportForm, ResponseForm, HTTPRedirect, HTTPResponse, HTTPErrorResponse}]


End[]

EndPackage[]
