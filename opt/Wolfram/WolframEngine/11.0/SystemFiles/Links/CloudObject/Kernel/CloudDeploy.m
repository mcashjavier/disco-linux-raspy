BeginPackage["CloudObject`"]

System`CloudDeploy;
CloudObject`CloudDeployActiveQ;

Begin["`Private`"]

(* Dependencies *)
System`EmbeddedHTML;
System`GrammarRules;
System`CloudBase;
System`SourceLink;
Hold[System`$SourceLink];
Hold[System`$CloudEvaluation];

SetAttributes[{headDeployFormat, expressionMimeType}, HoldFirst];

headDeployFormat[APIFunction] = "API";
headDeployFormat[Delayed|Dynamic] = "Computation";
headDeployFormat[FormFunction] = "Form";
headDeployFormat[ScheduledTask] = "Task";
headDeployFormat[GrammarRules] = "Grammar";
headDeployFormat[expr_] := SymbolName[expr];

expressionMimeType["CloudCDF"] := "application/vnd.wolfram.notebook";
expressionMimeType["HTMLCloudCDF"] := "application/vnd.wolfram.cloudcdf.html";
expressionMimeType["NBElement"] := "application/vnd.wolfram.notebook.element";
expressionMimeType["Expression"|Expression] := "application/vnd.wolfram.expression";
expressionMimeType["Notebook"|Notebook] := "application/mathematica";
expressionMimeType["ExternalBundle"|ExternalBundle] := "application/vnd.wolfram.bundle";
expressionMimeType[expr_String] := "application/vnd.wolfram.expression." <> ToLowerCase[expr];
expressionMimeType[expr_[___]] := expressionMimeType[expr];
expressionMimeType[expr_] := "application/vnd.wolfram.expression." <> ToLowerCase[headDeployFormat[expr]];

CloudDeployActiveQ[HoldPattern[Alternatives[
    _Delayed,
    _FormFunction,
    _System`FormPage, (* System should be removed after FormPage will be in the build *)
    _APIFunction,
    _ScheduledTask,
    _URLDispatcher,
    _GrammarRules
    ]]] := True;
    
CloudDeployActiveQ[_] := False;


Unprotect[CloudDeploy];

$SourceLink = Automatic;

Options[CloudDeploy] = 
	{Permissions->Automatic, IconRules->Automatic, MetaInformation->{}, CloudBase->Automatic, SourceLink->Automatic};

CloudDeploy[bundle:ExternalBundle[bundleElements_List], dest_CloudObject, opts:OptionsPattern[]] :=
    Module[{optsNew, elementObjects, bundleexpr},
        optsNew = Sequence @@ FilterRules[{opts}, Except[CloudBase]];
        (* Step 1 of 3. Ensure the bundle directory exists *)
        Replace[
            Quiet[createBundle[dest], CloudDeploy::notparam],
            HTTPError[___] :> Return[$Failed]
        ];

        (* Step 2 of 3. deploy the individual elements *)
        elementObjects = $lastBundleDeployResult = Map[
            deployBundleElement[dest, #, optsNew]&,
            bundleElements
        ];
        If[Position[elementObjects, $Failed, Infinity, 1] =!= {},
            Return[$Failed]
        ];

        (* Step 3 of 3. deploy the ExternalBundle content *)
        bundleexpr = ExternalBundle[elementObjects];
        CloudPut[bundleexpr, FileNameJoin[{dest, ".bundle"}], optsNew];

        dest
    ];

assocToList[assoc_Association] := Map[assoc[#]&, Keys[assoc]] (* workaround for certain Mathematica builds where Normal[_Association] normalizes deeply *)

deployBundleElement[dir_CloudObject, name_String -> elements_List, opts:OptionsPattern[]] := Replace[
    CreateDirectory[FileNameJoin[{dir, name}]],
    {
        subdir_CloudObject :>
        name -> Map[deployBundleElement[subdir, #, opts]&, elements],
        _ :> $Failed
    }
]

deployBundleElement[dir_CloudObject, name_String -> direlements_Association, opts:OptionsPattern[]] :=
    deployBundleElement[dir, name -> assocToList[direlements], opts]

deployBundleElement[dir_CloudObject, name_String -> expr_, opts:OptionsPattern[]] := Replace[
    CloudDeploy[expr, FileNameJoin[{dir, name}], opts],
    {
        obj_CloudObject :> name -> obj,
        _ :> $Failed
    }
]

CloudDeploy[ExternalBundle[elements_Association], dest_CloudObject, opts:OptionsPattern[]] :=
    CloudDeploy[ExternalBundle[assocToList[elements]], dest, Sequence @@ FilterRules[{opts}, Except[CloudBase]]]

CloudDeploy[HoldPattern[grammar_GrammarRules], obj_CloudObject, opts:OptionsPattern[]] :=
With[{newGrammar = Semantic`PLIDump`addDefinitions[grammar]},
    Which[
        MatchQ[newGrammar, Except[_GrammarRules]],
        (* presumably a Message was already issued *)
        $Failed
        ,
        TrueQ[$CloudEvaluation],
        Semantic`PLIDump`iGrammarDeploy[newGrammar, obj, Sequence @@ FilterRules[{opts}, Except[CloudBase]]]
        ,
        True,
        internalCloudEvaluate[CloudDeploy[newGrammar, obj, Sequence @@ FilterRules[{opts}, Except[CloudBase]]]]
    ]
]

CloudDeploy[expr_?CloudDeployActiveQ, obj_CloudObject, opts:OptionsPattern[]] :=
    iCloudPut[Unevaluated[expr], obj, expressionMimeType[expr], SaveDefinitions -> True, Sequence @@ FilterRules[{opts}, Except[CloudBase]]]

CloudDeploy[ExportForm[expr_, format_, rest___], obj_CloudObject, opts:OptionsPattern[]] :=
    CloudExport[Unevaluated[expr], format, obj, rest, Sequence @@ FilterRules[{opts}, Except[CloudBase]]]
    
CloudDeploy[expr:HTTPResponse[$bodyPattern, _?AssociationQ, ___]|HTTPRedirect[_String|_URL|_CloudObject, ___], obj_CloudObject, opts:OptionsPattern[]] :=
    iCloudPut[GenerateHTTPResponse[expr], obj, "application/vnd.wolfram.httpresponse", Sequence @@ FilterRules[{opts}, Except[CloudBase]]]
    
CloudDeploy[expr_, obj_CloudObject, opts:OptionsPattern[]] :=
    CloudDeploy[ExportForm[expr], obj, Sequence @@ FilterRules[{opts}, Except[CloudBase]]]

CloudDeploy[expr_, uri_String, opts:OptionsPattern[]] :=
    Module[{cbase, obj},
    	cbase = handleCBase[OptionValue[CloudBase]];
    	obj = Block[{$CloudBase = cbase}, CloudObject[uri]];
        cloudDeployPreprocess[cbase, Unevaluated[expr], obj, Sequence @@ FilterRules[{opts}, Except[CloudBase]]]
    ]
    
CloudDeploy[expr_, URL[uri_String], opts:OptionsPattern[]] := CloudDeploy[Unevaluated[expr], uri, opts]  
    
CloudDeploy[expr_, opts:OptionsPattern[]] :=
    Module[{cbase, obj},
    	cbase = handleCBase[OptionValue[CloudBase]];
    	obj = Block[{$CloudBase = cbase}, CloudObject[]];
        cloudDeployPreprocess[cbase, Unevaluated[expr], obj, Sequence @@ FilterRules[{opts}, Except[CloudBase]]]
    ]

(* this needs to follow the CloudDeploy[expr_, opts:OptionsPattern[]] definition *)
CloudDeploy[expr_, dest_, opts:OptionsPattern[]]:=
    (Message[CloudDeploy::invcloudobj, dest]; $Failed)
    
CloudDeploy[args___] := (ArgumentCountQ[CloudDeploy,Length[DeleteCases[{args},_Rule,Infinity]],1,2];Null/;False)

Options[cloudDeployPreprocess] = FilterRules[Options[CloudDeploy], Except[CloudBase]]

cloudDeployPreprocess[cbase_, expr_, obj_CloudObject, opts:OptionsPattern[]] :=
    Module[ {src, meta, metaNew, optsNew},
        src = handleSourceLink[OptionValue[SourceLink]];        
        If[ src===None
        	,
            optsNew = FilterRules[{opts}, Except[CloudBase|SourceLink]]
            ,
            meta = Replace[OptionValue[MetaInformation], info_Rule->{info}];
            metaNew = Join[meta,{"__SourceLink" -> normalizeSourceLink[src]}];
            optsNew = Join[FilterRules[{opts}, Except[CloudBase|SourceLink|MetaInformation]], {MetaInformation->metaNew}]         
        ];
        Block[{$CloudBase = cbase}, CloudDeploy[Unevaluated[expr], obj, optsNew]]
    ]
	

handleCBase[Automatic] := $CloudBase
handleCBase[cbase_String] := Replace[cbase, $cloudBaseAbbreviations]
handleCBase[URL[cbase_String]] := handleCBase[cbase]
handleCBase[cbase_] := (Message[CloudObject::invbase, cbase]; $CloudBase)

handleSourceLink[Automatic] := handleSLink[$SourceLink]
handleSourceLink[None] := None
handleSourceLink[src_CloudObject] := src
handleSourceLink[src_] := (Message[CloudDeploy::invsrc, src]; None)  

handleSLink[Automatic] := $EvaluationCloudObject
handleSLink[src_] := src

normalizeSourceLink[CloudObject[url_String]] := url
normalizeSourceLink[None] := None

SetAttributes[CloudDeploy, {ReadProtected}];
Protect[CloudDeploy];

createBundle[dest_CloudObject, mimeTypeExtension_String:""] :=
    responseCheck[execute[dest, Automatic, UseUUID -> False,
        Type -> "application/vnd.wolfram.bundle"<>mimeTypeExtension], CloudDeploy, dest];

End[]

EndPackage[]
