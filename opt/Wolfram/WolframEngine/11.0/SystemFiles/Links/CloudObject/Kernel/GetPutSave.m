BeginPackage["CloudObject`"]

System`CloudGet;
System`CloudPut;
System`CloudSave;

CloudObject`GetObjectMetadataFilename;

Begin["`Private`"]

Needs["Iconize`"]

Unprotect[CloudObject];

(* general read/write *)

cleanup[tempfilename_, expr_: Null] := (DeleteCloudObject[tempfilename, Asynchronous->True]; expr)

readObject[obj_CloudObject, head_Symbol : CloudObject] :=
    responseToFile[execute[obj], head]

(* content_ is a list of bytes, as returned by e.g. BinaryReadList *)
writeObject[obj_CloudObject, content_?StringQ, rest___] :=
    writeObject[obj, ToCharacterCode[content], rest]
writeObject[obj_CloudObject, content_, mimetype_,
        permissions_ : Automatic, iconRulesArg_ : None, iconExpr_ : Null, metaInformation_ : {},
        params_ : {}, head_Symbol : CloudObject] :=
    Module[{result, autoIcons, iconRules},
        If[permissions =!= Automatic && invalidPermissionsGroups[permissions],
            Return[$Failed]
        ];
        {autoIcons, iconRules} = normalizeIconRules[iconRulesArg];
        result = responseToString @ execute[obj, Automatic,
            UseUUID -> False, Body -> content, Type -> mimetype,
            Parameters -> Flatten@Join[params, {
                "permissions" -> escapeAndNormalizePermissions[permissions, mimetype, head],
                If[metaInformation === {}, {}, "properties" -> encodeMetaInformation[metaInformation]],
                If[TrueQ[autoIcons], "icons" -> iconSizeString[iconRules], {}] 
            }]];
        If[result === $Failed, Return[$Failed]];
        If[!TrueQ[autoIcons] && iconRules =!= None,
            SetCloudIcons[obj, iconRules, Asynchronous->True, "Content" -> iconExpr,
                "Deployment" -> iconDeploymentType[mimetype, permissions]]
        ];
        obj
    ]

normalizeIconRules[rule_Rule] := normalizeIconRules[{rule}]

normalizeIconRules[Automatic] := {True, $automaticIconRules}

normalizeIconRules[rules:{Rule[_, Automatic]..}] := {True, rules}

normalizeIconRules[rules:{_Rule ..}] := {False, rules}

normalizeIconRules[expr_] := {False, Table[env -> expr, {env, $cloudIconNames}]}

iconSizeString[sizes_List] := 
	(iconSizeString[sizes] = StringJoin[Riffle[Map[First, sizes], ","]])

(*Put*)

Unprotect[CloudPut];

Options[CloudPut] = {SaveDefinitions->False, Permissions->Automatic, IconRules->Automatic, MetaInformation->{}, CloudBase->Automatic};
Options[iCloudPut] = Join[Options[CloudPut], {"Append" -> False}];

iCloudPut[expr_, obj:CloudObject[uri_, objopts:OptionsPattern[CloudObject]], mimetype_String, opts:OptionsPattern[]] :=
    Module[{content, tempfilename,
        iconRules = OptionValue[iCloudPut, {opts, objopts}, IconRules],
        metaInformation = OptionValue[iCloudPut, {opts, objopts}, MetaInformation],
        permissions = OptionValue[iCloudPut, {opts, objopts}, Permissions],
        params = {"append" -> ExportString[TrueQ[OptionValue["Append"]], "JSON"]}
    },
        If[TrueQ[OptionValue[SaveDefinitions]],
        (* save definitions *)
            content = exprToStringBytesWithSaveDefinitions[Unevaluated[expr]],
        (* do not save definitions *)
            tempfilename = CreateTemporary[];
            Block[{$ContextPath={"System`"}, $Context = "System`"}, Put[Unevaluated[expr], tempfilename]];
            content = BinaryReadList[tempfilename];
        ];
        writeObject[obj, content, mimetype, permissions, iconRules, Unevaluated[expr], metaInformation, params]
    ]

$IncludedContexts = {}; (* Block override this with {"CloudObject"} to use CloudEvaluate from within CloudObject`Private` code *)

(*
    neutralContextBlock[expr] evalutes expr without any contexts on the context path,
    to ensure symbols are serialized including their context (except System` symbols).
*)
Attributes[neutralContextBlock] = {HoldFirst};
neutralContextBlock[expr_] := Block[{$ContextPath={"System`"}, $Context="System`"}, expr]

exprToStringWithSaveDefinitions[expr_] :=
    Module[{defs, defsString, exprLine},
        (* This fn is used by the package itself, so make sure the package context
         * is not excluded. *)
        defs = With[{excl = Join[OptionValue[Language`ExtendedFullDefinition, "ExcludedContexts"], {"MailReceiver", "CloudSystem"}]},
            Language`ExtendedFullDefinition[expr,
                "ExcludedContexts" -> Complement[excl, $IncludedContexts]]
        ];
        defsString = If[defs =!= Language`DefinitionList[],
            neutralContextBlock[With[{d = defs},
                (* Language`ExtendedFullDefinition[] can be used as the LHS of an assignment to restore
                 * all definitions. *)
                ToString[Unevaluated[Language`ExtendedFullDefinition[] = d], InputForm,
                	CharacterEncoding -> "PrintableASCII"]
            ]] <> ";\n\n",
        (* else *)
            ""
        ];
        exprLine = neutralContextBlock[ToString[Unevaluated[expr], InputForm, 
        	CharacterEncoding -> "PrintableASCII"]];
        StringTrim[defsString <> exprLine] <> "\n"
    ]

exprToStringBytesWithSaveDefinitions[expr_] := 
    ToCharacterCode[exprToStringWithSaveDefinitions[Unevaluated[expr]], "UTF-8"]

CloudPut[expr_, options : OptionsPattern[]] :=
	Block[{$CloudBase = handleCBase[OptionValue[CloudBase]]},
		CloudPut[Unevaluated[expr], CloudObject[], Sequence @@ FilterRules[{options}, Except[CloudBase]]]
	]

CloudPut[expr_, obj_CloudObject, opts:OptionsPattern[]] :=
    iCloudPut[Unevaluated[expr], obj, expressionMimeType["Expression"], opts]

CloudPut[expr_, uri_String, opts:OptionsPattern[]] :=
	Block[{$CloudBase = handleCBase[OptionValue[CloudBase]]},
		CloudPut[Unevaluated[expr], CloudObject[uri], Sequence @@ FilterRules[{opts}, Except[CloudBase]]]
	]    
    
CloudPut[expr_, URL[dest_String], opts:OptionsPattern[]] := 
	CloudPut[Unevaluated[expr], dest, opts]    

CloudPut[expr_, obj_, opts:OptionsPattern[]]:=
    (Message[CloudPut::invcloudobj, obj];$Failed)

CloudPut[args___] := (ArgumentCountQ[CloudPut,Length[DeleteCases[{args},_Rule,Infinity]],1,2];Null/;False)

CloudObject /: Put[expr_, obj_CloudObject] := CloudPut[Unevaluated[expr], obj]

CloudObject /: PutAppend[expr_, obj_CloudObject] := iCloudPut[Unevaluated[expr], obj, expressionMimeType["Expression"], "Append" -> True]

SetAttributes[CloudPut, {ReadProtected}];
Protect[CloudPut];

(*Save*)

Unprotect[CloudSave];

Options[CloudSave] = {IconRules->Automatic, MetaInformation->{}, Permissions->Automatic};
Attributes[CloudSave] = {HoldFirst};

CloudSave[expr_, obj:CloudObject[uri_, objopts:OptionsPattern[CloudObject]], opts:OptionsPattern[]] :=
    Module[{type, content, tempfilename},
        If[FileExistsQ[obj],
            {tempfilename, type} = readObject[obj, CloudSave];
            If[tempfilename === $Failed, Return[$Failed]],
        (* else *)
            tempfilename = CreateTemporary[]
        ];
        Save[tempfilename, Unevaluated[expr]];
        content = BinaryReadList[tempfilename];
        writeObject[obj, content, expressionMimeType["Expression"],
            OptionValue[CloudSave, {opts, objopts}, Permissions],
            OptionValue[CloudSave, {opts, objopts}, IconRules], Unevaluated[expr],
            OptionValue[CloudSave, {opts, objopts}, MetaInformation],
            {},
            CloudSave
        ]
    ]

CloudSave[expr_, uri_String, opts:OptionsPattern[]] := CloudSave[expr, CloudObject[uri], opts]

CloudSave[expr_, URL[uri_String], opts:OptionsPattern[]] := CloudSave[expr, uri, opts]

CloudSave[expr_, opts:OptionsPattern[]] := CloudSave[expr, CloudObject[], opts]

CloudSave[args___] := (ArgumentCountQ[CloudSave,Length[DeleteCases[{args},_Rule,Infinity]],1,2];Null/;False)

CloudObject /: Save[obj_CloudObject, expr_] := CloudSave[expr, obj]

SetAttributes[CloudSave, {ReadProtected}];
Protect[CloudSave];

(*Get*)

Unprotect[CloudGet];

Options[CloudGet] = {CloudBase->Automatic};

bundleMimeTypeQ[mimetype_] :=
    StringQ[mimetype] &&
        StringMatchQ[mimetype, "application/vnd.wolfram.bundle" ~~ ___]

CloudGet[co_CloudObject, opts:OptionsPattern[]] :=
    Module[{tempfilename, mimetype},
        {tempfilename, mimetype} = readObject[co, CloudGet];
        Which[
            tempfilename === $Failed, $Failed,

            mimetype === "inode/directory", Message[Get::noopen, co]; $Failed,

            bundleMimeTypeQ[mimetype], CloudGet[FileNameJoin[{co, ".bundle"}]],

            True, cleanup[tempfilename, Block[{$CharacterEncoding = "UTF-8"},
                Get[tempfilename]
            ]]
        ]
    ];

CloudGet[uri_String, opts:OptionsPattern[]] :=
    Block[{$CloudBase = handleCBase[OptionValue[CloudBase]]},
        CloudGet[CloudObject[uri]]
    ]

CloudGet[URL[uri_String], opts:OptionsPattern[]] := CloudGet[uri]

CloudObject /: Get[co_CloudObject] := CloudGet[co]

CloudGet[args___] := (ArgumentCountQ[CloudSave,Length[DeleteCases[{args},_Rule,Infinity]],1,1];Null/;False)

SetAttributes[CloudGet,{ReadProtected}];
Protect[CloudGet];

Protect[CloudObject];

(* From Jan, plus a tiny amount of error checking, and also allow CloudObject or UUID. *)
GetObjectMetadataFilename[obj_CloudObject, subpath___String] :=
    Replace[getCloudAndUUID[obj], {
                {_, uuid_} :> GetObjectMetadataFilename[uuid, subpath],
                _ :> $Failed
    }];

GetObjectMetadataFilename[uuid_String?UUIDQ, subpath___String] :=
    (* TODO (Jan?): does this need to use the $HomeDirectory of the CloudObject owner, not the caller?
       Or is that tautological? *)
    FileNameJoin[{$HomeDirectory, ".Objects", "metadata", StringTake[uuid, 3], uuid, subpath}];

GetObjectMetadataFilename[___] := $Failed;

End[]

EndPackage[]
