BeginPackage["CloudObject`"]

Begin["`Private`"]

(* URLFetch *)

Unprotect[CloudObject];

CloudObject /: HoldPattern[URLFetch][CloudObject[url_, ___], opts:OptionsPattern[]] :=
    If[TrueQ[$CloudConnected], authenticatedURLFetch, URLFetch][url, opts]

CloudObject /: HoldPattern[URLFetch][CloudObject[url_, ___], arg_, opts:OptionsPattern[]] :=
    If[TrueQ[$CloudConnected], authenticatedURLFetch, URLFetch][url, arg, opts]

CloudObject /: HoldPattern[URLFetchAsynchronous][CloudObject[url_, ___], arg_, opts:OptionsPattern[]] :=
    If[TrueQ[$CloudConnected], authenticatedURLFetchAsynchronous, URLFetchAsynchronous][url, arg, opts]

CloudObject /: HoldPattern[URLFetch][CloudObject[url_, ___], args__, opts:OptionsPattern[]] := (
	Message[URLFetch::argb, URLFetch, 1 + Length[{args}],0,1]; $Failed
)

CloudObject /: HoldPattern[URLSave][CloudObject[url_, ___], file:Automatic|_String|_File:Automatic, content:(_String|_List|All):"Content", opts___?OptionQ] :=
    If[TrueQ[$CloudConnected], authenticatedURLSave, URLSave][url, file, content, opts]

Protect[CloudObject];

End[]

EndPackage[]
