
BeginPackage["StreamLink`"]

StreamLinkBegin
SetupPreemptiveLink

$MainLinkSwitchPre
$MainLinkSwitchPost

$PreemptiveLinkSwitchPre
$PreemptiveLinkSwitchPost


Begin["`Private`"]

(* Clients of this package, like CloudSystem.m, can override these definitions of the LinkSwitchPre/Post functions, for example to
   call SetFrontEnd as appropriate.
*)
$MainLinkSwitchPre = Function[{link}, MathLink`SetFrontEnd[Null]]
$MainLinkSwitchPost = Function[{prev}, MathLink`RestoreFrontEnd[prev]]

$PreemptiveLinkSwitchPre = Function[{link}, MathLink`SetFrontEnd[Null]; MathLink`SetMessageLink[Null] (* preemptive links should set this to Null if they SetFrontEnd[Null] *)]
$PreemptiveLinkSwitchPost = Function[{prev}, MathLink`RestoreFrontEnd[prev]]


(* This must be global, otherwise the library will be unloaded when the StreamLinkBegin Module exits. *)
createStreamLinkName
(* These are globals so they can be loaded during StreamLinkBegin, otherwise later loading could fail when cloud sandbox has been set up (the path to the lib is no longer visible).  *)
createPreemptiveLink
logDepthFunction


StreamLinkBegin[] :=
	Module[{name, link},
		createStreamLinkName = LibraryFunctionLoad["StreamLink", "getLinkName",  {}, "UTF8String"];
        createPreemptiveLink = LibraryFunctionLoad["StreamLink", "createPreemptiveLink", {Integer}, "UTF8String"];
        logDepthFunction = LibraryFunctionLoad["StreamLink", "setLogDepth", {Integer}, Integer];
		name = createStreamLinkName[];
		link = LinkConnect[name, LinkProtocol->"IntraProcess"];
		$ParentLink = link;
		(* By setting $ParentLink, we have entered our link into the kernel's link-sharing system, but now we need to modify some of the default behavior.
		   This is needed to keep $FrontEnd setup correctly in the presence of switching back and forth between the main and preemptive links.
		*)
		MathLink`AddSharingLink[link, MathLink`LinkSwitchPre :> $MainLinkSwitchPre, MathLink`LinkSwitchPost :> $MainLinkSwitchPost, MathLink`Terminating -> True]
	]


SetupPreemptiveLink[portNumber_Integer] :=
    Module[{preemptiveName, preemptiveLink},
        preemptiveName = createPreemptiveLink[portNumber];
        If[preemptiveName =!= "$Failed",
            preemptiveLink = LinkConnect[preemptiveName, LinkProtocol->"IntraProcess"];
            MathLink`AddSharingLink[preemptiveLink, MathLink`AllowPreemptive -> True,
                    MathLink`SendInputNamePacket -> False, MathLink`Daemon -> True, MathLink`ImmediateStart -> False,
                    MathLink`LinkSwitchPre :> $PreemptiveLinkSwitchPre, MathLink`LinkSwitchPost :> $PreemptiveLinkSwitchPost];
            preemptiveLink,
        (* else *)
            $Failed
        ]
    ]


(* Private function to control logging. Can only be called after StreamLinkBegin[]. Use -1 for no logging at all, 0 for absolutely minimal, 1 to only see the heads of expressions, etc. 
   Returns the previous log depth.
*)
setLogDepth[logDepth_Integer] := logDepthFunction[logDepth]
 

End[]

EndPackage[]
