(* Wolfram Language Package *)

(Unprotect[#]; Clear[#])& /@ {System`ResourceObject,System`ResourceSubmissionObject}

BeginPackage["ResourceSystemClient`"]

System`ResourceObject
System`ResourceSubmissionObject

Begin["`Private`"] (* Begin Private Context *) 

(* ResourceObject *)
System`ResourceObject[str_String]:=System`ResourceObject[usableResourceInfo[resourceInfo[str]]]/;MemberQ[$loadedResources, str]

System`ResourceObject[str_String]:=With[{res=Catch[loadResource[str]]},
	If[AssociationQ[res],
		System`ResourceObject[usableResourceInfo[res]]
		,
		$Failed
	]
]

(resource_System`ResourceObject)[str_String, rest___]:=ResourceSystemClient`ResourceInformation[resource, Association["Property"->str,"Parameters"->{rest}]]
(resource_System`ResourceObject)[All]:=With[{info=sortBasicInfo[getResourceInfo[resourceObjectID[resource]]]},
	If[AssociationQ[info],info,$Failed]]

System`ResourceObject[info_Association]:=Catch[System`ResourceObject[usableResourceInfo[standardizecustomResourceInfo[info]]]]/;!filledResourceQ[info]

System`ResourceObject[info_Association]:=Catch[autoloadResource[info]]/;autoloadResourceQ[info]
System`ResourceObject[ro:HoldPattern[System`ResourceObject][args___]]:=ro
System`ResourceObject[expr:Except[_Association]]:=(Message[ResourceObject::noas,expr];$Failed)

System`ResourceObject/:
MakeBoxes[resource:System`ResourceObject[_Association], form:StandardForm|TraditionalForm] := (
Catch[standardResourceObjectBoxes[resource, form]])

standardResourceObjectBoxes[resource_, form_]:=With[{id=Quiet[resourceObjectID[resource]]},
	If[StringQ[id],
        	With[{info=If[AssociationQ[#],#,First[resource]]&@resourceInfo[id]},
        		With[{rtype=Lookup[info,"ResourceType",None]},
        			loadResourceType[rtype];
		            BoxForm`ArrangeSummaryBox[
		                        (* Head *)System`ResourceObject, 
		                        (* Interpretation *)resource, 
		                        (* Icon *)resourceIcon[rtype], 
		                        (* Column or Grid *)
		                        {
		                        BoxForm`SummaryItem[{"Name: ", Lookup[info,"Name",Missing["NotAvailable"]]}],
		                        BoxForm`SummaryItem[{"Type: ", rtype}],
		                        resourceSystemDescriptionSummaryItem[rtype,Lookup[info,"Description",Missing["NotAvailable"]]]
		                        }
		                        ,
		                        (* Plus Box Column or Grid *)
		                        repositoryBelowFoldItems[rtype,id, info]
		                        ,
		            form]
        		]
        	]
        	,
        	ToBoxes[$Failed]
	]
]


repositoryBelowFoldItems[_,id_, info_]:={
	BoxForm`SummaryItem[{"Categories: ", Short[Row[Lookup[info,"Categories",{}],","]]}],
	BoxForm`SummaryItem[{"ContentTypes: ", Short[Row[Lookup[info,"ContentTypes",{}],","]]}],
	BoxForm`SummaryItem[{"Keywords: ", Short[Row[Lookup[info,"Keywords",{}],","]]}],
	BoxForm`SummaryItem[{"UUID: ", id}],
	BoxForm`SummaryItem[{"Version:", Lookup[info,"Version",None]}]
	}

resourceSystemDescriptionSummaryItem[_,str_String]:=BoxForm`SummaryItem[{"Description: ", 
		str
 	}]/;Snippet[str,1]===str

resourceSystemDescriptionSummaryItem[_,str_String]:=DynamicModule[{len=1},
	BoxForm`SummaryItem[{"Description: ", 
		Button[Dynamic[Snippet[str, len]], len = Ceiling[len*1.5], Appearance -> None,BaseStyle -> {}]
 	}]
]
resourceSystemDescriptionSummaryItem[_,expr_]:=Nothing

fallbackResourceIcon=formatresourceicon[Graphics[]]

resourceicon[file_]:=With[{img=Import[file]},
	Switch[Head[img],
		Graphics,
		formatresourceicon[img],
		List,
		formatresourceicon[img[[1]]],
		_,
		fallbackResourceIcon
	]		
]/;FileExistsQ[file]

resourceIcon[_]=fallbackResourceIcon;

formatresourceicon[gr_Graphics]:=Graphics[gr[[1]],
  AspectRatio -> 1, Axes -> False, Background -> None, Frame -> None, 
 FrameTicks -> None, 
  ImageSize -> {Automatic, Dynamic[3.5*(CurrentValue["FontCapHeight"]/
             AbsoluteCurrentValue[Magnification]), 
    ImageSizeCache -> {45., {0., 9.}}]}]

filledResourceQ[info_Association]:=KeyExistsQ[info,"UUID"]
filledResourceQ[_]:=False

autoloadResourceQ[info_Association]:=TrueQ[Lookup[info,"Autoload"]]
autoloadResourceQ[_]:=False

System`ResourceSubmissionObject[str_String]:=Catch[importSubmission[str]]

System`ResourceSubmissionObject[info_]:=$Failed/;!Quiet[KeyExistsQ[info, "Name"]]

sub_System`ResourceSubmissionObject[req_,args___]:=Catch[submissionRequest[First[sub],req,{args}]]

System`ResourceSubmissionObject/:
MakeBoxes[resource_System`ResourceSubmissionObject, form:StandardForm|TraditionalForm] := (
Catch[With[{info=First[resource]},
            With[{id=Lookup[info,"UUID"],
            	rtype=Lookup[info,"ResourceType"],
            	name=Lookup[info,"Name"]},
                BoxForm`ArrangeSummaryBox[
                            (* Head *)System`ResourceSubmissionObject, 
                            (* Interpretation *)resource, 
                            (* Icon *)formatSubmissionIcon[resourceIcon[rtype]], 
                            (* Column or Grid *)
                            {
                            BoxForm`SummaryItem[{"Name: ", name}],
                            BoxForm`SummaryItem[{"Type: ", rtype}],
                            BoxForm`SummaryItem[{"SubmissionID: ", Lookup[info,"SubmissionID",id]}]
                            }
                            ,
                            (* Plus Box Column or Grid *)
                            {
                            BoxForm`SummaryItem[{"UUID: ", id}],
                            BoxForm`SummaryItem[{"SubmissionDate: ", Lookup[info,"SubmissionDate"]}]
                            }, 
                form]
            ]
        ]])

formatSubmissionIcon[icon_Graphics]:=Graphics[Replace[icon[[1]], RGBColor[___] :> RGBColor[.4, .4, .4],  Infinity], 
	AspectRatio -> 1, Axes -> False, Background -> GrayLevel[0.9], Frame -> True,FrameStyle -> GrayLevel[0.6], FrameTicks -> None, 
 	ImageSize -> {Automatic, Dynamic[3.5*(CurrentValue["FontCapHeight"]/AbsoluteCurrentValue[Magnification]), ImageSizeCache -> {45., {0., 9.}}]}]

End[] (* End Private Context *)

EndPackage[]

SetAttributes[{ResourceObject,ResourceSubmissionObject},
   {ReadProtected, Protected}
];