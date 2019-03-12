(* Wolfram Language Package *)

BeginPackage["DataResource`"]

Begin["`Private`"] (* Begin Private Context *) 


$ElementNameLenghtLimit=200;
ResourceSystemClient`Private`respositoryMetadataSchema[$DataResourceType]:=(
	ResourceSystemClient`Private`respositoryMetadataSchema[$DataResourceType]=
	Association[
		"Content"->(With[{expr = #}, Interpreter[Evaluate, (SameQ[expr, #] &)][#]] &),
		"ContentElementAccessType"->Restricted["String", RegularExpression[".{1,50}"]],
		"DefaultContentElement"->Restricted["String", RegularExpression[".{1,"<>ToString[$ElementNameLenghtLimit]<>"}"]],
		"ContentElements"->RepeatingElement[Restricted["String", RegularExpression[".{1,"<>ToString[$ElementNameLenghtLimit]<>"}"]]]
	]
	)



validateParameter[$DataResourceType,"InformationElements",as_]:=as/;AssociationQ[as]
validateParameter[$DataResourceType,"ContentElementFunctions",as_]:=as/;AssociationQ[as]

validateParameter[$DataResourceType,"ContentElementLocations",Automatic]=Automatic;
validateParameter[$DataResourceType,"ContentElementLocations",as_Association]:=validateParameter["DataResource","ContentElementLocations",#]&/@as
validateParameter[$DataResourceType,"ContentElementLocations",co_CloudObject]:=With[{res=setReviewerPermissions[co]},
    If[!ListQ[res],
        Message[ResourceSubmit::appperms,co];
        Throw[$Failed]
    ];
    co
]

End[] (* End Private Context *)

EndPackage[]