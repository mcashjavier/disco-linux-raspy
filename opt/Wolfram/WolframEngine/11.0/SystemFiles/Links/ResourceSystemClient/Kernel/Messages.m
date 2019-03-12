(* Wolfram Language Package *)

BeginPackage["ResourceSystemClient`"]
(* Exported symbols added here with SymbolName::usage *)  

Begin["`Private`"] (* Begin Private Context *) 

ResourceSystemClient`MyResources::cloudc="Connect to the Wolfram cloud using CloudConnect to retrieve your marketplace resources";

ResourceSystemClient`MyResources::unavail="The Wolfram Resource System service is not responding, please verify your internet connection and try again later.";

ResourceSystemClient`MyResources::apierr="The Wolfram Resource System service gave the following message: `1`";

ResourceExecute::invapp="`1` is not a valid resource."

ResourceObject::accfun="`1` is not supported by the specified ResourceObject."
ResourceObject::noas="The argument `1` should be the name or id of an existing resource or an Association defining a new resource."
ResourceObject::crname="The ResourceObject information must include a Name."
ResourceObject::unkbase="The resource system location for this $CloudBase is unknown. Please set $ResourceSystemRequestBase."

ResourceObject::invro="The ResourceObject should contain an Association."
ResourceObject::invcont="\"Content\" and `1` can not both be used to specify the content of a location."
ResourceObject::twocont="The default \"Content\" element is defined twice."
ResourceObject::twodef="Only one of \"Content\", \"ContentLocation\", and \"DefaultContentElement\" can be used."
ResourceObject::invas="The value of `1` should be an Association."
ResourceObject::elemcon="The element(s) `1` have multiple definitions."
ResourceObject::nocont="The ResourceObject does not contain any content."
ResourceObject::invdefa="The default element `1` is not one of the elements provided."
ResourceObject::invloc="ContentLocation should be a LocalObject or CloudObject."
ResourceObject::notf="The specified ResourceObject could not be found."
ResourceObject::cloudc="You must connect to the Wolfram cloud to access the resource."

ResourceSystemClient`ResourceDownload::exists="The resource `1` is already downloaded."
ResourceObject::exists="There is already a stored version of the resource."
ResourceSubmit::exists="That resource already exists in the Wolfram Resource System."
ResourceSubmit::cloudc="You must connect to the Wolfram cloud to submit the resource."
ResourceSearch::cloudc="You must connect to the Wolfram cloud to search for resources."
ResourceAcquire::cloudc="You must connect to the Wolfram cloud to acquire for resources."
ResourceSystemClient`ResourceExecute::nodata="The data was not found in the local copy of the resource."
ResourceObject::nocdep="The resource contains local content that may not be available in the deployment."
ResourceObject::nocdepe="The resource element `1` contains local content that may not be available in the deployment."

ResourceSystemClient`ResourceExecute::invparam="The argument `1` should be an Association or list of rules defining parameters for the app."

ResourceData::invelem="`1` is not an element of the resource."
ResourceData::invelem1="The element `1` is not available."

ResourceObject::unkpar="The argument `1` is not a known property."
ResourceObject::unkrt="`1` is not a supported ResourceType."

ResourceSubmit::noro="ResourceSubmit takes a ResourceObject."
ResourceSubmit::invparam="The value given for `1` is invalid."
ResourceSubmit::invparam2="Some of the specified options are not valid."
ResourceSubmit::invinfo="The information value `1` should be a short string."
ResourceSubmit::invcon="The provided ContentLocations could not be used."
ResourceSubmit::noncont="The resource must include content to be submitted."
ResourceSystemClient`ResourceExecute::invapp="The content of the app, `1`, is not properly formatted."
ResourceSubmit::invrt="The resource type `1` is not valid. Try DataResource."
ResourceSubmit::appperms="The permissions of `1` must allow the marketplace reviewer to read the contents."
ResourceSubmit::invprop="The submission includes invalid properties."
ResourceSubmit::enbdf="The example notebook could not be used."

ResourceSearch::invcount="The count `1` should be an integer greater than 0."
ResourceSearch::invquery="The query `1` should be a string."

ResourceObject::apierr="`1`"
ResourceSystemClient`ResourceDownload::apierr="`1`"
ResourceSystemClient`ResourceExecute::apierr="`1`"
ResourceAcquire::apierr="`1`"
ResourceSystemClient`MyResources::apierr="`1`"
ResourceSearch::apierr="`1`"
ResourceSubmissionObject::apierr="`1`"

ResourceObject::apiwarn="`1`"
ResourceSystemClient`ResourceDownload::apiwarn="`1`"
ResourceSystemClient`ResourceExecute::apiwarn="`1`"
ResourceSystemClient`MyResources::apiwarn="`1`"
ResourceSearch::apiwarn="`1`"

End[] (* End Private Context *)

EndPackage[]