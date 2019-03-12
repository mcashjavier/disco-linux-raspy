Begin["OpenPHACTSAPI`"] (* Begin Private Context *) 

ServiceExecute::niden = "Only one identifier is allowed";

Begin["`Private`"](* Begin Private Context *) 

(******************************* Open PHACTS *************************************)

(* Authentication information *)

openphactsdata[]={
		"ServiceName" 		-> "Open PHACTS", 

        "URLFetchFun"		:> (With[{params=Lookup[{##2},"Parameters",{}]},
        	URLFetch[#1,"ContentData",
        		Sequence@@FilterRules[{##2},Except["Parameters"|"Headers"]], 
        		(*"Parameters" -> params/."apikey"->"app_key",*)
        		"Parameters" -> params,
        		"Headers" -> {}]]&),
        		
        "ClientInfo"		:> OAuthDialogDump`Private`MultipleKeyDialog["OpenPHACTS",{"Application Key"->{"app_key",FieldMasked->True},"Application Id"->"app_id"},
        								"https://dev.openphacts.org/login","http://www.openphacts.org/terms-and-conditions/terms-of-use"],
	 	"Gets"				-> {"CompoundInformation","CompoundClassMembersList","CompoundClassMembersCount","CompoundClassifications","GetURI",(*"InChIToURI","InChIKeyToURI","SMILESToURI",*)"TargetInformation","TargetClassMembersList","TargetClassMembersCount","TargetTypes","PathwayInformation","PathwayCompounds","PathwayOrganisms","StructureSearchBySimilarity","StructureSearchBySubstructure"},
	 	"RawGets"			-> {"RawCompound", "RawCompoundBatch", "RawCompoundCount", "RawCompoundList", "RawCompoundClassifications", "RawStructureInchiToURI", "RawStructureInchiKeyToURI", "RawStructureSMILESToURI", "RawStructureSearchSimilarity", "RawStructureSearchSubstructure", "RawTargetInformation", "RawTargetInformationBatch", "RawTargetClassMembersCount", "RawTargetClassMembersList", "RawTargetTypes", "RawPathwayInformation", "RawPathwayInformationGetCompounds", "RawPathwayOrganisms"},
	 	"Posts"				-> {},
	 	"RawPosts"			-> {},
 		"Information"		-> "Import Open PHACTS API data to the Wolfram Language"
 		}

openphactsimport[rawdata_]:=FromCharacterCode[rawdata, "UTF-8"]

(* Raw *)
openphactsdata["RawCompound"] := {
        "URL"				-> "https://beta.openphacts.org/1.5/compound",
        "HTTPSMethod"		-> "GET",
        "Parameters"		-> {"uri","_format","_callback","_metadata"},
        "RequiredParameters"-> {"uri"},
        "ResultsFunction"	-> openphactsimport
        }

openphactsdata["RawCompoundBatch"] := {
		"URL"				-> "https://beta.openphacts.org/1.5/compound/batch",
        "HTTPSMethod"		-> "GET",
        "Parameters"		-> {"uri_list","_format","_callback","_metadata"},
        "RequiredParameters"-> {"uri_list"},
        "ResultsFunction"	-> openphactsimport
        }

openphactsdata["RawCompoundCount"] := {
		"URL"				-> "https://beta.openphacts.org/1.5/compound/members/count",
        "HTTPSMethod"		-> "GET",
        "Parameters"		-> {"uri","_format","_callback","_metadata"},
        "RequiredParameters"-> {"uri"},
        "ResultsFunction"	-> openphactsimport
        }
        
openphactsdata["RawCompoundList"] := {
		"URL"				-> "https://beta.openphacts.org/1.5/compound/members/pages",
        "HTTPSMethod"		-> "GET",
        "Parameters"		-> {"uri","_page","_pageSize","_orderBy","_format", "_callback", "_metadata"},
        "RequiredParameters"-> {"uri"},
        "ResultsFunction"	-> openphactsimport
        }
        
openphactsdata["RawCompoundClassifications"] := {
		"URL"				-> "https://beta.openphacts.org/1.5/compound/classifications",
        "HTTPSMethod"		-> "GET",
        "Parameters"		-> {"uri","tree","_format", "_callback", "_metadata"},
        "RequiredParameters"-> {"uri"},
        "ResultsFunction"	-> openphactsimport
        }
        
openphactsdata["RawStructureInchiToURI"] := {
		"URL"				-> "https://beta.openphacts.org/1.5/structure",
        "HTTPSMethod"		-> "GET",
        "Parameters"		-> {"inchi", "_format", "_callback", "_metadata"},
        "RequiredParameters"-> {"inchi"},
        "ResultsFunction"	-> openphactsimport
        }
        
openphactsdata["RawStructureInchiKeyToURI"] := {
		"URL"				-> "https://beta.openphacts.org/1.5/structure",
        "HTTPSMethod"		-> "GET",
        "Parameters"		-> {"inchi_key", "_format", "_callback", "_metadata"},
        "RequiredParameters"-> {"inchi_key"},
        "ResultsFunction"	-> openphactsimport
        }
        
openphactsdata["RawStructureSMILESToURI"] := {
		"URL"				-> "https://beta.openphacts.org/1.5/structure",
        "HTTPSMethod"		-> "GET",
        "Parameters"		-> {"smiles", "_format", "_callback", "_metadata"},
        "RequiredParameters"-> {"smiles"},
        "ResultsFunction"	-> openphactsimport
        }
        
openphactsdata["RawStructureSearchSimilarity"] := {
		"URL"				-> "https://beta.openphacts.org/1.5/structure/similarity",
        "HTTPSMethod"		-> "GET",
        "Parameters"		-> {"searchOptions.Molecule", "searchOptions.SimilarityType", "searchOptions.Threshold", "searchOptions.Alpha", "searchOptions.Beta", "resultOptions.Start", "resultOptions.Count", "_format", "_callback", "_metadata"},
        "RequiredParameters"-> {"searchOptions.Molecule"},
        "ResultsFunction"	-> openphactsimport
        }

openphactsdata["RawStructureSearchSubstructure"] := {
        "URL"                   -> "https://beta.openphacts.org/1.5/structure/substructure",
        "HTTPSMethod"           -> "GET",
        "Parameters"            -> {"searchOptions.Molecule", "searchOptions.MolType", "resultOptions.Start", "resultOptions.Count", "_format", "_callback", "_metadata"},
        "RequiredParameters"-> {"searchOptions.Molecule"},
        "ResultsFunction"       -> openphactsimport
        }                    
        
openphactsdata["RawTargetInformation"] := {
		"URL"				-> "https://beta.openphacts.org/1.5/target",
        "HTTPSMethod"		-> "GET",
        "Parameters"		-> {"uri","drug_type","_format", "_callback", "_metadata"},
        "RequiredParameters"-> {"uri"},
        "ResultsFunction"	-> openphactsimport
        }    
        
openphactsdata["RawTargetInformationBatch"] := {
		"URL"				-> "https://beta.openphacts.org/1.5/target/batch",
        "HTTPSMethod"		-> "GET",
        "Parameters"		-> {"uri_list","drug_type","_format", "_callback", "_metadata"},
        "RequiredParameters"-> {"uri_list"},
        "ResultsFunction"	-> openphactsimport
        }        
        
openphactsdata["RawTargetClassMembersCount"] := {
		"URL"				-> "https://beta.openphacts.org/1.5/target/members/count",
        "HTTPSMethod"		-> "GET",
        "Parameters"		-> {"uri","target_organism", "target_organism_uri" ,"_format", "_callback", "_metadata"},
        "RequiredParameters"-> {"uri"},
        "ResultsFunction"	-> openphactsimport
        }    
        
openphactsdata["RawTargetClassMembersList"] := {
		"URL"				-> "https://beta.openphacts.org/1.5/target/members/pages",
        "HTTPSMethod"		-> "GET",
        "Parameters"		-> {"uri","target_organism", "target_organism_uri","_page", "_pageSize", "orderBy", "_format", "_callback", "_metadata"},
        "RequiredParameters"-> {"uri"},
        "ResultsFunction"	-> openphactsimport
        }       
        
openphactsdata["RawTargetTypes"] := {
		"URL"				-> "https://beta.openphacts.org/1.5/target/types",
        "HTTPSMethod"		-> "GET",
        "Parameters"		-> {"_format", "_callback", "_metadata"},
        "RequiredParameters"-> {},
        "ResultsFunction"	-> openphactsimport
        }    
        
openphactsdata["RawPathwayInformation"] := {
		"URL"				-> "https://beta.openphacts.org/1.5/pathway",
        "HTTPSMethod"		-> "GET",
        "Parameters"		-> {"uri", "_format", "_callback", "_metadata"},
        "RequiredParameters"-> {"uri"},
        "ResultsFunction"	-> openphactsimport
        } 
        
openphactsdata["RawPathwayInformationGetCompounds"] := {
		"URL"				-> "https://beta.openphacts.org/1.5/pathway/getCompounds",
        "HTTPSMethod"		-> "GET",
        "Parameters"		-> {"uri", "_format", "_callback", "_metadata"},
        "RequiredParameters"-> {"uri"},
        "ResultsFunction"	-> openphactsimport
        }   
        
openphactsdata["RawPathwayOrganisms"] := {
		"URL"				-> "https://beta.openphacts.org/1.5/pathways/organisms",
        "HTTPSMethod"		-> "GET",
        "Parameters"		-> {"_page","_pageSize","_orderBy","_format", "_callback", "_metadata"},
        "RequiredParameters"-> {},
        "ResultsFunction"	-> openphactsimport
        }

(* Cooked *)

openphactscookeddata[req_, id_]:=openphactscookeddata[req, id,{}]

openphactscookeddata[prop_,id_,rules___Rule]:=openphactscookeddata[prop,id,{rules}]

camelCase[text_] := Module[{split, partial}, (
    split = StringSplit[text, {" ","_","-"}];
    partial = Prepend[Rest[Characters[#]], ToUpperCase[Characters[#][[1]]]] & /@ split;
    StringJoin[partial]
    )]

openphactscookeddata["CompoundInformation", id_,args_]:=Block[{rawdata,newparams,params={},invalidParameters,uri,withCamelTitles,input},
	newparams=args;
	invalidParameters = Select[Keys[newparams],!MemberQ[{"URI","InChI","InChIKey","SMILES"},#]&]; 
	If[Length[invalidParameters]>0,
	(
		Message[ServiceObject::noget,#,"OpenPHACTS"]&/@invalidParameters;
		Throw[$Failed]
	)];
	input = Select[Keys[newparams],MemberQ[{"URI","InChI","InChIKey","SMILES"},#]&]; 
	If[Length[input]>1,
	(
		Message[ServiceExecute::niden];
		Throw[$Failed]
	)];
	
	Switch[input,
	{"URI"},
	(
		If[!(MatchQ["URI"/.newparams,List[__String]]||StringQ["URI"/.newparams]),
		(	
			Message[ServiceExecute::nval,"URI","OpenPHACTS"];
			Throw[$Failed]
		)];
		uri = "URI"/.newparams		
	),{"InChI"},
	(
		If[!(MatchQ["InChI"/.newparams,List[__String]]||StringQ["InChI"/.newparams]),
		(	
			Message[ServiceExecute::nval,"InChI","OpenPHACTS"];
			Throw[$Failed]
		)];
		If[StringQ["InChI"/.newparams],
		(
			uri=ServiceExecute["OpenPHACTS","GetURI", {"InChI" -> ("InChI"/.newparams)}];
			If[! MatchQ[Head[uri], Dataset],
   			(
      			Message[ServiceExecute::serrormsg,""];
       			Throw[$Failed]
 			)];
			uri="URI" /. Normal[uri]
		),
		(
			uri=(ServiceExecute["OpenPHACTS","GetURI", {"InChI" -> #}] &) /@("InChI"/.newparams);
			If[! MatchQ[Head[#], Dataset],
   			(
      			Message[ServiceExecute::serrormsg,""];
       			Throw[$Failed]
 			)]&/@uri;
 			uri=("URI" /. Normal[#] &) /@uri
		)]
		
	),{"InChIKey"},
	(
		If[!(MatchQ["InChIKey"/.newparams,List[__String]]||StringQ["InChIKey"/.newparams]),
		(	
			Message[ServiceExecute::nval,"InChIKey","OpenPHACTS"];
			Throw[$Failed]
		)];
		If[StringQ["InChIKey"/.newparams],
		(
			uri=ServiceExecute["OpenPHACTS","GetURI", {"InChIKey" -> ("InChIKey"/.newparams)}];
			If[! MatchQ[Head[uri], Dataset],
   			(
      			Message[ServiceExecute::serrormsg,""];
       			Throw[$Failed]
 			)];
			uri="URI" /. Normal[uri]
		),
		(
			uri=(ServiceExecute["OpenPHACTS","GetURI", {"InChIKey" -> #}] &) /@("InChIKey"/.newparams);
			If[! MatchQ[Head[#], Dataset],
   			(
      			Message[ServiceExecute::serrormsg,""];
       			Throw[$Failed]
 			)]&/@uri;
 			uri=("URI" /. Normal[#] &) /@uri
		)]
	),{"SMILES"},
	(
		If[!(MatchQ["SMILES"/.newparams,List[__String]]||StringQ["SMILES"/.newparams]),
		(	
			Message[ServiceExecute::nval,"SMILES","OpenPHACTS"];
			Throw[$Failed]
		)];
		If[StringQ["SMILES"/.newparams],
		(
			uri=ServiceExecute["OpenPHACTS","GetURI", {"SMILES" -> ("SMILES"/.newparams)}];
			If[! MatchQ[Head[uri], Dataset],
   			(
      			Message[ServiceExecute::serrormsg,""];
       			Throw[$Failed]
 			)];
			uri="URI" /. Normal[uri]
		),
		(
			uri=(ServiceExecute["OpenPHACTS","GetURI", {"SMILES" -> #}] &) /@("SMILES"/.newparams);
			If[! MatchQ[Head[#], Dataset],
   			(
      			Message[ServiceExecute::serrormsg,""];
       			Throw[$Failed]
 			)]&/@uri;
 			uri=("URI" /. Normal[#] &) /@uri
		)]
	),{},
	(
		Message[ServiceExecute::nparam,"URI, InChI, InChIKey or SMILES","PubChem"];
		Throw[$Failed]
	)];

	If[StringQ[uri],
	(
		rawdata = Quiet[ImportString[ServiceExecute["OpenPHACTS","RawCompound",{"uri"->uri,"_format"->"json"}],"RawJSON"]];
		If[MatchQ[rawdata,$Failed],
   		(
      		Message[ServiceExecute::serrormsg,""];
       		Throw[$Failed]
 		)];
 		rawdata=FixedPoint[Normal,rawdata];
		withCamelTitles=Replace[("exactMatch" /. ("primaryTopic" /. ("result" /. rawdata))),{Rule[a_, b_] :> Rule[camelCase[a], b],Null -> Missing["NotAvailable"]}, Infinity]/.{"About"->"URI"};
		Dataset[Replace[Select[withCamelTitles, ListQ], r : {__Rule} :> Association[r], -1]]
	),
	(
		uri=StringJoin @@ Riffle[uri, "|"];
		rawdata = Quiet[ImportString[ServiceExecute["OpenPHACTS","RawCompoundBatch",{"uri_list"->uri,"_format"->"json"}],"RawJSON"]];
		If[MatchQ[rawdata,$Failed],
   		(
      		Message[ServiceExecute::serrormsg,""];
       		Throw[$Failed]
 		)];
 		rawdata=FixedPoint[Normal,rawdata];
		withCamelTitles=Replace[("exactMatch" /. ("items" /. ("result" /. rawdata))),{Rule[a_, b_] :> Rule[camelCase[a], b],Null -> Missing["NotAvailable"]}, Infinity]/.{"About"->"URI"};
		Dataset[Replace[(Select[#, ListQ] & /@ withCamelTitles), r : {__Rule} :> Association[r], -1]]
	)]
]

openphactscookeddata["CompoundClassMembersCount", id_,args_]:=Block[{rawdata,newparams,params={},invalidParameters,input,uri},
	newparams=args;
	invalidParameters = Select[Keys[newparams],!MemberQ[{"URI","InChI","InChIKey","SMILES"},#]&]; 
	If[Length[invalidParameters]>0,
	(
		Message[ServiceObject::noget,#,"OpenPHACTS"]&/@invalidParameters;
		Throw[$Failed]
	)];
	input = Select[Keys[newparams],MemberQ[{"URI","InChI","InChIKey","SMILES"},#]&]; 
	If[Length[input]>1,
	(
		Message[ServiceExecute::niden];
		Throw[$Failed]
	)];
	
	Switch[input,
	{"URI"},
	(
		If[!StringQ["URI"/.newparams],
		(	
			Message[ServiceExecute::nval,"URI","OpenPHACTS"];
			Throw[$Failed]
		)];
		params = Append[params,Rule["uri","URI"/.newparams]]		
	),{"InChI"},
	(
		If[!StringQ["InChI"/.newparams],
		(	
			Message[ServiceExecute::nval,"InChI","OpenPHACTS"];
			Throw[$Failed]
		)];
		uri=ServiceExecute["OpenPHACTS","GetURI", {"InChI" -> ("InChI"/.newparams)}];
		If[! MatchQ[Head[uri], Dataset],
   		(
      		Message[ServiceExecute::serrormsg,""];
       		Throw[$Failed]
 		)];
 		params = Append[params,Rule["uri","URI" /. Normal[uri]]]
	),{"InChIKey"},
	(
		If[!StringQ["InChIKey"/.newparams],
		(	
			Message[ServiceExecute::nval,"InChIKey","OpenPHACTS"];
			Throw[$Failed]
		)];
		uri=ServiceExecute["OpenPHACTS","GetURI", {"InChIKey" -> ("InChIKey"/.newparams)}];
		If[! MatchQ[Head[uri], Dataset],
   		(
      		Message[ServiceExecute::serrormsg,""];
       		Throw[$Failed]
 		)];
 		params = Append[params,Rule["uri","URI" /. Normal[uri]]]
	),{"SMILES"},
	(
		If[!StringQ["SMILES"/.newparams],
		(	
			Message[ServiceExecute::nval,"SMILES","OpenPHACTS"];
			Throw[$Failed]
		)];
		uri=ServiceExecute["OpenPHACTS","GetURI", {"SMILES" -> ("SMILES"/.newparams)}];
		If[! MatchQ[Head[uri], Dataset],
   		(
      		Message[ServiceExecute::serrormsg,""];
       		Throw[$Failed]
 		)];
 		params = Append[params,Rule["uri","URI" /. Normal[uri]]]
	),{},
	(
		Message[ServiceExecute::nparam,"URI, InChI, InChIKey or SMILES","PubChem"];
		Throw[$Failed]
	)];
	
	rawdata = Quiet[ImportString[ServiceExecute["OpenPHACTS","RawCompoundCount",Append[params,"_format"->"json"]],"RawJSON"]];
	If[MatchQ[rawdata,$Failed],
   	(
      	Message[ServiceExecute::serrormsg,""];
       	Throw[$Failed]
 	)];
 	rawdata=FixedPoint[Normal,rawdata];
	Dataset[Association["MemberCount"->("memberCount" /. ("primaryTopic" /. ("result" /. rawdata)))]]
]

openphactscookeddata["CompoundClassMembersList", id_,args_]:=Block[{rawdata,newparams,params={},invalidParameters,withCamelTitles,maxitems,startindex,length,inf,sup,input,uri},
	newparams=args/.{MaxItems:>"MaxItems"};
	invalidParameters = Select[Keys[newparams],!MemberQ[{"URI","InChI","InChIKey","SMILES","StartIndex","MaxItems","SortBy"},#]&]; 
	If[Length[invalidParameters]>0,
	(
		Message[ServiceObject::noget,#,"OpenPHACTS"]&/@invalidParameters;
		Throw[$Failed]
	)];
	input = Select[Keys[newparams],MemberQ[{"URI","InChI","InChIKey","SMILES"},#]&]; 
	If[Length[input]>1,
	(
		Message[ServiceExecute::niden];
		Throw[$Failed]
	)];
	
	Switch[input,
	{"URI"},
	(
		If[!StringQ["URI"/.newparams],
		(	
			Message[ServiceExecute::nval,"URI","OpenPHACTS"];
			Throw[$Failed]
		)];
		params = Append[params,Rule["uri","URI"/.newparams]]		
	),{"InChI"},
	(
		If[!StringQ["InChI"/.newparams],
		(	
			Message[ServiceExecute::nval,"InChI","OpenPHACTS"];
			Throw[$Failed]
		)];
		uri=ServiceExecute["OpenPHACTS","GetURI", {"InChI" -> ("InChI"/.newparams)}];
		If[! MatchQ[Head[uri], Dataset],
   		(
      		Message[ServiceExecute::serrormsg,""];
       		Throw[$Failed]
 		)];
 		params = Append[params,Rule["uri","URI" /. Normal[uri]]]
	),{"InChIKey"},
	(
		If[!StringQ["InChIKey"/.newparams],
		(	
			Message[ServiceExecute::nval,"InChIKey","OpenPHACTS"];
			Throw[$Failed]
		)];
		uri=ServiceExecute["OpenPHACTS","GetURI", {"InChIKey" -> ("InChIKey"/.newparams)}];
		If[! MatchQ[Head[uri], Dataset],
   		(
      		Message[ServiceExecute::serrormsg,""];
       		Throw[$Failed]
 		)];
 		params = Append[params,Rule["uri","URI" /. Normal[uri]]]
	),{"SMILES"},
	(
		If[!StringQ["SMILES"/.newparams],
		(	
			Message[ServiceExecute::nval,"SMILES","OpenPHACTS"];
			Throw[$Failed]
		)];
		uri=ServiceExecute["OpenPHACTS","GetURI", {"SMILES" -> ("SMILES"/.newparams)}];
		If[! MatchQ[Head[uri], Dataset],
   		(
      		Message[ServiceExecute::serrormsg,""];
       		Throw[$Failed]
 		)];
 		params = Append[params,Rule["uri","URI" /. Normal[uri]]]
	),{},
	(
		Message[ServiceExecute::nparam,"URI, InChI, InChIKey or SMILES","PubChem"];
		Throw[$Failed]
	)];
	If[KeyExistsQ[newparams,"MaxItems"],
	(
		If[!((IntegerQ["MaxItems"/.newparams]&&("MaxItems"/.newparams)>0)||MatchQ["MaxItems"/.newparams,All]),
		(	
			Message[ServiceExecute::nval,"MaxItems","OpenPHACTS"];
			Throw[$Failed]
		)];
		maxitems="MaxItems"/.newparams
	),
  	(
  		maxitems=10
  	)];
	If[KeyExistsQ[newparams,"StartIndex"],
	(
		If[!(IntegerQ["StartIndex"/.newparams]&&("StartIndex"/.newparams)>0),
		(	
			Message[ServiceExecute::nval,"StartIndex","OpenPHACTS"];
			Throw[$Failed]
		)];
		startindex="StartIndex"/.newparams
	),
  	(
  		startindex=1
  	)];
  	If[KeyExistsQ[newparams,"SortBy"],
	(
		If[!StringMatchQ["SortBy"/.newparams, "ChildNode"|"CompoundName"|"CWCompound"|"Item"|"NodeURI"|"OCRSCompound"],
		(	
			Message[ServiceExecute::nval,"SortBy","OpenPHACTS"];
			Throw[$Failed]
		)];
		params = Append[params,Rule["_orderBy",("SortBy"/.newparams)/.{"ChildNode"->"?child_node","CompoundName"->"?compound_name","CWCompound"->"?cw_compound","Item"->"?item","NodeURI"->"?node_uri","OCRSCompound"->"?ocrs_compound"}]]
	)];
	rawdata = Quiet[ImportString[ServiceExecute["OpenPHACTS","RawCompoundList",Append[Append[params,"_format"->"json"],"_pageSize"->"all"]],"RawJSON"]];
	If[MatchQ[rawdata,$Failed],
   	(
      	Message[ServiceExecute::serrormsg,""];
       	Throw[$Failed]
 	)];
 	rawdata=FixedPoint[Normal,rawdata];
	withCamelTitles=Replace[("items" /. ("result" /. rawdata)),{Rule[a_, b_] :> Rule[camelCase[a], b],Null -> Missing["NotAvailable"]}, Infinity]/.{"About"->"URI"};
	If[MatchQ[maxitems,All],
	(
		Dataset[Replace[withCamelTitles, r : {__Rule} :> Association[r], -1]]
	),
	(
		withCamelTitles=Partition[withCamelTitles,UpTo[maxitems]];
		length=Length[withCamelTitles];
		If[startindex>length,
		(
			Dataset[{}]
		),
		(
			Dataset[Replace[withCamelTitles[[startindex]], r : {__Rule} :> Association[r], -1]]
		)]
	)]
		
]

openphactscookeddata["CompoundClassifications", id_,args_]:=Block[{rawdata,newparams,params={},invalidParameters,withCamelTitles,input,uri},
	newparams=args;
	invalidParameters = Select[Keys[newparams],!MemberQ[{"URI","InChI","InChIKey","SMILES","Tree"},#]&]; 
	If[Length[invalidParameters]>0,
	(
		Message[ServiceObject::noget,#,"OpenPHACTS"]&/@invalidParameters;
		Throw[$Failed]
	)];
	input = Select[Keys[newparams],MemberQ[{"URI","InChI","InChIKey","SMILES"},#]&]; 
	If[Length[input]>1,
	(
		Message[ServiceExecute::niden];
		Throw[$Failed]
	)];
	
	Switch[input,
	{"URI"},
	(
		If[!StringQ["URI"/.newparams],
		(	
			Message[ServiceExecute::nval,"URI","OpenPHACTS"];
			Throw[$Failed]
		)];
		params = Append[params,Rule["uri","URI"/.newparams]]		
	),{"InChI"},
	(
		If[!StringQ["InChI"/.newparams],
		(	
			Message[ServiceExecute::nval,"InChI","OpenPHACTS"];
			Throw[$Failed]
		)];
		uri=ServiceExecute["OpenPHACTS","GetURI", {"InChI" -> ("InChI"/.newparams)}];
		If[! MatchQ[Head[uri], Dataset],
   		(
      		Message[ServiceExecute::serrormsg,""];
       		Throw[$Failed]
 		)];
 		params = Append[params,Rule["uri","URI" /. Normal[uri]]]
	),{"InChIKey"},
	(
		If[!StringQ["InChIKey"/.newparams],
		(	
			Message[ServiceExecute::nval,"InChIKey","OpenPHACTS"];
			Throw[$Failed]
		)];
		uri=ServiceExecute["OpenPHACTS","GetURI", {"InChIKey" -> ("InChIKey"/.newparams)}];
		If[! MatchQ[Head[uri], Dataset],
   		(
      		Message[ServiceExecute::serrormsg,""];
       		Throw[$Failed]
 		)];
 		params = Append[params,Rule["uri","URI" /. Normal[uri]]]
	),{"SMILES"},
	(
		If[!StringQ["SMILES"/.newparams],
		(	
			Message[ServiceExecute::nval,"SMILES","OpenPHACTS"];
			Throw[$Failed]
		)];
		uri=ServiceExecute["OpenPHACTS","GetURI", {"SMILES" -> ("SMILES"/.newparams)}];
		If[! MatchQ[Head[uri], Dataset],
   		(
      		Message[ServiceExecute::serrormsg,""];
       		Throw[$Failed]
 		)];
 		params = Append[params,Rule["uri","URI" /. Normal[uri]]]
	),{},
	(
		Message[ServiceExecute::nparam,"URI, InChI, InChIKey or SMILES","PubChem"];
		Throw[$Failed]
	)];
	If[KeyExistsQ[newparams,"Tree"],
	(
		If[!StringQ["Tree"/.newparams],
		(	
			Message[ServiceExecute::nval,"Tree","OpenPHACTS"];
			Throw[$Failed]
		)];
		params = Append[params,Rule["tree","Tree"/.newparams]]
	)];
	rawdata = Quiet[ImportString[ServiceExecute["OpenPHACTS","RawCompoundClassifications",Append[params,"_format"->"json"]],"RawJSON"]];
	If[MatchQ[rawdata,$Failed],
   	(
      	Message[ServiceExecute::serrormsg,""];
       	Throw[$Failed]
 	)];
 	rawdata=FixedPoint[Normal,rawdata];
	withCamelTitles=Replace[("exactMatch" /. ("primaryTopic" /. ("result" /. rawdata))),{Rule[a_, b_] :> Rule[camelCase[a], b],Null -> Missing["NotAvailable"]}, Infinity]/.{"About"->"URI"};
	Dataset[Replace[Select[withCamelTitles, ListQ], r : {__Rule} :> Association[r], -1]]
]


openphactscookeddata["GetURI", id_,args_]:=Block[{rawdata,newparams,params={},invalidParameters,inputParameter},
	newparams=args;
	invalidParameters = Select[Keys[newparams],!MemberQ[{"InChI","InChIKey","SMILES"},#]&]; 
	If[Length[invalidParameters]>0,
	(
		Message[ServiceObject::noget,#,"OpenPHACTS"]&/@invalidParameters;
		Throw[$Failed]
	)];
	inputParameter = Select[Keys[newparams],MemberQ[{"InChI","InChIKey","SMILES"},#]&]; 
	If[Length[inputParameter]>1,
	(
		Message[ServiceExecute::niden];
		Throw[$Failed]
	)];
	
	Switch[inputParameter,
	{"InChI"},
	(
		If[!StringQ["InChI"/.newparams],
		(	
			Message[ServiceExecute::nval,"InChI","OpenPHACTS"];
			Throw[$Failed]
		)];
		rawdata = Quiet[ImportString[ServiceExecute["OpenPHACTS","RawStructureInchiToURI",{"inchi"->("InChI"/.newparams),"_format"->"json"}],"RawJSON"]]
	),{"InChIKey"},
	(
		If[!StringQ["InChIKey"/.newparams],
		(	
			Message[ServiceExecute::nval,"InChIKey","OpenPHACTS"];
			Throw[$Failed]
		)];
		rawdata = Quiet[ImportString[ServiceExecute["OpenPHACTS","RawStructureInchiKeyToURI",{"inchi_key"->("InChIKey"/.newparams),"_format"->"json"}],"RawJSON"]]
	),{"SMILES"},
	(
		If[!StringQ["SMILES"/.newparams],
		(	
			Message[ServiceExecute::nval,"SMILES","OpenPHACTS"];
			Throw[$Failed]
		)];
		rawdata = Quiet[ImportString[ServiceExecute["OpenPHACTS","RawStructureSMILESToURI",{"smiles"->("SMILES"/.newparams),"_format"->"json"}],"RawJSON"]]
	),{},
	(
		Message[ServiceExecute::nparam,"InChI, InChIKey or SMILES","OpenPHACTS"];
		Throw[$Failed]
	)];

	If[MatchQ[rawdata,$Failed],
   	(
      	Message[ServiceExecute::serrormsg,""];
       	Throw[$Failed]
 	)];
 	rawdata=FixedPoint[Normal,rawdata];
	Dataset[Association["URI"->("_about" /. ("primaryTopic" /. ("result" /. rawdata)))]]
]


openphactscookeddata["StructureSearchBySimilarity", id_,args_]:=Block[{rawdata,newparams,params={},invalidParameters,withCamelTitles,maxitems,startindex,length,inf,sup},
	newparams=args/.{MaxItems:>"MaxItems"};
	invalidParameters = Select[Keys[newparams],!MemberQ[{"Molecule","SimilarityType","Threshold","Alpha","Beta","MaxItems","StartIndex"},#]&]; 
	If[Length[invalidParameters]>0,
	(
		Message[ServiceObject::noget,#,"OpenPHACTS"]&/@invalidParameters;
		Throw[$Failed]
	)];
	If[KeyExistsQ[newparams,"Molecule"],
	(
		If[!StringQ["Molecule"/.newparams],
		(	
			Message[ServiceExecute::nval,"Molecule","OpenPHACTS"];
			Throw[$Failed]
		)];
		params = Append[params,Rule["searchOptions.Molecule","Molecule"/.newparams]]
	),
	(
		Message[ServiceExecute::nparam,"Molecule","OpenPHACTS"];
		Throw[$Failed]
	)];
	If[KeyExistsQ[newparams,"SimilarityType"],
	(
		If[!StringMatchQ[ToString["SimilarityType" /. newparams], "Tanimoto"|"Tversky"|"Euclidian"],
		(	
			Message[ServiceExecute::nval,"SimilarityType","OpenPHACTS"];
			Throw[$Failed]
		)];
		params = Append[params,Rule["searchOptions.SimilarityType",(ToString["SimilarityType" /. newparams]/.{"Tanimoto"->"0","Tversky"->"1","Euclidian"->"2"})]]
	)];
	If[KeyExistsQ[newparams,"Threshold"],
	(
		If[!(NumberQ["Threshold"/.newparams]&&IntervalMemberQ[Interval[{0, 1}], "Threshold"/.newparams]),
		(	
			Message[ServiceExecute::nval,"Threshold","OpenPHACTS"];
			Throw[$Failed]
		)];
		params = Append[params,Rule["searchOptions.Threshold",ToString["Threshold"/.newparams]]]
	)];
	If[KeyExistsQ[newparams,"Alpha"],
	(
		If[!(NumberQ["Alpha"/.newparams]&&IntervalMemberQ[Interval[{0, 1}], "Alpha"/.newparams]),
		(	
			Message[ServiceExecute::nval,"Alpha","OpenPHACTS"];
			Throw[$Failed]
		)];
		params = Append[params,Rule["searchOptions.Alpha",ToString["Alpha"/.newparams]]]
	)];
	If[KeyExistsQ[newparams,"Beta"],
	(
		If[!(NumberQ["Beta"/.newparams]&&IntervalMemberQ[Interval[{0, 1}], "Beta"/.newparams]),
		(	
			Message[ServiceExecute::nval,"Beta","OpenPHACTS"];
			Throw[$Failed]
		)];
		params = Append[params,Rule["searchOptions.Beta",ToString["Beta"/.newparams]]]
	)];
	If[KeyExistsQ[newparams,"MaxItems"],
	(
		If[!((IntegerQ["MaxItems"/.newparams]&&("MaxItems"/.newparams)>0)||MatchQ["MaxItems"/.newparams,All]),
		(	
			Message[ServiceExecute::nval,"MaxItems","OpenPHACTS"];
			Throw[$Failed]
		)];
		maxitems="MaxItems"/.newparams
	),
  	(
  		maxitems=10
  	)];
	If[KeyExistsQ[newparams,"StartIndex"],
	(
		If[!(IntegerQ["StartIndex"/.newparams]&&("StartIndex"/.newparams)>0),
		(	
			Message[ServiceExecute::nval,"StartIndex","OpenPHACTS"];
			Throw[$Failed]
		)];
		startindex="StartIndex"/.newparams
	),
  	(
  		startindex=1
  	)];
	rawdata = Quiet[ImportString[ServiceExecute["OpenPHACTS","RawStructureSearchSimilarity",Append[Append[params,"_format"->"json"],"resultOptions.Count"->"-1"]],"RawJSON"]];
	If[MatchQ[rawdata,$Failed],
   	(
      	Message[ServiceExecute::serrormsg,""];
       	Throw[$Failed]
 	)];
 	rawdata=FixedPoint[Normal,rawdata];
	withCamelTitles=Replace[("result" /. ("primaryTopic" /. ("result" /. rawdata))),{Rule[a_, b_] :> Rule[camelCase[a], b],Null -> Missing["NotAvailable"]}, Infinity]/.{"About"->"URI"};
	If[MatchQ[maxitems,All],
	(
		If[startindex>1,
		(
			Dataset[{}]
		),
		(
			Dataset[Replace[withCamelTitles, r : {__Rule} :> Association[r], -1]]
		)]
	),
	(
		withCamelTitles=Partition[withCamelTitles,UpTo[maxitems]];
		length=Length[withCamelTitles];
		If[startindex>length,
		(
			Dataset[{}]
		),
		(
			Dataset[Replace[withCamelTitles[[startindex]], r : {__Rule} :> Association[r], -1]]
		)]
	)]
		
]

openphactscookeddata["StructureSearchBySubstructure", id_,args_]:=Block[{rawdata,newparams,params={},invalidParameters,withCamelTitles,maxitems,startindex,length,inf,sup},
	newparams=args/.{MaxItems:>"MaxItems"};
	invalidParameters = Select[Keys[newparams],!MemberQ[{"Molecule","MolType","MaxItems","StartIndex"},#]&]; 
	If[Length[invalidParameters]>0,
	(
		Message[ServiceObject::noget,#,"OpenPHACTS"]&/@invalidParameters;
		Throw[$Failed]
	)];
	If[KeyExistsQ[newparams,"Molecule"],
	(
		If[!StringQ["Molecule"/.newparams],
		(	
			Message[ServiceExecute::nval,"Molecule","OpenPHACTS"];
			Throw[$Failed]
		)];
		params = Append[params,Rule["searchOptions.Molecule","Molecule"/.newparams]]
	),
	(
		Message[ServiceExecute::nparam,"Molecule","OpenPHACTS"];
		Throw[$Failed]
	)];
	If[KeyExistsQ[newparams,"MolType"],
	(
		If[!StringMatchQ[ToString["MolType" /. newparams], "SMILES"|"SMARTS"],
		(	
			Message[ServiceExecute::nval,"MolType","OpenPHACTS"];
			Throw[$Failed]
		)];
		params = Append[params,Rule["searchOptions.MolType",(ToString["MolType" /. newparams]/.{"SMILE"->"0","SMARTS"->"1"})]]
	)];
	If[KeyExistsQ[newparams,"MaxItems"],
	(
		If[!((IntegerQ["MaxItems"/.newparams]&&("MaxItems"/.newparams)>0)||MatchQ["MaxItems"/.newparams,All]),
		(	
			Message[ServiceExecute::nval,"MaxItems","OpenPHACTS"];
			Throw[$Failed]
		)];
		maxitems="MaxItems"/.newparams
	),
  	(
  		maxitems=10
  	)];
	If[KeyExistsQ[newparams,"StartIndex"],
	(
		If[!(IntegerQ["StartIndex"/.newparams]&&("StartIndex"/.newparams)>0),
		(	
			Message[ServiceExecute::nval,"StartIndex","OpenPHACTS"];
			Throw[$Failed]
		)];
		startindex="StartIndex"/.newparams
	),
  	(
  		startindex=1
  	)];
	rawdata = Quiet[ImportString[ServiceExecute["OpenPHACTS","RawStructureSearchSubstructure",Append[Append[params,"_format"->"json"],"resultOptions.Count"->"-1"]],"RawJSON"]];
	If[MatchQ[rawdata,$Failed],
   	(
      	Message[ServiceExecute::serrormsg,""];
       	Throw[$Failed]
 	)];
 	rawdata=FixedPoint[Normal,rawdata];
	withCamelTitles=Replace[("result" /. ("primaryTopic" /. ("result" /. rawdata))),{Rule[a_, b_] :> Rule[camelCase[a], b],Null -> Missing["NotAvailable"]}, Infinity]/.{"About"->"URI"};
	If[MatchQ[maxitems,All],
	(
		If[startindex>1,
		(
			Dataset[{}]
		),
		(
			Dataset[Replace[withCamelTitles, r : {__Rule} :> Association[r], -1]]
		)]
	),
	(
		withCamelTitles=Partition[withCamelTitles,UpTo[maxitems]];
		length=Length[withCamelTitles];
		If[startindex>length,
		(
			Dataset[{}]
		),
		(
			Dataset[Replace[withCamelTitles[[startindex]], r : {__Rule} :> Association[r], -1]]
		)]
	)]
		
]

openphactscookeddata["TargetInformation", id_,args_]:=Block[{rawdata,newparams,params={},invalidParameters,uri,withCamelTitles},
	newparams=args;
	invalidParameters = Select[Keys[newparams],!MemberQ[{"URI"},#]&]; 
	If[Length[invalidParameters]>0,
	(
		Message[ServiceObject::noget,#,"OpenPHACTS"]&/@invalidParameters;
		Throw[$Failed]
	)];
	If[KeyExistsQ[newparams,"URI"],
	(
		If[!(MatchQ["URI"/.newparams,List[__String]]||StringQ["URI"/.newparams]),
		(	
			Message[ServiceExecute::nval,"URI","OpenPHACTS"];
			Throw[$Failed]
		)];
		uri = "URI"/.newparams
	),
	(
		Message[ServiceExecute::nparam,"URI","OpenPHACTS"];
		Throw[$Failed]
	)];
	If[KeyExistsQ[newparams,"DrugType"],
	(
		If[!StringQ["DrugType"/.newparams],
		(	
			Message[ServiceExecute::nval,"DrugType","OpenPHACTS"];
			Throw[$Failed]
		)];
		params = Append[params,Rule["drug_type","DrugType"/.newparams]]
	)];
	If[StringQ[uri],
	(
		rawdata = Quiet[ImportString[ServiceExecute["OpenPHACTS","RawTargetInformation",Append[Append[params,"uri"->uri],"_format"->"json"]],"RawJSON"]];
		If[MatchQ[rawdata,$Failed],
   		(
      		Message[ServiceExecute::serrormsg,""];
       		Throw[$Failed]
 		)];
 		rawdata=FixedPoint[Normal,rawdata];
		withCamelTitles=Replace[("exactMatch" /. ("primaryTopic" /. ("result" /. rawdata))),{Rule[a_, b_] :> Rule[camelCase[a], b],Null -> Missing["NotAvailable"]}, Infinity]/.{"About"->"URI"};
		Dataset[Replace[Select[withCamelTitles, ListQ], r : {__Rule} :> Association[r], -1]]
	),
	(
		uri=StringJoin @@ Riffle[uri, "|"];
		rawdata = Quiet[ImportString[ServiceExecute["OpenPHACTS","RawTargetInformationBatch",Append[Append[params,"uri_list"->uri],"_format"->"json"]],"RawJSON"]];
		If[MatchQ[rawdata,$Failed],
   		(
      		Message[ServiceExecute::serrormsg,""];
       		Throw[$Failed]
 		)];
 		rawdata=FixedPoint[Normal,rawdata];
		withCamelTitles=Replace[(("exactMatch" /. ("items" /. ("result" /. rawdata)))),{Rule[a_, b_] :> Rule[camelCase[a], b],Null -> Missing["NotAvailable"]}, Infinity]/.{"About"->"URI"};
		Dataset[Replace[(Select[#, ListQ] & /@ withCamelTitles), r : {__Rule} :> Association[r], -1]]
	)]
]

openphactscookeddata["TargetClassMembersList", id_,args_]:=Block[{rawdata,newparams,params={},invalidParameters,withCamelTitles,maxitems,startindex,length,inf,sup},
	newparams=args/.{MaxItems:>"MaxItems"};
	invalidParameters = Select[Keys[newparams],!MemberQ[{"URI","TargetOrganism","TargetOrganismURI","StartIndex","MaxItems","SortBy"},#]&]; 
	If[Length[invalidParameters]>0,
	(
		Message[ServiceObject::noget,#,"OpenPHACTS"]&/@invalidParameters;
		Throw[$Failed]
	)];
	If[KeyExistsQ[newparams,"URI"],
	(
		If[!StringQ["URI"/.newparams],
		(	
			Message[ServiceExecute::nval,"URI","OpenPHACTS"];
			Throw[$Failed]
		)];
		params = Append[params,Rule["uri","URI"/.newparams]]
	),
	(
		Message[ServiceExecute::nparam,"URI","OpenPHACTS"];
		Throw[$Failed]
	)];
	If[KeyExistsQ[newparams,"TargetOrganism"],
	(
		If[!StringQ["TargetOrganism"/.newparams],
		(	
			Message[ServiceExecute::nval,"TargetOrganism","OpenPHACTS"];
			Throw[$Failed]
		)];
		params = Append[params,Rule["target_organism","TargetOrganism"/.newparams]]
	)];
	If[KeyExistsQ[newparams,"TargetOrganismURI"],
	(
		If[!StringQ["TargetOrganismURI"/.newparams],
		(	
			Message[ServiceExecute::nval,"TargetOrganismURI","OpenPHACTS"];
			Throw[$Failed]
		)];
		params = Append[params,Rule["target_organism_uri","TargetOrganismURI"/.newparams]]
	)];
	If[KeyExistsQ[newparams,"MaxItems"],
	(
		If[!((IntegerQ["MaxItems"/.newparams]&&("MaxItems"/.newparams)>0)||MatchQ["MaxItems"/.newparams,All]),
		(	
			Message[ServiceExecute::nval,"MaxItems","OpenPHACTS"];
			Throw[$Failed]
		)];
		maxitems="MaxItems"/.newparams
	),
  	(
  		maxitems=10
  	)];
	If[KeyExistsQ[newparams,"StartIndex"],
	(
		If[!(IntegerQ["StartIndex"/.newparams]&&("StartIndex"/.newparams)>0),
		(	
			Message[ServiceExecute::nval,"StartIndex","OpenPHACTS"];
			Throw[$Failed]
		)];
		startindex="StartIndex"/.newparams
	),
  	(
  		startindex=1
  	)];
  	If[KeyExistsQ[newparams,"SortBy"],
	(
		If[!StringMatchQ["SortBy"/.newparams, "ChildNode"|"Item"|"NodeURI"|"ChEMBLName"|"ChEMBLOrganism"|"ChEMBLTarget"|"G"|"G2"|"IMSUniprotTargetURI"|"Mapping"|"MappingDataset"|"MappingName"|"MappingOrg"|"MappingOrgURI"|"MappingType"|"TargetType"|"UniprotName"|"UniprotOrganism"|"UniprotTarget"],
		(	
			Message[ServiceExecute::nval,"SortBy","OpenPHACTS"];
			Throw[$Failed]
		)];
		params = Append[params,Rule["_orderBy",("SortBy"/.newparams)/.{"ChildNode"->"?child_node","Item"->"?item","NodeURI"->"?node_uri","ChEMBLName"->"?chembl_name","ChEMBLOrganism"->"?chembl_organism","ChEMBLTarget"->"?chembl_target","G"->"?g","G2"->"?g2","IMSUniprotTargetURI"->"?ims_uniprot_target_uri","Mapping"->"?mapping","MappingDataset"->"?mapping_dataset","MappingName"->"?mapping_name","MappingOrg"->"?mapping_org","MappingOrgURI"->"?mapping_org_uri","MappingType"->"?mapping_type","TargetType"->"?target_type","UniprotName"->"?uniprot_name","UniprotOrganism"->"?uniprot_organism","UniprotTarget"->"?uniprot_target"}]]
	)];
	rawdata = Quiet[ImportString[ServiceExecute["OpenPHACTS","RawTargetClassMembersList",Append[Append[params,"_format"->"json"],"_pageSize"->"all"]],"RawJSON"]];
	If[MatchQ[rawdata,$Failed],
   	(
      	Message[ServiceExecute::serrormsg,""];
       	Throw[$Failed]
 	)];
 	rawdata=FixedPoint[Normal,rawdata];
	withCamelTitles=Replace[("items" /. ("result" /. rawdata)),{Rule[a_, b_] :> Rule[camelCase[a], b],Null -> Missing["NotAvailable"]}, Infinity]/.{"About"->"URI"};
	If[MatchQ[maxitems,All],
	(
		Dataset[Replace[withCamelTitles, r : {__Rule} :> Association[r], -1]]
	),
	(
		withCamelTitles=Partition[withCamelTitles,UpTo[maxitems]];
		length=Length[withCamelTitles];
		If[startindex>length,
		(
			Dataset[{}]
		),
		(
			Dataset[Replace[withCamelTitles[[startindex]], r : {__Rule} :> Association[r], -1]]
		)]
	)]
		
]

openphactscookeddata["TargetClassMembersCount", id_,args_]:=Block[{rawdata,newparams,params={},invalidParameters},
	newparams=args;
	invalidParameters = Select[Keys[newparams],!MemberQ[{"URI","TargetOrganism","TargetOrganismURI"},#]&]; 
	If[Length[invalidParameters]>0,
	(
		Message[ServiceObject::noget,#,"OpenPHACTS"]&/@invalidParameters;
		Throw[$Failed]
	)];
	If[KeyExistsQ[newparams,"URI"],
	(
		If[!StringQ["URI"/.newparams],
		(	
			Message[ServiceExecute::nval,"URI","OpenPHACTS"];
			Throw[$Failed]
		)];
		params = Append[params,Rule["uri","URI"/.newparams]]
	),
	(
		Message[ServiceExecute::nparam,"URI","OpenPHACTS"];
		Throw[$Failed]
	)];
	If[KeyExistsQ[newparams,"TargetOrganism"],
	(
		If[!StringQ["TargetOrganism"/.newparams],
		(	
			Message[ServiceExecute::nval,"TargetOrganism","OpenPHACTS"];
			Throw[$Failed]
		)];
		params = Append[params,Rule["target_organism","TargetOrganism"/.newparams]]
	)];
	If[KeyExistsQ[newparams,"TargetOrganismURI"],
	(
		If[!StringQ["TargetOrganismURI"/.newparams],
		(	
			Message[ServiceExecute::nval,"TargetOrganismURI","OpenPHACTS"];
			Throw[$Failed]
		)];
		params = Append[params,Rule["target_organism_uri","TargetOrganismURI"/.newparams]]
	)];
	rawdata = Quiet[ImportString[ServiceExecute["OpenPHACTS","RawCompoundCount",Append[params,"_format"->"json"]],"RawJSON"]];
	If[MatchQ[rawdata,$Failed],
   	(
      	Message[ServiceExecute::serrormsg,""];
       	Throw[$Failed]
 	)];
 	rawdata=FixedPoint[Normal,rawdata];
	Dataset[Association["MemberCount"->("memberCount" /. ("primaryTopic" /. ("result" /. rawdata)))]]
]

openphactscookeddata["TargetTypes", id_,args_]:=Block[{rawdata,newparams,params={},invalidParameters,withCamelTitles},
	newparams=args;
	invalidParameters = Select[Keys[newparams],!MemberQ[{},#]&]; 
	If[Length[invalidParameters]>0,
	(
		Message[ServiceObject::noget,#,"OpenPHACTS"]&/@invalidParameters;
		Throw[$Failed]
	)];
	rawdata = Quiet[ImportString[ServiceExecute["OpenPHACTS","RawTargetTypes",Append[params,"_format"->"json"]],"RawJSON"]];
	If[MatchQ[rawdata,$Failed],
   	(
      	Message[ServiceExecute::serrormsg,""];
       	Throw[$Failed]
 	)];
 	rawdata=FixedPoint[Normal,rawdata];
	withCamelTitles=Replace[("hasTargetType" /. ("primaryTopic" /. ("result" /. rawdata))),{Rule[a_, b_] :> Rule[camelCase[a], b],Null -> Missing["NotAvailable"]}, Infinity]/.{"About"->"URI"};
	Dataset[Replace[Select[withCamelTitles, ListQ], r : {__Rule} :> Association[r], -1]]
]

openphactscookeddata["PathwayInformation", id_,args_]:=Block[{rawdata,newparams,params={},invalidParameters,withCamelTitles},
	newparams=args;
	invalidParameters = Select[Keys[newparams],!MemberQ[{"URI"},#]&]; 
	If[Length[invalidParameters]>0,
	(
		Message[ServiceObject::noget,#,"OpenPHACTS"]&/@invalidParameters;
		Throw[$Failed]
	)];
	If[KeyExistsQ[newparams,"URI"],
	(
		If[!StringQ["URI"/.newparams],
		(	
			Message[ServiceExecute::nval,"URI","OpenPHACTS"];
			Throw[$Failed]
		)];
		params = Append[params,Rule["uri","URI"/.newparams]]
	),
	(
		Message[ServiceExecute::nparam,"URI","OpenPHACTS"];
		Throw[$Failed]
	)];
	rawdata = Quiet[ImportString[ServiceExecute["OpenPHACTS","RawPathwayInformation",Append[params,"_format"->"json"]],"RawJSON"]];
	If[MatchQ[rawdata,$Failed],
   	(
      	Message[ServiceExecute::serrormsg,""];
       	Throw[$Failed]
 	)];
 	rawdata=FixedPoint[Normal,rawdata];
	withCamelTitles=Replace[("latest_version" /. ("primaryTopic" /. ("result" /. rawdata))),{Rule[a_, b_] :> Rule[camelCase[a], b],Null -> Missing["NotAvailable"]}, Infinity]/.{"About"->"URI"};
	Dataset[Association[Replace[withCamelTitles, r : {__Rule} :> Association[r], -1]]]
]

openphactscookeddata["PathwayCompounds", id_,args_]:=Block[{rawdata,newparams,params={},invalidParameters,withCamelTitles},
	newparams=args;
	invalidParameters = Select[Keys[newparams],!MemberQ[{"URI"},#]&]; 
	If[Length[invalidParameters]>0,
	(
		Message[ServiceObject::noget,#,"OpenPHACTS"]&/@invalidParameters;
		Throw[$Failed]
	)];
	If[KeyExistsQ[newparams,"URI"],
	(
		If[!StringQ["URI"/.newparams],
		(	
			Message[ServiceExecute::nval,"URI","OpenPHACTS"];
			Throw[$Failed]
		)];
		params = Append[params,Rule["uri","URI"/.newparams]]
	),
	(
		Message[ServiceExecute::nparam,"URI","OpenPHACTS"];
		Throw[$Failed]
	)];
	rawdata = Quiet[ImportString[ServiceExecute["OpenPHACTS","RawPathwayInformationGetCompounds",Append[params,"_format"->"json"]],"RawJSON"]];
	If[MatchQ[rawdata,$Failed],
   	(
      	Message[ServiceExecute::serrormsg,""];
       	Throw[$Failed]
 	)];
 	rawdata=FixedPoint[Normal,rawdata];
	withCamelTitles=Replace[("latest_version" /. ("primaryTopic" /. ("result" /. rawdata))),{Rule[a_, b_] :> Rule[camelCase[a], b],Null -> Missing["NotAvailable"]}, Infinity]/.{"About"->"URI"};
	Dataset[Association[Replace[withCamelTitles, r : {__Rule} :> Association[r], -1]]]
]

openphactscookeddata["PathwayOrganisms", id_,args_]:=Block[{rawdata,newparams,params={},invalidParameters,withCamelTitles,maxitems,startindex,length,inf,sup},
	newparams=args/.{MaxItems:>"MaxItems"};
	invalidParameters = Select[Keys[newparams],!MemberQ[{"StartIndex","MaxItems","SortBy"},#]&]; 
	If[Length[invalidParameters]>0,
	(
		Message[ServiceObject::noget,#,"OpenPHACTS"]&/@invalidParameters;
		Throw[$Failed]
	)];
	If[KeyExistsQ[newparams,"MaxItems"],
	(
		If[!((IntegerQ["MaxItems"/.newparams]&&("MaxItems"/.newparams)>0)||MatchQ["MaxItems"/.newparams,All]),
		(	
			Message[ServiceExecute::nval,"MaxItems","OpenPHACTS"];
			Throw[$Failed]
		)];
		maxitems="MaxItems"/.newparams
	),
  	(
  		maxitems=10
  	)];
	If[KeyExistsQ[newparams,"StartIndex"],
	(
		If[!(IntegerQ["StartIndex"/.newparams]&&("StartIndex"/.newparams)>0),
		(	
			Message[ServiceExecute::nval,"StartIndex","OpenPHACTS"];
			Throw[$Failed]
		)];
		startindex="StartIndex"/.newparams
	),
  	(
  		startindex=1
  	)];
  	If[KeyExistsQ[newparams,"SortBy"],
	(
		If[!StringMatchQ["SortBy"/.newparams, "Count"|"Item"|"Label"|"Pathway"],
		(	
			Message[ServiceExecute::nval,"SortBy","OpenPHACTS"];
			Throw[$Failed]
		)];
		params = Append[params,Rule["_orderBy",("SortBy"/.newparams)/.{"Count"->"?count","Item"->"?item","Label"->"?label","Pathway"->"?pathway"}]]
	)];
	rawdata = Quiet[ImportString[ServiceExecute["OpenPHACTS","RawPathwayOrganisms",Append[Append[params,"_format"->"json"],"_pageSize"->"all"]],"RawJSON"]];
	If[MatchQ[rawdata,$Failed],
   	(
      	Message[ServiceExecute::serrormsg,""];
       	Throw[$Failed]
 	)];
 	rawdata=FixedPoint[Normal,rawdata];
	withCamelTitles=Replace[("items" /. ("result" /. rawdata)),{Rule[a_, b_] :> Rule[camelCase[a], b],Null -> Missing["NotAvailable"]}, Infinity]/.{"About"->"URI"};
	If[MatchQ[maxitems,All],
	(
		Dataset[Replace[withCamelTitles, r : {__Rule} :> Association[r], -1]]
	),
	(
		withCamelTitles=Partition[withCamelTitles,UpTo[maxitems]];
		length=Length[withCamelTitles];
		If[startindex>length,
		(
			Dataset[{}]
		),
		(
			Dataset[Replace[withCamelTitles[[startindex]], r : {__Rule} :> Association[r], -1]]
		)]
	)]
		
]

openphactscookeddata[___]:=$Failed
openphactssendmessage[___]:=$Failed

End[]

End[]

SetAttributes[{},{ReadProtected, Protected}];


(* Return three functions to define oauthservicedata, oauthcookeddata, oauthsendmessage  *)
{OpenPHACTSAPI`Private`openphactsdata,OpenPHACTSAPI`Private`openphactscookeddata,OpenPHACTSAPI`Private`openphactssendmessage}
