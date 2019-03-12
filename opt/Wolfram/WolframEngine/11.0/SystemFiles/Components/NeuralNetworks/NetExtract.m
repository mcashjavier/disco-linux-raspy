Package["NeuralNetworks`"]

PackageImport["GeneralUtilities`"]
PackageImport["Developer`"]


PackageExport["NetExtract"]

NetExtract[head_[assoc_Association] ? System`Private`HoldNoEntryQ, spec_] := 
	CatchFailure @ extract[spec, assoc];

NetExtract[spec_][expr_] := NetExtract[expr, spec];


extract[spec_, assoc_] := extractOne[spec, assoc];

extract[spec_List, assoc_] := Apply[extractList, spec] @ assoc;

Clear[extractOne, extractList];
extractOne[spec:(All | _List), assoc_] := toValue /@ dekey @ getElement[assoc, spec];
extractOne[pos_Integer | pos_String, assoc_] := toValue @ getElement[assoc, pos];

NetExtract::invspec = "`` is not a valid specification for NetExtract."
extractOne[spec_, _] := ThrowFailure["invspec", spec];

extractList[][data_] := toValue @ data;
extractList[spec:(All | _List), rest___][data_] := extractList[rest] /@ dekey @ getElement[data, spec];
extractList[pos_, rest___][data_] := extractList[rest] @ getElement[data, pos];

(* TODO: make this a method, part of a container API *)
getElement[_, 0] := ThrowFailure["nopart", 0];
getElement[assoc_, spec_] := Switch[
	assoc["Type"],
	"Graph", Internal`UnsafeQuietCheck[assoc[["Vertices", spec]], ThrowFailure["nopart", spec]],
	"Chain", Internal`UnsafeQuietCheck[assoc[["Layers", spec]], ThrowFailure["nopart", spec]],
	_String, getParam[assoc, spec],
	_, ThrowFailure["nopart", spec]
];

(* undo the associationification that happens during construction for 
pure list versions of NetGraph and NetChain *)
dekey[assoc_Association] := 
	If[DigitStringKeysQ[assoc], Values @ assoc, assoc];

dekey[e_] := e;

NetExtract::nopart = "Part `` does not exist.";

getParam[assoc_Association, spec_] := Block[{},
	Do[	
		If[AssociationQ[subassoc] && KeyExistsQ[subassoc, spec],
			Return[subassoc[spec], Block]
		],
		{subassoc, Values[assoc]}
	];
	toValue @ Lookup[assoc, spec, ThrowFailure["nopart", spec]]
];

getParam[_, spec_] := ThrowFailure["nopart", spec];
 
toValue[assoc_Association] /; KeyExistsQ[assoc, "Type"] := ConstructLayer[assoc];
toValue[list_List /; !PackedArrayQ[list]] := Map[toValue, list];
toValue[assoc_Association] := Map[toValue, assoc];
toValue[ra_RawArray] := Normal[ra];
toValue[sym_Symbol] := If[Context[sym] === "System`", sym, Indeterminate];
toValue[e_ ? AtomQ] := e;
toValue[e_] := Automatic;


pfail[msg_, args___] := ThrowTaggedFailure["MissingPart", msg, args];
