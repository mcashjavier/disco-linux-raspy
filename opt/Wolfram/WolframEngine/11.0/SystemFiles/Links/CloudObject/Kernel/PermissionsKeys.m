BeginPackage["CloudObject`"]

System`PermissionsKey;
System`DeletePermissionsKey;
System`PermissionsKeys;

Begin["`Private`"]

validatePermissionsKey[PermissionsKey[key_String]] := validatePermissionsKey[key]

validatePermissionsKey[key_String] := StringLength[key] > 0

validatePermissionsKey[PermissionsKey[]] := True
    
validatePermissionsKey[key_] := False

(* PermissionsKey *)

PermissionsKey[] := PermissionsKey[CreateUUID[]]
    
(* DeletePermissionsKey *)

DeletePermissionsKey[PermissionsKey[key_]]:= DeletePermissionsKey[key]

DeletePermissionsKey[key_] :=
	If[validatePermissionsKey[key]
			,
			Replace[
				execute[$CloudBase, "DELETE", {"permissionskeys", key}],
				{
					{_String, {}} :> Null,
					HTTPError[404, ___] :> (Message[DeletePermissionsKey::keynf, key]; $Failed),
					other_ :> (checkError[other, DeletePermissionsKey]; $Failed)
				}
			]
			,
			Message[DeletePermissionsKey::invkey, key]; 
			$Failed
		]
	
DeletePermissionsKey[args___] := (ArgumentCountQ[DeletePermissionsKey,Length[DeleteCases[{args},_Rule,Infinity]],1,1];Null/;False)	

(* PermissionsKeys *)

PermissionsKeys[] :=
	Replace[
		execute[$CloudBase, "GET", {"permissionskeys"}],
		{
			{_String, res:{_Integer ...}} :> constructPermsKeyList[res],
			other_ :> (checkError[other, PermissionsKeys]; $Failed)
		}
	]
	
PermissionsKeys[args___] := (ArgumentCountQ[PermissionsKeys,Length[DeleteCases[{args},_Rule,Infinity]],0,0];Null/;False)

constructPermsKeyList[bytes_] :=
	With[{json = ImportString[FromCharacterCode[bytes], "JSON"]},
		Map[PermissionsKey, json]
	]
		
End[]

EndPackage[]
