BeginPackage["CloudObject`"]

System`CloudAccountData;
Hold[System`$CloudCreditsAvailable];

Begin["`Private`"]

CloudAccountData[property_] := Replace[
	CloudAccountData[],
	{
		data_?AssociationQ /; KeyExistsQ[data, property] :> data[property],
		_ :> $Failed
	}
]

CloudAccountData[] := 
	Module[{json, data, basic, subscriptions, storageLimit, storageUsedBytes, 
	   storageUsedMB},
		json = $lastCloudAccountDataJSON = Replace[
			execute[$CloudBase, "GET", {"REST", "user", "subscriptions"}],
			{
				{_, bytes_List} :> FromCharacterCode[bytes],
				other_ :> (
					checkError[other, CloudAccountData];
					Return[$Failed]
				)
			}
		];

		data = $lastCloudAccountData = ImportString[json, "JSON"];
		If[!validAccountDataQ[data],
			Message[CloudAccountData::srverr];
			Return[$Failed]
		];

		basic = Lookup[data, "sharedObject"];
		subscriptions = Lookup[data, "subscriptions", {}];

		storageLimit = Quantity[
					interpretLimit["Number", Lookup[basic, "maxStorage"]], "Megabytes"];

		storageUsedBytes = Quantity[ 
			Replace[
				interpretLimit["Number", Lookup[basic, "usedStorage"]], 
				Except[_Integer] :> 0
				], 
			"Bytes"];
		storageUsedMB = Round@UnitConvert[storageUsedBytes, "Megabytes"];
		
		DeleteCases[
		  <|
			"Products" -> Map[createProduct, subscriptions],
			"CloudStorage" -> storageLimit,
			"CloudStorageUsed" -> storageUsedMB,
			"CloudStorageAvailable" -> 
				Max[Quantity[0, "Megabytes"], storageLimit - storageUsedMB],
			"CloudCreditsAvailable" -> 
			With[{value = Lookup[basic, "credits", Indeterminate]},
				If[value =!= Indeterminate, Interpreter["Integer"][value], value]],
			"WolframAlphaCallsAvailable" -> 
				With[{value = Lookup[basic, "wolframAlphaCalls", Indeterminate]},
					If[value =!= Indeterminate, Interpreter["Integer"][value], value]]
		  |>
		  , 
		  _Missing
		]
	]

validAccountDataQ[data_] := 
	MatchQ[data, _Association | _List] && KeyExistsQ[data, "sharedObject"]

If[!MemberQ[Attributes@$CloudCreditsAvailable,Locked],
	Unprotect[$CloudCreditsAvailable];
	$CloudCreditsAvailable := 
		With[{credits = CloudAccountData["CloudCreditsAvailable"]},
			If[TrueQ[$CloudConnected],
				credits,
				Indeterminate
			]
		];
	Protect[$CloudCreditsAvailable];
]

createProduct[subscription_] := 
	Module[{info = Lookup[subscription, "planInfo"], product, 
		featureMap = createFeatureMap[Lookup[subscription, "planInfo"]],
		limitQuantity},

		product = Lookup[info, "productInfo"];
		
		limitQuantity = 
        	Function[feature, 
        		With[{limitValue = featureMap[feature, "limitValue"], unit = featureMap[feature, "unit"]},
                If[ AnyTrue[{unit, limitValue}, Head[#] === Missing &],
                    Missing["KeyAbsent", feature],
                    Quantity[limitValue, normalizeUnits[StringTrim[unit]]]
                ]
            ]];   
			

		DeleteCases[	
		  <|
			"Product" -> Lookup[product, "name"],
			"Plan" -> Lookup[info, "name"],
			"StartDate" -> formatAccountDate[Lookup[subscription, "startDate"]],
			"EndDate" -> 
				formatAccountDate[Lookup[subscription, "finalAccessDate"]],
			"NextBillingDate" -> 
				formatAccountDate[Lookup[subscription, "nextBillingDate"]],
			"CloudStoragePoolable" -> limitQuantity["storage"],
			"CloudCreditsRecurring" -> limitQuantity["cloudcredits"],
			"CloudCreditsPurchasingAllowed" ->
				TrueQ[featureMap["addcloudcredits", "limitValue"]],
			"WolframAlphaCallsRecurring"->limitQuantity["waapicalls"],
			"DeveloperSeats" -> featureMap["devseats", "limitValue"],
			"TechnicalSupportType" -> featureMap["techsupport", "limitValue"],
			"DesktopAccessAllowed" -> 
				TrueQ[featureMap["desktop", "limitValue"]],
			"LocalFileAccessAllowed" -> 
				TrueQ[featureMap["localfileaccess", "limitValue"]],
			"FileSizeLimit" -> limitQuantity["filesize"],
			"SessionEvaluationTimeLimit" -> limitQuantity["complength"],
			"SessionMemoryLimit" -> limitQuantity["sessionmem"],
			"DeployedEvaluationTimeLimit" -> limitQuantity["publiccomplength"],
			"DeployedMemoryLimit" -> limitQuantity["publicsessionmem"],
			"ScheduledTaskEvaluationTimeLimit" -> 
				limitQuantity["servicecomplength"],
			"ScheduledTaskMemoryLimit" -> limitQuantity["servicesessionmem"]
		  |> 
		  , 
		  _Missing
		]
	]

formatAccountDate[date_Integer] := fromAccountTime[date]
formatAccountDate[_] := None

createFeatureMap[planInfo_] := 
	Association@Map[
  		With[{info = Association[Lookup[#, "cloudPlanFeatureInfo"]]}, 
    		Lookup[info, "shortName"] -> 
			Association@Append[info, 
				"limitValue" -> 
					interpretLimit[info["unitType"], Lookup[#, "limitValue"]]]
		]&,
		Lookup[planInfo, "planFeatureLimitsInfo"]
	]

fromAccountTime[accountTimestamp_] := 
	DateObject[accountTimestamp/1000 + 25567*86400]

interpretLimit[unitType_, value_] := 
	Interpreter[Replace[unitType, "Integer" -> "Number"]][value]

(* Make unit specifications canonical so Quantity doesn't need a network call *)
normalizeUnits[units_] := Replace[
	StringTrim[units],
	{
		"/month" -> 1/"Months",
		"seconds" -> "Seconds",
		"MB" -> "Megabytes",
		"GB" -> "Gigabytes"
	}
]

End[]
EndPackage[]
