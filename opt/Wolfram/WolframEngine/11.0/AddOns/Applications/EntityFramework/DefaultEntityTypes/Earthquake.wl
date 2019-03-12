EntityStore["Earthquake" -> <|
	"EntityValidationFunction" -> Function[True],
	"Properties" -> Join[
		<|
			"Label" -> <|
				"DefaultFunction" -> CanonicalName
			|>
		|>,
		AssociationMap[
			<|
				"EntityListDefaultFunction" -> Function[ents, EarthquakeData[ents, #]]
			|> &,
			EarthquakeData["Properties"]
		]
	]
|>]