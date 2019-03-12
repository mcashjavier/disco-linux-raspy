Paclet[
	Name -> "MachineLearning",
	Version -> "1.0.0",
	MathematicaVersion -> "10+",
	Description -> "Automatic machine learning functionality",
	Creator -> "Etienne Bernard <etienneb@wolfram.com>, Sebastian Bodenstein <sebastianb@wolfram.com>",
	Loading -> Automatic,
	Extensions -> {
		{
			"Resource", 
			Root -> "Resources", 
			Resources -> {"Binaries", "Unicode", "SentenceSplitter", "Libraries"}
		},
		{
			"Resource",
			Root -> "Resources",
			SystemID -> "Windows",
			Resources -> {{"KenLM", "Binaries/Windows/lmplz.exe"}}
		},
		{
			"Resource",
			Root -> "Resources",
			SystemID -> "Windows-x86-64",
			Resources -> {{"KenLM", "Binaries/Windows-x86-64/lmplz.exe"}}
		},
		{
			"Resource",
			Root -> "Resources",
			SystemID -> "Linux",
			Resources -> {{"KenLM", "Binaries/Linux/lmplz"}}
		},
		{
			"Resource",
			Root -> "Resources",
			SystemID -> "Linux-x86-64",
			Resources -> {{"KenLM", "Binaries/Linux-x86-64/lmplz"}}
		},
		{
			"Resource",
			Root -> "Resources",
			SystemID -> "MacOSX-x86-64",
			Resources -> {{"KenLM", "Binaries/MacOSX-x86-64/lmplz"}}
		},
		{
			"Kernel", 
			Context -> {"MachineLearningLoader`", "MachineLearning`"}, 
			Symbols -> {
				"System`Classify", 
				"System`ClassifierFunction", 
				"System`ClassifierMeasurements",
				"System`ClassifierMeasurementsObject",
				"System`ClassifierInformation",
				"System`ClassPriors",
				"System`IndeterminateThreshold",
				
				"System`Predict",
				"System`PredictorFunction",
				"System`PredictorMeasurements",
				"System`PredictorMeasurementsObject",
				"System`PredictorInformation",
				
				"System`UtilityFunction",
				"System`ValidationSet",
				"System`FeatureTypes",
				"System`FeatureNames",
				
				"System`DimensionReduction",
				"System`DimensionReduce",
				"System`DimensionReducerFunction",
				
				"System`ImageIdentify",
				"System`ImageInstanceQ",
				"System`SpecificityGoal",
				"System`RecognitionThreshold",
				"System`RecognitionPrior",
				
				"System`DistanceMatrix",
				"System`ConformationRules",
				
				"System`ClusterClassify",
				"System`CriterionFunction",
				
				"System`FeatureExtract",
				"System`FeatureExtraction",
				"System`FeatureExtractorFunction",
				"System`FeatureExtractor",
				"System`FeatureDistance",

				"System`FindClusters",
				"System`ClusteringComponents",
				
				"System`BayesianMinimization",
				"System`BayesianMaximization",
				"System`BayesianMinimizationObject",
				"System`BayesianMaximizationObject",
				"System`AssumeDeterministic",
				"System`InitialEvaluationHistory"
			}
		}
	}
]


