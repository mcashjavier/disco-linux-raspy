(* All loading of the paclet's Wolfram Language code should go through this file. *)

(* Developer maintains this list of symbols.
   autoloadSymbols must agree with the symbols listed in the Kernel extension in the PacletInfo.m file.
*)

Begin["MachineLearning`Private`"]

autoloadSymbols = {
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
};

symsToUnprotect = {
	"MachineLearning`PackageScope`Evaluation",
	"MachineLearning`PackageScope`PredictorEvaluation",
	"MachineLearning`PackageScope`ClassifierEvaluation",
	"MachineLearning`PackageScope`BuiltInFunction"
};

symsToProtect = Hold[Module[
	{names},
	names = Join[Names["MachineLearning`*"], Names["MachineLearning`PackageScope`*"]];
	names = Select[names,
		ToExpression[#, InputForm, 
			Function[{sym}, Length[DownValues[sym]] > 0 || Length[SubValues[sym]] > 0, HoldFirst]
		] &
	];
	names = Join[names, autoloadSymbols];
	names = Complement[names, symsToUnprotect];
	names
]];


(***** General messages *****)

General::"mlbdopt" = "Value of option `1` is not valid.";

(*Bad dataset*)
General::"mlbddata" = "The data is not formatted correctly."
General::"mldpsa" = "The dataset cannot be a SparseArray of depth larger than 2."
General::"mlbddataev" = "The data being evaluated is not formatted correctly."
General::"mlbftlgth" = "Examples should have the same number of features."
General::"mlbftlgth2" = "Example `1` should have `2` features instead of `3`."
General::"mlmpty" = "The dataset cannot be empty."
General::"mlinclgth" = "Incompatible lengths: all features should contain the same number of examples."

(*Data processing*)
General::"mlnoprocinv" = "The processor `1` cannot be inverted."
General::"mlinvprocft" = "Invalid feature type. Processor `1` needs feature types `2` instead of `3`"

(* PerformanceGoal *)
General::"mlbdpg" = "Value of option PerformanceGoal \[Rule] `` should be Automatic, \"Speed\", \"Memory\", \"Quality\", \"TrainingSpeed\", or a list of these properties."

(*FeatureTypes*)
General::"mlbftyp" = "Value of option FeatureTypes \[Rule] `1` should be a string, a list of string, an association, or Automatic"
General::"mlbofttypel" = "Value of option FeatureTypes \[Rule] `1` should be a list of length `2`, or an association."
General::"mlukwnfttype" = "Unknown feature type `1` in value of option FeatureTypes \[Rule] `2`. Feature types can be Automatic`3`"
General::"mlunknwnft" = "Warning: feature `1` in value of option FeatureTypes \[Rule] `2` is not in the dataset."
General::"mlnotnomfeat" = "Feature `1` cannot be declared as non-nominal in value of option NominalVariables \[Rule] \[Ellipsis]."
General::"mlincfttp" = "Incompatible variable type (`2`) and variable value (`1`)."

General::"mlbfttype" = "Invalid argument `1`. `3` is a `4` feature."
General::"mlbofttype" = "In value of option FeatureTypes \[Rule] \[Ellipsis], feature `1` cannot be of type `2`."


General::"mlcntinft" = "The feature type of `1` cannot be interpreted."
(*"Nominal features cannot be lists, associations, datasets, or any other data wrappers."*)

General::"mlincvecl" = "Incompatible vector length. Vector `1` should be of size `2` instead of `3`."
General::"mlinctsdim" = "Incompatible tensor dimension. Tensor `1` should be of dimension `2` instead of `3`."
General::"mlzerovec" = "Vector features cannot be of length zero."
General::"mlzerodim" = "Tensor features cannot have an emtpy dimension."

General::"mlnomvar" = "Inactive option NominalVariables. Use FeatureTypes instead."

(* FeatureNames *)
General::"mlbftnm" = "Value of option FeatureNames \[Rule] `1` is not compatible with feature names in the dataset."
General::"mlbftnmf" = "Value of option FeatureNames \[Rule] `1` should be a string, a list of string, or Automatic."
General::"mlbftnmlgth" = " Value of option FeatureNames \[Rule] `1` and the number of features (`2`) do not match."

(* FeatureExtractor *)
General::"mlinvfe" = "Invalid value of option FeatureExtractor."

(* Weights *)
General::"mlbdwt" = "Value of option Weights \[Rule] `` should be Automatic or a list of real positive values."
General::"mlbdwtl" = "The number of weights (`1`) and the number of examples (`2`) should be equal."
General::"mlbdfwtl" = "The number of feature weights (`1`) and the number of features (`2`) should be equal."

(* Options *)
General::"mlbdutil" = "Value of option UtilityFunction \[Rule] `1` is not valid."
General::"mlbdth" = "Value of option IndeterminateThreshold \[Rule] `` should a positive real value."
General::"mlbdoptv" = "Value of option `1` in `2` is not valid."
General::"mlbdcp" = "Value of option ClassPriors \[Rule] `` should be an association of positive values."
General::"mlbdnv" = "Value of option NominalVariables \[Rule] `` should be Automatic, All, None, an integer, a list of integers, a string, or a list of string."



(* Method *)
General::"mluknwnmtd" = "Unknown method `1` in value of option Method. Possible methods are: Automatic`2`."
General::"mlbdmtdind" = "Unknown feature index `1` in method specification `2`."
General::"mlbdmtdkey" = "Unknown feature name `1` in method specification `2`.";

(* Sound *)
General::"mlnosn" = "Sound features cannot contain SoundNote."

(*Interpretation*)
General::"mlintfail" = "Input `1` cannot be interpreted."

(* Property/Method check *)

General::"mlnaset" = "`1` `2` is not available.";
General::"mlnaseth" = "`1` `2` is not available. You could use `3` instead.";
General::"mlnasetl" = "`1` `2` is not available. Possible settings include `3`.";
General::"mlnasethl" = "`1` `2` is not available. You could use `3` instead. Possible settings include `4`.";


(* Miscellaneous *)
General::"mlukwnerr" = "An unknwon error occured."
General::"mlwkey" = "``"
General::"mlwcol" = "Unkown variable index ``. The index of the output variable should a be string or an integer (when variables are unnammed)."
General::"mlcntconf" = "The data cannot be conformed to the appropriate type." (* temporary *)

General::"mlcntclnet" = "This neural network cannot be converted to a ClassifierFunction[\[Ellipsis]]."

(***** End - General messages *****)





Options[findPackageFiles] = {
	"ExcludedDirectories" -> {}
};
findPackageFiles[package_, opts:OptionsPattern[]] := Module[
	{directories, rootdirectory, files, excludeddirectories, excludeddirectorychildren},
	rootdirectory = DirectoryName[$InputFileName];
	directories = Select[
		FileNames[All, rootdirectory, Infinity]
		,
		DirectoryQ
	];
	directories = Prepend[directories, rootdirectory];
	excludeddirectories = OptionValue["ExcludedDirectories"];
	excludeddirectories = FileNameJoin[{rootdirectory, #}] & /@ excludeddirectories;
	excludeddirectorychildren = Select[directories, 
		Function[{dir}, 
			Apply[
				Or, 
				ancesterDirectoryQ[#, dir] & /@ excludeddirectories
			]
		]
	];
	excludeddirectories = Join[excludeddirectories, excludeddirectorychildren];
	directories = Complement[directories, excludeddirectories];
	files = FileNames["*.m", #] & /@ directories;
	files = Flatten[files];
	files = Select[files, packageFileQ[#, package] &]; (* packageFileQ speeds-up the loading *)
	files
];
packageFileQ[file_, package_] := UnsameQ[
	FindList[file, "Package[\"" <> package <> "`\"]", 1]
	, 
	{}
];
ancesterDirectoryQ[ancesterdir_, file_] := Module[
	{a, f},
	a = FileNameSplit[ancesterdir];
	f = FileNameSplit[file];
	And[
		Length[f] > Length[a]
		,
		Take[f, Length[a]] === a
	]
];
Options[reorganizePackageFiles] = {
	"ExcludedDirectories" -> {}
};
reorganizePackageFiles[package_, opts:OptionsPattern[]] := Module[
	{initialfile, originaldir, loadmfiles, originalfiles, newfiles, newdir, dir, filelinks},
	originaldir = DirectoryName[$InputFileName];
	initialfile = FileNameJoin[{originaldir, "InitialFile.m"}];
	loadmfiles = FileExistsQ[initialfile];
	If[loadmfiles,
		originalfiles = findPackageFiles["MachineLearning", opts];
		originalfiles = DeleteDuplicates[Prepend[originalfiles, initialfile]];
		newdir = CreateDirectory[];
		newfiles = MapIndexed[newFileName[originaldir, newdir, #1, #2] &, originalfiles];
		MapThread[CopyFile[#1, #2, OverwriteTarget -> True] &, {originalfiles, newfiles}];
		initialfile = FileNameTake[First[newfiles]];
		filelinks = AssociationThread[
			FileNameDrop[#, FileNameDepth[originaldir]] & /@ originalfiles,
			FileNameTake /@ newfiles
		];
		dir = newdir;
		,
		dir = originaldir;
	];
	{dir, initialfile, loadmfiles, filelinks}
];
newFileName[originaldir_, newdir_, originalfile_, {counter_}] := FileNameJoin[{
	newdir,
	StringJoin[
		"file",
		ToString[counter],
		FileBaseName[originalfile],
		".m"
		(*FileNameSplit[FileNameDrop[originalfile, FileNameDepth[originaldir]]]*)
	]
}];


Options[LoadFlatPackage] = {
	"AutoloadSymbols" -> {},
	"SymbolsToProtect" -> Automatic,
	"HiddenImports" -> {},
	"ExcludedDirectories" -> {}
}; 
LoadFlatPackage::"usage" = "LoadFlatPackage[package] loads all the files of package, including in subdirectories. 
Files in main directory and in subdirectories are considered equivalent in terms of context."
LoadFlatPackage[package_, opts:OptionsPattern[]] := Module[
	{dir, initialfile, loadmfiles, filelinks},
	{dir, initialfile, loadmfiles, filelinks} = 
		reorganizePackageFiles["MachineLearning", "ExcludedDirectories" -> OptionValue["ExcludedDirectories"]];
	PacletManager`Package`loadWolframLanguageCode[
		package, 
		package <> "`", 
		dir, 
		initialfile,
		"AutoUpdate" -> True,
		"Lock" -> False,
		"AutoloadSymbols" -> OptionValue["AutoloadSymbols"],
		"HiddenImports" -> OptionValue["HiddenImports"],
		"SymbolsToProtect" -> OptionValue["SymbolsToProtect"]
	];
	If[loadmfiles && dir =!= DirectoryName[$InputFileName], (* to be safe *)
		DeleteDirectory[dir, DeleteContents -> True];
	];
	filelinks
];

$filelinks = LoadFlatPackage["MachineLearning", 
	"ExcludedDirectories" -> {},
	"AutoloadSymbols" -> autoloadSymbols,
	"HiddenImports" -> {"PacletManager`", "Developer`", "GeneralUtilities`"},
	"SymbolsToProtect" -> symsToProtect
];


(* Provide a way for tools to know that this package uses tabs for indentation. *)
$useTabsOrSpaces = "Tabs";

End[];

