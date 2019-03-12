(*******************************************************************************

Optimization Package. 

*******************************************************************************)

Package["MXNetLink`"]

PackageImport["GeneralUtilities`"]	

PackageExport["MXOptimizerCreate"]
PackageScope["$MXOptimizerDefaults"]

(******************************************************************************)
(****** Load Library Functions ******)

DeclareLibraryFunction[mxOptimizerCreateOptimizer, "WL_MXOptimizerCreateOptimizer", 
	{
		Integer, 						(* optimizer handle key *)				
		"UTF8String", 		(* optimizer name *)
		"UTF8String",		(* keys *)
		"UTF8String"		(* vals *)
	}, 
	"Void"						
]	

DeclareLibraryFunction[mxOptimizerUpdate, "WL_MXOptimizerUpdate", 
	{
		Integer, 		(* optimizer handle key *)				
		Integer, 		(* index *)
		Integer,		(* weight *)
		Integer,		(* grad *)
		Real,			(* learning rate *)
		Real			(* L2 Reg *)
	}, 
	"Void"						
]	

(******************************************************************************)
(* CPP Optimizer interface *)

MXNetOptimizerCreate[optimName_, params_] := Scope[
	mxInit[];
	optimHandle = CreateManagedLibraryExpression["MXOptimizer", MXOptimizerCpp];
	System`Private`SetNoEntry@optimHandle;
	mxparams = mxStringParameterFormat@params;
	MXNetInvoke[mxOptimizerCreateOptimizer, MLEID@optimHandle, optimName, mxparams["Keys"], mxparams["Values"]];
	optimHandle
]

MXNetOptimizerUpdate[optimHandle_MXOptimizerCpp, weight_NDArray, grad_NDArray, lr_, l2reg_] := Scope[
	MXNetInvoke[mxOptimizerUpdate, MLEID@optimHandle, MLEID@optimHandle, MLEID@weight, MLEID@grad, lr, l2reg];
]


(******************************************************************************)
(* Utilities*)

listify[param_, len_] := If[ListQ@param, param, ConstantArray[param, len]];

(******************************************************************************)
(* 
	SGDOptimizer: 
	An efficient C++ implementation of Stochastic Gradient Descent with momentum and l2 reg.
	No auxilliary states are used for momentum, halving memory usage.
*)

PackageExport["CreateSGDOptimizer"]

(* 1. Defaults *)
Options[CreateSGDOptimizer] = {
	"InitialLearningRate" -> 0.001,
	"Momentum" -> 0.93,
	"RescaleGradient" -> 1,
	"GradientClipping" -> None,
	"L2Regularization" -> 0.0,
	"LearningRateSchedule" -> Automatic
};

General::badlrs = "Learning rate scheduler should return a real value between 0 and 1."

(* 2. Initialization *)
CreateSGDOptimizer[weights_List, grads_List, maxIters_, OptionsPattern[]] := Scope[	
	(* overwrite defaults with specified params *)
	(* TODO: use UnpackAssociation on config? Or UnpackOptions? *)
	len = Length[weights];
	(* params for optim *)
	UnpackOptions[
		gradientClipping, momentum, rescaleGradient, initialLearningRate, 
		l2Regularization, learningRateSchedule
	];
	gradientClipping = ReplaceAll[gradientClipping, None -> -1]; 
	handles = MapThread[
		MXNetOptimizerCreate["ccsgd", <|"momentum" -> #1, "rescale_grad" -> #2, "clip_gradient" -> #3|>]&, 
		{
			listify[momentum, len], 
			listify[rescaleGradient, len],
			listify[gradientClipping, len]
		}
	];
	initialLearningRate = listify[initialLearningRate, len];

	learningRateSchedule = Replace[learningRateSchedule, "Polynomial" -> polynomialScheduler];
	checkUnitReal @ N @ learningRateSchedule[0,100,0.5];

	NewIterator["SGDOptimizer",
		{
		$InitialLearningRate = initialLearningRate, 
		$CurrentLearningRate = initialLearningRate,
		$L2Regularization = listify[l2Regularization, len],
		$LearningRateSchedule = learningRateSchedule,
		$OptimizerHandles = handles,
		$IterationNumber = 0,
		$Weights = weights,
		$Gradients = grads,
		$MaxIterations = maxIters
		},
		ScanThread[
			MXNetOptimizerUpdate,
			{$OptimizerHandles, $Weights, $Gradients, $CurrentLearningRate, $L2Regularization}
		];
		$IterationNumber++;
		If[$LearningRateSchedule =!= None,
			$CurrentLearningRate = Map[
				checkUnitReal @ N @ $LearningRateSchedule[$IterationNumber, $MaxIterations, #]&,
				$InitialLearningRate
			]
		];
	]
]

checkUnitReal[r_] := If[Developer`MachineRealQ[r] && 0. < r < 1., r, ThrowFailure["badlrs"]];

polynomialScheduler[iter_, maxiters_, rate_] := 
	rate * Re@Sqrt[1. - iter/(maxiters + 1.)];


PackageExport["CreateADAMOptimizer"]

(******************************************************************************)
(* 
	ADAM:
		"ADAM: A METHOD FOR STOCHASTIC OPTIMIZATION"
		D. Kingma et al, 2015
		http://arxiv.org/pdf/1412.6980v8.pdf
		Following the Torch implementation: https://github.com/torch/optim/blob/master/adam.lua
*)


PackageExport["RunOptimizerStep"]

RunOptimizerStep[Iterator[_, body_, _]] := (body; Null);

RunOptimizerStep[_] := Panic["InvalidArgs", "Invalid arguments provided to RunOptimizerStep."];


PackageExport["CreateOptimizer"]

CreateOptimizer[method_String, weights_, grads_, maxIters_, settings_] := Scope[
	func = Match[method, 
		"ADAM" :> CreateADAMOptimizer, 
		"SGD" :> CreateSGDOptimizer
	];
	func[weights, grads, maxIters, FilterOptions[func, settings]]
];

(* 1. Defaults *)
Options[CreateADAMOptimizer] = {
	"InitialLearningRate" -> 0.001,
	"Beta1" -> 0.93,
	"Beta2" -> 0.999,
	"Epsilon" -> 10^-8,
	"L2Regularization" -> 0.
}

CreateADAMOptimizer[weights_List, gradients_List, maxIters_, OptionsPattern[]] := Scope[
	
	UnpackOptions[initialLearningRate, beta1, beta2, epsilon, l2Regularization];

	len = Length[weights];
	dims = Dimensions /@ weights;
	
	metadata = iNDArrayMetadata@First@weights;
	mean = Map[NDArrayCreateZero[#, Sequence@@metadata]&, dims];
	var = Map[NDArrayCreateZero[#, Sequence@@metadata]&, dims];
	
	(* A temporary state to hold sqrt(var) + epsilon *)
	temp = Map[NDArrayCreateZero[#, Sequence@@metadata]&, dims];

	NewIterator["AdamOptimizer", 
		{
		$Variance = var, 
		$Time = ConstantArray[0., len], 
		$Beta1 = listify[N@beta1, len],
		$Beta2 = listify[N@beta2, len],
		$Epsilon = listify[N@epsilon, len],
		$InitialLearningRate = listify[N@initialLearningRate, len],
		$L2Regularization = listify[N@l2Regularization, len],
		$Weights = weights,
		$Gradients = gradients,
		$Temporary = temp,
		$Temporary2 = Map[NDArrayCopy, temp],
		$Mean = mean
		}
		,
		$Time++;
		ScanThread[
			ADAMOptimizerUpdate,
			{
				$Weights, $Gradients, $Time, $Temporary, $Temporary2, $Mean, $Variance, $Beta1, $Beta2,
				$Epsilon, $InitialLearningRate, $L2Regularization
			}
		]
	]
]

ADAMOptimizerUpdate[weight_NDArray, grad_NDArray, time_, temp_NDArray, temp2_NDArray,
	mean_NDArray, variance_NDArray, beta1_, beta2_, eps_, learnRate_, L2reg_] := Scope[

	(* mean = beta1 * mean + (1-beta1) * grad *)
	NDArraySetScaledPlus[mean, beta1, mean, 1-beta1, grad];

	(* variance = beta2 * variance + (1-beta2) * grad^2 *)
	NDArraySetSquare[temp, grad];
	NDArraySetScaledPlus[variance, beta2, variance, 1-beta2, temp];
		
	(* compute the new step size *)
	biasCorrection1 = 1. - beta1^time;
	biasCorrection2 = 1. - beta2^time;
	dx = learnRate * Sqrt[biasCorrection2] / biasCorrection1;

	(* weight = (1 - dx*L2reg) * weight  +  
	                       -dx * (mean / (sqrt(variance) + eps)) *)
	NDArraySetSqrt[temp, variance];
	NDArrayAddTo[temp, eps];
	NDArraySetDivide[temp2, mean, temp];
	NDArraySetScaledPlus[weight, 1-dx*L2reg, weight, -dx, temp2];
]

ADAMOptimizerUpdate[___] := Panic["InvalidArgs", "Invalid args provided to AdamOptimizerUpdate."];