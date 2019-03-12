Output: ComputedType[ChannelT[SizeT, TensorT[]], ChannelT[$ColorChannels, TensorT[2, Reverse[$ImageSize]]]]

Parameters:
	$ImageSize: Defaulting[SizeListT[2], {128,128}]
	$ColorSpace: ComputedType[ColorSpaceT, ToColorSpace[$ColorSpace, $ColorChannels], {$ColorChannels}]
	$ColorChannels: ComputedType[Defaulting[SizeT, 3], ToChannelCount[$ColorSpace]]
	$$AugmentationFunction: Defaulting[ExpressionT, None]
	$Parallelize: Defaulting[BooleanT, False]
	$MeanImage: Defaulting[Nullable[EitherT[{ScalarT, ListT[$ColorChannels, ScalarT], ImageT[]}]]]

ToEncoderFunction: Function[
	func = makeEncoderFunction[#1];
	If[#2,
		If[TrueQ[#Parallelize],
			ParallelEvaluate[
				If[DownValues[NeuralNetworks`Encoders`Image`ToReal32Image] === {},
					Internal`InheritedBlock[{$ContextPath, $Context}, Get["NeuralNetworks`"];]
				],
				DistributedContexts -> None
			];
			With[{func = func},
				Function[inputs, ParallelMap[func, inputs, DistributedContexts -> None]]
			],
			Map[func]
		],
		func
	]
]

MLType: Function["Image"]

makeEncoderFunction = Function[
	conformedImage = Image[RandomImage[1, #ImageSize, ColorSpace -> #ColorSpace], "Real32", Interleaving -> False];
	finalizer = MakeFinalizer[#MeanImage, #ImageSize, #ColorSpace];
	augmentor = Replace[#$AugmentationFunction, None -> Identity];
	LoadImage /* ConformColors[#ColorSpace] /* ConformSize[#ImageSize] /* ConformAlpha[#ColorChannels] /*
		ToReal32Image /* augmentor /* finalizer /* If[#ColorChannels === 1, PadRank, Identity]
];

ToChannelCount["Grayscale"] = 1;
ToChannelCount["CMYK"] = 4;
ToChannelCount[Automatic] := SizeT;
ToChannelCount[_] := 3;

Clear[ToColorSpace];
ToColorSpace[space_String, _] := space;
ToColorSpace[_, 1] = "Grayscale";
ToColorSpace[_, 3] = "RGB";
ToColorSpace[_, _Integer] = Automatic;
ToColorSpace[_, SizeT] = "RGB";

MakeFinalizer[None, _, _] := 
	ImageData[#, Interleaving -> False]&;

MakeFinalizer[mean_Image, size_, space_] := 
	With[{mean2 = ColorConvert[ImageResize[mean, size], space]},
		ImageData[
			Image`ArithmeticOperationsDump`imageSubtract[#, mean2],
			Interleaving -> False
		]&
	];

MakeFinalizer[mean_Real | mean_List, size_, space_] :=
	ImageData[#, Interleaving -> False] - mean&;

ConformSize[size_][img_] := If[ImageDimensions[img] === size, img, ImageResize[img, size]];

ConformColors[colors_][img_] := If[ImageColorSpace[img] === colors, img, ColorConvert[img, colors]];

ConformAlpha[channels_][img_] := Switch[
	ImageChannels[img],
	channels + 1, RemoveAlphaChannel[img, White], 
	channels, img,
	_, EncodeFail[StringForm["image had wrong number of color channels (`` instead of ``)", ImageChannels[img], channels]]
];

(* If grayscale image, might need to add channel dim *)
PadRank[array_] := If[ArrayDepth[array] < 3, List[array], array];

LoadImage[File[ipath_String]] := Scope[
	(* infer from filename *)
	path = ExpandFileName[ipath];
	If[!FileExistsQ[path],
		path = FindFile[ipath];
		If[FailureQ[path], EncodeFail[StringForm["path `` does not exist", ipath]]];
	];
	image = Switch[
		ToLowerCase @ FileExtension[path],
		"jpg" | "jpeg",
			First @ Image`ImportExportDump`ImageReadJPEG[path],
		"png",
			First @ Image`ImportExportDump`ImageReadPNG[path],
		"tiff",
			First @ Image`ImportExportDump`ImageReadTIFF[path],
		_,
			Quiet @ Import[path]
	];
	If[!ImageQ[image], EncodeFail["couldn't load image"]];
	image
];

LoadImage[image_Image] := image;

LoadImage[_] := EncodeFail["input is neither a 2D image or a File"];

(* TODO: Investigate using ImageData[.., "Byte"] downstream *)
ToReal32Image[img: HoldPattern[Image[ra_RawArray /; Developer`RawArrayType[ra] =!= "Real32", ___]]] :=
	Image[img, "Real32"];

ToReal32Image[img_Image] := img;

ToReal32Image[_] := EncodeFail["couldn't load image"];
