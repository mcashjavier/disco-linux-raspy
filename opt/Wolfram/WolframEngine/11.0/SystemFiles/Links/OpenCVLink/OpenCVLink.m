(* ::Package:: *)

BeginPackage["OpenCVLink`"]
Begin["`Private`"]

$InitOpenCVLink = False;

$ThisDirectory = FileNameDrop[$InputFileName, -1]
$BaseLibraryDirectory = FileNameJoin[{$ThisDirectory, "LibraryResources", $SystemID}];
$OpenCVLinkLibrary = "OpenCVLink";
dlls["Windows"|"Windows-x86-64"] = {"opencv_core310", "opencv_flann310", "opencv_imgproc310",  
				   "opencv_features2d310", "opencv_imgcodecs310", "opencv_videoio310", "opencv_highgui310", 
				   "opencv_ml310", "opencv_objdetect310", "opencv_photo310",
				   "opencv_video310", "opencv_shape310", "opencv_calib3d310", "opencv_stitching310", 
				   "opencv_superres310","opencv_videostab310"};

dlls["MacOSX-x86-64" | "MacOSX-x86"] = {"libopencv_core.3.1.dylib", "libopencv_flann.3.1.dylib",
                         "libopencv_imgproc.3.1.dylib", "libopencv_features2d.3.1.dylib",
                         "libopencv_imgcodecs.3.1.dylib", "libopencv_videoio.3.1.dylib",
                         "libopencv_highgui.3.1.dylib", "libopencv_ml.3.1.dylib",
                         "libopencv_objdetect.3.1.dylib", "libopencv_photo.3.1.dylib",
                         "libopencv_video.3.1.dylib", "libopencv_shape.3.1.dylib",
                         "libopencv_calib3d.3.1.dylib", "libopencv_stitching.3.1.dylib",
                         "libopencv_superres.3.1.dylib", "libopencv_videostab.3.1.dylib"};

dlls["Linux"|"Linux-x86-64"] = {"libopencv_core.so.3.1", "libopencv_flann.so.3.1",
                                "libopencv_imgproc.so.3.1", "libopencv_ml.so.3.1",
                                "libopencv_photo.so.3.1", "libopencv_imgcodecs.so.3.1",
                                "libopencv_video.so.3.1", "libopencv_videoio.so.3.1",
                                "libopencv_highgui.so.3.1", "libopencv_shape.so.3.1",
                                "libopencv_superres.so.3.1", "libopencv_features2d.so.3.1",
                                "libopencv_objdetect.so.3.1", "libopencv_calib3d.so.3.1",
                                "libopencv_stitching.so.3.1", "libopencv_videostab.so.3.1"};
dlls[___] := $Failed;

safeLibraryLoad[debug_, lib_] :=
	Quiet[
		Check[
			LibraryLoad[lib],
			If[TrueQ[debug],
				Print["Failed to load ", lib]
			];
			Throw[$InitOpenCVLink = $Failed]
		]
	]
safeLibraryFunctionLoad[debug_, args___] :=
	Quiet[
		Check[
			LibraryFunctionLoad[$OpenCVLinkLibrary, args],
			If[TrueQ[debug],
				Print["Failed to load the function ", First[{args}], " from ", $OpenCVLinkLibrary]
			];
			Throw[$InitOpenCVLink = $Failed]
		]
	]
  
InitOpenCVLink[debug_:False] := If[TrueQ[$InitOpenCVLink],
	$InitOpenCVLink,
	$InitOpenCVLink = Catch[
	  If[dlls[$SystemID] === $Failed,
	  	Message[OpenCVLink::sys, "Incompatible SystemID"];
	  	Throw[$Failed]
	  ];
	  Block[{$LibraryPath = Prepend[$LibraryPath, $BaseLibraryDirectory]},
		  safeLibraryLoad[debug, #]& /@ Flatten[{dlls[$SystemID], $OpenCVLinkLibrary}];
		  $Dilation = safeLibraryFunctionLoad[debug, "opencv_Dilation", {{"Image", "Constant"}, Integer}, {"Image"}];
          $MedianFilter = safeLibraryFunctionLoad[debug,"opencv_MedianFilter",{{"Image","Constant"}, Integer},{"Image"}];
		  $ImagePad = safeLibraryFunctionLoad[debug,"opencv_ImagePad",{{"Image","Constant"},Integer,Integer,Integer,Integer,"UTF8String",{_Real,1}},{"Image"}];
		  $ImagePerspectiveTransformation = safeLibraryFunctionLoad[debug, "opencv_ImagePerspectiveTransformation", {{"Image", "Constant"}, {_Real,2}, "UTF8String", "UTF8String", {_Real,1}}, {"Image"}];
		  $ImageConvolve = safeLibraryFunctionLoad[debug,"opencv_ImageConvolve",{{"Image", "Constant"},{_Real,1},{_Real,1},"UTF8String",{_Real,1}},{"Image"}];
		  $LocalAdaptiveBinarize = safeLibraryFunctionLoad[debug,"opencv_LocalAdaptiveBinarize",{{"Image","Constant"},Integer,{Real,1},"UTF8String",Real},{"Image"}];
 		  $SuperResolution = safeLibraryFunctionLoad[debug, "opencv_SuperResolution",{"UTF8String", "UTF8String", Integer},{"Image"}];
 		  $PyrDown = safeLibraryFunctionLoad[debug, "opencv_PyrDown",{{"Image","Constant"}, "UTF8String",_Integer,_Integer},{"Image"}];
 		  $PyrUp = safeLibraryFunctionLoad[debug, "opencv_PyrUp",{{"Image","Constant"},_Integer,_Integer},{"Image"}];    
		  $HoughLines = safeLibraryFunctionLoad[debug, "opencv_HoughLines", { {"Image", "Constant"}, _Real, _Real, _Integer, _Real, _Real, "UTF8String"}, {_Real, 1}];
		  $HoughLinesP = safeLibraryFunctionLoad[debug, "opencv_HoughLinesP", { {"Image", "Constant"}, _Real, _Real, _Integer, _Real, _Real}, {_Real, 1}];
		  $HoughCircles = safeLibraryFunctionLoad[debug, "opencv_HoughCircles", {{"Image", "Constant"}, _Real, _Real, _Real, _Real, _Integer, _Integer}, {_Real,1}];
		  $LineSegmentDetector = safeLibraryFunctionLoad[debug, "opencv_LineSegmentDetector", { {"Image", "Constant"}}, {_Real, 1}];
		  $PhaseCorrelate = safeLibraryFunctionLoad[debug, "opencv_PhaseCorrelate",{{"Image","Constant"},{"Image","Constant"},{_Real,_,"Constant"}},{_Real,1}];
		  $PyrMeanShiftFilter = safeLibraryFunctionLoad[debug, "opencv_PyrMeanShiftFilter",{{"Image","Constant"},_Real,_Real,_Integer,_Integer},{"Image"}];
          $FastNlMeansDenoising = safeLibraryFunctionLoad[debug, "opencv_FastNlMeansDenoising",{{"Image","Constant"}, Real,Integer,Integer},{"Image"}];
		  $FastNlMeansDenoisingColored = safeLibraryFunctionLoad[debug,"opencv_FastNlMeansDenoisingColored",{{"Image","Constant"},Real,Real,Integer,Integer},{"Image"}];
		  $FastNlMeansDenoisingMulti = safeLibraryFunctionLoad[debug,"opencv_FastNlMeansDenoisingMulti",{{"Image3D","Constant"}, Integer, Integer, Real, Integer, Integer},{"Image"}];
		  $FastNlMeansDenoisingColoredMulti = safeLibraryFunctionLoad[debug,"opencv_FastNlMeansDenoisingColoredMulti",{{"Image3D","Constant"}, Integer, Integer, Real, Real, Integer, Integer},{"Image"}];
		  $MergeMertens = safeLibraryFunctionLoad[debug,"opencv_MergeMertens",{{"Image3D","Constant"}},{"Image"}];
		  $Decolor = safeLibraryFunctionLoad[debug,"opencv_Decolor",{{"Image","Constant"}},{"Image"}];
		  $ColorBoost = safeLibraryFunctionLoad[debug,"opencv_Color_Boost",{{"Image","Constant"}},{"Image"}];
		  $EarthMoverDistanceFlowMatrix = safeLibraryFunctionLoad[debug,"opencv_EarthMoverDistanceFlowMatrix",{{Real,2},{Real,1},{Real,2},{Real,1}, String, {Real,2}},{Real,2}];
 		  $EarthMoverDistance = safeLibraryFunctionLoad[debug,"opencv_EarthMoverDistance",{{Real,2},{Real,1},{Real,2},{Real,1}, String,{Real,2}},Real];
 		  $EarthMoverDistanceWeightsFlowMatrix = safeLibraryFunctionLoad[debug,"opencv_EarthMoverDistanceWeightsFlowMatrix",{{Real,1},{Real,1}, {Real,2}},{Real,2}];
 		  $ImageCorrelate = safeLibraryFunctionLoad[debug, "opencv_ImageCorrelate",{{"Image","Constant"},{"Image","Constant"},"UTF8String","UTF8String",Real},{"Image"}]; 
		  $OpticalFlowFarneback = safeLibraryFunctionLoad[debug, "opencv_OpticalFlowFarneback",{{"Image3D", "Constant"}, Real, Integer, Real, Integer, Integer, Real, "UTF8String", "Boolean", {Real,3}, Real}, {Real,4}];
		  $OpticalFlowDualTVL1 = safeLibraryFunctionLoad[debug, "opencv_OpticalFlowDualTVL1",{{"Image3D", "Constant"}, Real, Integer, Real, Integer, Real, Real, Integer, "Boolean", {Real,3}, Real}, {Real,4}];
		  $Stylization = safeLibraryFunctionLoad[debug, "opencv_Stylization",{{"Image","Constant"}, _Real, _Real},{"Image"}];
		  $FastDetect = safeLibraryFunctionLoad[debug, "opencv_FastDetect", {{"Image", "Constant"},"Boolean",{"Image","Constant"}, Integer, "Boolean", Integer},{_Real,2}];
		  $EdgePreservingFilter = safeLibraryFunctionLoad[debug, "opencv_EdgePreservingFilter",{{"Image","Constant"},"UTF8String",_Real,_Real},{"Image"}];
		  $DetailEnhance = safeLibraryFunctionLoad[debug, "opencv_DetailEnhance",{{"Image","Constant"},_Real,_Real},{"Image"}];
		  $ImageDemosaic = safeLibraryFunctionLoad[debug,"opencv_imageDemosaic",{{"Image","Constant"},"UTF8String"},{Image}];
   		  $MergeRobertson = safeLibraryFunctionLoad[debug,"opencv_MergeRobertson", {{"Image3D", "Constant"}, {Real, 1}, {Real, 1}}, {"Image"}];
	      $CalibrateRobertson = safeLibraryFunctionLoad[debug,"opencv_CalibrateRobertson", {{"Image3D", "Constant"}, {Real, 1}, Integer, Real}, {Real, 1}];
	      $RemapTensor = safeLibraryFunctionLoad[debug,"opencv_RemapTensor",{{"Image","Constant"},"UTF8String","UTF8String",{Real,1},{Real,2},{Real,2}},{"Image"}];
	      $LogPolarCV = safeLibraryFunctionLoad[debug, "opencv_LogPolarCV",{{"Image", "Constant"},{Real,1},Real,"UTF8String","Boolean","Boolean"},{"Image"}];
 		  $LogPolarRemap = safeLibraryFunctionLoad[debug, "opencv_LogPolarRemap",{{"Image", "Constant"},{Real,1},Real,"UTF8String","Boolean","Boolean"},{"Image"}];
 		  $Tonemap = safeLibraryFunctionLoad[debug,"opencv_Tonemap",{{"Image","Constant"},String,{Real,1}},{"Image"}];
 		  $MergeDebevec = safeLibraryFunctionLoad[debug, "opencv_MergeDebevec",{{"Image3D","Constant"},{Real,1},{Real,1}},{"Image"}];
		  $CalibrateDebevec = safeLibraryFunctionLoad[debug,"opencv_CalibrateDebevec", {{"Image3D","Constant"},{Real,1}, Integer, Real, "Boolean"},{Real,1}];
		  $AlignMTB = safeLibraryFunctionLoad[debug,"opencv_AlignMTB",{{"Image3D","Constant"}, Integer, Integer},{"Image3D"}];
		  $CalibrateDebevec = safeLibraryFunctionLoad[debug,"opencv_CalibrateDebevec", {{"Image3D","Constant"},{Real,1}, Integer, Real, "Boolean"},{Real,1}];		  
 True
	]
	]

]

InitOpenCVLink[];
     
End[]
EndPackage[]

