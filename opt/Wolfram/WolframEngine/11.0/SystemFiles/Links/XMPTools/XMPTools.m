BeginPackage["XMPTools`"]
Begin["`Private`"]

$InitXMPTools = False;

$ThisDirectory = FileNameDrop[$InputFileName, -1]
$BaseLibraryDirectory = FileNameJoin[{$ThisDirectory, "LibraryResources", $SystemID}];
$XMPToolsLibrary = "XMPTools";

safeLibraryLoad[debug_, lib_] :=
	Quiet[
		Check[
			LibraryLoad[lib],
			If[TrueQ[debug],
				Print["Failed to load ", lib]
			];
			Throw[$InitXMPTools = $Failed]
		]
	]
safeLibraryFunctionLoad[debug_, args___] :=
	Quiet[
		Check[
			LibraryFunctionLoad[$XMPToolsLibrary, args],
			If[TrueQ[debug],
				Print["Failed to load the function ", First[{args}], " from ", $XMPToolsLibrary]
			];
			Throw[$InitXMPTools = $Failed]
		]
	]
    
InitXMPTools[debug_:False] := If[TrueQ[$InitXMPTools],
	$InitXMPTools,
	$InitXMPTools = Catch[
	  Block[{$LibraryPath = Prepend[$LibraryPath, $BaseLibraryDirectory]},
		  safeLibraryLoad[debug, $XMPToolsLibrary];
		  (*Init*)
		  $XMPInitialize = safeLibraryFunctionLoad[debug, "XMPInitialize", {"UTF8String"}, "Boolean"];
		  $XMPUnInitialize = safeLibraryFunctionLoad[debug, "XMPUnInitialize", {}, "Boolean"];
		  (*Exif reading*)
		  $ReadExifAllRaw = safeLibraryFunctionLoad[debug, "ReadExifAllRaw", {"Boolean"}, "UTF8String"];
		  $ReadExifIndividualTag = safeLibraryFunctionLoad[debug, "ReadExifIndividualTag", {"UTF8String"}, "UTF8String"];
		  (*Exif writing*)
		  $WriteExifInt = safeLibraryFunctionLoad[debug, "WriteExifInt", {{"UTF8String"}, _Integer}, {"Boolean"}];
		  $WriteExifReal = safeLibraryFunctionLoad[debug, "WriteExifReal", {{"UTF8String"}, _Real}, {"Boolean"}];
		  $WriteExifString = safeLibraryFunctionLoad[debug, "WriteExifString", {{"UTF8String"}, {"UTF8String"}}, {"Boolean"}];
		  (*XMP reading*)
		  $ReadXMPAll = safeLibraryFunctionLoad[debug, "ReadXMPAll", {}, "UTF8String"];
		  $ReadXMPAllRaw = safeLibraryFunctionLoad[debug, "ReadXMPAllRaw", {}, "UTF8String"];
		  (*XMP writing*)
		  $WriteXMPNumber = safeLibraryFunctionLoad[debug, "WriteXMPNumber", {{"UTF8String"}, _Integer}, {"Boolean"}];
		  $WriteXMPString = safeLibraryFunctionLoad[debug, "WriteXMPString", {{"UTF8String"}, {"UTF8String"}}, {"Boolean"}];
		  $WriteXMPStructure = safeLibraryFunctionLoad[debug, "WriteXMPStructure", {{"UTF8String"}, {"UTF8String"}, {"UTF8String"}}, {"Boolean"}];
		  (*IPTC reading*)
		  $ReadIPTCAll = safeLibraryFunctionLoad[debug, "ReadIPTCAll", {}, "UTF8String"];
		  $ReadIPTCAllRaw = safeLibraryFunctionLoad[debug, "ReadIPTCAllRaw", {}, "UTF8String"];
		  $ReadIPTCIndividualTag = safeLibraryFunctionLoad[debug, "ReadIPTCIndividualTag", {"UTF8String"}, "UTF8String"];
		  (*IPTC writing*)
		  $WriteIPTCInt = safeLibraryFunctionLoad[debug, "WriteIPTCInt", {{"UTF8String"}, _Integer}, {"Boolean"}];
		  $WriteIPTCString = safeLibraryFunctionLoad[debug, "WriteIPTCString", {{"UTF8String"}, {"UTF8String"}}, {"Boolean"}];
	  ];
	  True
	]
]

(**************************)
(**************************)
(**************************)
(*********ALL TAGS*********)
(**************************)
(**************************)
(**************************)

MNStringTags = {"ImageType", "FirmwareVersion", "OwnerName", "LensModel", "InternalSerialNumber", "DustRemovalData", "SerialNumber", "Quality", "FileSource", "CameraSettingsZ1", 
	"PrintIM", "CameraSettings5D", "WBInfoA100", "ImageStabilizationData", "CameraSettings7D", "CameraSettingsStdNew", "CameraSettingsStdOld", "ColorMode", "Sharpening",
	"WhiteBalance", "Focus", "FlashSetting", "ISOSelection", "ImageAdjustment", "AuxiliaryLens", "BodyFirmwareVersion", "CameraType", "PictureInfo", "CameraID", "Software", 
	"Firmware", "SerialNumber2", "BabyAge1", "LensType", "LensSerialNumber", "AccessoryType", "AccessorySerialNumber", "BabyAge2", "LocalLocationName", "LocationName", "FirmwareName", 
    "LensFirmware", "DriveMode", "ResolutionMode", "AutofocusMode", "FocusSetting","ExposureMode", "MeteringMode", "LensRange", "ColorSpace", "Exposure", "Contrast",
    "Shadow", "Highlight", "Saturation", "Sharpness", "FillLight", "ColorAdjustment", "AdjustmentMode", "AutoBracket", "0x2003", "ColorReproduction"}

IntegerTags = {(*Exif*) "SubjectArea", "NewSubfileType", "SubfileType", "ImageWidth", "ImageLength", "BitsPerSample", "PhotometricInterpretation",
"Threshholding", "CellWidth", "CellLength", "FillOrder", "StripOffsets", "Orientation", "SamplesPerPixel", "RowsPerStrip", "StripByteCounts", "PlanarConfiguration",
"GrayResponseUnit", "GrayResponseCurve", "T4Options", "T6Options", "TransferFunction", "Predictor", "ColorMap", "HalftoneHints", "TileWidth",
"TileLength", "TileOffsets", "TileByteCounts", "SubIFDs", "InkSet", "NumberOfInks", "DotRange", "ExtraSamples", "SampleFormat", "SMinSampleValue", "SMaxSampleValue",
"TransferRange", "ClipPath", "XClipPathUnits", "YClipPathUnits", "Indexed", "OPIProxy", "JPEGProc", "JPEGRestartInterval",
"JPEGLosslessPredictors", "JPEGPointTransforms", "JPEGQTables", "JPEGDCTables", "JPEGACTables", "YCbCrSubSampling", "YCbCrPositioning", "XMLPacket", "Rating", "RatingPercent",
"CFARepeatPatternDim", "IPTCNAA", "ImageResources", "ExifTag", "SpectralSensitivity", "GPSTag", "Interlace", "LocalizedCameraModel", "CFAPlaneColor", "CFALayout", "LinearizationTable",
"TimeZoneOffset", "SelfTimerMode", "ImageNumber", "TIFFEPStandardID", "XPTitle", "XPComment", "XPAuthor", "XPKeywords", "XPSubject", "DNGVersion", "DNGBackwardVersion",
"BlackLevelRepeatDim", "WhiteLevel", "DefaultCropOrigin", "DefaultCropSize", "AsShotNeutral", "BayerGreenSplit", "DNGPrivateData", "MakerNoteSafety", "CalibrationIlluminant1",
"CalibrationIlluminant2", "RawDataUniqueID", "OriginalRawFileName", "ActiveArea", "MaskedAreas", "ColorimetricReference", "CameraCalibrationSignature", "ProfileCalibrationSignature",
"AsShotProfileName", "ProfileName", "ProfileHueSatMapDims", "ProfileEmbedPolicy", "ProfileCopyright", "PreviewApplicationName", "PreviewApplicationVersion", "PreviewSettingsName",
"PreviewSettingsDigest", "PreviewColorSpace", "SubTileBlockSize", "RowInterleaveFactor", "ProfileLookTableDims", "NewSubfileType", "SubfileType", "ImageWidth", "ImageLength",
"Compression", "ResolutionUnit", "JPEGInterchangeFormat", "JPEGInterchangeFormatLength", "ExposureProgram", "ISOSpeedRatings", "SensitivityType", "StandardOutputSensitivity", "RecommendedExposureIndex", "ISOSpeed",
"ISOSpeedLatitudeyyy", "ISOSpeedLatitudezzz", "MeteringMode", "LightSource", "FlashInfo", "ColorSpace", "PixelXDimension", "PixelYDimension", "InteroperabilityTag", "FocalPlaneResolutionUnit",
"SubjectLocation", "SensingMethod", "CustomRendered", "ExposureMode", "FocalLengthIn35mmFilm", "SceneCaptureType", "GainControl", "Contrast", "Saturation", "Sharpness",
"SubjectDistanceRange",  "RelatedImageWidth", "RelatedImageLength",

(*IPTC*)"ARMVersion", "ARMId", "FileVersion", "FileFormat", "ModelVersion", "PreviewVersion", "PreviewFormat", "RecordVersion",

(*XMP*)
"BlueHue", "BlueSaturation", "Brightness", "ChromaticAberrationB", "ChromaticAberrationR", "ColorNoiseReduction", "Contrast", "CropUnits", "GreenHue", "GreenSaturation",
"LuminanceSmoothing", "RedHue", "RedSaturation", "Saturation", "Shadows", "ShadowTint", "Temperature", "Tint", "VignetteAmount", "VignetteMidpoint",
"AutoLateralCA", "AutoWhiteVersion", "Blacks2012", "BlueHue", "BlueSaturation", "Brightness", "ChromaticAberrationB",
"ChromaticAberrationR", "Clarity", "Clarity2012", "ColorNoiseReduction", "ColorNoiseReductionDetail", "ColorNoiseReductionSmoothness",
"Contrast", "Contrast2012", "CropConstrainToWarp", "Defringe", "DefringeGreenAmount", "DefringeGreenHueHi", "DefringeGreenHueLo",
"DefringePurpleAmount", "DefringePurpleHueHi", "DefringePurpleHueLo", "FillLight", "GrainAmount", "GrainFrequency", "GrainSize",
"GrayMixerAqua", "GrayMixerBlue", "GrayMixerGreen", "GrayMixerMagenta", "GrayMixerOrange", "GrayMixerPurple", "GrayMixerRed", 
"GrayMixerYellow", "GreenHue", "GreenSaturation", "HighlightRecovery", "Highlights2012", "HueAdjustmentAqua", "HueAdjustmentBlue",
"HueAdjustmentGreen", "HueAdjustmentMagenta", "HueAdjustmentOrange", "HueAdjustmentPurple", "HueAdjustmentRed", "HueAdjustmentYellow",
"IncrementalTemperature", "IncrementalTint", "LensManualDistortionAmount", "LensProfileChromaticAberrationScale", "LensProfileDistortionScale",
"LensProfileEnable", "LensProfileVignettingScale", "LuminanceAdjustmentAqua", "LuminanceAdjustmentBlue", "LuminanceAdjustmentGreen", 
"LuminanceAdjustmentMagenta", "LuminanceAdjustmentOrange", "LuminanceAdjustmentPurple", "LuminanceAdjustmentRed", "LuminanceAdjustmentYellow",
"LuminanceNoiseReductionContrast", "LuminanceNoiseReductionDetail", "LuminanceSmoothing", "ParametricDarks", "ParametricHighlights", 
"ParametricHighlightSplit", "ParametricLights", "ParametricMidtoneSplit", "ParametricShadows", "ParametricShadowSplit", "PerspectiveAspect",
"PerspectiveHorizontal", "PerspectiveScale", "PerspectiveUpright", "PerspectiveVertical", "PostCropVignetteAmount", "PostCropVignetteFeather",
"PostCropVignetteHighlightContrast", "PostCropVignetteMidpoint", "PostCropVignetteRoundness", "PostCropVignetteStyle", "RedHue", "RedSaturation",
"Saturation", "SaturationAdjustmentAqua", "SaturationAdjustmentBlue", "SaturationAdjustmentGreen", "SaturationAdjustmentMagenta", "SaturationAdjustmentOrange",
"SaturationAdjustmentPurple", "SaturationAdjustmentRed", "SaturationAdjustmentYellow", "Shadows", "Shadows2012", "ShadowTint", "SharpenDetail", 
"SharpenEdgeMasking", "Sharpness", "Smoothness", "SplitToningBalance", "SplitToningHighlightHue", "SplitToningHighlightSaturation", 
"SplitToningShadowHue", "SplitToningShadowSaturation", "ColorTemperature", "Tint", "UprightCenterMode", "UprightFocalMode",
"UprightTransformCount", "UprightVersion", "Vibrance", "VignetteAmount", "VignetteMidpoint", "Exposure2012", "SharpenRadius", "ColorMode", "id"
};

RealTags = { "WhitePoint", "PrimaryChromaticities", "YCbCrCoefficients", "ReferenceBlackWhite", "BatteryLevel", "ProfileLookTableData",
"BlackLevel", "BlackLevelDeltaH", "BlackLevelDeltaV", "DefaultScale", "ColorMatrix1", "ColorMatrix2", "AsShotWhiteXY", "NoiseReductionApplied", "ForwardMatrix1", "ForwardMatrix2", "NoiseProfile",
"CameraCalibration1", "CameraCalibration2", "ReductionMatrix1", "ReductionMatrix2", "AnalogBalance", "BaselineExposure", "BaselineNoise", "BaselineSharpness", "LinearResponseLimit", "LensInfo",
"ChromaBlurRadius", "AntiAliasStrength", "ShadowScale", "BestQualityScale", "AsShotPreProfileMatrix", "CurrentPreProfileMatrix", "ProfileHueSatMapData1", "ProfileHueSatMapData2", "ProfileToneCurve",
"ExposureTime", "FNumber", "ShutterSpeedValue", "CompressedBitsPerPixel", "ApertureValue", "FocalLengthIn35mmFilm", "DigitalZoomRatio", "LensSpecification",
"BrightnessValue", "ExposureBiasValue", "MaxApertureValue", "SubjectDistance", "FocalLength", "FlashEnergy", "FocalPlaneXResolution", "FocalPlaneYResolution", "ExposureIndex",
"XResolution", "YResolution", "GPSDOP","GPSSpeed", "GPSTrack", "GPSImgDirection",

"CropTop", "CropLeft", "CropBottom", "CropRight", "CropAngle", "CropWidth", "CropHeight", "Exposure", "CorrectionAmount", "LocalBrightness", "LocalClarity",
"LocalClarity2012", "LocalContrast", "LocalContrast2012", "LocalDefringe", "LocalExposure", "LocalExposure2012", "LocalHighlights2012", "LocalLuminanceNoise", 
"LocalMoire", "LocalSaturation", "LocalShadows2012", "LocalSharpness", "LocalTemperature", "LocalTint", "LocalToningHue", "LocalToningSaturation",
"MaskValue", "Radius", "Flow", "CenterWeight"
};

RationalTags = {"XResolution", "YResolution", "WhitePoint", "PrimaryChromaticities", "YCbCrCoefficients", "ReferenceBlackWhite", "BatteryLevel", 
"ExposureTime", "FNumber", "CompressedBitsPerPixel", "ApertureValue", "MaxApertureValue", "FocalLength", "FlashEnergy", "FocalPlaneXResolution", "FocalPlaneYResolution", 
"ExposureIndex", "BlackLevel", "DefaultScale", "AnalogBalance", "AsShotWhiteXY", "BaselineNoise", "BaselineSharpness", "LinearResponseLimit", "LensInfo", "ChromaBlurRadius",
"AntiAliasStrength", "BestQualityScale", "NoiseReductionApplied", "SubjectDistance", "DigitalZoomRatio", 
"ShutterSpeedValue", "BrightnessValue", "ExposureBiasValue", "BlackLevelDeltaH", "BlackLevelDeltaV", "ColorMatrix1", "ColorMatrix2", "CameraCalibration1",
"CameraCalibration2", "ReductionMatrix1", "ReductionMatrix2", "BaselineExposure", "ShadowScale", "AsShotPreProfileMatrix", "CurrentPreProfileMatrix", "ForwardMatrix1", 
"ForwardMatrix2"
};

StringTags = {(*Exif*) "ProcessingSoftware", "ImageDescription", "Make", "Model", "Software", "Artist", "HostComputer",
"InkNames", "TargetPrinter", "ImageID", "Copyright", "SpectralSensitivity", "SecurityClassification", "ImageHistory", "UniqueCameraModel",
"CameraSerialNumber", "DateTime", "PreviewDateTime", "SpectralSensitivity", "SubSecTime", "SubSecTimeOriginal", "SubSecTimeDigitized", "DateTimeDigitized", "GPSDateStamp",
"RelatedSoundFile", "ImageUniqueID", "CameraOwnerName", "BodySerialNumber", "LensMake", "LensModel", "LensSerialNumber", "DateTimeOriginal", "InteroperabilityIndex", "RelatedImageFileFormat", "GPSLatitudeRef", "GPSLongitudeRef", "GPSSatellites", "GPSStatus", "GPSMeasureMode", "GPSSpeedRef", "GPSTrackRef",
"GPSImgDirectionRef", "GPSMapDatum", "GPSDestLatitudeRef", "GPSDestLongitudeRef", "GPSDestBearingRef", "GPSDestDistanceRef", "JPEGTables", "InterColorProfile", "OECF", "SpatialFrequencyResponse", "Noise", "PrintImageMatching", "OriginalRawFileData",
"AsShotICCProfile", "CurrentICCProfile", "RawImageDigest", "OriginalRawFileDigest", "Opcodelist1", "Opcodelist2", "Opcodelist3", "OECF", "ExifVersion", "ComponentsConfiguration", "MakerNote", "UserComment", "FlashpixVersion", "SpatialFrequencyResponse",
"FileSource", "SceneType", "CFAPattern", "DeviceSettingDescription", "InteroperabilityVersion", "Opcodelist2", "Opcodelist3", "ColorSpace", "FocalPlaneResolutionUnit", "GPSDifferential",
"SecurityClassification", "ExposureProgram", "SensitivityType", "Predictor", "ExtraSamples", "InkSet", "Orientation", "ResolutionUnit", "FillOrder", "OldSubfileType", "Thresholding",
"SubfileType", "Compression", "PhotometricInterpretation", "PlanarConfiguration", "YCbCrPositioning", "Contrast", "CustomRendered", "ExposureMode", "GainControl", "LightSource",
"FlashInfo", "MeteringMode", "WhiteBalance", "SubjectDistanceRange", "Saturation", "SceneCaptureType", "SensingMethod", "FileSource", "Sharpness", 

(*XMP*) "contributor", "coverage", "creator", "date", "description", "format", "identifier",
"language", "publisher", "rights", "source", "subject", "title", "type", "Thumbnails", "Rating", "Nickname", "ModifyDate", "MetadataDate", "Label", "CreatorTool",
"CreateDate", "BaseURL", "Identifier", "Advisory", "Certificate", "Marked", "Owner", "UsageTerms", "WebStatement", "DerivedFrom", "DocumentID", "InstanceID", "ManagedFrom", "Manager", "ManageTo", "ManageUI",
"ManagerVariant", "RenditionClass", "RenditionParams", "VersionID", "Versions", "LastURL", "RenditionOf", "SaveID", "JobRef", 
"MaxPageSize", "NPages", "Fonts", "Colorants", "PlateNames", "AutoBrightness", "AutoContrast", "AutoExposure", "AutoShadows", "BlueHue", "BlueSaturation", "Brightness",
"CameraProfile", "ChromaticAberrationB", "ChromaticAberrationR", "ColorNoiseReduction", "Contrast", "CropTop", "CropLeft", "CropBottom", "CropRight",
"CropAngle", "CropWidth", "CropHeight", "CropUnits", "Exposure", "GreenHue", "GreenSaturation", "HasCrop", "HasSettings", "LuminanceSmoothing",
"RawFileName", "RedHue", "RedSaturation", "Saturation", "Shadows", "ShadowTint", "Sharpness", "Temperature", "Tint", "ToneCurve", "ToneCurveName",
"Version", "VignetteAmount", "VignetteMidpoint", "WhiteBalance", "Tagslist", "CaptionsAuthorNames", "CaptionsDateTimeStamps", "ImageHistory", "LensCorrectionSettings",
"Tagslist", "CaptionsAuthorNames", "CaptionsDateTimeStamps", "ImageHistory", "LensCorrectionSettings", "CameraSerialNumber", "DateAcquired", "FlashManufacturer", "FlashModel", "LastKeywordIPTC",
"LastKeywordXMP", "LensManufacturer", "Rating", "Keywords", "PDFVersion", "Producer", "AuthorsPosition", "CaptionWriter", "Category", "City", "Country", "Credit", "DateCreated", "Headline", "Instructions",
"Source", "State", "SupplementalCategories", "TransmissionReference", "Urgency", "ICCProfile", "AutoRotated", "Software",

(*IPTC*) "ServiceId", "EnvelopeNumber", "ProductId", "EnvelopePriority", "CharacterSet",
"UNO", "TimeSent", "DateSent", "TimeSent", "ObjectType", "ObjectAttribute", "ObjectName", "EditStatus", "EditorialUpdate", "Urgency", "Subject", "Category",
"FixtureId", "Keywords", "LocationCode", "LocationName", "SpecialInstructions", "ActionAdvised", "ReferenceService", "ReferenceNumber", "Program", "ProgramVersion",
"ObjectCycle", "Byline", "BylineTitle", "City", "SubLocation", "ProvinceState", "CountryCode", "CountryName", "TransmissionReference", "Headline", "Credit", "Source",
"Copyright", "Contact", "Caption", "Writer", "ImageType", "ImageOrientation", "Language", "AudioType", "AudioRate", "AudioResolution", "AudioDuration", "AudioOutcue",
"DigitizationDate", "DateCreated", "ReferenceDate", "ExpirationDate", "ReleaseDate", "DigitizationTime", "TimeCreated", "ExpirationTime", "ReleaseTime",
"TimeSent", "ReleaseTime", "DigitizationTime", "RasterizedCaption", "Preview", "SuppCategory"};

QuantityTags = {"SubSecTime", 
   "SubSecTimeOriginal", "SubSecTimeDigitized", "ExposureTime", 
   "FocalLength", "Lens", "FocalLengthIn35mmFilm",
   "GPSAltitude", "TargetShutterSpeed", "GPSLatitude", "GPSLongitude", "Temperature", "SubjectDistance", "GPSImgDirection", "GPSTrack", "GPSSpeed", 
   "BaselineExposure", "ExposureBiasValue"};
   
DateTags = {"DateTime", "DateTimeOriginal", "DateTimeDigitized", "PreviewDateTime", "GPSDateStamp",
   "DateSent", "DigitizationDate", "DateCreated", "ReferenceDate", "ExpirationDate", "ReleaseDate",
 
   "ModifyDate", "MetadataDate", "CreateDate", "CaptionsDateTimeStamps", "DateAcquired", "DateCreated", "DeprecatedOn",
   
   "When", "DateSent", "SentDate"
   };
   
TimeTags = { "TimeSent", "DigitizationTime", "TimeCreated", "ReleaseTime", "GPSTimeStamp"}

MultiValues = { "BitsPerSample", "HalftoneHints", "YCbCrSubSampling", 
	"CFARepeatPatternDim", "TimeZoneOffset", "WhitePoint", "PrimaryChromaticities", 
	"YCbCrCoefficients", "ReferenceBlackWhite", "LensInfo", "CameraInfo",
	"Version", "LensSpecification", "SubjectArea" , "StripByteCounts", "StripOffsets", "RowsPerStrip", "CFAPattern", "PrintImageMatching",
	
	"ToneCurvePV2012", "ToneCurvePV2012Blue", "ToneCurvePV2012Green", "ToneCurvePV2012Red", "ToneCurve", "ToneCurveBlue", "ToneCurveGreen", "ToneCurveRed"
};

BooleanTags = {
	"FlashUsed",
	
	"AlreadyApplied", "AutoBrightness", "AutoContrast", "AutoExposure", "AutoShadows", "CircGradBasedCorrActive", "ConvertToGrayscale", 
	"HasCrop", "HasSettings", "CorrectionActive",
	
	"Marked", "AutoRotated"
};

GPSTags = {
	"GPSVersionID", "GPSLatitudeRef", "GPSLatitude", "GPSLongitudeRef", "GPSLongitude", "GPSAltitudeRef", "GPSAltitude",
"GPSTimeStamp", "GPSSatellites", "GPSStatus", "GPSMeasureMode", "GPSDOP", "GPSSpeedRef", "GPSSpeed", "GPSTrackRef", "GPSTrack", "GPSImgDirectionRef",
"GPSImgDirection", "GPSMapDatum", "GPSDestLatitudeRef", "GPSDestLatitude", "GPSDestLongitudeRef", "GPSDestLongitude", "GPSDestBearingRef", "GPSDestBearing",
"GPSDestDistanceRef", "GPSDestDistance", "GPSProcessingMethod", "GPSAreaInformation", "GPSDateStamp", "GPSDifferential"
};

IPTCEnvelope = { "ModelVersion", "Destination", "FileFormat", "FileVersion", "ServiceId", "EnvelopeNumber", "ProductId", "EnvelopePriority", "DateSent", "TimeSent", "CharacterSet",
  "UNO", "ARMId", "ARMVersion"
};

$AllExif = { "BitsPerSample", "Compression", "PhotometricInterpretation", "Threshholding", "CellWidth", "CellLength", "FillOrder", "StripOffsets", "Orientation", "SamplesPerPixel", "RowsPerStrip", "StripByteCounts", "PlanarConfiguration",
		"GrayResponseUnit", "GrayResponseCurve", "T4Options", "T6Options", "ResolutionUnit", "TransferFunction", "Predictor", "ColorMap", "HalftoneHints", "TileWidth", "TileLength", "TileOffsets", "TileByteCounts", "SubIFDs",
		"InkSet", "NumberOfInks", "DotRange", "ExtraSamples", "SampleFormat", "SMinSampleValue", "SMaxSampleValue", "TransferRange", "ClipPath", "XClipPathUnits", "YClipPathUnits", "Indexed", "OPIProxy", "JPEGProc", "JPEGInterchangeFormat",
		"JPEGInterchangeFormatLength", "JPEGRestartInterval", "JPEGLosslessPredictors", "JPEGPointTransforms", "JPEGQTables", "JPEGDCTables", "JPEGACTables", "YCbCrSubSampling", "YCbCrPositioning", "XMLPacket", "Rating", "RatingPercent",
		"CFARepeatPatternDim", "IPTCNAA", "SpectralSensitivity","Interlace", "LocalizedCameraModel", "CFAPlaneColor", "CFALayout", "LinearizationTable", "TimeZoneOffset", "SelfTimerMode",
		"ImageNumber", "TIFFEPStandardID", "XPTitle", "XPComment", "XPAuthor", "XPKeywords", "XPSubject", "DNGVersion", "DNGBackwardVersion", "BlackLevelRepeatDim", "WhiteLevel", "DefaultCropOrigin", "DefaultCropSize", "AsShotNeutral",
		"BayerGreenSplit", "DNGPrivateData", "MakerNoteSafety", "CalibrationIlluminant1", "CalibrationIlluminant2", "RawDataUniqueID", "OriginalRawFileName", "ActiveArea", "MaskedAreas", "ColorimetricReference", "CameraCalibrationSignature",
		"ProfileCalibrationSignature", "AsShotProfileName", "ProfileName", "ProfileHueSatMapDims", "ProfileEmbedPolicy", "ProfileCopyright", "PreviewApplicationName", "PreviewApplicationVersion", "PreviewSettingsName", "PreviewSettingsDigest",
		"PreviewColorSpace", "SubTileBlockSize", "RowInterleaveFactor", "ProfileLookTableDims", "NewSubfileType", "SubfileType", "ImageWidth", "ImageLength", "ExposureProgram", "ISOSpeedRatings", "SensitivityType", "StandardOutputSensitivity",
		"RecommendedExposureIndex", "ISOSpeed", "ISOSpeedLatitudeyyy", "ISOSpeedLatitudezzz", "MeteringMode", "LightSource", "FlashInfo", "ColorSpace", "PixelXDimension", "PixelYDimension", "InteroperabilityTag", "FocalPlaneResolutionUnit",
		"SubjectLocation", "SensingMethod", "CustomRendered", "ExposureMode", "WhiteBalance", "SceneCaptureType", "GainControl", "Contrast", "Saturation", "Sharpness", "SubjectDistanceRange", "RelatedImageWidth", "RelatedImageLength",
		"ProcessingSoftware", "ImageDescription", "Make", "Model", "Software", "DateTime", "Artist", "HostComputer", "InkNames", "TargetPrinter", "ImageID", "Copyright", "SecurityClassification", "ImageHistory", "UniqueCameraModel",
		"CameraSerialNumber", "PreviewDateTime", "DateTimeOriginal", "DateTimeDigitized", "SubSecTime", "SubSecTimeOriginal", "SubSecTimeDigitized", "RelatedSoundFile", "ImageUniqueID", "CameraOwnerName", "BodySerialNumber", "LensMake",
		"LensModel", "LensSerialNumber", "InteroperabilityIndex", "RelatedImageFileFormat", "JPEGTables", "InterColorProfile", "SpatialFrequencyResponse", "Noise", "PrintImageMatching", "OriginalRawFileData", "AsShotICCProfile",
		"CurrentICCProfile", "RawImageDigest", "OriginalRawFileDigest", "OpcodeList1", "OpcodeList2", "OpcodeList3", "OECF", "ExifVersion", "ComponentsConfiguration",(* "UserComment",*) "FlashpixVersion", "FileSource", "SceneType",
		"CFAPattern", "DeviceSettingDescription", "InteroperabilityVersion", "XResolution", "YResolution", "WhitePoint", "PrimaryChromaticities", "YCbCrCoefficients", "ReferenceBlackWhite", "BatteryLevel", "ProfileLookTableData",
		"BlackLevel", "BlackLevelDeltaH", "BlackLevelDeltaV", "DefaultScale", "ColorMatrix1", "ColorMatrix2", "AsShotWhiteXY", "NoiseReductionApplied", "ForwardMatrix1", "ForwardMatrix2", "NoiseProfile", "CameraCalibration1",
		"CameraCalibration2", "ReductionMatrix1", "ReductionMatrix2", "AnalogBalance", "BaselineExposure", "BaselineNoise", "BaselineSharpness", "LinearResponseLimit", "ChromaBlurRadius", "AntiAliasStrength",
		"ShadowScale", "BestQualityScale", "AsShotPreProfileMatrix", "CurrentPreProfileMatrix", "ProfileHueSatMapData1", "ProfileHueSatMapData2", "ProfileToneCurve", "ExposureTime", "FNumber", "ShutterSpeedValue", "CompressedBitsPerPixel",
		"ApertureValue", "FocalLengthIn35mmFilm", "DigitalZoomRatio", "LensSpecification", "BrightnessValue", "ExposureBiasValue", "MaxApertureValue", "SubjectDistance", "FocalLength", "FlashEnergy", "FocalPlaneXResolution",
		"FocalPlaneYResolution", "ExposureIndex", "GPSVersionID", "GPSLatitudeRef", "GPSLatitude", "GPSLongitudeRef", "GPSLongitude", "GPSAltitudeRef", "GPSAltitude", "GPSTimeStamp", "GPSSatellites", "GPSStatus", "GPSMeasureMode",
		"GPSDOP", "GPSSpeedRef", "GPSSpeed", "GPSTrackRef", "GPSTrack", "GPSImgDirectionRef", "GPSImgDirection", "GPSMapDatum", "GPSDestLatitudeRef", "GPSDestLatitude", "GPSDestLongitudeRef", "GPSDestLongitude", "GPSDestBearingRef",
		"GPSDestBearing", "GPSDestDistanceRef", "GPSDestDistance", "GPSProcessingMethod", "GPSAreaInformation", "GPSDateStamp", "GPSDifferential", "Gamma", "PhotographicSensitivity"
	};
	
$AllIPTC = {
		"DateSent", "ReleaseDate", "ExpirationDate", "ReferenceDate", "DateCreated", "DigitizationDate", "ModelVersion", "FileFormat", "FileVersion", "ARMId", "ARMVersion", "RecordVersion",
		"PreviewFormat", "PreviewVersion", "Destination", "ServiceId", "EnvelopeNumber", "ProductId", "EnvelopePriority", "CharacterSet", "Writer", "UNO", "ObjectType", "ObjectAttribute",
		"ObjectName", "EditStatus", "EditorialUpdate", "Urgency", "Subject", "Category", "SuppCategory", "FixtureId", "Keywords", "LocationCode", "LocationName", "SpecialInstructions",
		"ActionAdvised", "ReferenceService", "ReferenceNumber", "Program", "ProgramVersion", "ObjectCycle", "Byline", "BylineTitle", "City", "SubLocation", "ProvinceState", "CountryCode",
		"CountryName", "TransmissionReference", "Headline", "Credit", "Source", "Copyright", "Contact", "Caption", "Writer", "ImageType", "ImageOrientation", "Language", "AudioType",
		"AudioRate", "AudioResolution", "AudioDuration", "AudioOutcue", "TimeSent", "ReleaseTime", "ExpirationTime", "TimeCreated", "DigitizationTime", "RasterizedCaption", "Preview"
	};

$AllXMP = { "Advisory", "AuthorsPosition", "AutoBrightness", "AutoContrast", "AutoExposure", "AutoLateralCA", "AutoShadows", "AutoWhiteVersion", "BaseURL",
	"Blacks2012", "BlueHue", "BlueSaturation", "Brightness", "CameraProfile", "CameraSerialNumber", "CaptionsAuthorNames", "CaptionsDateTimeStamps", "CaptionWriter", "Category", "CenterWeight",
	"Certificate", "ChromaticAberrationB", "ChromaticAberrationR", "City", "Clarity", "Clarity2012", "Colorants", "ColorNoiseReduction", "ColorNoiseReductionDetail", "ColorNoiseReductionSmoothness",
	"ColorTemperature", "Contrast", "Contrast2012", "contributor", "CorrectionAmount", "Country", "coverage", "CreateDate", "creator", "CreatorTool", "Credit", "CropAngle", "CropBottom",
	"CropConstrainToWarp", "CropHeight", "CropLeft", "CropRight", "CropTop", "CropUnits", "CropWidth", "date", "DateAcquired", "DateCreated", "Defringe", "DefringeGreenAmount", "DefringeGreenHueHi",
	"DefringeGreenHueLo", "DefringePurpleAmount", "DefringePurpleHueHi", "DefringePurpleHueLo", "DerivedFrom", "description", "DocumentID", "Exposure", "FillLight", "FlashManufacturer", "FlashModel",
	"Flow", "Fonts", "format", "GrainAmount", "GrainFrequency", "GrainSize", "GrayMixerAqua", "GrayMixerBlue", "GrayMixerGreen", "GrayMixerMagenta", "GrayMixerOrange", "GrayMixerPurple", "GrayMixerRed",
	"GrayMixerYellow", "GreenHue", "GreenSaturation", "HasCrop", "HasSettings", "Headline", "HighlightRecovery", "Highlights2012", "History", "HueAdjustmentAqua", "HueAdjustmentBlue", "HueAdjustmentGreen",
	"HueAdjustmentMagenta", "HueAdjustmentOrange", "HueAdjustmentPurple", "HueAdjustmentRed", "HueAdjustmentYellow", "identifier", "Identifier", "ImageHistory", "IncrementalTemperature", "IncrementalTint",
	"InstanceID", "Instructions", "JobRef", "Keywords", "Label", "language", "LastKeywordIPTC", "LastKeywordXMP", "LastURL", "LensCorrectionSettings", "LensManualDistortionAmount", "LensManufacturer",
	"LensModel", "LensProfileChromaticAberrationScale", "LensProfileDistortionScale", "LensProfileEnable", "LensProfileVignettingScale", "LocalBrightness", "LocalClarity", "LocalClarity2012", "LocalContrast",
	"LocalContrast2012", "LocalDefringe", "LocalExposure", "LocalExposure2012", "LocalHighlights2012", "LocalLuminanceNoise", "LocalMoire", "LocalSaturation", "LocalShadows2012", "LocalSharpness",
	"LocalTemperature", "LocalTint", "LocalToningHue", "LocalToningSaturation", "LuminanceAdjustmentAqua", "LuminanceAdjustmentBlue", "LuminanceAdjustmentGreen", "LuminanceAdjustmentMagenta", "LuminanceAdjustmentOrange",
	"LuminanceAdjustmentPurple", "LuminanceAdjustmentRed", "LuminanceAdjustmentYellow", "LuminanceNoiseReductionContrast", "LuminanceNoiseReductionDetail", "LuminanceSmoothing", "ManagedFrom", "Manager",
	"ManagerVariant", "ManageTo", "ManageUI", "Marked", "MaskValue", "MaxPageSize", "MetadataDate", "ModifyDate", "Nickname", "NPages", "Owner", "ParametricDarks", "ParametricHighlights",
	"ParametricHighlightSplit", "ParametricLights", "ParametricMidtoneSplit", "ParametricShadows", "ParametricShadowSplit", "PDFVersion", "PerspectiveAspect", "PerspectiveHorizontal", "PerspectiveScale",
	"PerspectiveUpright", "PerspectiveVertical", "PlateNames", "PostCropVignetteAmount", "PostCropVignetteFeather", "PostCropVignetteHighlightContrast", "PostCropVignetteMidpoint", "PostCropVignetteRoundness",
	"PostCropVignetteStyle", "Producer", "publisher", "Radius", "Rating", "RawFileName", "RedHue", "RedSaturation", "RenditionClass", "RenditionOf", "RenditionParams", "rights", "Saturation", "SaturationAdjustmentAqua",
	"SaturationAdjustmentBlue", "SaturationAdjustmentGreen", "SaturationAdjustmentMagenta", "SaturationAdjustmentOrange", "SaturationAdjustmentPurple", "SaturationAdjustmentRed", "SaturationAdjustmentYellow",
	"SaveID", "Shadows", "Shadows2012", "ShadowTint", "SharpenDetail", "SharpenEdgeMasking", "Sharpness", "Smoothness", "source", "Source", "SplitToningBalance", "SplitToningHighlightHue", "SplitToningHighlightSaturation",
	"SplitToningShadowHue", "SplitToningShadowSaturation", "State", "subject", "SupplementalCategories", "Tagslist", "TagsList", "Temperature", "Thumbnails", "Tint", "title", "ToneCurve", "ToneCurveBlue",
	"ToneCurveGreen", "ToneCurveName", "ToneCurvePV2012", "ToneCurvePV2012Blue", "ToneCurvePV2012Green", "ToneCurvePV2012Red", "ToneCurveRed", "TransmissionReference", "type", "UprightCenterMode",
	"UprightFocalMode", "UprightTransformCount", "UprightVersion", "Urgency", "UsageTerms", "Version", "VersionID", "Versions", "Vibrance", "VignetteAmount", "VignetteMidpoint", "WebStatement", "WhiteBalance", "Flash", "ColorMode", "ICCProfile"
	};

(**************************)
(**************************)
(**************************)
(******INITIALIZATION******)
(**************************)
(**************************)
(**************************)

ExifAll = <||>;
XMPAll= <||>;
IPTCAll= <||>;

ExifRaw = <||>;
XMPRaw = <||>;
IPTCRaw = <||>;

MakerNote = <||>;

Init[on_, meta_String, fname___] := If[on === False, $XMPUnInitialize[];
														Which[			 
	                                                        meta === "Exif",      ExifAll = $Failed
	                                                               ,
	                                                        meta === "ExifRaw",   ExifRaw = $Failed
	                                                               ,
	                                                        meta === "MakerNote", MakerNote = $Failed
	                                                        	   ,
															meta === "XMP",       XMPAll = $Failed
																   ,
														    meta === "XMPRaw",    XMPRaw = $Failed
																   ,
															meta === "IPTC",      IPTCAll = $Failed
															       ,
															meta === "IPTCRaw",   IPTCRaw = $Failed],
				If[Quiet@$XMPInitialize[Quiet@FindFile[fname]] === LibraryFunctionError["LIBRARY_USER_ERROR", -1],
					Quiet[Which[			 
	                                                        meta === "Exif",      ExifAll = $Failed
	                                                               ,
	                                                        meta === "ExifRaw",   ExifRaw = $Failed
	                                                               ,
	                                                        meta === "MakerNote", MakerNote = $Failed
	                                                        	   ,
															meta === "XMP",       XMPAll = $Failed
																   ,
														    meta === "XMPRaw",    XMPRaw = $Failed
																   ,
															meta === "IPTC",      IPTCAll = $Failed
															       ,
															meta === "IPTCRaw",   IPTCRaw = $Failed]];
					 False
					,
					Quiet[
						Which[
						meta === "IPTC",      IPTCAll = ReadIPTC["All", False]
						   ,
				    	meta === "IPTCRaw",   IPTCRaw = ReadIPTC["AllRaw", False]
						   ,	
						meta === "Exif",      ExifAll = ReadExif["All", False]
					       ,
						meta === "ExifRaw",   ExifRaw = ReadExif["AllRaw", False]
					       ,
						meta === "MakerNote", MakerNote = ReadExif["MakerNote", False]
					       ,
						meta === "XMP",       XMPAll = ReadXMP["All", False]
					       ,
						meta === "XMPRaw",    XMPRaw = ReadXMP["AllRaw", False]
						]];
						True
				]				
			]

(**************************)
(**************************)
(**************************)
(********VALIDATION********)
(**************************)
(**************************)
(**************************)
			
GetExif[tname_] := Module[{res = ExifAll},
 					Quiet@Which[
 					  tname === "All", Module[{x = ValidateExif[res]}, If[AssociationQ[x], x, <||>]],
 					  tname === "Raw", ExifRaw,
 					  tname === "MakerNote", MakerNote,
 					  True, If[MatchQ[res[tname], $Failed[tname]] , $Failed, If[MatchQ[res[tname], Missing["KeyAbsent", tname]], LibraryFunctionError["LIBRARY_USER_ERROR",-2], res[tname]]]]]


GetXMP[tname_] := Module[{res = XMPAll},
 					Quiet@Which[
 					  tname === "All",  Module[{x = ValidateXMP[res]}, If[AssociationQ[x], x, <||>]],
 					  tname === "Raw", XMPRaw,
 					  True, If[MatchQ[res[tname], $Failed[tname]] , $Failed, If[MatchQ[res[tname], Missing["KeyAbsent", tname]], LibraryFunctionError["LIBRARY_USER_ERROR",-2], res[tname]]]]]
 					  
GetIPTC[tname_] := Module[{res = IPTCAll},
 					Quiet@Which[
 					  tname === "All",  Module[{x = If[res === <||>, <||>, ValidateIPTC[res]]}, If[AssociationQ[x], x, <||>]],
 					  tname === "Raw", IPTCRaw,
 					  True, If[MatchQ[res[tname], $Failed[tname]] , $Failed, If[MatchQ[res[tname], Missing["KeyAbsent", tname]], LibraryFunctionError["LIBRARY_USER_ERROR",-2], res[tname]]]]]

ValidateExif[res_] := AssociationMap[ExifObjectValidate, DeleteCases[Association@KeyValueMap[#1 ->  DeleteCases[#2, _?(StringMatchQ[ToString@#,Whitespace ..] &)] &, res], _?(# == <||> &)]]
ValidateXMP[res_]  := AssociationMap[XMPObjectValidate, DeleteCases[Association@KeyValueMap[#1 ->  DeleteCases[#2, _?(StringMatchQ[ToString@#,Whitespace ..] &)] &, res], _?(# == <||> &)]]
ValidateIPTC[res_] := AssociationMap[IPTCObjectValidate, DeleteCases[Association@KeyValueMap[#1 ->  DeleteCases[#2, _?(StringMatchQ[ToString@#,Whitespace ..] &)] &, res], _?(# == <||> &)]]

IPTCObjectValidate[Rule[key_, assoc_Association]] := Rule[key, AssociationMap[IPTCObjectValidate, assoc]]
IPTCObjectValidate[Rule[key_, val_]] := Which[
										(MemberQ[DateTags, key] && !DateObjectQ[val]), Rule[key, Missing["Disputed"]],
   
   									    (MemberQ[TimeTags, key] && !TimeObjectQ[val]), Rule[key, Missing["Disputed"]],
   
  									    (MemberQ[QuantityTags, key] && QuantityQ@val && (List @@ val // First) < 0), Rule[key, Missing["Disputed"]],
										
										SameQ[key, "PreviewFormat"] || SameQ[key, "FileFormat"], If[val < 0 || val > 29, Rule[key, Missing["Disputed"]], Rule[key, val]],
										
										True                                        , Rule[key, val]]

XMPPositiveValuesOnly = {"SaveID", "TrackNumber", "VideoPixelAspectRatio", "FileDataRate", "Tempo", "NumberofBeats", "CropWidth", "CropHeight" }

XMPObjectValidate[Rule[key_, assoc_Association]] := Rule[key, AssociationMap[XMPObjectValidate, assoc]]
XMPObjectValidate[Rule[key_, val_]] := Module[{miss = Rule[key, Missing["Disputed"]], vRes = Rule[key, val], strToExpr = Quiet[ToExpression[val]]},
										If[SameQ[ToString@strToExpr,"Disputed"], strToExpr = -10000];
											Which[
										MemberQ[DateTags, key] && ! DateObjectQ[val]                                                , miss,
										MemberQ[XMPPositiveValuesOnly, key]                                       	                , miss,
									   (MemberQ[QuantityTags, key] && QuantityQ@val && (List @@ val // First) < 0)				    , miss,
										SameQ["Rating", key] && (strToExpr < -1 || strToExpr > 5)                      		, miss,
										SameQ["Urgency", key] && (strToExpr < 0 || strToExpr > 8)                     	    , miss,
										SameQ["BlueHue", key] && (strToExpr < -100 || strToExpr > 100)                 	    , miss,
										SameQ["BlueSaturation", key] && (strToExpr < -100 || strToExpr > 100)      		    , miss, 
										SameQ["Brightness", key] && (strToExpr < 0 || strToExpr > 150)                 		, miss,
										SameQ["ChromaticAberrationBlue", key] && (strToExpr < -100 || strToExpr > 100) 		, miss,
										SameQ["ChromaticAberrationRed", key] && (strToExpr < -100 || strToExpr > 100) 	    , miss,
										SameQ["ColorNoiseReduction", key] && (strToExpr < 0 || strToExpr > 100)    		    , miss,
										SameQ["Contrast", key] && (strToExpr < -50 || strToExpr > 100)             		    , miss, 
										SameQ["GreenHue", key] && (strToExpr < -100 || strToExpr > 100)            		    , miss, 
										SameQ["GreenSaturation", key] && (strToExpr < -100 || strToExpr > 100)      		    , miss, 
										SameQ["LuminanceSmoothing", key] && (strToExpr < -100 || strToExpr > 100)    	    , miss, 
										SameQ["RedHue", key] && (strToExpr < -100 || strToExpr > 100)               		    , miss, 
										SameQ["RedSaturation", key] && (strToExpr < -100 || strToExpr > 100)         	    , miss, 
										SameQ["Saturation", key] && (strToExpr < -100 || strToExpr > 100)              		, miss, 
										SameQ["Shadows", key] && (strToExpr < 0 || strToExpr > 100)                  	    , miss, 
										SameQ["ShadowTint", key] && (strToExpr < -100 || strToExpr > 100)      		        , miss, 
										SameQ["Sharpness", key] && (strToExpr < 0 || strToExpr > 100)    	                , miss, 
										SameQ["Temperature", key] && ((List @@ val // First) < 2000 || 
										                                         (List @@ val // First) > 50000)  			        , miss,            
										SameQ["Tint", key] && (strToExpr < -150 || strToExpr > 150)                    		, miss, 
										SameQ["VignetteAmount", key] && (strToExpr < -100 || strToExpr > 100)     	        , miss, 
										SameQ["VignetteMidpoint", key] && (strToExpr < 0 || strToExpr > 100)		            , miss, 
										SameQ["Exposure", key] && (strToExpr < -4.0 || strToExpr > 4.0)                		, miss, 
										SameQ["CropUnits", key] && (strToExpr =!= 0 || strToExpr =!= 1 || strToExpr =!= 2)   , miss, 
										True                                                                      , vRes]]

ExifPositiveValuesOnly = {"XResolution", "YResolution", "CompressedBitsPerPixel",  "SubjectDistance", "FocalPlaneXResolution", 
   "FocalPlaneYResolution", "ExposureIndex", "BlackLevelDeltaH", "BlackLevelDeltaV", "LinearResponseLimit", "ChromaBlurRadius"."AntiAliasStrength",
   "BestQualityScale", "DigitalZoomRatio", "GPSLongitude", "NewSubfileType", "ImageWidth", "ImageLength", "StripOffsets", "RowsPerStrip", "StripByteCounts", 
   "SubIFDs", "JPEGInterchangeFormat", "JPEGInterchangeFormatLength", "IPTCNAA", "ExifTag", "GPSTag", "ProfileHueSatMapDims", "RowInterleaveFactor", 
   "ProfileLookTableDims", "PixelXDimension", "PixelYDimension", "InteroperabilityTag", "RelatedImageWidth", "RelatedImageHeight", "ISOSpeed", "BitsPerSample", 
   "SensitivityType", "GrayResponseCurve", "SubTileBlockSize", "StandardOutputSensitivity", "RecommendedExposureIndex", "ISOSpeedLatitudeyyy", "ISOSpeedLatitudezzz", 
   "FocalLengthIn35mmFilm", "BaselineExposure", "FlashEnergy"};


 ExifObjectValidate[Rule[key_, assoc_Association]] := Rule[key, AssociationMap[ExifObjectValidate, assoc]]
 ExifObjectValidate[Rule[key_, val_]] :=  Module[{miss = Rule[key, Missing["Disputed"]], vRes = Rule[key, val]}, 
 								 Which[
 									  (MemberQ[DateTags, key] && !DateObjectQ[val]), Rule[key, Missing["Disputed"]],
   
   									  (MemberQ[TimeTags, key] && !TimeObjectQ[val]), Rule[key, Missing["Disputed"]],
   
                                      (MemberQ[QuantityTags, key] && !QuantityQ@val) , Rule[key, Missing["Disputed"]],
   
									  (SameQ[key, "FocalLength"] && (N@(List @@ val // First) === 0 || N@(List @@ val // First) === 0.)), Rule[key, Missing["Unknown"]],   
									  
   
   									  SameQ[key, "GPSDifferential"] || SameQ["ColorimetricReference", key], 
   									  If[If[NumberQ@val, val < 0 || val > 1 , !StringMatchQ[ToString@val, "Without correction", 
        							  IgnoreCase -> True] && !StringMatchQ[ToString@val, "Differential correction applied", IgnoreCase -> True] && !StringMatchQ[ToString@val, "Correction applied", IgnoreCase -> True]], miss, vRes],
   
   									  SameQ[key, "SubjectDistanceRange"], If[If[NumberQ@val, val < 0 || val > 3 , !StringMatchQ[ToString@val, "Unknown", IgnoreCase -> True] && 
   									  !StringMatchQ[ToString@val, "Macro", IgnoreCase -> True] && !StringMatchQ[ToString@val, "Close view", IgnoreCase -> True] && 
   									  !StringMatchQ[ToString@val, "Distant view", IgnoreCase -> True]], miss, vRes],
   
   									  SameQ[key, "Sharpness"], If[If[NumberQ@val, val < 0 || val > 2, !StringMatchQ[ToString@val, "Normal", IgnoreCase -> True] && 
   									  !StringMatchQ[ToString@val, "Soft", IgnoreCase -> True] && !StringMatchQ[ToString@val, "Hard", IgnoreCase -> True] ], miss, vRes],
   
   								      SameQ[key, "Saturation"], If[If[NumberQ@val, val < 0 || val > 2, !StringMatchQ[ToString@val, "Normal", IgnoreCase -> True] && 
   								      !StringMatchQ[ToString@val, "Low", IgnoreCase -> True] && !StringMatchQ[ToString@val, "High", IgnoreCase -> True]], miss, vRes],
   								      
   								      StringMatchQ[key, "SceneType"], If[If[NumberQ@val, val < 0 || val > 2, !StringMatchQ[ToString@val, "Directly photographed", IgnoreCase -> True]], miss, vRes],
   								      
   								      SameQ[key, "FileSource"], If[If[NumberQ@val, val < 0 || val > 2, !StringMatchQ[ToString@val, "Film scanner", IgnoreCase -> True] && 
   								      !StringMatchQ[ToString@val, "Reflexion print scanner", IgnoreCase -> True] && !StringMatchQ[ToString@val, "Digital still camera", IgnoreCase -> True]], miss, vRes],
   
   									  SameQ[key, "Contrast"], If[If[NumberQ@val, val < 0 || val > 2, !StringMatchQ[ToString@val, "Normal", IgnoreCase -> True] && 
   									  !StringMatchQ[ToString@val, "Soft", IgnoreCase -> True] && !StringMatchQ[ToString@val, "Hard", IgnoreCase -> True] ], miss, vRes],
   
  									  SameQ[key, "GainControl"], If[If[NumberQ@val, (val < 0 || val > 4), !StringMatchQ[ToString@val, "None", IgnoreCase -> True] && 
  									  !StringMatchQ[ToString@val, "Low gain up", IgnoreCase -> True] && !StringMatchQ[ToString@val, "High gain up", IgnoreCase -> True] && 
  									  !StringMatchQ[ToString@val, "Low gain down", IgnoreCase -> True] && !StringMatchQ[ToString@val, "High gain down", IgnoreCase -> True] ], miss, vRes],
   
   									  SameQ[key, "SceneCaptureType"], If[ If[NumberQ@val, (val < 0 || val > 3), (!StringMatchQ[ToString@val, "Standard", IgnoreCase -> True] && 
   									  !StringMatchQ[ToString@val, "Landscape", IgnoreCase -> True] && !StringMatchQ[ToString@val, "Portrait", IgnoreCase -> True] && 
   									  !StringMatchQ[ToString@val, "Night scene", IgnoreCase -> True])], miss, vRes],
      
                                      SameQ[key, "PlanarConfiguration"], If[val=!=1 && val=!=2, miss, vRes],
   
   									  SameQ[key, "WhiteBalance"], If[If[NumberQ@val, val < 0 && val > 1 ,  !StringMatchQ[ToString@val, "Auto", IgnoreCase -> True] && 
   									  !StringMatchQ[ToString@val, "Manual", IgnoreCase -> True]], miss, vRes],
   
   									  SameQ[key, "ExposureMode"], If[If[NumberQ@val, val < 0 || val > 2,  !StringMatchQ[ToString@val, "Auto", IgnoreCase -> True] &&  
   									  !StringMatchQ[ToString@val, "Manual", IgnoreCase -> True] && !StringMatchQ[ToString@val, "Auto bracket", IgnoreCase -> True] ], miss, vRes],
   
   									  SameQ[key, "SensingMethod"], If[If[NumberQ@val, val < 1 || val > 8,  !StringMatchQ[ToString@val, "Not defined", IgnoreCase -> True] && 
   									  !StringMatchQ[ToString@val, "One-chip color area", IgnoreCase -> True] && !StringMatchQ[ToString@val, "Two-chip color area", 
        							  IgnoreCase -> True] && !StringMatchQ[ToString@val, "Three-chip color area", IgnoreCase -> True] && !StringMatchQ[ToString@val, "Color sequential area", 
        							  IgnoreCase -> True] && !StringMatchQ[ToString@val, "Trilinear", IgnoreCase -> True] && !StringMatchQ[ToString@val, "Color sequential linear", 
        							  IgnoreCase -> True]], miss, vRes],
   
   									  StringMatchQ[key, "FocalPlaneResolutionUnit"], If[If[NumberQ@val, val < 1 || val > 3 , !StringMatchQ[ToString@val, "No absolute unit of measurement", 
        							  IgnoreCase -> True] && !StringMatchQ[ToString@val, "None", IgnoreCase -> True] && !StringMatchQ[ToString@val, "Inch", IgnoreCase -> True] && !StringMatchQ[ToString@val, "cm", IgnoreCase -> True] && !StringMatchQ[ToString@val, "Centimeter", IgnoreCase -> True]], 
    								  miss, vRes],
      
   									  SameQ[key, "MeteringMode"], If[If[NumberQ@val, val < 0 || (val > 6 && val =!= 255), !StringMatchQ[ToString@val, "Unknown", IgnoreCase -> True] && 
        							  !StringMatchQ[ToString@val, "Average", IgnoreCase -> True] && !StringMatchQ[ToString@val, "Center Weighted Average", IgnoreCase -> True] && 
        							  !StringMatchQ[ToString@val, "Spot", IgnoreCase -> True] && !StringMatchQ[ToString@val, "MultiSpot", IgnoreCase -> True] && !StringMatchQ[ToString@val, "Multi-segment", IgnoreCase -> True] && 
        							  !StringMatchQ[ToString@val, "Pattern", IgnoreCase -> True] && !StringMatchQ[ToString@val, "Partial", IgnoreCase -> True] && !StringMatchQ[ToString@val, "Segment", IgnoreCase -> True] && 
        							  !StringMatchQ[ToString@val, "Reserved", IgnoreCase -> True] && !StringMatchQ[ToString@val, "Other", IgnoreCase -> True]], miss, vRes],
   
   									  SameQ[key, "ExposureProgram"], If[If[NumberQ@val, val < 0  ||  val > 9 , !StringMatchQ[ToString@val, "Not defined", IgnoreCase -> True] && 
   									  !StringMatchQ[ToString@val, "Auto", IgnoreCase -> True] && !StringMatchQ[ToString@val, "Manual", IgnoreCase -> True] && !StringMatchQ[ToString@val, "Normal program", IgnoreCase -> True] && 
   									  !StringMatchQ[ToString@val, "Aperture priority", IgnoreCase -> True] && !StringMatchQ[ToString@val, "Shutter priority", IgnoreCase -> True] && 
   									  !StringMatchQ[ToString@val, "Creative program", IgnoreCase -> True] && !StringMatchQ[ToString@val, "Action program", IgnoreCase -> True] && 
   									  !StringMatchQ[ToString@val, "Portrait mode", IgnoreCase -> True] && !StringMatchQ[ToString@val, "Landscape mode", IgnoreCase -> True] && 
   									  !StringMatchQ[ToString@val, "Bulb", IgnoreCase -> True]], miss, vRes],
   
   									  SameQ[key, "CalibrationIlluminant2"] || SameQ[key, "CalibrationIlluminant1"], If[val === 0, miss, vRes],
   									  
   									  SameQ[ToString@key, "FlashInfo"] ||  SameQ[ToString@key, "Orientation"], If[val === {}, miss, vRes],
   									  
   									  SameQ[key, "CFALayout"], If[val < 1 || val > 5, miss, vRes], 
   									  
   									  SameQ[key, "LightSource"], If[If[NumberQ@val, (val < 0  || (val > 4  && (9! < val && val ! < 15) && (17! < val! < 24)) && val =!= 255), 
   									  !StringMatchQ[ToString@val, "Unknown", IgnoreCase -> True] && !StringMatchQ[ToString@val, "Daylight", IgnoreCase -> True] && 
   									  !StringMatchQ[ToString@val, "Fluorescent", IgnoreCase -> True] && !StringMatchQ[ToString@val, "Tungsten (incandescent light)", IgnoreCase -> True] && 
   									  !StringMatchQ[ToString@val, "Flash", IgnoreCase -> True] && !StringMatchQ[ToString@val, "Fine weather", IgnoreCase -> True] && 
   									  !StringMatchQ[ToString@val, "Cloudy weather", IgnoreCase -> True] && !StringMatchQ[ToString@val, "Shade", IgnoreCase -> True] && 
   									  !StringMatchQ[ToString@val, "Daylight fluorescent (D 5700 - 7100K", IgnoreCase -> True] && !StringMatchQ[ToString@val, "Day white fluorescent (N 4600 - 5400K)", 
        							  IgnoreCase -> True] && !StringMatchQ[ToString@val, "Cool white fluorescent (W 3900 - 4500K)", IgnoreCase -> True] && !StringMatchQ[ToString@val, 
        							  "White fluorescent (WW 3200 - 3700K", IgnoreCase -> True] && !StringMatchQ[ToString@val, "Standard light A", IgnoreCase -> True] && 
        							  !StringMatchQ[ToString@val, "Standard light B", IgnoreCase -> True] && !StringMatchQ[ToString@val, "Standard light C", IgnoreCase -> True] && 
        							  !StringMatchQ[ToString@val, "D55", IgnoreCase -> True] && !StringMatchQ[ToString@val, "D65", IgnoreCase -> True] && !StringMatchQ[ToString@val, "D75", IgnoreCase -> True] && 
        							  !StringMatchQ[ToString@val, "D50", IgnoreCase -> True] && !StringMatchQ[ToString@val, "ISO studio tungsten", IgnoreCase -> True] && !StringMatchQ[ToString@val, "Other light source", 
        							  IgnoreCase -> True] && !StringMatchQ[ToString@ToString@val, "Reserved", IgnoreCase -> True]], miss, vRes],
   
   									  SameQ[key, "YCbCrPositioning"], If[If[NumberQ@val, val < 1 || val > 2, !StringMatchQ[ToString@val, "Centered", IgnoreCase -> True] && 
   									  !StringMatchQ[ToString@val, "Cosited", IgnoreCase -> True] && 
   									  !StringMatchQ[ToString@val, "Co-sited", IgnoreCase -> True] ], miss, vRes],
   
   									  SameQ[key, "YCbCrSubSampling"], If[val[[1]] < 0 || val[[2]] < 0, miss, vRes], SameQ[key, "OPIProxy"], If[If[NumberQ@val, val < 0 || val > 1, 
   									  !StringMatchQ[ToString@val, "A higher-resolution version of this image does not exist", IgnoreCase -> True] && !StringMatchQ[ToString@val, "A higher-resolution version of this image exists, and the
									  name of that image is found in the ImageID tag", IgnoreCase -> True]], miss, vRes],
   
  		 							  SameQ[key, "Indexed"], If[If[NumberQ@val, val < 0 || val > 1, !StringMatchQ[ToString@val, "Not indexed", IgnoreCase -> True] && 
  		 							  !StringMatchQ[ToString@val, "Indexed", IgnoreCase -> True] ], miss, vRes],
   
   									  SameQ[key, "TileLength"] || SameQ[key, "TileWidth"] || SameQ[key, "NumberOfInks"] || SameQ[key, "XClipPathUnits"] || 
   	 								  SameQ[key, "YClipPathUnits"] || SameQ[key, "CellWidth"] || SameQ[key, "CellLength"] || SameQ[key, "SamplesPerPixel"], If[val < 0 , miss, vRes],
   
   									  SameQ[key, "InkSet"], If[val < 1 || val > 2, miss, vRes], 
   									  
   									  SameQ[key, "SubfileType"], If[If[NumberQ@val, val < 1 || val > 3, !StringMatchQ[ToString@val, "full-resolution image data", 
        							  IgnoreCase -> True] && !StringMatchQ[ToString@val, "reduced-resolution image data", IgnoreCase -> True] && !StringMatchQ[ToString@val,  "single page of a multi-page image", IgnoreCase -> True]], 
   									  miss, vRes],
   
   									  SameQ[key, "Compression"], If[If[NumberQ@val, val < 1 || (val > 10 && val =!= 32773), !StringMatchQ[ToString@val, "No compression", IgnoreCase -> True] && !StringMatchQ[ToString@val, "Uncompressed", IgnoreCase -> True] &&
   									  !StringMatchQ[ToString@val, "CCITT modified Huffman RLE", IgnoreCase -> True] && !StringMatchQ[ToString@val, "PackBits compression, aka Macintosh RLE", 
        							  IgnoreCase -> True] && !StringMatchQ[ToString@val, "CCITT modified Huffman RLE", IgnoreCase -> True] && !StringMatchQ[ToString@val, "CCITT Group 3 fax encoding", 
        							  IgnoreCase -> True] && !StringMatchQ[ToString@val, "CCITT Group 4 fax encoding", IgnoreCase -> True] && !StringMatchQ[ToString@val, "LZW", IgnoreCase -> True] && 
        							  !StringMatchQ[ToString@val, "JPEG", IgnoreCase -> True]&&!StringMatchQ[ToString@val, "JPEG (new-style)", IgnoreCase -> True] && !StringMatchQ[ToString@val, "JPEG (old-style)", IgnoreCase -> True] && !StringMatchQ[ToString@val, "Deflate", IgnoreCase -> True] && 
        							  !StringMatchQ[ToString@val, "Defined by TIFF-F and TIFF-FX standard (RFC 2301) as ITU-T Rec. T.82 coding, using ITU-T Rec. T.85", IgnoreCase -> True] && 
        							  !StringMatchQ[ToString@val, "Defined by TIFF-F and TIFF-FX standard (RFC 2301) as ITU-T Rec. T.82 coding, using ITU-T Rec. T.43", IgnoreCase -> True]], miss, 
    								  vRes],
   
   									  SameQ[key, "BrightnessValue"], If[val < -99.99 || val > 99.99, miss, vRes],
      
   									  SameQ[key, "BlackLevel"], If[val < 0 || val >= 1, miss, vRes], SameQ[key, "BayerGreenSplit"], If[val < 0 || val >= 5000, miss, vRes],
   
   								      SameQ[key, "JPEGProc"], If[val =!= 0 && val != 14, miss, vRes],
   
   									  SameQ[key, "T6ptions"], If[val =!= 0 && val != 2, miss, vRes],
   
   									  SameQ[key, "T4Options"], If[val < 0 || val > 4, miss, vRes],
   
   									  SameQ[key, "WhitePoint"], If[N@val[[1]] <= 0 || N@val[[1]] > 1 || N@val[[2]] <= 0 || N@val[[2]] > 1, miss, vRes],
   
   									  SameQ[key, "LensSpecification"], If[(val[[1]] < 0 && val[[1]] != Missing["Indeterminate"] ) || (val[[2]] < 0 && val[[2]] != Missing["Indeterminate"] ) || (val[[3]] < 0 && 
       								  val[[3]] != Missing["Indeterminate"] ) || (val[[4]] < 0 && 
      								  val[[4]] != Missing["Indeterminate"] ), miss, vRes],
   
  									  SameQ[key, "FillOrder"], If[val =!= 1 && val =!= 2, miss, vRes],
   
   									  SameQ[key, "WhiteLevel"], If[val =!= 0 && val =!= 1 && val =!= 2, miss, vRes],
   									  
   									  SameQ[key, "GPSVersionID"], 
   									               Module[{tmp},
   									               	   Which[
   									               	      StringLength[StringReplace[val, "." | " " -> ""]] === 4, tmp = N[ToExpression[StringReplace[val, "." -> ""]]/1000, 3],
   									               	      StringLength[StringReplace[val, "." | " " -> ""]]   > 4, tmp = miss,
   									               	      True, tmp = val
   									               	     ];
   									               	   If[Quiet[MatchQ[ToExpression[tmp], _Real]], Rule[key, ToString@tmp], miss] 
   									               	 ],
   
   									  (MemberQ[ExifPositiveValuesOnly, key] && !MemberQ[QuantityTags, val] && !SameQ[key, "FocalLength"] && !SameQ[key, "FocalLengthIn35mmFilm"] && (NumberQ[N@val] || Quiet[NumberQ[ToExpression@val]])), If[N@val < 0 && ! ListQ@val, miss, vRes],	
   									     
   									  SameQ[key, "CustomRendered"], If[If[NumberQ@val, val < 0 || val > 1,  !StringMatchQ[ToString@val, "Normal process", IgnoreCase -> True] && 
   									  !StringMatchQ[ToString@val, "Custom process", IgnoreCase -> True]], miss, vRes],
   									  	
   									  True, If[MatchQ[val, "(-1)"], miss, vRes]]]

(**************************)
(**************************)
(**************************)
(**********IMPORT**********)
(***********EXIF***********)
(**************************)
(**************************)

ParseFlashInfo[state_] := Module[{cs = state, flash, flashInfo, fu, ffs, fm, ffp, ryc},
	                      flashInfo = cs["FlashInfo"];
	                      If[!IntegerQ[flashInfo], Return[cs]];
	                      Switch[ToExpression@flashInfo,
	                      	0 , fu = False;              ffs = "No strobe return detection function"; fm = Missing["Unknown"];             ffp = True;               ryc = False,
	                      	1 , fu = True;               ffs = "No strobe return detection function"; fm = Missing["Unknown"];             ffp = True;               ryc = False,
	                      	5 , fu = True;               ffs = Missing["Unknown"];                    fm = Missing["Unknown"];             ffp = True;               ryc = False,
	                      	7 , fu = True;               ffs = "Strobe return light detected";        fm = Missing["Unknown"];             ffp = True;               ryc = False,
	                      	8 , fu = False;              ffs = "No strobe return detection function"; fm = "Compulsory flash suppression"; ffp = True;               ryc = False,
	                      	9 , fu = True;               ffs = "No strobe return detection function"; fm = "Compulsory flash suppression"; ffp = True;               ryc = False,
	                      	13, fu = True;               ffs = Missing["Unknown"];                    fm = "Compulsory flash suppression"; ffp = True;               ryc = False,
	                      	15, fu = True;               ffs = "Strobe return light detected";        fm = "Compulsory flash suppression"; ffp = True;               ryc = False,
	                      	16, fu = False;              ffs = "No strobe return detection function"; fm = "Compulsory flash firing";      ffp = True;               ryc = False,
	                      	20, fu = False;              ffs = Missing["Unknown"];                    fm = "Compulsory flash firing";      ffp = True;               ryc = False,
	                      	24, fu = False;              ffs = "No strobe return detection function"; fm = Automatic;                      ffp = True;               ryc = False,
	                      	25, fu = True;               ffs = "No strobe return detection function"; fm = Automatic;                      ffp = True;               ryc = False,
	                      	29, fu = True;               ffs = Missing["Unknown"];                    fm = Automatic;                      ffp = True;               ryc = False,
	                      	31, fu = True;               ffs = "Strobe return light detected";        fm = Automatic;                      ffp = True;               ryc = False,
	                      	32, fu = False;              ffs = "No strobe return detection function"; fm = Missing["Unknown"];             ffp = False;              ryc = False,
	                      	48, fu = False;              ffs = "No strobe return detection function"; fm = "Compulsory flash firing";      ffp = False;              ryc = False,
	                      	65, fu = True;               ffs = "No strobe return detection function"; fm = Missing["Unknown"];             ffp = True;               ryc = True,
	                      	69, fu = True;               ffs = Missing["Unknown"];                    fm = Missing["Unknown"];             ffp = True;               ryc = True,
	                      	71, fu = True;               ffs = "Strobe return light detected";        fm = Missing["Unknown"];             ffp = True;               ryc = True,
	                      	73, fu = True;               ffs = "No strobe return detection function"; fm = "Compulsory flash suppression"; ffp = True;               ryc = True,
	                      	77, fu = True;               ffs = Missing["Unknown"];                    fm = "Compulsory flash suppression"; ffp = True;               ryc = True,
	                      	79, fu = True;               ffs = "Strobe return light detected";        fm = "Compulsory flash suppression"; ffp = True;               ryc = True,
	                      	80, fu = True;               ffs = "No strobe return detection function"; fm = "Compulsory flash firing";      ffp = True;               ryc = True,
	                      	88, fu = True;               ffs = "No strobe return detection function"; fm = Automatic;                      ffp = True;               ryc = True,
	                      	89, fu = True;               ffs = "No strobe return detection function"; fm = Automatic;                      ffp = True;               ryc = True,
	                      	93, fu = True;               ffs = Missing["Unknown"];                    fm = Automatic;                      ffp = True;               ryc = True,
	                      	95, fu = True;               ffs = "Strobe return light detected";        fm = Automatic;                      ffp = True;               ryc = True,
	                      	_ , fu = Missing["Unknown"]; ffs = Missing["Unknown"];                    fm = Missing["Unknown"];             ffp = Missing["Unknown"]; ryc = Missing["Unknown"];
	                      ];
	                      flash = <|"FlashUsed" -> fu, "FlashFiringStatus" -> ffs, "FlashMode" -> fm, "FlashFunctionPresent" -> ffp, "RedEyeCorrection" -> ryc|>;
	                      cs = AssociateTo[cs, "FlashInfo"-> flash];
	                      cs
   ]

ParseOrientation[state_] := Module[{cs = state, orientation, orientationInfo, cto, mir},
						  orientationInfo = cs["Orientation"];
	                      If[!IntegerQ[orientationInfo], Return[cs]];
	                      Switch[ToExpression@orientationInfo,
	                      	1, cto = Top;                mir = False,
	                      	2, cto = Top;                mir = True,
	                      	3, cto = Bottom;             mir = False,
	                      	4, cto = Bottom;             mir = True,
	                      	5, cto = Left;               mir = True,
	                      	6, cto = Right;              mir = False,
	                      	7, cto = Right;              mir = True,
	                      	8, cto = Left;               mir = False,
	                      	_, cto = Missing["Unknown"]; mir = Missing["Unknown"];
	                      ];
	                      orientation = <|"CameraTopOrientation" -> cto, "Mirrored" -> mir|>;
	                      cs = AssociateTo[cs, "Orientation"-> orientation];
	                      cs
   ]
    
ValidateExifAssociation[exif_] := Module[{tmp, badList={}}, 
 If[StringLength[exif] > 5 && !SameQ[ToString@exif, "LibraryFunctionError[LIBRARY_USER_ERROR,-2]"], 
 	tmp = DeleteMissing[ToExpression[Quiet@StringReplace[exif, WordCharacter .. ~~ " -> ," -> ""]]]; 
 	badList = DeleteCases[Flatten@(If[StringTrim[ToString@tmp[[#]]] === "xxx", Append[badList, #]] & /@ Keys[tmp]), Null];
 	KeyDropFrom[tmp, # & /@ badList]
 	]]

ParseTagsExifRaw[state_]:= Module[{cs = state, badList = {}}, 
							cs = AssociateTo[cs, # -> If[StringQ@cs[#] && cs[#] =!= Missing["NotAvailable"], StringTrim@cs[#], cs[#]] & /@ Join[Intersection[GPSTags, Keys[cs]], Intersection[StringTags, Keys[cs]], Intersection[QuantityTags, Keys[cs]], Intersection[IntegerTags, Keys[cs]], Intersection[RationalTags, Keys[cs]], Intersection[RealTags, Keys[cs]]]];
 							cs = AssociateTo[cs, # -> ToString[cs[#]] & /@ DeleteCases[Intersection[StringTags, Keys[cs]], "Orientation"]];
 							cs = AssociateTo[cs, # -> If[cs[#] =!= Missing["NotAvailable"] && NumberQ[ToExpression[cs[#]]], ToExpression[cs[#]], cs[#]] & /@ DeleteDuplicates[Join[Intersection[IntegerTags, Keys[state]], Intersection[RealTags, Keys[state]]]]];
  							If[ToString[cs[#]] == "", badList = Append[badList, #]] & /@ Keys[cs];
  							cs = KeyDrop[cs, # &/@ Join[badList, {"GPSTag", "ExifTag", "XMLPacket", "IPTCNAA", "InterColorProfile"}]];
 							cs = Append[cs, # -> Missing["NotAvailable"] & /@  DeleteCases[$AllExif, Alternatives @@ Sequence @@@ Keys[cs]]];
 							cs
  ]

ParseStringTagsExif[state_]     := Module[{cs = state, badList = {}},
	                             cs = AssociateTo[cs, # -> If[StringQ[cs[#]], StringTrim@cs[#], cs[#]] & /@ Join[Intersection[GPSTags, Keys[cs]], Intersection[StringTags, Keys[cs]], Intersection[QuantityTags, Keys[cs]], Intersection[IntegerTags, Keys[cs]], Intersection[RationalTags, Keys[cs]], Intersection[RealTags, Keys[cs]]]];
  							     cs = AssociateTo[cs, # -> ToString[cs[#]] & /@ DeleteCases[Intersection[StringTags, Keys[cs]], "Orientation"]];
  							     If[ToString[cs[#]] == "", badList = Append[badList, #]] & /@ Keys[cs];
                                 cs = KeyDrop[cs, # &/@ Join[badList, {"GPSTag", "ExifTag", "XMLPacket", "IPTCNAA", "InterColorProfile"}]];
  								 cs
  ]
 
ParseDateTimeTagsExif[state_]   := Module[{cs = state, tz},
	                             tz = Quiet[cs["TimeZoneOffset"]];
	                             If[!IntegerQ[tz], tz = $TimeZone];   
  								 cs = AssociateTo[cs, # -> If[! MatchQ[ cs[#], Missing["NotAvailable"]], TimeObject[If[ListQ[cs[#]], cs[#], IntegerPart[ToExpression[StringSplit[cs[#], ":"]]]], TimeZone -> tz], cs[#]] & /@ Intersection[TimeTags, Keys[cs]]];
 								 cs = AssociateTo[cs, # -> If[! MatchQ[ cs[#], Missing["NotAvailable"]], With[{dt = cs[#]},If[SameQ[#, "GPSDateStamp"], tz = 0]; If[StringLength@dt <=10, DateObject[Take[DateList[{dt,{"Year", ":", "Month", ":", "Day"}}], 3]], DateObject[DateList[{dt,{"Year", ":", "Month", ":", "Day" , " ", "Hour", ":", "Minute", ":", "Second"}}], TimeZone -> tz]]], cs[#]] & /@ Intersection[DateTags, Keys[cs]]];
  								 cs
  ]

ParseMultiValueTagsExif[state_] := Module[{cs = state},      
  								 cs = AssociateTo[cs, # -> If[! MatchQ[ cs[#], Missing["NotAvailable"]], If[StringContainsQ[ToString[cs[#]], "," | " "], ToExpression[StringSplit[ToString[cs[#]], " "]], ToExpression[cs[#]]], cs[#]] & /@ Intersection[MultiValues, Keys[cs]]];
  								 cs
  ]

ParseIntAndRealTagsExif[state_] := Module[{cs = state},      
  							     cs = AssociateTo[cs, # -> If[NumberQ[ToExpression[cs[#]]], ToExpression[cs[#]], cs[#]] & /@ DeleteDuplicates[Join[Intersection[IntegerTags, Keys[state]], Intersection[RealTags, Keys[state]]]]];
  								 cs
  ]

ParseIndividualTagsExif[state_] := Module[{cs = state, GPSAR = state["GPSAltitudeRef"], GPSS = state["GPSSpeed"], GPSF = state["GPSDOP"], BV = state["BrightnessValue"], SSV = state["ShutterSpeedValue"], CC=state["ComponentsConfiguration"], CS = state["ColorSpace"], SA = state["SubjectArea"], RBW = state["ReferenceBlackWhite"], LS = state["LensSpecification"], AV = state["ApertureValue"], MAV = state["MaxApertureValue"], concatList = {}},            
                        If[GPSAR =!= Missing["KeyAbsent", "GPSAltitudeRef"]  , concatList = Append[concatList, "GPSAltitudeRef" -> If[SameQ[ToString[GPSAR], "Below sea level"], "BelowSeaLevel", "AboveSeaLevel"]]];
                        If[CS =!= Missing["KeyAbsent", "ColorSpace"]         , concatList = Append[concatList, "ColorSpace" -> If[SameQ[ToString[CS], "sRGB"], "RGBColor", CS]]];
  						If[SA =!= Missing["KeyAbsent", "SubjectArea"]        , concatList = Append[concatList, "SubjectArea" -> Switch[Count[SA, _Integer],      
       																														2, Point[SA],                                      
      	 																													3, Circle[{SA[[1]], SA[[2]]}, SA[[3]]],                                                            
       																														4, Rectangle[{SA[[1]] - SA[[3]]/2, SA[[2]] - SA[[4]]/2}, {SA[[1]] + SA[[3]]/2, SA[[2]] + SA[[4]]/2}],
 																															_, SA]]];
  						If[RBW =!= Missing["KeyAbsent", "ReferenceBlackWhite"] && RBW =!=Null, concatList = Append[concatList, "ReferenceBlackWhite" -> {{RBW[[1]], RBW[[3]], RBW[[5]]}, {RBW[[2]], RBW[[4]], RBW[[6]]}}]];
  						If[LS =!= Missing["KeyAbsent", "LensSpecification"]   , concatList = Append[concatList, "LensSpecification" -> If[StringContainsQ[ToString[LS], "Indeterminate"], ToExpression@StringReplace[ToString[LS], "Indeterminate" -> "Missing[\"Indeterminate\"]"], LS]]];
  						If[AV =!= Missing["KeyAbsent", "ApertureValue"]       , concatList = Append[concatList, "ApertureValue" -> N[AV]]];
  					    If[MAV =!= Missing["KeyAbsent", "MaxApertureValue"]   , concatList = Append[concatList, "MaxApertureValue" -> N[MAV]]];
  					    If[SSV =!= Missing["KeyAbsent", "ShutterSpeedValue"]  , concatList = Append[concatList, "ShutterSpeedValue" -> N[SSV]]];
  					    If[BV =!= Missing["KeyAbsent",  "BrightnessValue"]     , concatList = Append[concatList, "BrightnessValue" -> N[BV]]];
  					    If[GPSF =!= Missing["KeyAbsent", "GPSDOP"]            , concatList = Append[concatList, "GPSDOP" -> N[GPSF]]];
  					    If[GPSS =!= Missing["KeyAbsent", "GPSSpeed"]          , concatList = Append[concatList, "GPSSpeed" -> N[GPSS]]];
   						If[CC =!= Missing["KeyAbsent", "ComponentsConfiguration"]  , concatList = Append[concatList, "ComponentsConfiguration" -> ToString[CC]]];
  						cs = AssociateTo[cs, concatList];
  						cs
  ]

ParseRationalTagsExif[state_]  := Module[{cs = state},
  						cs = AssociateTo[cs, # -> If[MemberQ[QuantityTags, #], StringReplace[ToString[cs[#]], d1 : DigitCharacter .. ~~ "/" ~~ d2 : DigitCharacter .. :> "\!\(\*FractionBox[\(" ~~ d1 ~~ "\), \(" ~~ d2 ~~ "\)]\)"], 
        																						ToExpression@StringReplace[ToString[cs[#]], d1 : DigitCharacter .. ~~ "/" ~~ d2 : DigitCharacter .. :> "\!\(\*FractionBox[\(" ~~ d1 ~~ "\), \(" ~~ d2 ~~ "\)]\)"]
        																						] & /@ DeleteCases[DeleteCases[DeleteCases[Intersection[RationalTags,  Keys[state]], "FNumber"], "ExposureBiasValue"], "BaselineExposure"]];
  						cs
  ]

ParseQuantityTagsExif[state_] :=  Module[{cs = state, FLF = state["FocalLengthIn35mmFilm"], SST = state["SubSecTime"], SSTO = state["SubSecTimeOriginal"], SSTD = state["SubSecTimeDigitized"], ET = state["ExposureTime"], FL = state["FocalLength"], SD = state["SubjectDistance"], GPSS = state["GPSSpeed"], GPSA = state["GPSAltitude"], 
   									  GPST = state["GPSTrack"], GPSID = state["GPSImgDirection"], GPSBV = state["ExposureBiasValue"], GPSBE = state["BaselineExposure"], GPSLo = state["GPSLongitude"], GPSLa = state["GPSLatitude"], concatList = {}},                                                  	
 						If[FLF =!= Missing["KeyAbsent", "FocalLengthIn35mmFilm"], concatList = Append[concatList, "FocalLengthIn35mmFilm" -> If[Quiet@StringQ[FLF] && Quiet[StringLength[FLF] >= 3], With[{tmp = ToExpression@StringTrim@StringTake[FLF, {1, -3}]}, If[Quiet@Internal`RealValuedNumericQ[tmp] === True && tmp > 0, Quantity[IntegerPart[tmp], "Millimeters"], FLF]],If[Quiet@Internal`NonNegativeIntegerQ[FLF] === True,Quantity[FLF, "Millimeters"], FLF]]]];
 						If[SST =!= Missing["KeyAbsent", "SubSecTime"]          , concatList = Append[concatList, "SubSecTime" -> Quantity[SST, "Milliseconds"]]];
  						If[SSTO =!= Missing["KeyAbsent", "SubSecTimeOriginal"] , concatList = Append[concatList, "SubSecTimeOriginal" -> Quantity[SSTO, "Milliseconds"]]];
  						If[SSTD =!= Missing["KeyAbsent", "SubSecTimeDigitized"], concatList = Append[concatList, "SubSecTimeDigitized" -> Quantity[SSTD, "Milliseconds"]]];
  						If[ET =!= Missing["KeyAbsent", "ExposureTime"]         , concatList = Append[concatList, "ExposureTime" -> Quantity[ET, "Seconds"]]];
  						If[FL =!= Missing["KeyAbsent", "FocalLength"]          , concatList = Append[concatList, "FocalLength" -> Quantity[FL, "Millimeters"]]];
  						If[SD =!= Missing["KeyAbsent", "SubjectDistance"]      , concatList = Append[concatList, "SubjectDistance" -> Quantity[SD, "Meters"]]];
  						If[GPSS =!= Missing["KeyAbsent", "GPSSpeed"]           , concatList = Append[concatList, "GPSSpeed" -> Quantity[GPSS, "Kilometers"/"Hours"]]];
                        If[GPSA =!= Missing["KeyAbsent", "GPSAltitude"]        , concatList = Append[concatList, "GPSAltitude" -> Quantity[If[NumberQ[GPSA], GPSA, Select[ToExpression@GPSA, NumberQ[#] &]], "Meters"]]];
                        If[GPST =!= Missing["KeyAbsent", "GPSTrack"]           , concatList = Append[concatList, "GPSTrack" -> Quantity[GPST, "AngularDegrees"]]];
                        If[GPSID =!= Missing["KeyAbsent", "GPSImgDirection"]   , concatList = Append[concatList, "GPSImgDirection" -> Quantity[GPSID, "AngularDegrees"]]]; 
                        If[GPSBV =!= Missing["KeyAbsent", "ExposureBiasValue"] , concatList = Append[concatList, "ExposureBiasValue" -> Quantity[N[GPSBV], IndependentUnit["exposure values"]]]];
                        If[GPSBE =!= Missing["KeyAbsent", "BaselineExposure"]  , concatList = Append[concatList, "BaselineExposure" -> Quantity[N[GPSBE], IndependentUnit["exposure values"]]]];
                        If[GPSLo =!= Missing["KeyAbsent", "GPSLongitude"]      , concatList = Append[concatList, "GPSLongitude" -> 
                        	                                                                          Block[{tmpGPS = GPSLo}, 
                                             									                        If[ListQ[tmpGPS], 
                                             									                          tmpGPS = StringReplace[StringTake[ToString@N[tmpGPS], {2, -2}], "," -> ""]]; 
                                            									                            Module[{tmp = If[StringContainsQ[tmpGPS, "/"], 
                                            									                     	                    Select[N[ToExpression@StringSplit[tmpGPS, " "]], NumberQ[#] &], 
                                                									                                        ToExpression[StringCases[tmpGPS, NumberString]]]
                                                									                               },
                                                									                            
                                             									                                   Switch[Length@tmp, 
                                             									                                   	 1, First@Quantity[N[tmp], "AngularDegrees"], 
                                             									                              	     2, Quantity[N[tmp[[1]] + tmp[[2]]/60], "AngularDegrees"], 
                                             									                              	     3, Quantity[N[tmp[[1]] + tmp[[2]]/60 + tmp[[3]]/3600], "AngularDegrees"]]
                                             									                               ]
                                             									                          ]
                                             									                        ]
                                             									                     ];
  						If[GPSLa =!= Missing["KeyAbsent", "GPSLatitude"]       , concatList = Append[concatList, "GPSLatitude" -> 
                        	                                                                          Block[{tmpGPS = GPSLa}, 
                                             									                        If[ListQ[tmpGPS], 
                                             									                          tmpGPS = StringReplace[StringTake[ToString@N[tmpGPS], {2, -2}], "," -> ""]]; 
                                            									                            Module[{tmp = If[StringContainsQ[tmpGPS, "/"], 
                                            									                     	                    Select[N[ToExpression@StringSplit[tmpGPS, " "]], NumberQ[#] &], 
                                                									                                        ToExpression[StringCases[tmpGPS, NumberString]]]
                                                									                               },
                                                									                            
                                             									                                   Switch[Length@tmp, 
                                             									                                   	 1, First@Quantity[N[tmp], "AngularDegrees"], 
                                             									                              	     2, Quantity[N[tmp[[1]] + tmp[[2]]/60], "AngularDegrees"], 
                                             									                              	     3, Quantity[N[tmp[[1]] + tmp[[2]]/60 + tmp[[3]]/3600], "AngularDegrees"]]
                                             									                               ]
                                             									                          ]
                                             									                        ]
                                             									                     ];
  
 			 			cs = AssociateTo[cs, concatList] ;
  						cs
  ]


ParseValuesInGroupsExif[valEx_] := Module[{curState = valEx},	
  curState = ParseStringTagsExif[curState];
  curState = ParseDateTimeTagsExif[curState];
  curState = ParseMultiValueTagsExif[curState];
  curState = ParseIntAndRealTagsExif[curState];
  curState = ParseFlashInfo[curState];
  curState = ParseOrientation[curState];
  curState = ParseIndividualTagsExif[curState];
  curState = ParseQuantityTagsExif[curState];
  curState
  ]
  
  ParseValuesInGroupsExifOLD[valEx_] := Module[{curState = valEx},
  curState = ModifyMakerNoteRawExifOLD[curState];
  curState = RemoveNotExifTags[curState];
  curState = ParseDateTimeTagsExif[curState];
  curState = ParseMultiValueTagsExif[curState];
  curState = ParseIntAndRealTagsExif[curState];
  curState = ParseFlashInfo[curState];
  curState = ParseOrientation[curState];
  curState = ParseIndividualTagsExif[curState];
  curState = ParseQuantityTagsExif[curState];
  curState = ParseVersionTagsExifOLD[curState];  
  curState]
             
GetExifAll[] := RemoveNotExifTags[Delete[ParseValuesInGroupsExif[ValidateExifAssociation[$ReadExifAllRaw[True]]], "ImageResources"]]

RemoveNotExifTags[asc_] := Block[{newAsc=asc}, KeyDropFrom[newAsc,  # & /@ Complement[Keys[newAsc], Join[$AllExif, {"MakerNote"}]]]]

ModifyMakerNoteRawExif[asc_] := Module[{tmp, res = None, mkNote},
	                            mkNote = asc["MakerNote"];
	                            If[mkNote === Missing["KeyAbsent", "MakerNote"], Return[asc]];
	                            tmp = asc;
	                            mkNote = Quiet@ToExpression[StringSplit[mkNote]];
	                            mkNote = If[ListQ@mkNote, ByteArray[mkNote], ByteArray[{}]];
	                            res = Quiet@AssociateTo[tmp, "MakerNote"-> mkNote];
	                            If[AssociationQ[res], Return[res], Return[<||>]];
  ]
  
ModifyMakerNoteRawExifOLD[asc_] := Module[{tmp, res = None, mkNote}, 
	                            mkNote = asc["MakerNote"];
 	                            If[mkNote === Missing["KeyAbsent", "MakerNote"], Return[asc]];
 	                            tmp = asc;
 	                            mkNote = If[ListQ@mkNote, ByteArray[mkNote], ByteArray[{}]];
 	                            res = Quiet@AssociateTo[tmp, "MakerNote" -> mkNote];
 	                            If[AssociationQ[res], Return[res], Return[<||>]];
  ]
  
ParseVersionTagsExifOLD[asc_] := Module[{tmp, res = None, ex, fl, ip, gps, fnum, gpsLat, gpsLon}, 
	                            ex = asc["ExifVersion"];
	                            fl = asc["FlashpixVersion"];
	                            ip = asc["InteroperabilityVersion"];
	                            gps = asc["GPSVersionID"];
	                            gpsLat = asc["GPSLatitude"];
	                            gpsLon = asc["GPSLongitude"];
	                            fnum = asc["FNumber"];
	                            tmp = asc;
 	                            If[ex =!= Missing["KeyAbsent", "ExifVersion"], tmp = AssociateTo[tmp, "ExifVersion"->parstVersions[ex]]];
 	                            If[fl =!= Missing["KeyAbsent", "FlashpixVersion"], tmp = AssociateTo[tmp, "FlashpixVersion"->parstVersions[fl]]];
 	                            If[ip =!= Missing["KeyAbsent", "InteroperabilityVersion"], tmp = AssociateTo[tmp, "InteroperabilityVersion"->parstVersions[ip]]];
 	                            If[gps =!= Missing["KeyAbsent", "GPSVersionID"], tmp = AssociateTo[tmp, "GPSVersionID"->StringReplace[StringTake[ToString[gps], {2, -2}], ","->""]]];
 	                            If[fnum =!= Missing["KeyAbsent", "FNumber"], tmp = AssociateTo[tmp, "FNumber"->StringJoin["f/", ToString@N@fnum]]];
 	                            res = tmp;
 	                            If[AssociationQ[res], Return[res], Return[<||>]];
  ]

parstVersions[tmp_] := ToString@ToExpression@StringInsert[StringJoin[ToString /@ FromCharacterCode /@ tmp], ".", 3]

ReadExifIndividualTag[tag_]:= Module[{tmp, res = None},
								tmp = Quiet[StringTrim[$ReadExifIndividualTag[tag]]];
								tmp = If[SameQ[tag, "Orientation"] || SameQ[tag, "FlashInfo"], ToExpression@tmp, tmp];
								tmp = If[StringContainsQ[ToString[tmp], "LibraryFunctionError"] || tmp === Null || tmp === "", None, tmp];
								If[tmp =!= None, res = First[ValidateExif[ParseValuesInGroupsExif[<|tag->tmp|>]]]];
								res
  ]

ReadExif[tag_, rule_: False] := Switch[tag,
								  "AllRaw"              , Module[{tmp=ModifyMakerNoteRawExif@ParseTagsExifRaw[Quiet@ParseMultiValueTagsExif[ToExpression[$ReadExifAllRaw[False]]]]},
								  							      If[AssociationQ[tmp], RemoveNotExifTags[tmp], <||>]],	  
								  "All"                 , Module[{tmp=GetExifAll[]},If[Quiet[AssociationQ[tmp]], tmp, <||>]],
								   _                    , ReadExifIndividualTag[tag]  																	     									
  ]

ReadExif[tag_] := ReadExif[tag, False]


(**************************)
(**************************)
(**************************)
(**********IMPORT**********)
(***********IPTC***********)
(**************************)
(**************************)
ValidateIPTCAssociation[iptc_] := 
 If[StringLength[iptc] > 5 && !SameQ[ToString@iptc, "LibraryFunctionError[LIBRARY_USER_ERROR,-2]"], 
 	KeyMap[StringTrim, DeleteMissing[ToExpression[Quiet@StringReplace[iptc, WordCharacter .. ~~ " -> ," -> ""]]]]
 	]

ParseIntAndRealTagsIPTC[state_] := Module[{cs = state, app2 = state["Application2"], env = state["Envelope"]},
 									If[app2 =!= Missing["KeyAbsent", "Application2"], AssociateTo[app2, # -> If[ListQ@cs["Application2"][#], cs["Application2"][#], ToExpression@cs["Application2"][#]] & /@ DeleteDuplicates[Join[Intersection[ExportApplication2Number, Keys[cs["Application2"]]],Intersection[ExportEnvelopeNumber,Keys[cs["Application2"]]]]]]; AssociateTo[cs, "Application2" -> app2]];
 									If[env =!= Missing["KeyAbsent", "Envelope"], AssociateTo[env, # -> If[ListQ@cs["Envelope"][#], cs["Envelope"][#], ToExpression@cs["Envelope"][#]] & /@ DeleteDuplicates[Join[Intersection[ExportApplication2Number, Keys[cs["Envelope"]]],Intersection[ExportEnvelopeNumber,Keys[cs["Envelope"]]]]]]; AssociateTo[cs, "Envelope" -> env]];
 									cs
 ]

ParseDateTimeTagsIPTC[state_]   := Module[{cs = state, app2 = state["Application2"], env = state["Envelope"]},
 									(*TimeTags*)
 									If[app2 =!= Missing["KeyAbsent", "Application2"], AssociateTo[app2, # -> TimeObject[ToExpression@Drop[StringSplit[cs["Application2"][#], ":" | " " | "+"], -2], TimeZone->0] & /@ Intersection[TimeTags, Keys[cs["Application2"]]]]; AssociateTo[cs, "Application2" -> app2]];
 									If[env =!= Missing["KeyAbsent", "Envelope"], AssociateTo[env, # -> TimeObject[ToExpression@Drop[StringSplit[cs["Envelope"][#], ":" | " " | "+"], -2], TimeZone->0] & /@ Intersection[TimeTags, Keys[cs["Envelope"]]]]; AssociateTo[cs, "Envelope" -> env]];
 									(*DateTags*)
 									If[app2 =!= Missing["KeyAbsent", "Application2"], AssociateTo[app2, # -> With[{tstDate = ToExpression@StringSplit[cs["Application2"][#], "-"], date = Take[DateList[{cs["Application2"][#], {"Year", "-", "Month", "-", "Day"}}], 3]},If[!System`ContainsAny[tstDate, {0}], DateObject[date, TimeZone -> $TimeZone], -1]] & /@ Intersection[DateTags, Keys[cs["Application2"]]]]; AssociateTo[cs, "Application2" -> app2]];
 									If[env =!= Missing["KeyAbsent", "Envelope"], AssociateTo[env, # -> With[{tstDate = ToExpression@StringSplit[cs["Envelope"][#], "-"], date = Take[DateList[{cs["Envelope"][#], {"Year", "-", "Month", "-", "Day"}}], 3]},If[!System`ContainsAny[tstDate, {0}], DateObject[date, TimeZone -> $TimeZone], -1]] & /@ Intersection[DateTags, Keys[cs["Envelope"]]]]; AssociateTo[cs, "Envelope" -> env]];
 									cs
 ]

ParseMultiValueTagsIPTC[state_] := Module[{cs = state, app2 = state["Application2"], env = state["Envelope"]},
 									If[app2 =!= Missing["KeyAbsent", "Application2"], AssociateTo[app2, # -> If[StringContainsQ[ToString@cs["Application2"][#], "," | " "], ToExpression@StringSplit[ToString@cs["Application2"][#], ","], ToExpression@cs["Application2"][#]] & /@Intersection[MultiValues, Keys[cs["Application2"]]]]; AssociateTo[cs, "Application2" -> app2]];
 									If[env =!= Missing["KeyAbsent", "Envelope"], AssociateTo[env, # -> If[StringContainsQ[ToString@cs["Envelope"][#], "," | " "], ToExpression@StringSplit[ToString@cs["Envelope"][#], ","], ToExpression@cs["Envelope"][#]] & /@Intersection[MultiValues, Keys[cs["Envelope"]]]]; AssociateTo[cs, "Envelope" -> env]];
 									cs
 ]

ParseStringTagsIPTC[state_] := Module[{cs = state, app2 = state["Application2"], env = state["Envelope"]},
 									If[app2 =!= Missing["KeyAbsent", "Application2"], AssociateTo[app2, # -> If[StringQ@cs["Application2"][#], StringTrim[cs["Application2"][#]], cs["Application2"][#]] & /@ DeleteDuplicates[Join[Intersection[ExportApplication2Number, Keys[cs["Application2"]]], Intersection[ExportApplication2String, Keys[cs["Application2"]]]]]]; AssociateTo[cs, "Application2" -> app2]];
 									If[env =!= Missing["KeyAbsent", "Envelope"], AssociateTo[env, # -> If[StringQ@cs["Envelope"][#], StringTrim[cs["Envelope"][#]], cs["Envelope"][#]] & /@ DeleteDuplicates[Join[Intersection[ExportEnvelopeString, Keys[cs["Envelope"]]], Intersection[ExportEnvelopeString, Keys[cs["Envelope"]]]]]]; AssociateTo[cs, "Envelope" -> env]];
 									cs
 ]
 
 ParseIndividualTagsIPTC[state_] :=  Module[{cs = state, env = state["Envelope"]},
  									         If[env =!= Missing["KeyAbsent", "Envelope"], AssociateTo[env, "CharacterSet" -> If[StringContainsQ[cs["Envelope"]["CharacterSet"], "%G"], "UTF8", cs["Envelope"]["CharacterSet"]]]; 
                                             AssociateTo[cs, "Envelope" -> env]];
                                             cs
 ]
  
ParseValuesInGroupsIPTC[valEx_] := Module[{curState = valEx},
  curState = ParseDateTimeTagsIPTC[curState];
  curState = ParseMultiValueTagsIPTC[curState];
  curState = ParseIntAndRealTagsIPTC[curState];
  curState = ParseStringTagsIPTC[curState];
  curState = ParseIndividualTagsIPTC[curState];
  curState
  ]  
         
 ParseStringTagsIPTCRaw[state_] := Module[{cs = state, badList = {}},
	                             cs = AssociateTo[cs, # -> If[StringQ[cs[#]], StringTrim@cs[#], cs[#]] & /@ DeleteDuplicates[Join[Intersection[ExportApplication2Number, Keys[cs]], Intersection[ExportEnvelopeString, Keys[cs]], Intersection[ExportEnvelopeString, Keys[cs]], Intersection[ExportApplication2String, Keys[cs]]]]];
	                             If[StringTrim[ToString[cs[#]]] == "", badList = Append[badList, #]] & /@ Keys[cs];
  							     cs = KeyDrop[cs, # &/@ badList];
	                             cs = Append[cs, # -> Missing["NotAvailable"] & /@  DeleteCases[$AllIPTC, Alternatives @@ Sequence @@@ Keys[cs]]];
  								 cs
  ]        
             
GetIPTCAll[] := ParseValuesInGroupsIPTC[ValidateIPTCAssociation[$ReadIPTCAll[]]]

ReadIPTCIndividualTag[tag_]:= Module[{tmp, pth,res = None},
								tmp = Quiet[$ReadIPTCIndividualTag[tag]];
								tmp = If[StringContainsQ[ToString[tmp], "LibraryFunctionError"] || tmp === Null || tmp === "", None, tmp];
								If[MemberQ[IPTCEnvelope, tag], pth =  <|"Envelope"-> <|tag->tmp|>|>, pth = <|"Application2"-> <|tag->tmp|>|>];
								If[tmp =!= None, res = First@First@ValidateIPTC[ParseValuesInGroupsIPTC[pth]]];
								res
  ]
   						
ReadIPTC[tag_, rule_:False] := Module[{name = tag}, 
	Switch[name,
 	"All"   , With[{iptcAll = GetIPTCAll[]}, If[Quiet[AssociationQ[iptcAll]], iptcAll, <||>]],
  	"AllRaw", Module[{tmp = ParseStringTagsIPTCRaw[ParseIntAndRealTagsIPTC[ToExpression[$ReadIPTCAllRaw[]]]]}, If[Quiet[AssociationQ[tmp]], tmp, <||>]],
  	_       , ReadIPTCIndividualTag[tag]
  	]
  ]

ReadIPTC[tag_] := ReadIPTC[tag, False]

(**************************)
(**************************)
(**************************)
(**********IMPORT**********)
(***********XMP************)
(**************************)
(**************************)
splitAndGroup[list_List] := Module[{splitedList}, splitedList = MapAt[StringSplit[#, "."] &, list, {All, 1}];
  							If[Length@splitedList[[1, 1]] == 1, Return@list];
  							Normal@GroupBy[splitedList, (#[[1, 1]] &), MapAt[Last, {All, 1}]]]  

MakeAsoc[xmp_] := With[{getUtil = Needs["GeneralUtilities`"]},Quiet[DeleteCases[ReplaceAll[Replace[(xmp // GeneralUtilities`ToAssociations) /. Association -> foo, Rule[a_, b_] :> Rule[StringReplace[ToString[a], StringTake[ToString@a, 1] :> 
          			ToUpperCase[StringTake[ToString@a, 1]]], If[StringContainsQ[ToString@b, "del"], StringDelete[ToString@b, "del"], b]], {0, Infinity}] /. foo -> Association, {true -> True, false -> False}], del, Infinity]]]

ValidateXMPAssociation[xmp_] := If[StringLength[xmp] > 5 && ! SameQ[xmp, "LibraryFunctionError[LIBRARY_USER_ERROR,-2]"], Quiet[Module[{tmp = Map[splitAndGroup, ToExpression@StringReplace[StringReplace[Quiet@StringReplace[xmp, WordCharacter .. ~~ " -> ," -> ""],
								 {"/crs:" -> "."}], {"lang=\"x-default\"" | "\"type=\"Struct\"\"" | "\"type=\"Seq\"\"" -> "del"}], {-3}]}, RemoveBlankValues[MakeAsoc[tmp]]]], <||>]


TrimoutBadValuesXMPRaw[xmp_] := Module[{cs = xmp, badList = {}},
	 	                        If[StringTrim@ToString[cs[#]] == "" || StringTrim@ToString[cs[#]] == "del", badList = Append[badList, #]] & /@ Keys[cs];
  							    cs = KeyDrop[cs, # &/@ badList];
  							    cs = Append[cs, # -> Missing["NotAvailable"] & /@  DeleteCases[$AllXMP, Alternatives @@ Sequence @@@ Keys[cs]]];
  							    cs
]
FinalParseXMPRaw[xmp_]:= Module[{cs = xmp},
cs = AssociateTo[cs, # -> If[! MatchQ[cs[#], Missing["NotAvailable"]], If[StringContainsQ[ToString[cs[#]], "," | " "], ToExpression[StringSplit[ToString[cs[#]], " "]], ToExpression[cs[#]]], cs[#]] & /@ Intersection[MultiValues, Keys[cs]]];
cs = AssociateTo[cs, # -> If[NumberQ[ToExpression[cs[#]]], ToExpression[cs[#]], cs[#]] & /@ DeleteDuplicates[Join[Intersection[IntegerTags, Keys[cs]], Intersection[RealTags, Keys[cs]]]]];
cs
]

ValidateXMPAssociationRaw[xmp_] := If[StringLength[xmp] > 5 && ! SameQ[xmp, "LibraryFunctionError[LIBRARY_USER_ERROR,-2]"], Quiet[Module[{tmp = ToExpression@StringReplace[StringReplace[Quiet@StringReplace[xmp, WordCharacter .. ~~ " -> ," -> ""],
								 {"/crs:" -> "."}], {"lang=\"x-default\"" | "\"type=\"Struct\"\"" | "\"type=\"Seq\"\"" -> "del"}]}, FinalParseXMPRaw[TrimoutBadValuesXMPRaw[tmp]]]], <||>]

RemoveBlankValues[state_] := Module[{cs = state, badList = {}, xmpMM = state["MediaManagementSchema"], dc = state["DublinCoreSchema"], xmp = state["BasicSchema"], 
	                                digiKam = state["PhotoManagementSchema"], crs = state["CameraRawSchema"], MicrosoftPhoto = state["MicrosoftPhotoSchema"], 
   								    photoshop = state["PhotoshopSchema"], xmpRights = state["RightsManagementSchema"], xmpBJ = state["BasicJobTicketSchema"], 
   								    xmpTPg = state["PagedTextSchema"], pdf = state["AdobePDFSchema"]},

	                            If[pdf =!= Missing["KeyAbsent", "AdobePDFSchema"], 
	                            	If[StringTrim[ToString[pdf[#]]] == "", badList = Append[badList, #]] & /@ Keys[pdf];
	                            	pdf = KeyDrop[pdf, # &/@ badList];
	                                AssociateTo[cs, "AdobePDFSchema" -> pdf]];
	                            
	                            badList = {};
	                            
	                            If[xmpTPg =!= Missing["KeyAbsent", "PagedTextSchema"], 
	                            	If[StringTrim[ToString[xmpTPg[#]]] == "", badList = Append[badList, #]] & /@ Keys[xmpTPg];
	                            	xmpTPg = KeyDrop[xmpTPg, # &/@ badList];
	                                AssociateTo[cs, "PagedTextSchema" -> xmpTPg]];
	                            
	                            badList = {};

	                            If[xmpBJ =!= Missing["KeyAbsent", "BasicJobTicketSchema"], 
	                            	If[StringTrim[ToString[xmpBJ[#]]] == "", badList = Append[badList, #]] & /@ Keys[xmpBJ];
	                            	xmpBJ = KeyDrop[xmpBJ, # &/@ badList];
	                                AssociateTo[cs, "BasicJobTicketSchema" -> xmpBJ]];
	                            
	                            badList = {};
	                            
	                            If[xmpRights =!= Missing["KeyAbsent", "RightsManagementSchema"], 
	                            	If[StringTrim[ToString[xmpRights[#]]] == "", badList = Append[badList, #]] & /@ Keys[xmpRights];
	                            	xmpRights = KeyDrop[xmpRights, # &/@ badList];
	                                AssociateTo[cs, "RightsManagementSchema" -> xmpRights]];
	                            
	                            badList = {};
	                            
	                            If[photoshop =!= Missing["KeyAbsent", "PhotoshopSchema"], 
	                            	If[StringTrim[ToString[photoshop[#]]] == "", badList = Append[badList, #]] & /@ Keys[photoshop];
	                            	photoshop = KeyDrop[photoshop, # &/@ badList];
	                                AssociateTo[cs, "PhotoshopSchema" -> photoshop]];
	                            
	                            badList = {};

	                            If[MicrosoftPhoto =!= Missing["KeyAbsent", "MicrosoftPhotoSchema"], 
	                            	If[StringTrim[ToString[MicrosoftPhoto[#]]] == "", badList = Append[badList, #]] & /@ Keys[MicrosoftPhoto];
	                            	MicrosoftPhoto = KeyDrop[MicrosoftPhoto, # &/@ badList];
	                                AssociateTo[cs, "MicrosoftPhotoSchema" -> MicrosoftPhoto]];
	                            
	                            badList = {}; 
	                            
	                            If[crs =!= Missing["KeyAbsent", "CameraRawSchema"], 
	                            	If[StringTrim[ToString[crs[#]]] == "", badList = Append[badList, #]] & /@ Keys[crs];
	                            	crs = KeyDrop[crs, # &/@ badList];
	                                AssociateTo[cs, "CameraRawSchema" -> crs]];
	                            
	                            badList = {}; 

	                            If[digiKam =!= Missing["KeyAbsent", "PhotoManagementSchema"], 
	                            	If[StringTrim[ToString[digiKam[#]]] == "", badList = Append[badList, #]] & /@ Keys[digiKam];
	                            	digiKam = KeyDrop[digiKam, # &/@ badList];
	                                AssociateTo[cs, "PhotoManagementSchema" -> digiKam]];
	                            
	                            badList = {}; 
	          
	                            If[xmpMM =!= Missing["KeyAbsent", "MediaManagementSchema"], 
	                            	If[StringTrim[ToString[xmpMM[#]]] == "", badList = Append[badList, #]] & /@ Keys[xmpMM];
	                            	xmpMM = KeyDrop[xmpMM, # &/@ badList];
	                                AssociateTo[cs, "MediaManagementSchema" -> xmpMM]];
	                            
	                            badList = {};    

	                            If[dc =!= Missing["KeyAbsent", "DublinCoreSchema"], 
	                            	If[StringTrim[ToString[dc[#]]] == "", badList = Append[badList, #]] & /@ Keys[dc];
	                            	dc = KeyDrop[dc, # &/@ badList];
	                                AssociateTo[cs, "DublinCoreSchema" -> dc]];
	                            
	                            badList = {};

	                            If[xmp =!= Missing["KeyAbsent", "BasicSchema"], 
	                            	If[StringTrim[ToString[dc[#]]] == "", badList = Append[badList, #]] & /@ Keys[xmp];
	                            	xmp = KeyDrop[xmp, # &/@ badList];
	                                AssociateTo[cs, "BasicSchema" -> xmp]];

  								 cs
  ] 

ParseDateTimeTagsXMP[state_] :=  Module[{cs = state, xmp = state["BasicSchema"], digiKam = state["PhotoManagementSchema"], photoshop = state["PhotoshopSchema"]},
	
   If[xmp =!= Missing["KeyAbsent", "BasicSchema"], AssociateTo[xmp, # -> With[{dt = cs["BasicSchema"][#]}, If[StringLength@dt <= 10, DateObject[Take[DateList[{dt, {"Year", "-", "Month", "-", "Day"}}], {1, 3}], TimeZone -> $TimeZone],DateObject[DateList[{If[StringLength@dt > 19, StringTake[dt, {1, 19}], dt], {"Year", ":", "Month", ":", "Day", "T", "Hour", ":", "Minute", ":", "Second"}}], TimeZone -> $TimeZone]]] & /@ 
   		DeleteCases[Intersection[DateTags, Keys[cs["BasicSchema"]]], "When"]]; AssociateTo[cs, "BasicSchema" -> xmp]];
   
   If[digiKam =!= Missing["KeyAbsent", "PhotoManagementSchema"], AssociateTo[digiKam, # -> With[{dt=cs["PhotoManagementSchema"][#]},If[StringLength@dt <= 10, DateObject[Take[DateList[{dt, {"Year", "-", "Month", "-", "Day"}}], {1, 3}], TimeZone -> $TimeZone],DateObject[DateList[{If[StringLength@dt > 19, StringTake[dt, {1, 19}], dt], {"Year", ":", "Month", ":", "Day", "T", "Hour", ":", "Minute", ":", "Second"}}], TimeZone -> $TimeZone]]] & /@ 
   		DeleteCases[Intersection[DateTags, Keys[cs["PhotoManagementSchema"]]], "When"]]; AssociateTo[cs, "PhotoManagementSchema" -> digiKam]];
   
   If[photoshop =!= Missing["KeyAbsent", "PhotoshopSchema"], AssociateTo[photoshop, # -> With[{dt=cs["PhotoshopSchema"][#]},If[StringLength@dt <= 10, DateObject[Take[DateList[{dt, {"Year", "-", "Month", "-", "Day"}}], {1, 3}], TimeZone -> $TimeZone],DateObject[DateList[{If[StringLength@dt > 19, StringTake[dt, {1, 19}], dt], {"Year", ":", "Month", ":", "Day", "T", "Hour", ":", "Minute", ":", "Second"}}], TimeZone -> $TimeZone]]] & /@ 
   		DeleteCases[Intersection[DateTags, Keys[cs["PhotoshopSchema"]]], "When"]]; AssociateTo[cs, "PhotoshopSchema" -> photoshop]];
   
   cs
  ]

ParseMultiValueTagsXMP[state_] := Module[{cs = state, xmpMM = state["MediaManagementSchema"], dc = state["DublinCoreSchema"], xmp = state["BasicSchema"], digiKam = state["PhotoManagementSchema"], crs = state["CameraRawSchema"], MicrosoftPhoto = state["MicrosoftPhotoSchema"], 
   										  photoshop = state["PhotoshopSchema"], xmpRights = state["RightsManagementSchema"], xmpBJ = state["BasicJobTicketSchema"], xmpTPg = state["PagedTextSchema"], pdf = state["AdobePDFSchema"]},
   										  
  If[xmpMM =!= Missing["KeyAbsent", "MediaManagementSchema"], AssociateTo[xmpMM, # -> If[StringContainsQ[ToString@cs["MediaManagementSchema"][#], "," | " "], 
  		ToExpression@StringSplit[ToString@cs["MediaManagementSchema"][#], ","], ToExpression@cs["MediaManagementSchema"][#]] & /@ Intersection[MultiValues, Keys[cs["MediaManagementSchema"]]]]; 
  		AssociateTo[cs, "MediaManagementSchema" -> xmpMM]];
  
  If[dc =!= Missing["KeyAbsent", "DublinCoreSchema"], AssociateTo[dc, # -> If[StringContainsQ[ToString@cs["DublinCoreSchema"][#], "," | " "], 
  		ToExpression@StringSplit[ToString@cs["DublinCoreSchema"][#], ","], ToExpression@cs["DublinCoreSchema"][#]] & /@ Intersection[MultiValues, Keys[cs["DublinCoreSchema"]]]]; 
  		AssociateTo[cs, "DublinCoreSchema" -> dc]];
  
  If[xmp =!= Missing["KeyAbsent", "BasicSchema"], AssociateTo[xmp, # -> If[StringContainsQ[ToString@cs["BasicSchema"][#], "," | " "], 
  		ToExpression@StringSplit[ToString@cs["BasicSchema"][#], ","], ToExpression@cs["BasicSchema"][#]] & /@ Intersection[MultiValues, Keys[cs["BasicSchema"]]]]; 
  		AssociateTo[cs, "BasicSchema" -> xmp]];
  
  If[digiKam =!= Missing["KeyAbsent", "PhotoManagementSchema"], AssociateTo[digiKam, # -> If[StringContainsQ[ToString@cs["PhotoManagementSchema"][#], "," | " "], 
  		ToExpression@StringSplit[ToString@cs["PhotoManagementSchema"][#], ","], ToExpression@cs["PhotoManagementSchema"][#]] & /@ Intersection[MultiValues, Keys[cs["PhotoManagementSchema"]]]]; 
  		AssociateTo[cs, "PhotoManagementSchema" -> digiKam]];
  
  If[crs =!= Missing["KeyAbsent", "CameraRawSchema"], AssociateTo[crs, # -> If[StringContainsQ[ToString@cs["CameraRawSchema"][#],  "," | " "], 
  	    ToExpression@StringSplit[ToString@cs["CameraRawSchema"][#], ","], ToExpression@cs["CameraRawSchema"][#]] & /@ Intersection[MultiValues, Keys[cs["CameraRawSchema"]]]];
  	    AssociateTo[cs, "CameraRawSchema" -> crs]];
  
  If[MicrosoftPhoto =!= Missing["KeyAbsent", "MicrosoftPhotoSchema"], AssociateTo[MicrosoftPhoto, # -> If[StringContainsQ[ToString@cs["MicrosoftPhotoSchema"][#],  "," | " "], ToExpression@StringSplit[ToString@cs["MicrosoftPhotoSchema"][#], ","], 
        ToExpression@cs["MicrosoftPhotoSchema"][#]] & /@ Intersection[MultiValues, Keys[cs["MicrosoftPhotoSchema"]]]]; AssociateTo[cs, "MicrosoftPhotoSchema" -> MicrosoftPhoto]];
  
  If[photoshop =!= Missing["KeyAbsent", "PhotoshopSchema"], AssociateTo[photoshop, # -> If[StringContainsQ[ToString@cs["PhotoshopSchema"][#], "," | " "], ToExpression@StringSplit[ToString@cs["PhotoshopSchema"][#], ","], 
        ToExpression@cs["PhotoshopSchema"][#]] & /@ Intersection[MultiValues, Keys[cs["PhotoshopSchema"]]]]; AssociateTo[cs, "PhotoshopSchema" -> photoshop]];
  
  If[xmpRights =!= Missing["KeyAbsent", "RightsManagementSchema"], AssociateTo[xmpRights, # ->If[StringContainsQ[ToString@cs["RightsManagementSchema"][#], "," | " "], 
        ToExpression@StringSplit[ToString@cs["RightsManagementSchema"][#], ","], ToExpression@cs["RightsManagementSchema"][#]] & /@ Intersection[MultiValues, Keys[cs["RightsManagementSchema"]]]]; 
        AssociateTo[cs, "RightsManagementSchema" -> xmpRights]];
  
  If[xmpBJ =!= Missing["KeyAbsent", "BasicJobTicketSchema"], AssociateTo[xmpBJ, # -> If[StringContainsQ[ToString@cs["BasicJobTicketSchema"][#], "," | " "], 
        ToExpression@StringSplit[ToString@cs["BasicJobTicketSchema"][#], ","],ToExpression@cs["BasicJobTicketSchema"][#]] & /@Intersection[MultiValues, Keys[cs["BasicJobTicketSchema"]]]]; 
   		AssociateTo[cs, "BasicJobTicketSchema" -> xmpBJ]];
  
  If[xmpTPg =!= Missing["KeyAbsent", "PagedTextSchema"], AssociateTo[xmpTPg, # -> If[StringContainsQ[ToString@cs["PagedTextSchema"][#], "," | " "], 
        ToExpression@StringSplit[ToString@cs["PagedTextSchema"][#], ","],ToExpression@cs["PagedTextSchema"][#]] & /@Intersection[MultiValues, Keys[cs["PagedTextSchema"]]]]; 
   		AssociateTo[cs, "PagedTextSchema" -> xmpTPg]];
  
  If[pdf =!= Missing["KeyAbsent", "AdobePDFSchema"], AssociateTo[pdf, # -> If[StringContainsQ[ToString@cs["AdobePDFSchema"][#], "," | " "], 
        ToExpression@StringSplit[ToString@cs["AdobePDFSchema"][#], ","], ToExpression@cs["AdobePDFSchema"][#]] & /@ Intersection[MultiValues, Keys[cs["AdobePDFSchema"]]]]; 
   		AssociateTo[cs, "AdobePDFSchema" -> pdf]];
  
  cs
  ]

ParseIndividualTagsXMP[state_] := Module[{cs = state, crs = state["CameraRawSchema"]}, 
	If[crs =!= Missing["KeyAbsent", "CameraRawSchema"] && crs["Temperature"] =!= Missing["KeyAbsent", "Temperature"], 
		AssociateTo[crs, # -> Quantity[ToExpression[crs["Temperature"]], "Kelvins"] & /@ {"Temperature"}]; 
	   	AssociateTo[cs, "CameraRawSchema" -> crs]];
  
    cs
  ]

ParseIntAndRealTagsXMP[state_] := Module[{cs = state, xmpMM = state["MediaManagementSchema"], dc = state["DublinCoreSchema"], xmp = state["BasicSchema"], digiKam = state["PhotoManagementSchema"], crs = state["CameraRawSchema"], MicrosoftPhoto = state["MicrosoftPhotoSchema"], 
   										  photoshop = state["PhotoshopSchema"],xmpRights = state["RightsManagementSchema"], xmpBJ = state["BasicJobTicketSchema"], xmpTPg = state["PagedTextSchema"], pdf = state["AdobePDFSchema"]},
  
  If[xmpMM =!= Missing["KeyAbsent", "MediaManagementSchema"], AssociateTo[xmpMM, # -> If[ListQ@cs["MediaManagementSchema"][#], cs["MediaManagementSchema"][#], 
        ToExpression@cs["MediaManagementSchema"][#]] & /@ DeleteCases[DeleteDuplicates[Join[Intersection[IntegerTags, Keys[cs["MediaManagementSchema"]]], Intersection[BooleanTags, Keys[cs["MediaManagementSchema"]]], Intersection[RealTags, Keys[cs["MediaManagementSchema"]]]]], "WhiteBalance"]]; 
   		AssociateTo[cs, "MediaManagementSchema" -> xmpMM]];
  
  If[dc =!= Missing["KeyAbsent", "DublinCoreSchema"], AssociateTo[dc, # -> If[ListQ@cs["DublinCoreSchema"][#], cs["DublinCoreSchema"][#], 
  		ToExpression@cs["DublinCoreSchema"][#]] & /@ DeleteCases[DeleteDuplicates[Join[Intersection[IntegerTags, Keys[cs["DublinCoreSchema"]]], Intersection[BooleanTags, Keys[cs["DublinCoreSchema"]]], Intersection[RealTags, Keys[cs["DublinCoreSchema"]]]]], "WhiteBalance"]]; 
   		AssociateTo[cs, "DublinCoreSchema" -> dc]];
  
  If[xmp =!= Missing["KeyAbsent", "BasicSchema"], AssociateTo[xmp, # -> If[ListQ@cs["BasicSchema"][#], cs["BasicSchema"][#], 
  	    ToExpression@cs["BasicSchema"][#]] & /@ DeleteCases[DeleteDuplicates[Join[Intersection[IntegerTags, Keys[cs["BasicSchema"]]], Intersection[BooleanTags, Keys[cs["BasicSchema"]]], Intersection[RealTags, Keys[cs["BasicSchema"]]]]], "WhiteBalance"]]; 
  		AssociateTo[cs, "BasicSchema" -> xmp]];
  
  If[digiKam =!= Missing["KeyAbsent", "PhotoManagementSchema"], AssociateTo[digiKam, # -> If[ListQ@cs["PhotoManagementSchema"][#], cs["PhotoManagementSchema"][#], 
  		ToExpression@cs["PhotoManagementSchema"][#]] & /@ DeleteCases[DeleteDuplicates[Join[Intersection[IntegerTags, Keys[cs["Application2"]]], Intersection[BooleanTags, Keys[cs["PhotoManagementSchema"]]], Intersection[RealTags, Keys[cs["PhotoManagementSchema"]]]]], "WhiteBalance"]]; 
   		AssociateTo[cs, "PhotoManagementSchema" -> digiKam]];
  
  If[crs =!= Missing["KeyAbsent", "CameraRawSchema"], AssociateTo[crs, # -> If[ListQ@cs["CameraRawSchema"][#], cs["CameraRawSchema"][#], 
  	    ToExpression@cs["CameraRawSchema"][#]] & /@ DeleteCases[DeleteDuplicates[Join[Intersection[IntegerTags, Keys[cs["CameraRawSchema"]]], Intersection[BooleanTags, Keys[cs["CameraRawSchema"]]], Intersection[RealTags, Keys[cs["CameraRawSchema"]]]]], "WhiteBalance"]]; 
   		AssociateTo[cs, "CameraRawSchema" -> crs]];
  
  If[MicrosoftPhoto =!= Missing["KeyAbsent", "MicrosoftPhotoSchema"], AssociateTo[MicrosoftPhoto, # -> If[ListQ@cs["MicrosoftPhotoSchema"][#], cs["MicrosoftPhotoSchema"][#],
  		ToExpression@cs["MicrosoftPhotoSchema"][#]] & /@ DeleteCases[DeleteDuplicates[Join[Intersection[IntegerTags, Keys[cs["MicrosoftPhotoSchema"]]], Intersection[BooleanTags, Keys[cs["MicrosoftPhotoSchema"]]], Intersection[RealTags, Keys[cs["MicrosoftPhotoSchema"]]]]], "WhiteBalance"]]; 
   		AssociateTo[cs, "MicrosoftPhotoSchema" -> MicrosoftPhoto]];
  
  If[photoshop =!= Missing["KeyAbsent", "PhotoshopSchema"], AssociateTo[photoshop, # -> If[ListQ@cs["PhotoshopSchema"][#], cs["PhotoshopSchema"][#], 
        ToExpression@cs["PhotoshopSchema"][#]] & /@ DeleteCases[DeleteDuplicates[Join[Intersection[IntegerTags, Keys[cs["PhotoshopSchema"]]], Intersection[BooleanTags, Keys[cs["PhotoshopSchema"]]], Intersection[RealTags, Keys[cs["PhotoshopSchema"]]]]], "WhiteBalance"]]; 
   		AssociateTo[cs, "PhotoshopSchema" -> photoshop]];
  
  If[xmpRights =!= Missing["KeyAbsent", "RightsManagementSchema"], AssociateTo[xmpRights, # -> If[ListQ@cs["RightsManagementSchema"][#], cs["RightsManagementSchema"][#], 
        ToExpression@cs["RightsManagementSchema"][#]] & /@ DeleteCases[DeleteDuplicates[Join[Intersection[IntegerTags, Keys[cs["RightsManagementSchema"]]], Intersection[BooleanTags, Keys[cs["RightsManagementSchema"]]], Intersection[RealTags, Keys[cs["RightsManagementSchema"]]]]], "WhiteBalance"]]; 
   		AssociateTo[cs, "RightsManagementSchema" -> xmpRights]];
  
  If[xmpBJ =!= Missing["KeyAbsent", "BasicJobTicketSchema"], AssociateTo[xmpBJ, # -> If[ListQ@cs["BasicJobTicketSchema"][#], cs["BasicJobTicketSchema"][#], 
        ToExpression@cs["BasicJobTicketSchema"][#]] & /@ DeleteCases[DeleteDuplicates[Join[Intersection[IntegerTags, Keys[cs["BasicJobTicketSchema"]]], Intersection[BooleanTags, Keys[cs["BasicJobTicketSchema"]]], Intersection[RealTags, Keys[cs["BasicJobTicketSchema"]]]]], "WhiteBalance"]]; 
   		AssociateTo[cs, "BasicJobTicketSchema" -> xmpBJ]];
  
  If[xmpTPg =!= Missing["KeyAbsent", "PagedTextSchema"], AssociateTo[xmpTPg, # -> If[ListQ@cs["PagedTextSchema"][#], cs["PagedTextSchema"][#], 
        ToExpression@cs["PagedTextSchema"][#]] & /@ DeleteCases[DeleteDuplicates[Join[Intersection[IntegerTags, Keys[cs["PagedTextSchema"]]], Intersection[BooleanTags, Keys[cs["PagedTextSchema"]]], Intersection[RealTags, Keys[cs["PagedTextSchema"]]]]], "WhiteBalance"]]; 
  		AssociateTo[cs, "PagedTextSchema" -> xmpTPg]];
  
  If[pdf =!= Missing["KeyAbsent", "AdobePDFSchema"], AssociateTo[pdf, # -> If[ListQ@cs["AdobePDFSchema"][#], cs["AdobePDFSchema"][#], 
        ToExpression@cs["AdobePDFSchema"][#]] & /@ DeleteCases[DeleteDuplicates[Join[Intersection[IntegerTags, Keys[cs["AdobePDFSchema"]]], Intersection[BooleanTags, Keys[cs["AdobePDFSchema"]]], Intersection[RealTags, Keys[cs["AdobePDFSchema"]]]]], "WhiteBalance"]]; 
   		AssociateTo[cs, "AdobePDFSchema" -> pdf]];
  
  cs
  ]
  
  ParseStringTagsXMP[state_] := Module[{cs = state, xmpMM = state["MediaManagementSchema"], dc = state["DublinCoreSchema"], xmp = state["BasicSchema"], digiKam = state["PhotoManagementSchema"], crs = state["CameraRawSchema"], MicrosoftPhoto = state["MicrosoftPhotoSchema"], 
   									    photoshop = state["PhotoshopSchema"], xmpRights = state["RightsManagementSchema"], xmpBJ = state["BasicJobTicketSchema"], xmpTPg = state["PagedTextSchema"], pdf = state["AdobePDFSchema"]},
  
  If[xmpMM =!= Missing["KeyAbsent", "MediaManagementSchema"], AssociateTo[xmpMM, # -> If[StringQ@cs["MediaManagementSchema"][#], StringTrim@cs["MediaManagementSchema"][#], cs["MediaManagementSchema"][#]] & /@ Keys[cs["MediaManagementSchema"]]]; AssociateTo[cs, "MediaManagementSchema" -> xmpMM]];
  
  If[dc =!= Missing["KeyAbsent", "DublinCoreSchema"], AssociateTo[dc, # -> If[StringQ@cs["DublinCoreSchema"][#], StringTrim@cs["DublinCoreSchema"][#], cs["DublinCoreSchema"][#]] & /@ Keys[cs["DublinCoreSchema"]]]; AssociateTo[cs, "DublinCoreSchema" -> dc]]; 
  
  If[xmp =!= Missing["KeyAbsent", "BasicSchema"], AssociateTo[xmp, # ->   If[StringQ@cs["BasicSchema"][#], StringTrim@cs["BasicSchema"][#], cs["BasicSchema"][#]] & /@ Keys[cs["BasicSchema"]]]; AssociateTo[cs, "BasicSchema" -> xmp]]; 
  
  If[digiKam =!= Missing["KeyAbsent", "PhotoManagementSchema"], AssociateTo[digiKam, # -> If[StringQ@cs["PhotoManagementSchema"][#], StringTrim@cs["PhotoManagementSchema"][#], cs["PhotoManagementSchema"][#]] & /@ Keys[cs["PhotoManagementSchema"]]]; AssociateTo[cs, "PhotoManagementSchema" -> digiKam]];
  
  If[crs =!= Missing["KeyAbsent", "CameraRawSchema"], AssociateTo[crs, # -> If[StringQ@cs["CameraRawSchema"][#], StringTrim@cs["CameraRawSchema"][#], cs["CameraRawSchema"][#]] & /@ Keys[cs["CameraRawSchema"]]]; AssociateTo[cs, "CameraRawSchema" -> crs]];
  
  If[MicrosoftPhoto =!= Missing["KeyAbsent", "MicrosoftPhotoSchema"], AssociateTo[MicrosoftPhoto, # -> If[StringQ@cs["MicrosoftPhotoSchema"][#], StringTrim@cs["MicrosoftPhotoSchema"][#], cs["MicrosoftPhotoSchema"][#]] & /@ Keys[cs["MicrosoftPhotoSchema"]]]; AssociateTo[cs, "MicrosoftPhotoSchema" -> MicrosoftPhoto]];
  
  If[photoshop =!= Missing["KeyAbsent", "PhotoshopSchema"], AssociateTo[photoshop, # -> If[StringQ@cs["PhotoshopSchema"][#], StringTrim@cs["PhotoshopSchema"][#], cs["PhotoshopSchema"][#]] & /@ Keys[cs["PhotoshopSchema"]]]; AssociateTo[cs, "PhotoshopSchema" -> photoshop]];
  
  If[xmpRights =!= Missing["KeyAbsent", "RightsManagementSchema"], AssociateTo[xmpRights, # -> If[StringQ@cs["RightsManagementSchema"][#], StringTrim@cs["RightsManagementSchema"][#], cs["RightsManagementSchema"][#]] & /@ Keys[cs["RightsManagementSchema"]]]; AssociateTo[cs, "RightsManagementSchema" -> xmpRights]];
  
  If[xmpBJ =!= Missing["KeyAbsent", "BasicJobTicketSchema"], AssociateTo[xmpBJ, # -> If[StringQ@cs["BasicJobTicketSchema"][#], StringTrim@cs["BasicJobTicketSchema"][#], cs["BasicJobTicketSchema"][#]] & /@ Keys[cs["BasicJobTicketSchema"]]]; AssociateTo[cs, "BasicJobTicketSchema" -> xmpBJ]];
  
  If[xmpTPg =!= Missing["KeyAbsent", "PagedTextSchema"], AssociateTo[xmpTPg, # -> If[StringQ@cs["PagedTextSchema"][#], StringTrim@cs["PagedTextSchema"][#], cs["PagedTextSchema"][#]] & /@ Keys[cs["PagedTextSchema"]]]; AssociateTo[cs, "PagedTextSchema" -> xmpTPg]];
  
  If[pdf =!= Missing["KeyAbsent", "AdobePDFSchema"], AssociateTo[pdf, # -> If[StringQ@cs["AdobePDFSchema"][#], StringTrim@cs["AdobePDFSchema"][#], cs["AdobePDFSchema"][#]] & /@ Keys[cs["AdobePDFSchema"]]]; AssociateTo[cs, "AdobePDFSchema" -> pdf]];
  
  cs
  ]
  
  ParseValuesInGroupsXMP[valEx_] := Module[{curState = valEx},
  curState = ParseDateTimeTagsXMP[curState];
  curState = ParseMultiValueTagsXMP[curState];
  curState = ParseIntAndRealTagsXMP[curState];
  curState = ParseStringTagsXMP[curState];
  curState = ParseIndividualTagsXMP[curState];
  curState
  ]

GetXMPAll[] := ParseValuesInGroupsXMP[ValidateXMPAssociation[$ReadXMPAll[]]]

 ParseTagsXMPRaw[state_] := Module[{cs = state},
	                             cs = Append[cs, # -> Missing["NotAvailable"] & /@  DeleteCases[$AllXMP, Alternatives @@ Sequence @@@ Keys[cs]]];
  								 cs
  ]        

ReadXMP[tag_, rule_:False] := Module[{name = tag}, 
	If[SameQ[name, "All"], If[Quiet[AssociationQ[GetXMPAll[]]], GetXMPAll[], <||>], 
 	If[SameQ[name, "AllRaw"],  Module[{tmp = ParseTagsXMPRaw@ValidateXMPAssociationRaw[$ReadXMPAllRaw[]]}, If[Quiet[AssociationQ[tmp]], tmp, <||>]]]]]
		
ReadXMP[tag_] := ReadXMP[tag, False]

(**************************)
(**************************)
(**************************)
(**********EXPORT**********)
(***********EXIF***********)
(**************************)
(**************************)
GetOrientationNumber[assc_] :=  If[assc =!= Missing["KeyAbsent", "Orientation"],
								  Which[
								   assc["CameraTopOrientation"] === Top                && assc["Mirrored"] === False, 1,
								   assc["CameraTopOrientation"] === Top                && assc["Mirrored"] === True,  2,
								   assc["CameraTopOrientation"] === Bottom             && assc["Mirrored"] === False, 3,
								   assc["CameraTopOrientation"] === Bottom             && assc["Mirrored"] === True,  4,
								   assc["CameraTopOrientation"] === Left               && assc["Mirrored"] === True,  5,
								   assc["CameraTopOrientation"] === Right              && assc["Mirrored"] === False, 6,
								   assc["CameraTopOrientation"] === Right              && assc["Mirrored"] === True,  7,
								   assc["CameraTopOrientation"] === Left               && assc["Mirrored"] === False, 8,
								   True                                                                             , 255
								   ]]

GetFlashNumber[assc_] := If[assc =!= Missing["KeyAbsent", "FlashInfo"],
  							Which[
							   assc["FlashUsed"] === False && assc["FlashFiringStatus"] === "No strobe return detection function"  && assc["FlashMode"] === Missing["Unknown"] && 
		    				   assc["FlashFunctionPresent"] === True && assc["RedEyeCorrection"] === False, 0,
							   
							   assc["FlashUsed"] === True  && assc["FlashFiringStatus"] === "No strobe return detection function"  && assc["FlashMode"] === Missing["Unknown"] && 
							   assc["FlashFunctionPresent"] === True && assc["RedEyeCorrection"] === False, 1,
							   							   
							   assc["FlashUsed"] === True  && assc["FlashFiringStatus"] === Missing["Unknown"]                     && assc["FlashMode"] === Missing["Unknown"] && 
							   assc["FlashFunctionPresent"] === True && assc["RedEyeCorrection"] === False, 5,
							   
							   assc["FlashUsed"] === True  && assc["FlashFiringStatus"] === "Strobe return light detected"         && assc["FlashMode"] === Missing["Unknown"] && 
							   assc["FlashFunctionPresent"] === True && assc["RedEyeCorrection"] === False, 7,
							   
							   assc["FlashUsed"] === False && assc["FlashFiringStatus"] === "No strobe return detection function"  && assc["FlashMode"] === "Compulsory flash suppression" && 
							   assc["FlashFunctionPresent"] === True && assc["RedEyeCorrection"] === False, 8,
							   
							   assc["FlashUsed"] === True  && assc["FlashFiringStatus"] === "No strobe return detection function"  && assc["FlashMode"] === "Compulsory flash suppression" && 
							   assc["FlashFunctionPresent"] === True && assc["RedEyeCorrection"] === False, 9,
							   
							   assc["FlashUsed"] === True  && assc["FlashFiringStatus"] === Missing["Unknown"]                     && assc["FlashMode"] === "Compulsory flash suppression" && 
							   assc["FlashFunctionPresent"] === True && assc["RedEyeCorrection"] === False, 13,
							   
							   assc["FlashUsed"] === True && assc["FlashFiringStatus"] === "Strobe return light detected"          && assc["FlashMode"] === "Compulsory flash suppression" && 
							   assc["FlashFunctionPresent"] === True && assc["RedEyeCorrection"] === False, 15,
							   
							   assc["FlashUsed"] === False && assc["FlashFiringStatus"] === "No strobe return detection function"  && assc["FlashMode"] === "Compulsory flash firing" && 
							   assc["FlashFunctionPresent"] === True && assc["RedEyeCorrection"] === False, 16,
							   
							   assc["FlashUsed"] === False && assc["FlashFiringStatus"] === Missing["Unknown"]                     && assc["FlashMode"] === "Compulsory flash firing" && 
							   assc["FlashFunctionPresent"] === True && assc["RedEyeCorrection"] === False, 20,
							   
							   assc["FlashUsed"] === False && assc["FlashFiringStatus"] === "No strobe return detection function"  && assc["FlashMode"] === Automatic && 
							   assc["FlashFunctionPresent"] === True && assc["RedEyeCorrection"] === False, 24,
							   
							   assc["FlashUsed"] === True  && assc["FlashFiringStatus"] === "No strobe return detection function"  && assc["FlashMode"] === Automatic && 
							   assc["FlashFunctionPresent"] === True && assc["RedEyeCorrection"] === False, 25,
							   
							   assc["FlashUsed"] === True  && assc["FlashFiringStatus"] === Missing["Unknown"]                     && assc["FlashMode"] === Automatic && 
							   assc["FlashFunctionPresent"] === True && assc["RedEyeCorrection"] === False, 29,
							   
							   assc["FlashUsed"] === True  && assc["FlashFiringStatus"] === "Strobe return light detected"         && assc["FlashMode"] === Automatic && 
							   assc["FlashFunctionPresent"] === True && assc["RedEyeCorrection"] === False, 31,
							   
							   assc["FlashUsed"] === False && assc["FlashFiringStatus"] === "No strobe return detection function"  && assc["FlashMode"] === Missing["Unknown"] && 
							   assc["FlashFunctionPresent"] === False && assc["RedEyeCorrection"] === False, 32,
							   
							   assc["FlashUsed"] === False && assc["FlashFiringStatus"] === "No strobe return detection function"  && assc["FlashMode"] === "Compulsory flash firing" && 
							   assc["FlashFunctionPresent"] === False && assc["RedEyeCorrection"] === False, 48,
							   
							   assc["FlashUsed"] === True  && assc["FlashFiringStatus"] === "No strobe return detection function"  && assc["FlashMode"] === Missing["Unknown"] && 
							   assc["FlashFunctionPresent"] === True && assc["RedEyeCorrection"] === True, 65,
							   
							   assc["FlashUsed"] === True  && assc["FlashFiringStatus"] === Missing["Unknown"]                     && assc["FlashMode"] === Missing["Unknown"] && 
							   assc["FlashFunctionPresent"] === True && assc["Strobe return light not detected"] === True, 69,
							   
							   assc["FlashUsed"] === True  && assc["FlashFiringStatus"] === "Strobe return light detected"         && assc["FlashMode"] === Missing["Unknown"] && 
							   assc["FlashFunctionPresent"] === True && assc["RedEyeCorrection"] === True, 71,
							   
							   assc["FlashUsed"] === True  && assc["FlashFiringStatus"] === "No strobe return detection function"  && assc["FlashMode"] === "Compulsory flash suppression" && 
							   assc["FlashFunctionPresent"] === True && assc["RedEyeCorrection"] === True, 73,
							    					   
							   assc["FlashUsed"] === True  &&  assc["FlashFiringStatus"] === Missing["Unknown"]                    && assc["FlashMode"] === "Compulsory flash suppression" && 
							   assc["FlashFunctionPresent"] === True && assc["RedEyeCorrection"] === True, 77,
							   
							   assc["FlashUsed"] === True  &&  assc["FlashFiringStatus"] === "Strobe return light detected"        && assc["FlashMode"] === "Compulsory flash suppression" && 
							   assc["FlashFunctionPresent"] === True && assc["RedEyeCorrection"] === True, 79, 
							   
							   assc["FlashUsed"] === True  &&  assc["FlashFiringStatus"] === "No strobe return detection function" && assc["FlashMode"] === "Compulsory flash firing" && 
							   assc["FlashFunctionPresent"] === True && assc["RedEyeCorrection"] === True, 80,
							   
							   assc["FlashUsed"] === True  &&  assc["FlashFiringStatus"] === "No strobe return detection function" && assc["FlashMode"] === Automatic && 
							   assc["FlashFunctionPresent"] === True && assc["RedEyeCorrection"] === True, 88,
							   
							   assc["FlashUsed"] === True  &&  assc["FlashFiringStatus"] === "No strobe return detection function" && assc["FlashMode"] === Automatic && 
							   assc["FlashFunctionPresent"] === True && assc["RedEyeCorrection"] === True, 89,
							   
							   assc["FlashUsed"] === True  &&  assc["FlashFiringStatus"] === Missing["Unknown"]                    && assc["FlashMode"] === Automatic && 
							   assc["FlashFunctionPresent"] === True && assc["RedEyeCorrection"] === True, 93,
							   
							   assc["FlashUsed"] === True  &&  assc["FlashFiringStatus"] === "Strobe return light detected"        && assc["FlashMode"] === Automatic && 
							   assc["FlashFunctionPresent"] === True && assc["RedEyeCorrection"] === True, 95,
							   
							   True, 0
							   ]]

ExifProcessToRaw[Rule[key_, assoc_Association]] := Rule[key, AssociationMap[ExifProcessToRaw, assoc]]
ExifProcessToRaw[Rule[key_, val_]] := Which[   
									   SameQ["ReferenceBlackWhite", ToString@key]	, Rule[key, Module[{a = {val[[1, 1]], val[[2, 1]], val[[1, 2]], val[[2, 2]], val[[1, 3]], val[[2, 3]]}}, 
																							 					StringSplit[StringJoin[Riffle[ToString[#, InputForm] & /@ a, " "]], " " ] /. 
																							   				    s_String /; StringMatchQ[s, DigitCharacter ..] :> s <> "/1" // StringRiffle]],	
									   
									   SameQ[ToString[key], "GPSVersionID"]          , Rule[key, Module[{v = StringReplace[val, "." -> ""]},
																													 Switch[StringCount[v, _],
																														  1, v = StringInsert[v, "000", -1],
																														  2, v = StringInsert[v, "00", -1],
																														  3, v = StringInsert[v, "0", -1],
																														  4, v
																													  ];
																													 v = StringSplit[v, ""];
																													 v = StringReplace[StringTake[ToString@v, {2, -2}], "," -> ""];
																													 v
																											 ]
																									 ],
									   
									   StringContainsQ[ToString[key], "Version"]            , Rule[key, Module[{v = StringReplace[val, "." -> ""]},
																													 Switch[StringCount[v, _],
																														  1, v = StringInsert[v, "000", 1],
																														  2, v = StringInsert[v, "00", 1],
																														  3, v = StringInsert[v, "0", 1],
																														  4, v
																													  ];
																													 v = StringSplit[v, ""];
																													 v = First@ToCharacterCode[v[[#]]] & /@ Range[Count[v, _]];
																													 v = StringReplace[StringTake[ToString@v, {2, -2}], "," -> ""];
																													 v
																											 ]
																									 ],
									   
									   SameQ[ToString@key, "GPSSpeedRef"],                Rule[key, Which[StringContainsQ[val, "km"], "K", StringContainsQ[val, "mi"], "M", StringContainsQ[val, "knot"], "N", True, val]],
									  
									   SameQ[ToString@key, "GPSDestBearingRef"],          Rule[key, Which[StringContainsQ[val, "True"], "T", StringContainsQ[val, "Magnetic"], "M", True, val]],
									   
									   StringContainsQ[ToString[key], "GPSSpeed"] ||
									   StringContainsQ[ToString[key], "GPSTrack"] ||
									   StringContainsQ[ToString[key], "GPSImgDirection"] ||
									   StringContainsQ[ToString@key,  "GPS", IgnoreCase->True] && 
									   (StringContainsQ[ToString[key],"Latitude"] ||
									   StringContainsQ[ToString[key], "Longitude"] ||
									   StringContainsQ[ToString[key], "Altitude"]) &&
									   !StringContainsQ[ToString[key],"Ref"],                Rule[key, Normal@@val],
									   val === Missing["Disputed"],                          Rule[key, -1],
									   val === Missing["Unknown"] ,                          Rule[key,  0],
									   SameQ["SubjectArea", ToString@key],            Rule[key, Module[{v1 = Flatten[(List @@ val)]}, 
																										 Which[Count[v1, _] === 2, ToString[Round@N[v1[[1]]]], 
																										  Count[v1, _] === 3, 
																										  StringJoin[ToString[Round@N[v1[[1]]]], " ", 
																										   ToString[Round@N[v1[[2]]]], " ", ToString[Round@N[v1[[3]]]]], 
																										  Count[v1, _] === 4, 
																										  StringJoin[ToString[Round@N[(v1[[1]] + v1[[3]])/2]], " ", 
																										   ToString[Round@N[(v1[[4]] + v1[[2]])/2]], " ", 
																										   ToString[Round@N[(v1[[3]] + v1[[1]])/2]], " ", 
																										   ToString[Round@N[(v1[[4]] + v1[[2]])/2]]]]]],																								   
									  									   
									  StringContainsQ[ToString@key, "subsec", IgnoreCase->True], Rule[key, If[QuantityQ[val], QuantityMagnitude[val], val]],
									  
									  SameQ[ToString@key, "DateTime"] ||
									  SameQ[ToString@key, "DateTimeOriginal"] || 
									  SameQ[ToString@key, "DateTimeDigitized"] ,      	      Rule[key, StringJoin[StringTake[StringReplace[ToString[DateObject[][[1]]], ", " -> "-"], {2, -2}],
									   																			   " ", 
 																												   StringTake[StringReplace[ToString[Round[Normal @@ DateObject[][[2]]]], ", " -> ":"], {2, -2}]]],
									   
									   MemberQ[DateTags, ToString@key] ||
									   SameQ[ToString@key, "GPSDateStamp"] , 	      Rule[key, Module[{tmp = StringSplit[StringReplace[StringReplace[StringTake[StringReplace[StringJoin[ToString[val[[1]]], 
							             														   If[ListQ[val[[2]]], ToString[Normal @@ val[[2]]], ""]], "}{" -> "  "], {2, StringLength@StringReplace[StringJoin[ToString[val[[1]]], 
							                													   If[ListQ[val[[2]]], ToString[Normal @@ val[[2]]], ""]], "}{" -> ", "] - 1}], ", " -> ":"], "." -> ""], "  "]}, 
							     																   StringJoin[tmp[[1]], " ", If[ListQ[tmp[[2]]], tmp[[2]], ""]]]],
							     																   
									   SameQ[ToString@key, "GPSTimeStamp"] ,           Rule[key, StringInsert[StringReplace[StringReplace[StringJoin[ToString@(Round[Normal @@ val])[[1]], ":", 
																												 ToString@(Round[Normal @@ val])[[2]], ":", 
																												 ToString@(Round[Normal @@ val])[[3]]], ":"-> " "], " " -> "/1 "], "/1", -1]],
									   MemberQ[TimeTags, ToString@key],                       Rule[key,StringJoin[ToString@(Round[Normal @@ val])[[1]], ":", 
																												 ToString@(Round[Normal @@ val])[[2]], ":", 
																												 ToString@(Round[Normal @@ val])[[3]]]],
                                       SameQ[ToString@key, "FocalLengthIn35mmFilm"] ||
									   (
									    !SameQ[ToString@key, "FNumber"] &&
									    !SameQ[ToString@key, "BaselineExposure"] &&
									    !SameQ[ToString@key, "ExposureBiasValue"] &&
									    MemberQ[RationalTags, ToString@key]),				 If[!NumberQ[val], Rule[key, If[QuantityQ[val],
 																								 				If[StringContainsQ[ToString[Normal @@ val], "/"|"-"|"--"|"---"], 
 																								 						StringJoin[StringCases[ToString[Normal @@ val], NumberString][[1]],"/", StringCases[ToString[Normal @@ val], NumberString][[2]]]
     																													,
   																														StringCases[ToString[Normal @@ val], NumberString][[1]]
   																												]
 																					 						    ,
  																												If[StringContainsQ[ToString[val], LetterCharacter],
   																														StringJoin[ToString[Round[ToExpression[StringCases[val, NumberString][[1]]]]], "/", 
   																															If[Count[StringCases[val, NumberString], _] === 2, 
 																																ToString[Round[ToExpression[StringCases[val, NumberString][[2]]]]], "1"]], val]
  																																]
 																								], Rule[key, val]],
									   
									   SameQ[key, "LensSpecification"],  			 Rule[key, StringSplit[StringReplace[StringReplace[ToString@InputForm[val], "Missing[Indeterminate]" -> "0/0"], 
									        														   "," | "{" | "}" -> " "]]/. s_String /; StringMatchQ[s, DigitCharacter ..] :> s <> "/1" // StringRiffle],
									   
									   SameQ[key, "FNumber"] || 
									   SameQ[key, "Lens"],    						 Rule[key, StringDelete[val, "f/"]],
									   
									   SameQ[key, "MakerNote"],                      Rule[key, StringReplace[StringTake[ToString@Normal[val], {2, -2}], "," -> ""]],
							   
									   SameQ[ToString@key, "BaselineExposure"] ||
									   SameQ[ToString@key, "ExposureBiasValue"] ,     Rule[key, (List @@ val)[[1]]],
									   True,                                                 Rule[key, Which[ListQ@val, StringTake[ToString[val], {2, StringLength[ToString[val]] - 1}],
									     															         QuantityQ@val, StringJoin[ToString@(List @@ val[[1]]), " ", (ToString@List @@ val[[2]])],
									     																	 True, Normal @@ val]]
						];

convertOldStyleExif[oldData_] :=  Module[{asc = With[{ky = Select[Keys[Association[oldData]], NumberQ[#] &]},KeyDrop[Association[oldData], # & /@ ky]]},
  
  							If[asc["ReferenceBlackWhite"] =!= Missing["KeyAbsent", "ReferenceBlackWhite"], 
   								With[{val = Quiet@Partition[asc["ReferenceBlackWhite"], 3]}, 
   								    AssociateTo[asc, "ReferenceBlackWhite" -> Quiet@Module[{a = {val[[1, 1]], val[[2, 1]], val[[1, 2]], val[[2, 2]], val[[1, 3]], val[[2, 3]]}}, 
       																		  StringSplit[StringJoin[Riffle[ToString[#, InputForm] & /@ a, " "]], " "] /. s_String /; StringMatchQ[s, DigitCharacter ..] :> s <> "/1" // StringRiffle]]]];
  
  							If[asc["BitsPerSample"] =!= Missing["KeyAbsent", "BitsPerSample"], 
   								With[{val = asc["BitsPerSample"]}, 
   									AssociateTo[asc, "BitsPerSample" -> Quiet@StringTake[StringDelete[ToString[val], ","], {2, -2}]]]];
  
  							If[asc["ExifVersion"] =!= Missing["KeyAbsent", "ExifVersion"], 
   								With[{val = asc["ExifVersion"]}, 
    								AssociateTo[asc, "ExifVersion" -> Quiet@StringTake[StringDelete[ToString[val], ","], {2, -2}]]]];
    								
							If[asc["CFAPattern"] =!= Missing["KeyAbsent", "CFAPattern"], 
   								With[{val = asc["CFAPattern"]}, 
    								AssociateTo[asc, "CFAPattern" -> Quiet@StringTake[StringDelete[ToString[val], ","], {2, -2}]]]];
  
  							If[asc["ComponentsConfiguration"] =!= Missing["KeyAbsent", "ComponentsConfiguration"], 
   								With[{val = asc["ComponentsConfiguration"]}, 
    								AssociateTo[asc, "ComponentsConfiguration" -> Quiet@StringTake[StringDelete[ToString[val], ","], {2, -2}]]]];
  
  							If[asc["FlashpixVersion"] =!= Missing["KeyAbsent", "FlashpixVersion"], 
   								With[{val = asc["FlashpixVersion"]}, 
    								AssociateTo[asc, "FlashpixVersion" -> Quiet@StringTake[StringDelete[ToString[val], ","], {2, -2}]]]];
  
  							If[asc["GPSVersionID"] =!= Missing["KeyAbsent", "GPSVersionID"], 
   								With[{val = asc["GPSVersionID"]}, 
    								AssociateTo[asc, "GPSVersionID" -> Quiet@StringTake[StringDelete[ToString[val], ","], {2, -2}]]]];
  
   							If[asc["SceneType"] =!= Missing["KeyAbsent", "SceneType"], 
   								With[{val = asc["SceneType"]}, 
    								AssociateTo[asc, "SceneType" -> Quiet@ToString@val]]];
  
   							If[asc["GPSLongitude"] =!= Missing["KeyAbsent", "GPSLongitude"], 
   								With[{val = ToString@N@asc["GPSLongitude"]}, 
    								AssociateTo[asc, "GPSLongitude" -> Quiet@Module[{tmp = If[StringContainsQ[val, "/"], Select[N[ToExpression@StringSplit[val, " "]], NumberQ[#] &], ToExpression[StringCases[val, NumberString]]]}, 
       																			Switch[Length@tmp,
       																					 1, First@N[tmp],
       																					 2, N[tmp[[1]] + tmp[[1]]/60], 
       																					 3, N[tmp[[1]] + tmp[[1]]/60 + tmp[[1]]/3600]]]]]];
  
  							If[asc["GPSLatitude"] =!= Missing["KeyAbsent", "GPSLatitude"], 
   								With[{val = ToString@N@asc["GPSLatitude"]}, 
    								AssociateTo[asc, "GPSLatitude" -> Quiet@Module[{tmp =  If[StringContainsQ[val, "/"], Select[N[ToExpression@StringSplit[val, " "]], NumberQ[#] &], ToExpression[StringCases[val, NumberString]]]}, 
       																			Switch[Length@tmp, 
   																						 1, First@N[tmp], 
   																						 2, N[tmp[[1]] + tmp[[1]]/60], 
   																						 3, N[tmp[[1]] + tmp[[1]]/60 + tmp[[1]]/3600]]]]]];
  
   							If[asc["MakerNote"] =!= Missing["KeyAbsent", "MakerNote"], 
   								With[{val = asc["MakerNote"]}, 
    								AssociateTo[asc, "MakerNote" -> Quiet@ToString@val]]];
  
   							If[asc["LensSpecification"] =!= Missing["KeyAbsent", "LensSpecification"], 
   								With[{val = asc["LensSpecification"]}, 
    								AssociateTo[asc, "LensSpecification" -> Quiet[(StringSplit[StringReplace[StringReplace[ToString@InputForm[val], "Missing[Indeterminate]" -> "0/0"], "," | "{" | "}" -> " "]] /. 
         																	s_String /; StringMatchQ[s, DigitCharacter ..] :> s <> "/1" // StringRiffle)]]]];
  
  							asc
  ]

PrepareExifMetaFromProcess[assc_] := AssociationMap[ExifProcessToRaw, DeleteCases[Association@KeyValueMap[#1 -> DeleteCases[#2, _?(StringMatchQ[ToString@#, Whitespace ..] &)] &, assc], _?(# == <||> &)]]
PrepareExifMetaForExport[assc_]   := Module[{Exif = assc},Module[{or = If[Exif["Orientation"] =!= Missing["KeyAbsent", "Orientation"], 
    										Append[PrepareExifMetaFromProcess[Exif], <|"Orientation" -> GetOrientationNumber[PrepareExifMetaFromProcess[Exif]["Orientation"]]|>], 
    										PrepareExifMetaFromProcess[Exif]]},  If[Exif["FlashInfo"] =!= Missing["KeyAbsent", "FlashInfo"], 
  										    Append[or, "FlashInfo" -> GetFlashNumber[PrepareExifMetaFromProcess[Exif]["FlashInfo"]]], or]]
 ]
IntToString = 
 <|
  "GPSStatus"                -> <|"Measurement in progress" -> "A", "Measurement Interoperability" -> "V", "Measurement is Interoperability" -> "V", "Measurement is interoperability" -> "V"|>,
  "GPSMeasureMode"           -> <|"Three-dimensional measurement" -> "3", "Two-dimensional measurement" -> "2"|>,
  "GPSLongitudeRef"          -> <|"West" -> "W", "East" -> "E"|>,
  "GPSLatitudeRef"           -> <|"North" -> "N", "South" -> "S"|>,
  "GPSAltitudeRef"           -> <|"AboveSeaLevel" -> 0, "BelowSeaLevel" -> 1|>,
  "GPSTrackRef"              -> <|"Magnetic direction" -> "M", "True direction" -> "T"|>,
  "GPSImgDirectionRef"       -> <|"Magnetic direction" -> "M", "True direction" -> "T"|>,
  "Sharpness"                -> <|"Normal" -> 0, "Soft" -> 1, "Hard" -> 2|>,
  "Saturation"               -> <|"Normal" -> 0, "Low" -> 1, "Hard" -> 2|>,
  "Contrast"                 -> <|"Normal" -> 0, "Soft" -> 1, "Hard" -> 2|>,
  "GainControl"              -> <|"None" -> 0, "Low gain up" -> 1, "High gain up" -> 2, "Low gain down" -> 3, "High gain down" -> 4|>,
  "SceneCaptureType"         -> <|"Standard" -> 0, "Landscape" -> 1, "Portrait" -> 2, "Night" -> 3|>,
  "WhiteBalance"             -> <|"Auto" -> 0, "Manual" -> 1|>,
  "ExposureMode"             -> <|"Auto" -> 0, "Manual" -> 1, "Auto Bracket"-> 2|>,
  "CustomRendered"           -> <|"Normal process" -> 0, "Custom process" -> 1|>,
  "SensingMethod"            -> <|"Not defined" -> 1, "Monochrome" -> 1, "One-chip color area" -> 2, "Two-chip color area" -> 3, "Three-chip color area" -> 4, "Color sequential area" -> 5, "Monochrome linear" -> 6,  "Trilinear" -> 7, "Color sequential linear" -> 8|>,
  "FocalPlaneResolutionUnit" -> <|"None" -> 1, "inch" -> 2, "cm" -> 3, "mm" -> 4, "um" -> 5|>,
  "ColorSpace"               -> <|"RGBColor" -> 1, "Uncalibrated" -> 65535|>,
  "LightSource"              -> <|"Unknown" -> 0, "Daylight Fluorescent" -> 12, "D55" -> 20, "Daylight" -> 1, "Day White Fluorescent" -> 13, "D65" -> 21, "Fluorescent" -> 2 , "Cool White Fluorescent" -> 14 , "D75" -> 22, "Tungsten (Incandescent)" -> 3, 
    							  "White Fluorescent" -> 15, "D50" -> 23, "Flash" -> 4, "Warm White Fluorescent" -> 16, "ISO Studio Tungsten" -> 24, "Fine Weather" -> 9 , "Standard Light A" -> 17,
    				    		  "Cloudy" -> 10, "Standard Light B" -> 18, "Shade" -> 11, "Standard Light C" -> 19|>,
  "MeteringMode"             -> <|"Unknown" -> 0, "Average" -> 1, "Center weighted average" -> 2, "Spot" -> 3, "Multi-spot" -> 4, "Multi-segment" -> 5, "Partial" -> 6|>,
  "PreviewColorSpace"        -> <|"Unknown" -> 0, "Gray Gamma 2.2" -> 1, "sRGB" -> 2, "Adobe RGB" -> 3, "ProPhoto RGB" -> 4|>, 
  "ExposureProgram"          -> <|"Not defined" -> 0, "Auto" -> 2, "Manual" -> 1, "Normal program" -> 2, "Aperture priority" -> 3, "Shutter priority" -> 4, "Creative program" -> 5, "Action program" -> 6, "Portrait mode" -> 7, "Landscape mode" -> 8, "Bulb" -> 9|>,
  "YCbCrPositioning"         -> <|"Centered" -> 1, "Cosited" -> 2, "Co-sited" -> 2|>,
  "OPIProxy"                 -> <|"Higher resolution image does not exist" -> 0, "Higher resolution image exists" -> 1 |>,
  "Indexed"                  -> <|"Not indexed" -> 0, "Indexed" -> 1|>,
  "SampleFormat"             -> <|"Unsigned" -> 1, " Unsigned integer data " -> 1, "Signed" -> 2, " Signed integer data " -> 2, "Float" -> 3, "Undefined" -> 4, "Complex int" -> 5, "Complex float" -> 6|>,
  "ExtraSamples"             -> <|"Unspecified" -> 0, "Associated Alpha" -> 1, "Unassociated Alpha" -> 2|>, "InkSet" -> <|"CMYK" -> 1, "Not CMYK" -> 2|>,
  "ResolutionUnit"           -> <|"None" -> 1, "inch" -> 2, "cm" -> 3|>,
  "GrayResponseUnit"         -> <|"0.1" -> 1, "0.001" -> 2, "0.0001" -> 3, "1e-05" -> 4, "1e-06" -> 5|>,"PlanarConfiguration" -> <|"Chunky" -> 1, "Planar" -> 2|>,
  "FillOrder"                -> <|"Normal" -> 1, "Reserved" -> 2|>,
  "Thresholding"             -> <|"No dithering or halftoning" -> 1, "Ordered dither or halftone" -> 2, "Randomized dither" -> 3|>,
  "PhotometricInterpretation"-> <|"WhiteIsZero" -> 0, "BlackIsZero" -> 1, "RGB" -> 2, "RGB Palette" -> 3, "Transparency Mask" -> 4, "CMYK" -> 5, "YCbCr" -> 6, "CIELab" -> 8 ,
   								  "ICCLab" -> 9, "ITULab" -> 10, "Color Filter Array" -> 32803, "Pixar LogL" -> 32844, "Pixar LogLuv" -> 32845, "Linear Raw" -> 34892|>,
  "Compression"              -> <|"Uncompressed" -> 1, "CCITT modified Huffman RLE" -> 2, "PackBits compression, aka Macintosh RLE" -> 32773, "CCITT Group 3 fax encoding" -> 3, 
  								  "CITT Group 4 fax encoding" -> 4, "LZW" -> 5, "JPEG (new-style)" -> 7, "JPEG (old-style)" -> 6, "Deflate" -> 7, "Defined by TIFF-F and TIFF-FX standard (RFC 2301) as ITU-T Rec. T.82 coding, using ITU-T Rec. T.85" -> 9, 
  								  "Defined by TIFF-F and TIFF-FX standard (RFC 2301) as ITU-T Rec. T.82 coding, using ITU-T Rec. T.43" -> 10|>,
  "SubfileType"              -> <|"Full-resolution image data" -> 0, "Reduced-resolution image" -> 1, "Single page of multi-page image" -> 2|>,
  "SubjectDistanceRange"     -> <|"Unknown" -> 0, "Macro" -> 1, "Close view" -> 2, "Distance view" -> 3|>,
  "GPSDifferential"          -> <|"Without correction" -> 0, "Correction applied"-> 1|>,
  "FileSource"               -> <|"Film scanner" -> 1, "Reflexion print scanner" -> 2, "Digital still camera"-> 3, _ -> "None"|>,
  "SceneType"                -> <|"Directly photographed" -> "1", _ -> "None"|>
  |> 
 
ParseIntToString[tag_, value_] := If[IntToString[tag, value] =!= Missing["KeyAbsent", value], IntToString[tag, value], -1] 

ModifyIntValuesForExport[assc_]:= Module[{ass =  Quiet[PrepareExifMetaForExport[assc]] },
 										If[ass["ComponentsConfiguration"] =!= Missing["KeyAbsent", "ComponentsConfiguration"],
 										Module[{cc = StringReplace[ass["ComponentsConfiguration"], {"Y" -> "1,", "Cb" -> "2,", "Cr" -> "3,", "R" -> "4,", "G" -> "5,", "B" -> "6,"}]},
  											 Which[
   												 Count[StringSplit[cc, ","], _String] === 3, cc = StringJoin[cc, "0"],
    											 Count[StringSplit[cc, ","], _String] === 2, cc = StringJoin[cc, "0,0"],
    											 Count[StringSplit[cc, ","], _String] === 1, cc = StringJoin[cc, "0,0,0"],
    											 True, cc
    										  ];
   											ass["ComponentsConfiguration"] = StringReplace[cc, "," -> " "];
   										]];
 										Association@(Normal@ass /. (key_ /; (!SameQ[ToString@key, "FocalLengthIn35mmFilm"] && !SameQ[ToString@key, "GPSVersionID"] && (SameQ[ToString@key, "SceneType"] ||MemberQ[ExportExifGPSDualValues, ToString@key] || MemberQ[ExportExifGPSInt, ToString@key] || MemberQ[ExportExifPhotoInt, ToString@key] || MemberQ[ExportExifImageInt, ToString@key] && !MemberQ[MultiValues, ToString@key] && !SameQ[key, "CFAPattern"] || SameQ[ToString@key, "FileSource"])) ->val_) :> (key -> If[StringQ@val, ParseIntToString[key, val], val]))
 ]

WriteExif[tag_, val_] := Which[
  							 SameQ["ReferenceBlackWhite", ToString@tag]	,Quiet@$WriteExifString[tag, val],										      
 	                         SameQ["BitsPerSample", tag],      Quiet@$WriteExifString[tag, StringDelete[val, ","]],
 	                         SameQ["SubjectArea", tag],        Quiet@$WriteExifString[tag, val],
 	                         SameQ["MakerNote", tag]  ,        Quiet@$WriteExifString[tag, val],
 	                         SameQ["LensSpecification", tag]||
 	                         StringContainsQ[tag, "Version"]        , Quiet@$WriteExifString[tag, val],  
 	                         MemberQ[MultiValues, tag]              , Quiet@$WriteExifString[tag, Module[{res = val}, If[! ListQ@res, StringTrim@StringJoin[" " <> ToString[#, InputForm] & /@ (List@res)], 
  																							StringTrim@StringJoin[" " <> ToString[#, InputForm] & /@ res]]]],
 	                         
 	                         MemberQ[ExportExifGPSString, tag]   ||
 	                         MemberQ[ExportExifPhotoString, tag] ||
 	                         MemberQ[ExportExifImageString, tag] ||
 	                         MemberQ[ExportExifIopString, tag]      , Which[
 	                         	                           SameQ[tag, "CFAPattern"]              , If[StringContainsQ[val, " "] && StringFreeQ[val, LetterCharacter ..], Quiet@$WriteExifString[tag, ToString@val], _ ],
 	                         	                           SameQ[tag, "ComponentsConfiguration"] , If[StringContainsQ[val, " "] && StringFreeQ[val, LetterCharacter ..], $WriteExifString[tag, ToString@val], _ ],
 	                         	                           SameQ[tag, "OECF"]                    , If[StringContainsQ[val, " "] && StringFreeQ[val, LetterCharacter ..], $WriteExifString[tag, ToString@val], _ ],
 	                         	                           SameQ[tag, "SceneType"]               , Quiet@$WriteExifString[tag,  If[SameQ[val, "None"], "0", val]],
 	                         	                           SameQ[tag, "FileSource"]              , If[NumberQ[ToExpression@val], Quiet@$WriteExifString[tag, ToString@val], _ ],
 	                         	                           SameQ[tag, "SpatialFrequencyResponse"], If[NumberQ[ToExpression@val], Quiet@$WriteExifString[tag, ToString@val], _ ],
 	                         	                           SameQ[tag, "DeviceSettingDescription"], If[NumberQ[ToExpression@val], Quiet@$WriteExifString[tag, ToString@val], _ ],
 	                         	                           SameQ[tag, "RelatedSoundFile"]        , If[NumberQ[ToExpression@val], Quiet@$WriteExifString[tag, ToString@val], _ ],
 	                         	                           SameQ[tag, "GPSTimeStamp"]            , Quiet@$WriteExifString[tag, val],
 	                         	                           True, Quiet@$WriteExifString[tag, ToString@val]
 	                         	                          ],
 	                         MemberQ[ExportExifGPSInt, tag]    ||
 	                         MemberQ[ExportExifPhotoInt, tag]  ||
 	                         MemberQ[ExportExifImageInt, tag]  ||
 	                         MemberQ[ExportExifIopNumber, tag]      , Quiet@If[!NumberQ[ToExpression@val], 0 , Quiet@$WriteExifInt[tag, ToExpression@val]],
 	                         
 	                         MemberQ[ExportExifGPSRat, tag]  ||
 	                         MemberQ[ExportExifPhotoRat, tag]  ||
 	                         MemberQ[ExportExifImageReal, tag] ||
 	                         MemberQ[ExportExifImageRat, tag] &&
 	                         !SameQ[tag, "ExposureBiasValue"] &&
 	                         !SameQ[tag, "BaselineExposure"] , Quiet@$WriteExifReal[tag, If[StringQ@val,(ToExpression@val)//N, val//N]],
 	                         
 	                         True, _     
 ]

WriteExifRule[listOfRules : {__Rule}] := WriteExif @@@ listOfRules
WriteExifAssociation[list_Association]:= WriteExif @@@ Normal[list]

(**********)
(**PHOTO***)
(**********)
ExportExifPhotoInt = { "ExposureProgram", "SubjectDistanceRange", "Sharpness", "Saturation", "Contrast", "GainControl", "SceneCaptureType", "FocalLengthIn35mmFilm",
	"WhiteBalance", "ExposureMode", "CustomRendered", "SensingMethod", "SubjectLocation", "FocalPlaneResolutionUnit", "InteroperabilityTag", "PixelYDimension", 
	"PixelXDimension", "ColorSpace", "FlashInfo", "LightSource", "MeteringMode", "ISOSpeedRatings"
};

ExportExifPhotoRat = { "DigitalZoomRatio", "ExposureIndex", "FocalPlaneYResolution", "FocalPlaneXResolution", "FlashEnergy", "FocalLength", 
	"SubjectDistance", "MaxApertureValue", "ExposureBiasValue", "BrightnessValue", "ApertureValue", "ShutterSpeedValue", "CompressedBitsPerPixel",
	"ISOSpeedLatitudezzz", "ISOSpeedLatitudeyyy", "ISOSpeed", "RecommendedExposureIndex", "StandardOutputSensitivity", "SensitivityType", 
	"FNumber", "ExposureTime", 	"FNumber", "ExposureTime"
};

ExportExifPhotoString = { "LensSerialNumber", "LensModel", "LensMake", "BodySerialNumber", "CameraOwnerName", "ImageUniqueID", "DeviceSettingDescription", "CFAPattern",
	"SceneType", "FileSource", "SpatialFrequencyResponse", "RelatedSoundFile", "FlashpixVersion", "SubSecTimeDigitized", "SubSecTimeOriginal", "SubSecTime", "UserComment",
	"ComponentsConfiguration", "DateTimeDigitized", "DateTimeOriginal", "ExifVersion", "OECF", "SpectralSensitivity",
	
	"SubjectArea", "LensSpecification"
};

(**********)
(****IOP***)
(**********)
ExportExifIopNumber = { "RelatedImageLength", "RelatedImageWidth"
};

ExportExifIopString = { "RelatedImageFileFormat", "InteroperabilityVersion", "InteroperabilityIndex"
};

(**********)
(**Image***)
(**********)
ExportExifImageInt = { "ProfileLookTableDims", "RowInterleaveFactor", "SubTileBlockSize", "PreviewColorSpace", "PreviewSettingsDigest", "PreviewSettingsName", "PreviewApplicationVersion",
	"PreviewApplicationName", "ProfileCopyright", "ProfileEmbedPolicy", "ProfileToneCurve", "ProfileHueSatMapDims", "ProfileName", "AsShotProfileName", "ProfileCalibrationSignature",
	"CameraCalibrationSignature", "ColorimetricReference", "MaskedAreas", "ActiveArea", "OriginalRawFileName", "RawDataUniqueID", "CalibrationIlluminant2", "CalibrationIlluminant1",
	"MakerNoteSafety", "DNGPrivateData", "BayerGreenSplit", "AsShotNeutral", "DefaultCropSize", "DefaultCropOrigin", "WhiteLevel", "BlackLevelRepeatDim", "LinearizationTable",
	"CFALayout", "CFAPlaneColor", "LocalizedCameraModel", "DNGBackwardVersion", "DNGVersion", "XPSubject", "XPKeywords", "XPAuthor", "XPComment", "XPTitle", 
	"TIFFEPStandardID", "ImageNumber", "SelfTimerMode", "TimeZoneOffset", "Interlace",
	"GPSTag", "ExifTag", "ImageResources", "IPTCNAA", "CFAPattern", "CFARepeatPatternDim", "RatingPercent", "Rating", "XMLPacket", "YCbCrPositioning",
	"YCbCrCoefficients", "JPEGACTables", "JPEGDCTables", "JPEGQTables", "JPEGPointTransforms", "JPEGLosslessPredictors", "JPEGRestartInterval", "JPEGInterchangeFormatLength",
	"JPEGInterchangeFormat", "JPEGProc", "OPIProxy", "Indexed", "YClipPathUnits", "XClipPathUnits", "ClipPath", "TransferRange", "SMaxSampleValue", "SMinSampleValue", "SampleFormat",
	"ExtraSamples", "DotRange", "NumberOfInks", "InkSet", "SubIFDs", "TileByteCounts", "TileOffsets", "TileLength", "TileWidth", "HalftoneHints", "ColorMap", "Predictor",
	"TransferFunction", "PageNumber", "ResolutionUnit", "T6Options", "T4Options", "GrayResponseCurve", "GrayResponseUnit", "PlanarConfiguration", "StripByteCounts", "RowsPerStrip",
	"SamplesPerPixel", "Orientation", "StripOffsets", "FillOrder", "CellLength", "CellWidth", "Thresholding", "PhotometricInterpretation", "Compression", "BitsPerSample",
	"ImageLength", "ImageWidth", "SubfileType", "NewSubfileType"
};

ExportExifImageReal = { "NoiseProfile", "ProfileLookTableData", "ProfileHueSatMapData2", "ProfileHueSatMapData1"
};

ExportExifImageRat = { "ForwardMatrix2", "ForwardMatrix1", "NoiseReductionApplied", "CurrentPreProfileMatrix", "AsShotPreProfileMatrix", "BestQualityScale", "ShadowScale",
	"AntiAliasStrength", "ChromaBlurRadius", "LinearResponseLimit", "BaselineSharpness", "BaselineNoise", "BaselineExposure", "AsShotWhiteXY", "AnalogBalance", "ReductionMatrix2",
	"ReductionMatrix1", "CameraCalibration2", "CameraCalibration1", "ColorMatrix2", "ColorMatrix1", "DefaultScale", "BlackLevelDeltaV", "BlackLevelDeltaH", "BlackLevel",
	"FocalLength", "BatteryLevel", "ReferenceBlackWhite", "PrimaryChromaticities", "WhitePoint", "YResolution", "XResolution"
};

ExportExifImageString = { "OpcodeList3", "OpcodeList2", "OpcodeList1", "OriginalRawFileDigest", "RawImageDigest", "PreviewDateTime", "CurrentICCProfile", "AsShotICCProfile",
	"OriginalRawFileData", "CameraSerialNumber", "UniqueCameraModel", "PrintImageMatching", "ImageHistory", "SecurityClassification", "Noise",
    "InterColorProfile", "Copyright", "ImageID", "JPEGTables", "TargetPrinter", "InkNames", "HostComputer", "Artist",
	"DateTime", "Software", "Model", "Make", "ImageDescription", "DocumentName"
};

(**********)
(****GPS***)
(**********)
ExportExifGPSInt = {"GPSVersionID", "GPSAltitudeRef", "GPSDifferential"
};

ExportExifGPSRat = { "GPSDOP", "GPSSpeed", "GPSTrack", "GPSImgDirection", "GPSDestLatitude"	, "GPSLatitude", "GPSLongitude", "GPSAltitude"
};

ExportExifGPSString = {"GPSDestLongitude", "GPSDestBearing", "GPSDestDistance", "GPSTimeStamp", "GPSLatitudeRef", "GPSLongitudeRef", "GPSSatellites", "GPSStatus", "GPSMeasureMode", "GPSSpeedRef", "GPSTrackRef", "GPSImgDirectionRef",
	"GPSMapDatum", "GPSDestLatitudeRef", "GPSDestLongitudeRef", "GPSDestBearingRef", "GPSDestDistanceRef", "GPSProcessingMethod", "GPSAreaInformation", "GPSDateStamp"
};

ExportExifGPSDualValues = {"GPSStatus", "GPSMeasureMode", "GPSLongitudeRef", "GPSLatitudeRef", "GPSAltitudeRef", "GPSTrackRef", "GPSImgDirectionRef"
};


(**************************)
(**************************)
(**************************)
(**********EXPORT**********)
(***********IPTC***********)
(**************************)
(**************************)
IPTCProcessToRaw[Rule[key_, assoc_Association]] := Rule[key, AssociationMap[IPTCProcessToRaw, assoc]]
IPTCProcessToRaw[Rule[key_, val_]] := Which[
											MemberQ[DateTags, key], Rule[key, StringReplace[StringReplace[StringTake[ToString@(Normal @@ val), 
												                                               {2, StringLength@ToString@(Normal @@ val) - 1}], ", " -> "-"], "." -> ""]],
  
  											MemberQ[TimeTags, key], Rule[key, StringJoin[StringReplace[StringReplace[StringTake[ToString@(Normal @@ val),
  																							   {2, StringLength@ToString@(Normal @@ val) - 1}], ", " -> ":"], "." -> ""], "+00:00"]],
  																							   
                                            SameQ[key, "CharacterSet"], Rule[key, If[SameQ[val, "UTF8"], "\[RawEscape]%G", val]], 
  
  											True, Rule[key, Normal @@ val]
  									]
PrepareIPTCMetaFromProcess[assc_] := AssociationMap[IPTCProcessToRaw, DeleteCases[Association@KeyValueMap[#1 -> DeleteCases[#2, _?(StringMatchQ[ToString@#, Whitespace ..] &)] &, assc], _?(# == <||> &)]]

PrepareIPTCMeta[]:= Quiet@DeleteCases[ToExpression@StringReplace[$ReadIPTCAll[], WordCharacter .. ~~ " -> ," -> ""], Rule[_, _Missing], Infinity]/.
					(key_ /; (MemberQ[ExportApplication2Number, key] || MemberQ[ExportEnvelopeNumber, key]) -> val_) :> (key -> If[ListQ@val, val, ToExpression@val])

WriteIPTC[tag_, val_] := Which[
 	                         MemberQ[ExportApplication2Number, tag]  ||
 	                         MemberQ[ExportEnvelopeNumber, tag]      , Quiet@$WriteIPTCInt[tag, val],
 	                         
 	                         MemberQ[ExportApplication2String, tag]  ||
 	                         MemberQ[ExportEnvelopeString, tag]      , If[StringContainsQ[tag, "Date"] && ToString@val === "-1", val =  "0-00-00"];Quiet@$WriteIPTCString[tag, val],
 	                         
 	                         True, _     
 ]


WriteIPTCRule[listOfRules : {__Rule}] := WriteIPTC @@@ listOfRules
WriteIPTCAssociation[list_Association]:= WriteIPTC @@@ Normal[list]

(**********)
(*Envelope*)
(**********)
ExportEnvelopeNumber = {"ModelVersion", "FileVersion", "FileFormat", "ARMVersion", "ARMId"
};

ExportEnvelopeString = { "UNO", "TimeSent", "ServiceId", "ProductId", "EnvelopePriority", "EnvelopeNumber", "Destination", "DateSent", "CharacterSet"
};

(**************)
(*Application2*)
(**************)
ExportApplication2Number = { "PreviewFormat", "PreviewVersion", "RecordVersion"
};

ExportApplication2String = { "ActionAdvised", "AudioDuration", "AudioOutcue", "AudioRate", "AudioResolution", "AudioType", "Byline", "BylineTitle", "Caption",
	"Category", "City", "Contact", "Copyright", "CountryCode", "CountryName", "Credit", "DateCreated", "DigitizationDate", "DigitizationTime", "EditStatus",
	"EditorialUpdate", "ExpirationDate", "ExpirationTime", "FixtureId", "Headline", "ImageOrientation", "ImageType", "Keywords", "Language", "LocationCode",
	"LocationName", "ObjectAttribute", "ObjectCycle", "ObjectName", "ObjectType", "Preview", "Program", "ProgramVersion", "ProvinceState", "RasterizedCaption",
	"ReferenceDate", "ReferenceNumber", "ReferenceService", "ReleaseDate", "ReleaseTime", "Source", "SpecialInstructions", "SubLocation", "Subject", "SuppCategory",
	"TimeCreated", "TransmissionReference", "Urgency", "Writer"
};

(**************************)
(**************************)
(**************************)
(**********EXPORT**********)
(************XMP***********)
(**************************)
(**************************)
XMPProcessToRaw[Rule[key_, assoc_Association]] := Rule[key, AssociationMap[XMPProcessToRaw, assoc]]
XMPProcessToRaw[Rule[key_, val_]] := Which[
	                                     MemberQ[DateTags, key], 
											                 If[Quiet@DateObjectQ[val] && !Quiet@TimeObjectQ[val[[2]]],
											                 	Rule[key, StringReplace[ToString[Normal @@ val], {"{" | "}" -> "", ", " -> "-"}]]
											                 	,
											                    If[Quiet@DateObjectQ[val] && Quiet@TimeObjectQ[val[[2]]], 
											                    	  Rule[key, 
											                    	     Module[{tmp = StringSplit[StringReplace[StringReplace[StringTake[StringReplace[StringJoin[ToString[val[[1]]], 
											                                ToString[Normal @@ val[[2]]]], "}{" -> "  "], {2, StringLength@ StringReplace[StringJoin[ToString[val[[1]]], 
											               			        ToString[Normal @@ val[[2]]]], "}{" -> ", "] - 1}], ", " -> ":"], "." -> ""], "  "]}, 
											    					        tmp[[1]] = StringReplace[tmp[[1]], ":" -> "-"]; StringJoin[tmp[[1]], " ", tmp[[2]]]]
											                          ]
											    					  , 
											    					  Rule[key, "1900-01-01T00:00:00-00:00"]
											    				]],
										     					  
											  
										 True                  ,If[StringContainsQ[ToString@val, "Missing"], Rule[key, 0], Rule[key, If[ListQ@val, StringTake[ToString[val], {2, StringLength[ToString[val]] - 1}], Normal @@ val]]]];
PrepareXMPMetaFromProcessWithoutDomains[res_] := AssociationMap[XMPProcessToRaw, DeleteCases[Association@KeyValueMap[#1 -> DeleteCases[#2, _?(StringMatchQ[ToString@#, Whitespace ..] &)] &, res], _?(# == <||> &)]]

constractDomainList[domain_, ass_Association] := Module[{dom = domain},
  													XMPDomainName[Rule[key_, assoc_Association]] := Rule[key, AssociationMap[XMPDomainName, assoc] ];
      												XMPDomainName[Rule[key_, val_] ] := Rule[StringJoin["Xmp.", dom, ".", key], val];
  													ConstructXMPDomain[res_] := AssociationMap[XMPDomainName, DeleteCases[Association@KeyValueMap[#1 -> DeleteCases[#2, _?(StringMatchQ[ToString@#, Whitespace ..] &)] &, res], _?(# == <||> &)]];
  													ConstructXMPDomain[ass]
]

flattenStructuralTag[tag_, code_, ass_Association] := Module[{tagname = tag, tagCode = code},
	 												StructureName[Rule[key_, assoc_Association]] := Rule[key, AssociationMap[StructureName, assoc]];
  													StructureName[Rule[key_, val_]] := Rule[StringJoin[StringJoin[StringSplit[key, "."][[1]], "." , StringSplit[key, "."][[2]], "."], tagname, tagCode, StringSplit[key, "."][[3]]], val];
  													ConstructStructure[res_] := AssociationMap[StructureName, DeleteCases[Association@KeyValueMap[#1 -> DeleteCases[#2, _?(StringMatchQ[ToString@#, Whitespace ..] &)] &, res], _?(# == <||> &)]];
  													
  													ConstructStructure[ass]]

(*
DerivedFrom - ["DerivedFrom", "DerivedFrom"], /stRef:
History - History[1]...[4] /stEvt:
RenditionOf - RenditionOf[1]...[4] /stEvt:
ManagedFrom - ManagedFrom[1]...[4] /stEvt:
JobRef / JobRef[1]...[4] /stJob:
*)

concat[assc_] := Module[{WRI = <||>, xmpMM = <||>, dc = <||>, xmp = <||>, digiKam = <||>, crs = <||>, MicrosoftPhoto = <||>, photoshop = <||>, xmpRights = <||>, xmpBJ = <||>, xmpTPg = <||>, pdf = <||>, 
   																																												final = <||>},
  					 	  If[assc["MediaManagementSchema"] =!= Missing["KeyAbsent", "MediaManagementSchema"], xmpMM = assc["MediaManagementSchema"]];
  						  If[assc["AdobePDFSchema"] =!= Missing["KeyAbsent", "AdobePDFSchema"], pdf = assc["AdobePDFSchema"]];
						  If[assc["DublinCoreSchema"] =!= Missing["KeyAbsent", "DublinCoreSchema"], dc = assc["DublinCoreSchema"]];
						  If[assc["BasicSchema"] =!= Missing["KeyAbsent", "BasicSchema"], xmp = assc["BasicSchema"]];
						  If[assc["PhotoManagementSchema"] =!= Missing["KeyAbsent", "PhotoManagementSchema"], digiKam = assc["PhotoManagementSchema"]];
						  If[assc["MicrosoftPhotoSchema"] =!= Missing["KeyAbsent", "MicrosoftPhotoSchema"], MicrosoftPhoto = assc["MicrosoftPhotoSchema"]];
						  If[assc["CameraRawSchema"] =!= Missing["KeyAbsent", "CameraRawSchema"],crs = assc["CameraRawSchema"]];
						  If[assc["PhotoshopSchema"] =!= Missing["KeyAbsent", "PhotoshopSchema"], photoshop = assc["PhotoshopSchema"]];
						  If[assc["RightsManagementSchema"] =!= Missing["KeyAbsent", "RightsManagementSchema"], xmpRights = assc["RightsManagementSchema"]];
						  If[assc["BasicJobTicketSchema"] =!= Missing["KeyAbsent", "BasicJobTicketSchema"], xmpBJ = assc["BasicJobTicketSchema"]];
						  If[assc["PagedTextSchema"] =!= Missing["KeyAbsent", "PagedTextSchema"], xmpTPg = assc["PagedTextSchema"]];
						  If[assc["WolframSchema"] =!= Missing["KeyAbsent", "WolframSchema"], WRI = assc["WolframSchema"]];
  
						  If[xmpMM =!= <||>, xmpMM = constractDomainList["xmpMM", xmpMM]];
						  If[pdf =!= <||>, pdf = constractDomainList["pdf", pdf]];
						  If[dc =!= <||>, dc = constractDomainList["dc", dc]];
						  If[xmp =!= <||>, xmp = constractDomainList["xmp", xmp]];
						  If[digiKam =!= <||>, digiKam = constractDomainList["digiKam", digiKam]];
						  If[MicrosoftPhoto =!= <||>, MicrosoftPhoto = constractDomainList["MicrosoftPhoto", MicrosoftPhoto]];
						  If[crs =!= <||>, crs = constractDomainList["crs", crs]];
						  If[photoshop =!= <||>, photoshop = constractDomainList["photoshop", photoshop]];
						  If[xmpRights =!= <||>, xmpRights = constractDomainList["xmpRights", xmpRights]];
						  If[xmpBJ =!= <||>, xmpBJ = constractDomainList["xmpBJ", xmpBJ]];
						  If[xmpTPg =!= <||>, xmpTPg = constractDomainList["xmpTPg", xmpTPg]];
						  If[WRI =!= <||>, WRI = constractDomainList["Wolfram", WRI]];
  
						  If[xmpMM =!= <||>, final = Join[final, xmpMM]];
						  If[pdf =!= <||>, final = Join[final, pdf]];
						  If[dc =!= <||>, final = Join[final, dc]];
						  If[xmp =!= <||>, final = Join[final, xmp]];
						  If[digiKam =!= <||>, final = Join[final, digiKam]];
						  If[MicrosoftPhoto =!= <||>, final = Join[final, MicrosoftPhoto]];
						  If[crs =!= <||>, final = Join[final, crs]];
						  If[photoshop =!= <||>, final = Join[final, photoshop]];
						  If[xmpRights =!= <||>, final = Join[final, xmpRights]];
						  If[xmpBJ =!= <||>, final = Join[final, xmpBJ]];
						  If[xmpTPg =!= <||>, final = Join[final, xmpTPg]];
						  If[WRI =!= <||>, final = Join[final, WRI]];
  
  						  final
  ]

separateStructureTagsAndConstructBack[asc_]:= Module[{final = <||>, assc = asc, ver = <||>, his = <||>, der = <||>, rend = <||>, manag = <||>, job = <||>, h = <||>, d = <||>, 
  																														   v=<||>, r = <||>, m = <||>, j = <||>},
 													  final = assc;
 
 													  If[assc["History"] =!= Missing["KeyAbsent", "History"],
														  his = assc["History"];
														  final = KeyDrop[final, "History"];
  
  														  h = Module[{fin = <||>, his1 = <||>, his2 = <||>, his3 = <||>, his4 = <||>},
																	    
																	    If[his["History[1]"] =!= Missing["KeyAbsent", "History[1]"],
																	     his1 = flattenStructuralTag["History[1]", "/stEvt:", his["History[1]"]];
																	     PrependTo[his1, <|"Xmp.xmpMM.History[1]" -> "type= Seq"|>];
																	     ];
																	    
																	    If[his["History[2]"] =!= Missing["KeyAbsent", "History[2]"],
																	     his2 = flattenStructuralTag["History[2]", "/stEvt:", his["History[2]"]];
																	     PrependTo[his2, <|"Xmp.xmpMM.History[2]" -> "type= Seq"|>];
																	     ];
																	    
																	    If[his["History[3]"] =!= Missing["KeyAbsent", "History[3]"],
																	     his3 = flattenStructuralTag["History[3]", "/stEvt:", his["History[3]"]];
																	     PrependTo[his3, <|"Xmp.xmpMM.History[3]" -> "type= Seq"|>];
																	     ];
																	    
																	    If[his["History[4]"] =!= Missing["KeyAbsent", "History[4]"],
																	     his4 = flattenStructuralTag["History[4]", "/stEvt:", his["History[4]"]];
																	     PrependTo[his1, <|"Xmp.xmpMM.History[4]" -> "type= Seq"|>];
																	     ];
    
																	    If[his1 =!= <||>, fin = Join[fin, his1]];
																	    If[his2 =!= <||>, fin = Join[fin, his2]];
																	    If[his3 =!= <||>, fin = Join[fin, his3]];
																	    If[his4 =!= <||>, fin = Join[fin, his4]];
																	    fin
    																];
															  		If[h =!= <||>, PrependTo[h, <|"Xmp.xmpMM.History" -> "type= Seq"|>]];
															  		AppendTo[final, h];
															  ];
 
 													If[assc["Versions"] =!= Missing["KeyAbsent", "Versions"],
														  ver = assc["Versions"];
														  final = KeyDrop[final, "Versions"];
  
  														  v = Module[{fin = <||>, ver1 = <||>, ver2 = <||>, ver3 = <||>, ver4 = <||>},
																	    
																	    If[ver["Versions[1]"] =!= Missing["KeyAbsent", "Versions[1]"],
																	     ver1 = flattenStructuralTag["Versions[1]", "/stEvt:", ver["Versions[1]"]];
																	     PrependTo[ver1, <|"Xmp.xmpMM.Versions[1]" -> "type= Seq"|>];
																	     ];
																	    
																	    If[ver["Versions[2]"] =!= Missing["KeyAbsent", "Versions[2]"],
																	     ver2 = flattenStructuralTag["Versions[2]", "/stEvt:", ver["Versions[2]"]];
																	     PrependTo[ver2, <|"Xmp.xmpMM.Versions[2]" -> "type= Seq"|>];
																	     ];
																	    
																	    If[ver["Versions[3]"] =!= Missing["KeyAbsent", "Versions[3]"],
																	     ver3 = flattenStructuralTag["Versions[3]", "/stEvt:", ver["Versions[3]"]];
																	     PrependTo[ver3, <|"Xmp.xmpMM.Versions[3]" -> "type= Seq"|>];
																	     ];
																	    
																	    If[ver["Versions[4]"] =!= Missing["KeyAbsent", "Versions[4]"],
																	     ver4 = flattenStructuralTag["Versions[4]", "/stEvt:", ver["Versions[4]"]];
																	     PrependTo[ver1, <|"Xmp.xmpMM.Versions[4]" -> "type= Seq"|>];
																	     ];
    
																	    If[ver1 =!= <||>, fin = Join[fin, ver1]];
																	    If[ver2 =!= <||>, fin = Join[fin, ver2]];
																	    If[ver3 =!= <||>, fin = Join[fin, ver3]];
																	    If[ver4 =!= <||>, fin = Join[fin, ver4]];
																	    fin
    																];
															  		If[v =!= <||>, PrependTo[v, <|"Xmp.xmpMM.Versions" -> "type= Seq"|>]];
															  		AppendTo[final, v];
															  ];
 
 													  If[assc["DerivedFrom"] =!= Missing["KeyAbsent", "DerivedFrom"],
														  der = assc["DerivedFrom"];
														  final = KeyDrop[final, "DerivedFrom"];
														  
  														  d = Module[{fin = <||>, der0 = <||>,der1 = <||>, der2 = <||>, der3 = <||>, der4 = <||>},
																
																If[der["DerivedFrom"] =!= Missing["KeyAbsent", "DerivedFrom"],
															     der0 = flattenStructuralTag["DerivedFrom", "/stRef:", der["DerivedFrom"]];
															     PrependTo[der0, <|"Xmp.xmpMM.DerivedFrom" -> " type= Struct"|>];
															     ];
																	    
															    If[der["DerivedFrom[1]"] =!= Missing["KeyAbsent", "DerivedFrom[1]"],
															     der1 = flattenStructuralTag["DerivedFrom[1]", "/stRef:", der["DerivedFrom[1]"]];
															     PrependTo[der1, <|"Xmp.xmpMM.DerivedFrom[1]" -> " type= Struct"|>];
															     ];
															    
															    If[der["DerivedFrom[2]"] =!= Missing["KeyAbsent", "DerivedFrom[2]"],
															     der2 = flattenStructuralTag["DerivedFrom[2]", "/stRef:", der["DerivedFrom[2]"]];
															     PrependTo[der2, <|"Xmp.xmpMM.DerivedFrom[2]" -> " type= Struct"|>];
															     ];
															    
															    If[der["DerivedFrom[3]"] =!= Missing["KeyAbsent", "DerivedFrom[3]"],
															     der3 = flattenStructuralTag["DerivedFrom[3]", "/stRef:", der["dertory[3]"]];
															     PrependTo[der3, <|"Xmp.xmpMM.DerivedFrom[3]" -> " type= Struct"|>];
															     ];
															    
															    If[der["DerivedFrom[4]"] =!= Missing["KeyAbsent", "DerivedFrom[4]"],
															     der4 = flattenStructuralTag["DerivedFrom[4]", "/stRef:", der["DerivedFrom[4]"]];
															     PrependTo[der1, <|"Xmp.xmpMM.DerivedFrom[4]" -> " type= Struct"|>];
															     ];
																
																If[der0 =!= <||>, fin = Join[fin, der0]];
															    If[der1 =!= <||>, fin = Join[fin, der1]];
															    If[der2 =!= <||>, fin = Join[fin, der2]];
															    If[der3 =!= <||>, fin = Join[fin, der3]];
															    If[der4 =!= <||>, fin = Join[fin, der4]];
															    fin
															];
															
													  	  If[d =!= <||>, PrependTo[d, <|"Xmp.xmpMM.DerivedFrom" -> " type= Struct"|>]];
													  	  AppendTo[final, d];
													  ];
 
 													  If[assc["RenditionOf"] =!= Missing["KeyAbsent", "RenditionOf"],
														  rend = assc["RenditionOf"];
														  final = KeyDrop[final, "RenditionOf"];
  
  														  r = Module[{fin = <||>, rend1 = <||>, rend2 = <||>, rend3 = <||>, rend4 = <||>},
  														  	
															    If[rend["RenditionOf[1]"] =!= Missing["KeyAbsent", "RenditionOf[1]"],
															     rend1 = flattenStructuralTag["RenditionOf[1]", "/stEvt:", rend["RenditionOf[1]"]];
															     PrependTo[rend1, <|"Xmp.xmpMM.RenditionOf[1]" -> " type= Struct"|>];
															     ];
															    
															    If[rend["RenditionOf[2]"] =!= Missing["KeyAbsent", "RenditionOf[2]"],
															     rend2 = flattenStructuralTag["RenditionOf[2]", "/stEvt:", rend["RenditionOf[2]"]];
															     PrependTo[rend2, <|"Xmp.xmpMM.RenditionOf[2]" -> " type= Struct"|>];
															     ];
															    
															    If[rend["RenditionOf[3]"] =!= Missing["KeyAbsent", "RenditionOf[3]"],
															     rend3 = flattenStructuralTag["RenditionOf[3]", "/stEvt:", rend["RenditionOf[3]"]];
															     PrependTo[rend3, <|"Xmp.xmpMM.RenditionOf[3]" -> " type= Struct"|>];
															     ];
															    
															    If[rend["RenditionOf[4]"] =!= Missing["KeyAbsent", "RenditionOf[4]"],
															     rend4 = flattenStructuralTag["RenditionOf[4]", "/stEvt:", rend["RenditionOf[4]"]];
															     PrependTo[rend4, <|"Xmp.xmpMM.RenditionOf[4]" -> " type= Struct"|>];
															     ];
															    
															    If[rend1 =!= <||>, fin = Join[fin, rend1]];
															    If[rend2 =!= <||>, fin = Join[fin, rend2]];
															    If[rend3 =!= <||>, fin = Join[fin, rend3]];
															    If[rend4 =!= <||>, fin = Join[fin, rend4]];
															    fin
															 ];
  															If[r =!= <||>, PrependTo[r, <|"Xmp.xmpMM.RenditionOf" -> " type= Seq"|>]];
  															AppendTo[final, r];
  														];
 
 														If[assc["ManagedFrom"] =!= Missing["KeyAbsent", "ManagedFrom"],
															  manag = assc["ManagedFrom"];
															  final = KeyDrop[final, "ManagedFrom"];
  
 															  m = Module[{fin = <||>, manag1 = <||>, manag2 = <||>, manag3 = <||>, manag4 = <||>},
																	 
																	    If[manag["ManagedFrom[1]"] =!= Missing["KeyAbsent", "ManagedFrom[1]"],
																	       manag1 = flattenStructuralTag["ManagedFrom[1]", "/stEvt:", manag["ManagedFrom[1]"]];
																	       PrependTo[manag1, <|"Xmp.xmpMM.ManagedFrom[1]" -> " type= Struct"|>];
																	     ];
																	    
																	    If[manag["ManagedFrom[2]"] =!= Missing["KeyAbsent", "ManagedFrom[2]"],
																	       manag2 = flattenStructuralTag["ManagedFrom[2]", "/stEvt:", manag["ManagedFrom[2]"]];
																	       PrependTo[manag2, <|"Xmp.xmpMM.ManagedFrom[2]" -> " type= Struct"|>];
																	     ];
																	    
																	    If[manag["ManagedFrom[3]"] =!= Missing["KeyAbsent", "ManagedFrom[3]"],
																	       manag3 = flattenStructuralTag["ManagedFrom[3]", "/stEvt:", manag["ManagedFrom[3]"]];
																	       PrependTo[manag3, <|"Xmp.xmpMM.ManagedFrom[3]" -> " type= Struct"|>];
																	     ];
																	    
																	    If[manag["ManagedFrom[4]"] =!= Missing["KeyAbsent", "ManagedFrom[4]"],
																	       manag4 = flattenStructuralTag["ManagedFrom[4]", "/stEvt:", manag["ManagedFrom[4]"]];
																	       PrependTo[manag4, <|"Xmp.xmpMM.ManagedFrom[4]" -> " type= Struct"|>];
																	     ];
																	    
																	    If[manag1 =!= <||>, fin = Join[fin, manag1]];
																	    If[manag2 =!= <||>, fin = Join[fin, manag2]];
																	    If[manag3 =!= <||>, fin = Join[fin, manag3]];
																	    If[manag4 =!= <||>, fin = Join[fin, manag4]];
																	    fin
																   ];
																  If[m =!= <||>, PrependTo[m, <|"Xmp.xmpMM.ManagedFrom" -> " type= Seq"|>]];
																  AppendTo[final, m];
															 ];
 
 														 If[assc["JobRef"] =!= Missing["KeyAbsent", "JobRef"], 
 														 	 job = assc["JobRef"];
  															 final = KeyDrop[final, "JobRef"];
  
  															 j = Module[{fin = <||>, job1 = <||>, job2 = <||>, job3 = <||>, job4 = <||>},

													    If[job["JobRef[1]"] =!= Missing["KeyAbsent", "JobRef[1]"], 
													       job1 = flattenStructuralTag["JobRef[1]", "/stJob:", job["JobRef[1]"]];
													       PrependTo[job1, <|"Xmp.xmpBJ.JobRef[1]" -> " type= Struct"|>];
													     ];
													    
													    If[job["JobRef[2]"] =!= Missing["KeyAbsent", "JobRef[2]"],
													       job2 = flattenStructuralTag["JobRef[2]", "/stJob:", job["JobRef[2]"]];
													       PrependTo[job2, <|"Xmp.xmpBJ.JobRef[2]" -> " type= Struct"|>];
													     ];
													    
													    If[job["JobRef[3]"] =!= Missing["KeyAbsent", "JobRef[3]"],
													       job3 = flattenStructuralTag["JobRef[3]", "/stJob:", job["JobRef[3]"]];
													       PrependTo[job3, <|"Xmp.xmpBJ.JobRef[3]" -> " type= Struct"|>];
													     ];
													    
													    If[job["JobRef[4]"] =!= Missing["KeyAbsent", "JobRef[4]"],
													       job4 = flattenStructuralTag["JobRef[4]", "/stJob:", job["JobRef[4]"]];
													       PrependTo[job4, <|"Xmp.xmpBJ.JobRef[4]" -> " type= Struct"|>];
													     ];
													    
													    If[job1 =!= <||>, fin = Join[fin, job1]];
													    If[job2 =!= <||>, fin = Join[fin, job2]];
													    If[job3 =!= <||>, fin = Join[fin, job3]];
													    If[job4 =!= <||>, fin = Join[fin, job4]];
													    fin
													 ];
  
  													If[j =!= <||>, PrependTo[j, <|"Xmp.xmpBJ.JobRef" -> " type= Seq"|>]];
 													AppendTo[final, j];
 												 ];
 
 					final
 ]

PrepareXMPMetaFromProcess[assoc_]:= Module[{firstStepRes = PrepareXMPMetaFromProcessWithoutDomains[assoc]},
											separateStructureTagsAndConstructBack[concat[firstStepRes]]
			]

WriteXMP[tag_, val_] :=  $WriteXMPString[tag, ToString@val]


WriteXMPRule[listOfRules : {__Rule}] := WriteXMP @@@ listOfRules

WriteXMPAssociation[list_Association]:= Module[{normalValues = GroupBy[Normal@list,SameQ[First@#, "Xmp.xmpMM.ManagedFrom*"] ||SameQ[First@#, "Xmp.xmpMM.RenditionOf*"] ||SameQ[First@#, "Xmp.xmpMM.DerivedFrom*"] ||SameQ[First@#, "Xmp.crs.PaintBasedCorrections*"] || SameQ[First@#, "Xmp.xmpMM.History*"] || SameQ[First@#, "Xmp.xmpMM.Versions*"] || SameQ[First@#, "Xmp.xmpBJ.JobRef*"] &, Association][False]},
											(*|| SameQ[First@#, "Xmp.xmpMM.DerivedFrom*"]*)
											WriteXMP @@@ Normal[normalValues];
											
											
											If[list["Xmp.xmpMM.DerivedFrom"] =!=  Missing["KeyAbsent", "Xmp.xmpMM.DerivedFrom"],
											Module[{newAsc = GroupBy[Normal@list, SameQ[First@#, "Xmp.xmpMM.DerivedFrom*"] &, Association]},
 												Module[{finalK = {}, finalV = {},
 													    h0 = Quiet@(newAsc[True] // KeySelect[SameQ[#, "Xmp.xmpMM.DerivedFrom[1]*"] &]), 
 													    h1 = Quiet@(newAsc[True] // KeySelect[SameQ[#, "Xmp.xmpMM.DerivedFrom[1]*"] &]), 
   														h2 = Quiet@(newAsc[True] // KeySelect[SameQ[#, "Xmp.xmpMM.DerivedFrom[2]*"] &]), 
   														h3 = Quiet@(newAsc[True] // KeySelect[SameQ[#, "Xmp.xmpMM.DerivedFrom[3]*"] &]), 
   														h4 = Quiet@(newAsc[True] // KeySelect[SameQ[#, "Xmp.xmpMM.DerivedFrom[4]*"] &])}, 
  
  														If[Count[Keys@h1, _String] > 0, 
  															 Module[{h1Res = Module[{keys = ToString@StringSplit[ToString[Keys[h1[[2 ;; Count[Keys@h1, _String]]]]], ","]}, 
       																StringTake[keys, {3, StringLength@(keys) - 2}]]}, AppendTo[finalK, h1Res]]];
       													If[Count[Keys@h0, _String] > 0, 
  															 Module[{h0Res = Module[{keys = ToString@StringSplit[ToString[Keys[h0[[2 ;; Count[Keys@h0, _String]]]]], ","]}, 
       																StringTake[keys, {3, StringLength@(keys) - 2}]]}, AppendTo[finalK, h0Res]]];			
  														If[Count[Keys@h2, _String] > 0, 
   															 Module[{h2Res = Module[{keys = ToString@StringSplit[ToString[Keys[h2[[2 ;; Count[Keys@h2, _String]]]]], ","]}, 
       																StringTake[keys, {3, StringLength@(keys) - 2}]]}, AppendTo[finalK, h2Res]]];
  														If[Count[Keys@h3, _String] > 0, 
  															 Module[{h3Res = Module[{keys = ToString@StringSplit[ToString[Keys[h3[[2 ;; Count[Keys@h3, _String]]]]], ","]}, 
       																StringTake[keys, {3, StringLength@(keys) - 2}]]}, AppendTo[finalK, h3Res]]];
  														If[Count[Keys@h4, _String] > 0, 
  															 Module[{h4Res = Module[{keys = ToString@StringSplit[ToString[Keys[h4[[2 ;; Count[Keys@h4, _String]]]]], ","]}, 
       																StringTake[keys, {3, StringLength@(keys) - 2}]]}, AppendTo[finalK, h4Res]]];
  
  														If[Count[Values@h1, _String] > 0, 
   														     Module[{h1Res = Module[{Values = ToString@StringSplit[ToString[StringDelete[#, ","] & /@ Values[h1[[2 ;; Count[Values@h1, _String]]]]], ","]}, 
   														     	    StringTake[Values, {3, StringLength@(Values) - 2}]]}, AppendTo[finalV, h1Res]]];
   														If[Count[Values@h0, _String] > 0, 
   														     Module[{h0Res = Module[{Values = ToString@StringSplit[ToString[StringDelete[#, ","] & /@ Values[h0[[2 ;; Count[Values@h0, _String]]]]], ","]}, 
   														     	    StringTake[Values, {3, StringLength@(Values) - 2}]]}, AppendTo[finalV, h0Res]]];
  													    If[Count[Values@h2, _String] > 0, 
  													    	 Module[{h2Res = Module[{Values = ToString@StringSplit[ToString[StringDelete[#, ","] & /@ Values[h2[[2 ;; Count[Values@h2, _String]]]]], ","]},
  													    	 	    StringTake[Values, {3, StringLength@(Values) - 2}]]}, AppendTo[finalV, h2Res]]];
  														If[Count[Values@h3, _String] > 0, 
  															 Module[{h3Res = Module[{Values = ToString@StringSplit[ToString[StringDelete[#, ","] & /@ Values[h3[[2 ;; Count[Values@h3, _String]]]]], ","]}, 
  															 	    StringTake[Values, {3, StringLength@(Values) - 2}]]}, AppendTo[finalV, h3Res]]];
  														If[Count[Values@h4, _String] > 0, 
  															 Module[{h4Res = Module[{Values =  ToString@StringSplit[ToString[StringDelete[#, ","] & /@ Values[h4[[2 ;; Count[Values@h4, _String]]]]], ","]}, 
  															 	    StringTake[Values, {3, StringLength@(Values) - 2}]]}, AppendTo[finalV, h4Res]]];
  										
  														If[StringLength[ToString@finalK] > 5 &&  StringLength[ToString@finalV] > 5 ,
  															finalK = StringTake[ToString@finalK, {2, StringLength[ToString@finalK] - 1}] ;
  															finalV = StringTake[ToString@finalV, {2, StringLength[ToString@finalV] - 1}] ;
  														$WriteXMPStructure[finalK, finalV, "Xmp.xmpMM.DerivedFrom"];]
                                                       ]
                                                   ]]
                                                   ;
                                            If[list["Xmp.xmpMM.RenditionOf"] =!=  Missing["KeyAbsent", "Xmp.xmpMM.RenditionOf"],
											Module[{newAsc = GroupBy[Normal@list, SameQ[First@#, "Xmp.xmpMM.RenditionOf*"] &, Association]},
 												Module[{finalK = {}, finalV = {}, h1 = Quiet@(newAsc[True] // KeySelect[SameQ[#, "Xmp.xmpMM.RenditionOf[1]*"] &]), 
   														h2 = Quiet@(newAsc[True] // KeySelect[SameQ[#, "Xmp.xmpMM.RenditionOf[2]*"] &]), 
   														h3 = Quiet@(newAsc[True] // KeySelect[SameQ[#, "Xmp.xmpMM.RenditionOf"] &]), 
   														h4 = Quiet@(newAsc[True] // KeySelect[SameQ[#, "Xmp.xmpMM.RenditionOf[4]*"] &])}, 
  
  														If[Count[Keys@h1, _String] > 0, 
  															 Module[{h1Res = Module[{keys = ToString@StringSplit[ToString[Keys[h1[[2 ;; Count[Keys@h1, _String]]]]], ","]}, 
       																StringTake[keys, {3, StringLength@(keys) - 2}]]}, AppendTo[finalK, h1Res]]];
  														If[Count[Keys@h2, _String] > 0, 
   															 Module[{h2Res = Module[{keys = ToString@StringSplit[ToString[Keys[h2[[2 ;; Count[Keys@h2, _String]]]]], ","]}, 
       																StringTake[keys, {3, StringLength@(keys) - 2}]]}, AppendTo[finalK, h2Res]]];
  														If[Count[Keys@h3, _String] > 0, 
  															 Module[{h3Res = Module[{keys = ToString@StringSplit[ToString[Keys[h3[[2 ;; Count[Keys@h3, _String]]]]], ","]}, 
       																StringTake[keys, {3, StringLength@(keys) - 2}]]}, AppendTo[finalK, h3Res]]];
  														If[Count[Keys@h4, _String] > 0, 
  															 Module[{h4Res = Module[{keys = ToString@StringSplit[ToString[Keys[h4[[2 ;; Count[Keys@h4, _String]]]]], ","]}, 
       																StringTake[keys, {3, StringLength@(keys) - 2}]]}, AppendTo[finalK, h4Res]]];
  
  														If[Count[Values@h1, _String] > 0, 
   														     Module[{h1Res = Module[{Values = ToString@StringSplit[ToString[StringDelete[#, ","] & /@ Values[h1[[2 ;; Count[Values@h1, _String]]]]], ","]}, 
   														     	    StringTake[Values, {3, StringLength@(Values) - 2}]]}, AppendTo[finalV, h1Res]]];
  													    If[Count[Values@h2, _String] > 0, 
  													    	 Module[{h2Res = Module[{Values = ToString@StringSplit[ToString[StringDelete[#, ","] & /@ Values[h2[[2 ;; Count[Values@h2, _String]]]]], ","]},
  													    	 	    StringTake[Values, {3, StringLength@(Values) - 2}]]}, AppendTo[finalV, h2Res]]];
  														If[Count[Values@h3, _String] > 0, 
  															 Module[{h3Res = Module[{Values = ToString@StringSplit[ToString[StringDelete[#, ","] & /@ Values[h3[[2 ;; Count[Values@h3, _String]]]]], ","]}, 
  															 	    StringTake[Values, {3, StringLength@(Values) - 2}]]}, AppendTo[finalV, h3Res]]];
  														If[Count[Values@h4, _String] > 0, 
  															 Module[{h4Res = Module[{Values =  ToString@StringSplit[ToString[StringDelete[#, ","] & /@ Values[h4[[2 ;; Count[Values@h4, _String]]]]], ","]}, 
  															 	    StringTake[Values, {3, StringLength@(Values) - 2}]]}, AppendTo[finalV, h4Res]]];
  										
  														If[StringLength[ToString@finalK] > 5 &&  StringLength[ToString@finalV] > 5 ,
  															finalK = StringTake[ToString@finalK, {2, StringLength[ToString@finalK] - 1}] ;
  															finalV = StringTake[ToString@finalV, {2, StringLength[ToString@finalV] - 1}] ;
  														$WriteXMPStructure[finalK, finalV, "Xmp.xmpMM.RenditionOf"];]
                                                       ]
                                                   ]]
                                                   ;
                                            If[list["Xmp.xmpMM.ManagedFrom"] =!=  Missing["KeyAbsent", "Xmp.xmpMM.ManagedFrom"],
											Module[{newAsc = GroupBy[Normal@list, SameQ[First@#, "Xmp.xmpMM.ManagedFrom*"] &, Association]},
 												Module[{finalK = {}, finalV = {}, h1 = Quiet@(newAsc[True] // KeySelect[SameQ[#, "Xmp.xmpMM.History[1]*"] &]), 
   														h2 = Quiet@(newAsc[True] // KeySelect[SameQ[#, "Xmp.xmpMM.ManagedFrom[2]*"] &]), 
   														h3 = Quiet@(newAsc[True] // KeySelect[SameQ[#, "Xmp.xmpMM.ManagedFrom"] &]), 
   														h4 = Quiet@(newAsc[True] // KeySelect[SameQ[#, "Xmp.xmpMM.ManagedFrom[4]*"] &])}, 
  
  														If[Count[Keys@h1, _String] > 0, 
  															 Module[{h1Res = Module[{keys = ToString@StringSplit[ToString[Keys[h1[[2 ;; Count[Keys@h1, _String]]]]], ","]}, 
       																StringTake[keys, {3, StringLength@(keys) - 2}]]}, AppendTo[finalK, h1Res]]];
  														If[Count[Keys@h2, _String] > 0, 
   															 Module[{h2Res = Module[{keys = ToString@StringSplit[ToString[Keys[h2[[2 ;; Count[Keys@h2, _String]]]]], ","]}, 
       																StringTake[keys, {3, StringLength@(keys) - 2}]]}, AppendTo[finalK, h2Res]]];
  														If[Count[Keys@h3, _String] > 0, 
  															 Module[{h3Res = Module[{keys = ToString@StringSplit[ToString[Keys[h3[[2 ;; Count[Keys@h3, _String]]]]], ","]}, 
       																StringTake[keys, {3, StringLength@(keys) - 2}]]}, AppendTo[finalK, h3Res]]];
  														If[Count[Keys@h4, _String] > 0, 
  															 Module[{h4Res = Module[{keys = ToString@StringSplit[ToString[Keys[h4[[2 ;; Count[Keys@h4, _String]]]]], ","]}, 
       																StringTake[keys, {3, StringLength@(keys) - 2}]]}, AppendTo[finalK, h4Res]]];
  
  														If[Count[Values@h1, _String] > 0, 
   														     Module[{h1Res = Module[{Values = ToString@StringSplit[ToString[StringDelete[#, ","] & /@ Values[h1[[2 ;; Count[Values@h1, _String]]]]], ","]}, 
   														     	    StringTake[Values, {3, StringLength@(Values) - 2}]]}, AppendTo[finalV, h1Res]]];
  													    If[Count[Values@h2, _String] > 0, 
  													    	 Module[{h2Res = Module[{Values = ToString@StringSplit[ToString[StringDelete[#, ","] & /@ Values[h2[[2 ;; Count[Values@h2, _String]]]]], ","]},
  													    	 	    StringTake[Values, {3, StringLength@(Values) - 2}]]}, AppendTo[finalV, h2Res]]];
  														If[Count[Values@h3, _String] > 0, 
  															 Module[{h3Res = Module[{Values = ToString@StringSplit[ToString[StringDelete[#, ","] & /@ Values[h3[[2 ;; Count[Values@h3, _String]]]]], ","]}, 
  															 	    StringTake[Values, {3, StringLength@(Values) - 2}]]}, AppendTo[finalV, h3Res]]];
  														If[Count[Values@h4, _String] > 0, 
  															 Module[{h4Res = Module[{Values =  ToString@StringSplit[ToString[StringDelete[#, ","] & /@ Values[h4[[2 ;; Count[Values@h4, _String]]]]], ","]}, 
  															 	    StringTake[Values, {3, StringLength@(Values) - 2}]]}, AppendTo[finalV, h4Res]]];
  										
  														If[StringLength[ToString@finalK] > 5 &&  StringLength[ToString@finalV] > 5 ,
  															finalK = StringTake[ToString@finalK, {2, StringLength[ToString@finalK] - 1}] ;
  															finalV = StringTake[ToString@finalV, {2, StringLength[ToString@finalV] - 1}] ;
  														$WriteXMPStructure[finalK, finalV, "Xmp.xmpMM.ManagedFrom"];]
                                                       ]
                                                   ]]
                                                   ;
                                            If[list["Xmp.xmpMM.History"] =!=  Missing["KeyAbsent", "Xmp.xmpMM.History"],
											Module[{newAsc = GroupBy[Normal@list, SameQ[First@#, "Xmp.xmpMM.History*"] &, Association]},
 												Module[{finalK = {}, finalV = {}, h1 = Quiet@(newAsc[True] // KeySelect[SameQ[#, "Xmp.xmpMM.History[1]*"] &]), 
   														h2 = Quiet@(newAsc[True] // KeySelect[SameQ[#, "Xmp.xmpMM.History[2]*"] &]), 
   														h3 = Quiet@(newAsc[True] // KeySelect[SameQ[#, "Xmp.xmpMM.History[3]*"] &]), 
   														h4 = Quiet@(newAsc[True] // KeySelect[SameQ[#, "Xmp.xmpMM.History[4]*"] &])}, 
  
  														If[Count[Keys@h1, _String] > 0, 
  															 Module[{h1Res = Module[{keys = ToString@StringSplit[ToString[Keys[h1[[2 ;; Count[Keys@h1, _String]]]]], ","]}, 
       																StringTake[keys, {3, StringLength@(keys) - 2}]]}, AppendTo[finalK, h1Res]]];
  														If[Count[Keys@h2, _String] > 0, 
   															 Module[{h2Res = Module[{keys = ToString@StringSplit[ToString[Keys[h2[[2 ;; Count[Keys@h2, _String]]]]], ","]}, 
       																StringTake[keys, {3, StringLength@(keys) - 2}]]}, AppendTo[finalK, h2Res]]];
  														If[Count[Keys@h3, _String] > 0, 
  															 Module[{h3Res = Module[{keys = ToString@StringSplit[ToString[Keys[h3[[2 ;; Count[Keys@h3, _String]]]]], ","]}, 
       																StringTake[keys, {3, StringLength@(keys) - 2}]]}, AppendTo[finalK, h3Res]]];
  														If[Count[Keys@h4, _String] > 0, 
  															 Module[{h4Res = Module[{keys = ToString@StringSplit[ToString[Keys[h4[[2 ;; Count[Keys@h4, _String]]]]], ","]}, 
       																StringTake[keys, {3, StringLength@(keys) - 2}]]}, AppendTo[finalK, h4Res]]];
  
  														If[Count[Values@h1, _String] > 0, 
   														     Module[{h1Res = Module[{Values = ToString@StringSplit[ToString[StringDelete[#, ","] & /@ Values[h1[[2 ;; Count[Values@h1, _String]]]]], ","]}, 
   														     	    StringTake[Values, {3, StringLength@(Values) - 2}]]}, AppendTo[finalV, h1Res]]];
  													    If[Count[Values@h2, _String] > 0, 
  													    	 Module[{h2Res = Module[{Values = ToString@StringSplit[ToString[StringDelete[#, ","] & /@ Values[h2[[2 ;; Count[Values@h2, _String]]]]], ","]},
  													    	 	    StringTake[Values, {3, StringLength@(Values) - 2}]]}, AppendTo[finalV, h2Res]]];
  														If[Count[Values@h3, _String] > 0, 
  															 Module[{h3Res = Module[{Values = ToString@StringSplit[ToString[StringDelete[#, ","] & /@ Values[h3[[2 ;; Count[Values@h3, _String]]]]], ","]}, 
  															 	    StringTake[Values, {3, StringLength@(Values) - 2}]]}, AppendTo[finalV, h3Res]]];
  														If[Count[Values@h4, _String] > 0, 
  															 Module[{h4Res = Module[{Values =  ToString@StringSplit[ToString[StringDelete[#, ","] & /@ Values[h4[[2 ;; Count[Values@h4, _String]]]]], ","]}, 
  															 	    StringTake[Values, {3, StringLength@(Values) - 2}]]}, AppendTo[finalV, h4Res]]];
  										
  														If[StringLength[ToString@finalK] > 5 &&  StringLength[ToString@finalV] > 5 ,
  															finalK = StringTake[ToString@finalK, {2, StringLength[ToString@finalK] - 1}] ;
  															finalV = StringTake[ToString@finalV, {2, StringLength[ToString@finalV] - 1}] ;
  														$WriteXMPStructure[finalK, finalV, "Xmp.xmpMM.History"];]
                                                       ]
                                                   ]]
                                                   ;
                                            If[list["Xmp.xmpMM.Versions"] =!=  Missing["KeyAbsent", "Xmp.xmpMM.Versions"],
											Module[{newAsc = GroupBy[Normal@list, SameQ[First@#, "Xmp.xmpMM.Versions*"] &, Association]},
 												Module[{finalK = {}, finalV = {}, h1 = Quiet@(newAsc[True] // KeySelect[SameQ[#, "Xmp.xmpMM.Versions[1]*"] &]), 
   														h2 = Quiet@(newAsc[True] // KeySelect[SameQ[#, "Xmp.xmpMM.Versions[2]*"] &]), 
   														h3 = Quiet@(newAsc[True] // KeySelect[SameQ[#, "Xmp.xmpMM.Versions[3]*"] &]), 
   														h4 = Quiet@(newAsc[True] // KeySelect[SameQ[#, "Xmp.xmpMM.Versions[4]*"] &])}, 
  
  														If[Count[Keys@h1, _String] > 0, 
  															 Module[{h1Res = Module[{keys = ToString@StringSplit[ToString[Keys[h1[[2 ;; Count[Keys@h1, _String]]]]], ","]}, 
       																StringTake[keys, {3, StringLength@(keys) - 2}]]}, AppendTo[finalK, h1Res]]];
  														If[Count[Keys@h2, _String] > 0, 
   															 Module[{h2Res = Module[{keys = ToString@StringSplit[ToString[Keys[h2[[2 ;; Count[Keys@h2, _String]]]]], ","]}, 
       																StringTake[keys, {3, StringLength@(keys) - 2}]]}, AppendTo[finalK, h2Res]]];
  														If[Count[Keys@h3, _String] > 0, 
  															 Module[{h3Res = Module[{keys = ToString@StringSplit[ToString[Keys[h3[[2 ;; Count[Keys@h3, _String]]]]], ","]}, 
       																StringTake[keys, {3, StringLength@(keys) - 2}]]}, AppendTo[finalK, h3Res]]];
  														If[Count[Keys@h4, _String] > 0, 
  															 Module[{h4Res = Module[{keys = ToString@StringSplit[ToString[Keys[h4[[2 ;; Count[Keys@h4, _String]]]]], ","]}, 
       																StringTake[keys, {3, StringLength@(keys) - 2}]]}, AppendTo[finalK, h4Res]]];
  
  														If[Count[Values@h1, _String] > 0, 
   														     Module[{h1Res = Module[{Values = ToString@StringSplit[ToString[StringDelete[#, ","] & /@ Values[h1[[2 ;; Count[Values@h1, _String]]]]], ","]}, 
   														     	    StringTake[Values, {3, StringLength@(Values) - 2}]]}, AppendTo[finalV, h1Res]]];
  													    If[Count[Values@h2, _String] > 0, 
  													    	 Module[{h2Res = Module[{Values = ToString@StringSplit[ToString[StringDelete[#, ","] & /@ Values[h2[[2 ;; Count[Values@h2, _String]]]]], ","]},
  													    	 	    StringTake[Values, {3, StringLength@(Values) - 2}]]}, AppendTo[finalV, h2Res]]];
  														If[Count[Values@h3, _String] > 0, 
  															 Module[{h3Res = Module[{Values = ToString@StringSplit[ToString[StringDelete[#, ","] & /@ Values[h3[[2 ;; Count[Values@h3, _String]]]]], ","]}, 
  															 	    StringTake[Values, {3, StringLength@(Values) - 2}]]}, AppendTo[finalV, h3Res]]];
  														If[Count[Values@h4, _String] > 0, 
  															 Module[{h4Res = Module[{Values =  ToString@StringSplit[ToString[StringDelete[#, ","] & /@ Values[h4[[2 ;; Count[Values@h4, _String]]]]], ","]}, 
  															 	    StringTake[Values, {3, StringLength@(Values) - 2}]]}, AppendTo[finalV, h4Res]]];
  										
  														If[StringLength[ToString@finalK] > 5 &&  StringLength[ToString@finalV] > 5 ,
  															finalK = StringTake[ToString@finalK, {2, StringLength[ToString@finalK] - 1}] ;
  															finalV = StringTake[ToString@finalV, {2, StringLength[ToString@finalV] - 1}] ;
  														$WriteXMPStructure[finalK, finalV, "Xmp.xmpMM.Versions"];]
                                                       ]
                                                   ]]
                                                   ;       
                                          	If[list["Xmp.xmpBJ.JobRef"] =!=  Missing["KeyAbsent", "Xmp.xmpBJ.JobRef"],
                                          	Module[{newAsc = GroupBy[Normal@list, SameQ[First@#, "Xmp.xmpBJ.JobRef*"] &, Association]},
 												Module[{finalK = {}, finalV = {}, h1 = Quiet@(newAsc[True] // KeySelect[SameQ[#, "Xmp.xmpBJ.JobRef[1]*"] &]), 
   														h2 = Quiet@(newAsc[True] // KeySelect[SameQ[#, "Xmp.xmpBJ.JobRef[2]*"] &]), 
   														h3 = Quiet@(newAsc[True] // KeySelect[SameQ[#, "Xmp.xmpBJ.JobRef[3]*"] &]), 
   														h4 = Quiet@(newAsc[True] // KeySelect[SameQ[#, "Xmp.xmpBJ.JobRef[4]*"] &])}, 
  
  														If[Count[Keys@h1, _String] > 0, 
  															 Module[{h1Res = Module[{keys = ToString@StringSplit[ToString[Keys[h1[[2 ;; Count[Keys@h1, _String]]]]], ","]}, 
       																StringTake[keys, {3, StringLength@(keys) - 2}]]}, AppendTo[finalK, h1Res]]];
  														If[Count[Keys@h2, _String] > 0, 
   															 Module[{h2Res = Module[{keys = ToString@StringSplit[ToString[Keys[h2[[2 ;; Count[Keys@h2, _String]]]]], ","]}, 
       																StringTake[keys, {3, StringLength@(keys) - 2}]]}, AppendTo[finalK, h2Res]]];
  														If[Count[Keys@h3, _String] > 0, 
  															 Module[{h3Res = Module[{keys = ToString@StringSplit[ToString[Keys[h3[[2 ;; Count[Keys@h3, _String]]]]], ","]}, 
       																StringTake[keys, {3, StringLength@(keys) - 2}]]}, AppendTo[finalK, h3Res]]];
  														If[Count[Keys@h4, _String] > 0, 
  															 Module[{h4Res = Module[{keys = ToString@StringSplit[ToString[Keys[h4[[2 ;; Count[Keys@h4, _String]]]]], ","]}, 
       																StringTake[keys, {3, StringLength@(keys) - 2}]]}, AppendTo[finalK, h4Res]]];
  
  														If[Count[Values@h1, _String] > 0, 
   														     Module[{h1Res = Module[{Values = ToString@StringSplit[ToString[StringDelete[#, ","] & /@ Values[h1[[2 ;; Count[Values@h1, _String]]]]], ","]}, 
   														     	    StringTake[Values, {3, StringLength@(Values) - 2}]]}, AppendTo[finalV, h1Res]]];
  													    If[Count[Values@h2, _String] > 0, 
  													    	 Module[{h2Res = Module[{Values = ToString@StringSplit[ToString[StringDelete[#, ","] & /@ Values[h2[[2 ;; Count[Values@h2, _String]]]]], ","]},
  													    	 	    StringTake[Values, {3, StringLength@(Values) - 2}]]}, AppendTo[finalV, h2Res]]];
  														If[Count[Values@h3, _String] > 0, 
  															 Module[{h3Res = Module[{Values = ToString@StringSplit[ToString[StringDelete[#, ","] & /@ Values[h3[[2 ;; Count[Values@h3, _String]]]]], ","]}, 
  															 	    StringTake[Values, {3, StringLength@(Values) - 2}]]}, AppendTo[finalV, h3Res]]];
  														If[Count[Values@h4, _String] > 0, 
  															 Module[{h4Res = Module[{Values =  ToString@StringSplit[ToString[StringDelete[#, ","] & /@ Values[h4[[2 ;; Count[Values@h4, _String]]]]], ","]}, 
  															 	    StringTake[Values, {3, StringLength@(Values) - 2}]]}, AppendTo[finalV, h4Res]]];
  										
  														If[StringLength[ToString@finalK] > 5 &&  StringLength[ToString@finalV] > 5 ,
  															finalK = StringTake[ToString@finalK, {2, StringLength[ToString@finalK] - 1}] ;
  															finalV = StringTake[ToString@finalV, {2, StringLength[ToString@finalV] - 1}] ;
  														$WriteXMPStructure[finalK, finalV, "Xmp.xmpBJ.JobRef"];]
                                                       ]]
                                                   ]
]


End[]
EndPackage[]
