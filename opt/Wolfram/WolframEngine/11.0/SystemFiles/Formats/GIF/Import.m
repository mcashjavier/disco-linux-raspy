(* ::Package:: *)

Begin["System`Convert`CommonGraphicsDump`"]


ImportExport`RegisterImport["GIF",
	{
		(*Custom elements*)        
			"Animation"													        					:> GetAnimationElement["GIF"],
			"Graphics"													        					:> GetGraphicsElement["GIF"],
			"Image"													            					:> GetImageElement["GIF"],
			"Image3D"												           						:> GetImage3DElement["GIF"],
			"ImageCount"												        					:> GetImageCountElement["GIF"],
			"Thumbnail" 				       							    						:> GetThumbnailElement["GIF"],
			"ThumbnailList"												        					:> GetThumbnailListElement["GIF"], 
			"Summary"                                                           					:> CreateSummary["GIF", False],
			"SummarySlideView"                                                  					:> CreateSummary["GIF", True],
			meta:("ImageSize"|"DataType"|"BitDepth"|"ColorSpace"|"AnimationRepetitions") 			:> GetImageMetaData["GIF",meta, All],
			"GlobalColorMap"							        			    					:> GetGlobalColorMapElement["GIF"],
			"Channels"|{"Channels", All|"All"}														:> GetImageMetaData["GIF","Channels", All],
			{"Channels", f:(_Integer|_List)}														:> GetImageMetaData["GIF","Channels", f],
			"ColorMap"|{"ColorMap", All|"All"}			        									:> GetRawDataAndColorMapElements["GIF", All],
			{"ColorMap", f:(_Integer|_List)}							    						:> GetRawDataAndColorMapElements["GIF", f],
			"Frames"|{"Frames", All|"All"}															:> GetFramesElement["GIF", All],
			{"Frames", f:(_Integer|_List)} 															:> GetFramesElement["GIF", f],
			"Background"|{"Background", All|"All"}													:> GetImageMetaData["GIF","Background", All],
			{"Background", f:(_Integer|_List)}														:> GetImageMetaData["GIF","Background", f],
			"DisplayDurations"|{"DisplayDurations", All|"All"}										:> GetImageMetaData["GIF","DisplayDurations", All],
			{"DisplayDurations", f:(_Integer|_List)}												:> GetImageMetaData["GIF","DisplayDurations", f],
			"TransitionEffect"|{"TransitionEffect", All|"All"}										:> GetImageMetaData["GIF","TransitionEffect", All],
			{"TransitionEffect", f:(_Integer|_List)}												:> GetImageMetaData["GIF","TransitionEffect", f],
			"ImageOffset"|{"ImageOffset", All|"All"}												:> GetImageMetaData["GIF","ImageOffset", All],
			{"ImageOffset", f:(_Integer|_List)}														:> GetImageMetaData["GIF","ImageOffset", f],
			"UserInputFlag"|{"UserInputFlag", All|"All"}											:> GetImageMetaData["GIF","UserInputFlag", All],
			{"UserInputFlag", f:(_Integer|_List)}													:> GetImageMetaData["GIF","UserInputFlag", f],
			"Comments"|{"Comments", All|"All"}														:> GetImageMetaData["GIF","Comments", All],
			{"Comments", f:(_Integer|_List)}														:> GetImageMetaData["GIF","Comments", f],
			"Data"|{"Data", All|"All"}								         						:> GetDataElement["GIF", All],
			{"Data", f:(_Integer|_List)}															:> GetDataElement["GIF", f],
			"RawData"|{"RawData", All|"All"}						          	 					:> GetRawDataAndColorMapElements["GIF", All],
			{"RawData", f:(_Integer|_List)}															:> GetRawDataAndColorMapElements["GIF", f],
			"GraphicsList"|{"GraphicsList", All|"All"}				         						:> GetGraphicsListElement["GIF", All],
			{"GraphicsList", f:(_Integer|_List)}					         						:> GetGraphicsListElement["GIF", f],
			"ImageList"|{"ImageList", All|"All"}				         							:> GetImageListElement["GIF", All],
			{"ImageList", f:(_Integer|_List)}					         							:> GetImageListElement["GIF", f],
			"ThumbnailList"|{"ThumbnailList", All|"All"}				         					:> GetThumbnailListElement["GIF", All],
			{"ThumbnailList", f:(_Integer|_List)}					         						:> GetThumbnailListElement["GIF", f],
			{"ThumbnailList", f:(_Integer|_List|All|"All"), s:(_Integer|_Symbol)} 					:> GetThumbnailListElement["GIF", f, s],  
			"GrayLevels"|{"GrayLevels", All|"All"}				         							:> GetGrayLevelsElement["GIF", All],
			{"GrayLevels", f:(_Integer|_List)}							         					:> GetGrayLevelsElement["GIF", f],
			"RGBColorArray"|{"RGBColorArray", All|"All"}				         					:> GetRGBColorArrayElement["GIF", All],
			{"RGBColorArray", f:(_Integer|_List)}						          					:> GetRGBColorArrayElement["GIF", f],
			
			(*"DefaultElement" intentionally left out, converter decides whether to return Image or ImageList*)
			Automatic 													          					:> GetDefaultImageElement["GIF"],
			(*All elements*)
			"Elements"					                                           					:> GetListOfElements["GIF"],
			GetListOfElements["GIF"]
	},
	
	"AlphaChannel"      -> True,
    "BinaryFormat"      -> True,
    "Options" 			-> {"Background", "ImageSize", "ColorSpace", "DisplayDurations", "AnimationRepetitions", "TransitionEffect", "UserInputFlag"},
    "Sources"           -> {"Convert`CommonGraphics`"},
	"AvailableElements" -> {"Animation", "AnimationRepetitions", "Background", "BitDepth",
  	 	"Channels", "ColorMap", "ColorSpace", "Comments", "Data", "DataType", "DisplayDurations", "Frames",
    	"GlobalColorMap", "Graphics", "GraphicsList", "GrayLevels", "Image", "Image3D", "ImageCount", 
        "ImageList", "ImageOffset", "ImageSize", "RawData", "RGBColorArray", "TransitionEffect",
        "UserInputFlag", "Summary", "SummarySlideView", "Thumbnail", "ThumbnailList"}
]



End[]
