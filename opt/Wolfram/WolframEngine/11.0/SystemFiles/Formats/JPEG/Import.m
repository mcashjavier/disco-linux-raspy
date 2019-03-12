(* ::Package:: *)

Begin["System`Convert`CommonGraphicsDump`"]


ImportExport`RegisterImport["JPEG",
	{
        (*Custom elements*)
        "BitDepth" 				                                    	 :> GetImageMetaData["JPEG", "BitDepth", All],
		"ColorMap"|"RawData" 		                                       :> GetRawDataAndColorMapElements["JPEG", All],		
        "ColorProfileData"		                                    	  :> GetImageMetaData["JPEG", "ColorProfileData", All],
        "ColorSpace" 			                                    	   :> GetImageMetaData["JPEG", "ColorSpace", All],
        "Data" 						                                     :> GetDataElement["JPEG", All],
        "Graphics" 					                                     :> GetGraphicsElement["JPEG"],
		"GrayLevels" 				                                       :> GetGrayLevelsElement["JPEG", All],
		"Image" 					                                    	:> GetImageElement["JPEG"],
		"ImageNoExif" 			                                    	  :> GetImageElementNoExif["JPEG"],
		"ImageSize" 					                                    :> GetImageMetaData["JPEG", "ImageSize", All],
		"ImageWithExif" 				                                    :> GetImageElementWithExif["JPEG"],
		"RGBColorArray" 			                                   	 :> GetRGBColorArrayElement["JPEG", All],
        "Channels"                                                           :> GetChannelsElement["JPEG"],
        "Summary"                                                            :> CreateSummary["JPEG"],
		"Thumbnail" | {"Thumbnail", s:(_Integer|_Symbol)}                  :> GetThumbnailElement["JPEG", s], 

        {"Exif",  "Elements"}                                               :> GetExifInformation["JPEG", True],
        {"IPTC",  "Elements"}                                               :> GetIPTCInformation["JPEG", True],

        (*Metadata elements*)
		"Exif"					                                          :> GetExifInformation["JPEG"],
		"ExifLegacy"					                                    :> GetExifInformationLegacy["JPEG"],
        "RawExif"   				                                        :> GetExifInformationRaw["JPEG"],
		"IPTC"                                                              :> GetIPTCInformation["JPEG"],
		"RawIPTC"                                                           :> GetIPTCInformationRaw["JPEG"],
		"XMP"		                                    			       :> GetXMPInformation["JPEG"],
		"RawXMP"				                                        	:> GetXMPInformationRaw["JPEG"],
        tag: (Alternatives[Sequence@@System`ConvertersDump`$MetadataTags]) :> GetExifIndividualElement[tag],

        (*All elements*)
		"Elements"				                                           :> GetListOfElements["JPEG"][#]&,
		                                                                        GetListOfElements["JPEG"]
	},

	"DefaultElement"    -> "Image",
    "BinaryFormat"      -> True,
    "Options"           -> {"BitDepth", "ColorSpace", "ImageSize"},
    "Sources"           -> {"JLink`", "Convert`Exif`", "Convert`XMP`", "Convert`IPTC`", "Convert`CommonGraphics`"},
	"AvailableElements" -> Join[System`ConvertersDump`$MetadataTags, {"Channels", "BitDepth","ColorMap","ColorProfileData","ColorSpace","Data", "Graphics","GrayLevels","Image","ImageNoExif", "ImageSize","ImageWithExif", 
                                                                       "Manufacturer","RawData", "RGBColorArray","Thumbnail", "Summary", "Exif","RawExif","ExifLegacy","IPTC","RawIPTC","XMP","RawXMP"}]
]


End[]
