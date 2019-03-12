(* ::Package:: *)

Begin["System`Convert`CommonGraphicsDump`"]


ImportExport`RegisterImport["PNG",
	{
        (*Custom elements*)		
		"BitDepth" 				                                    	:> GetImageMetaData["PNG", "BitDepth", All],
		"ColorMap"				                                     	:> GetRawDataAndColorMapElements["PNG", All, "Element" -> "ColorMap"],
        "ColorProfileData" 		                                    	:> GetImageMetaData["PNG", "ColorProfileData", All],
		"ColorSpace" 			  	                                    :> GetImageMetaData["PNG", "ColorSpace", All],
		"Comments" 				                                    	:> GetImageMetaData["PNG", "Comments", All],
		"Data" 					                                    	:> GetDataElement["PNG", All],
		"Graphics" 				                                  	  :> GetGraphicsElement["PNG"],
		"GrayLevels" 				                                  	:> GetGrayLevelsElement["PNG", All],
		"Image" 					                                  	 :> GetImageElement["PNG"],
		"ImageSize" 				                                   	:> GetImageMetaData["PNG", "ImageSize", All],
		"RawData"					                                  	:> GetRawDataAndColorMapElements["PNG", All, "Element" -> "RawData"],
		"RGBColorArray" 			 	                                  :> GetRGBColorArrayElement["PNG", All],
        "Channels"                                                          :> GetChannelsElement["PNG"],
        "Summary"                                                           :> CreateSummary["PNG"],
		"Thumbnail" | {"Thumbnail", s:(_Integer|_Symbol)}                 :> GetThumbnailElement["PNG", s],      

        {"Exif",  "Elements"}                                               :> GetExifInformation["PNG", True],
        {"IPTC",  "Elements"}                                               :> GetIPTCInformation["PNG", True],

        (*Metadata elements*)
        "Exif"	   				                                       :> GetExifInformation["PNG"],
		"RawExif"	   				                                    :> GetExifInformationRaw["PNG"],
		"XMP"					                                           :> GetXMPInformation["PNG"],
		"RawXMP"				                                        	:> GetXMPInformationRaw["PNG"],
		"IPTC"                                                               :> GetIPTCInformation["PNG"],
		"RawIPTC"                                                            :> GetIPTCInformationRaw["PNG"],	
        tag: (Alternatives[Sequence@@System`ConvertersDump`$MetadataTags]) :> GetExifIndividualElement[tag],

        (*All elements*)
		"Elements"					                                       :> GetListOfElements["PNG"],
		                                                                        GetListOfElements["PNG"]
	},

	"DefaultElement"    -> "Image",
	"BinaryFormat"      -> True,
	"Options"           -> {"BitDepth", "ColorSpace", "ImageSize"},
	"Sources"           -> {"JLink`",  "Convert`Exif`", "Convert`XMP`", "Convert`IPTC`", "Convert`CommonGraphics`"},
	"AvailableElements" -> Join[System`ConvertersDump`$MetadataTags, {"Channels", "BitDepth","ColorMap","ColorProfileData","ColorSpace", "Comments","Data","Graphics","GrayLevels","Image", "ImageSize",
                                                                      "RawData","RGBColorArray", "Thumbnail", "Summary", "Exif","RawExif","XMP","RawXMP","IPTC","RawIPTC"}]
]


End[]
