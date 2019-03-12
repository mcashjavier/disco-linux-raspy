(* ::Package:: *)

Begin["System`Convert`CommonGraphicsDump`"]


ImportExport`RegisterImport["TIFF",
	{	
        (*Custom elements*)        
		"Animation"													        :> GetAnimationElement["TIFF"],
		"Graphics"													         :> GetGraphicsElement["TIFF"],
		"Image"													            :> GetImageElement["TIFF"],
		"Image3D"												            :> GetImage3DElement["TIFF"],
		"ImageCount"												           :> GetImageCountElement["TIFF"],
        "Thumbnail" 				       							    	:> GetThumbnailElement["TIFF"],
		"ThumbnailList"												        :> GetThumbnailListElement["TIFF"], 
        "Channels"                                                              :> GetChannelsElement["TIFF"],
        "Summary"                                                               :> CreateSummary["TIFF", False],
        "SummarySlideView"                                                      :> CreateSummary["TIFF", True],

		"Author"|{"Author", All|"All"}								         :> GetImageMetaData["TIFF","Author", All],
		"BitDepth"|{"BitDepth", All|"All"}							         :> GetImageMetaData["TIFF","BitDepth", All],
		"ColorMap"|{"ColorMap", All|"All"}							         :> GetRawDataAndColorMapElements["TIFF", All],
		"ColorProfileData"|{"ColorProfile", All|"All"}				         :> GetImageMetaData["TIFF","ColorProfileData", All],
		"ColorSpace"|{"ColorSpace", All|"All"}						         :> GetImageMetaData["TIFF","ColorSpace", All],
		"Comments"|{"Comments", All|"All"}							         :> GetImageMetaData["TIFF","Comments", All],
		"CopyrightNotice"|{"CopyrightNotice", All|"All"}			           :> GetImageMetaData["TIFF","CopyrightNotice", All],
		"Data"|{"Data", All|"All"}								         	:> GetDataElement["TIFF", All],
		"Device"|{"Device", All|"All"}							         	:> GetImageMetaData["TIFF","Device", All],
		"DeviceManufacturer"|{"DeviceManufacturer", All|"All"}	             :> GetImageMetaData["TIFF","DeviceManufacturer", All],
		"GraphicsList"|{"GraphicsList", All|"All"}				         	:> GetGraphicsListElement["TIFF", All],
		"GrayLevels"|{"GrayLevels", All|"All"}					 	        :> GetGrayLevelsElement["TIFF", All],
		"ImageCreationDate"|{"ImageCreationDate", All|"All"}	            	:> GetImageMetaData["TIFF","ImageCreationDate", All],
		"ImageEncoding"|{"ImageEncoding", All|"All"}			           	:> GetImageMetaData["TIFF","ImageEncoding", All],
		"ImageList"|{"ImageList", All|"All"}					           	:> GetImageListElement["TIFF", All],
		"ImageResolution"|{"ImageResolution", All|"All"}		           	:> GetImageMetaData["TIFF","ImageResolution", All],
		"ImageSize"|{"ImageSize", All|"All"}						           :> GetImageMetaData["TIFF","ImageSize", All],
		"RawData"|{"RawData", All|"All"}						          	 :> GetRawDataAndColorMapElements["TIFF", All],
		"RGBColorArray"|{"RGBColorArray", All|"All"}			           	:> GetRGBColorArrayElement["TIFF", All],
		"RowsPerStrip"|{"RowsPerStrip", All|"All"}				         	:> GetImageMetaData["TIFF", "RowsPerStrip", All],
		"TileSize"|{"TileSize", All|"All"}							         :> GetImageMetaData["TIFF", "TileSize", All],

		{"Author", f:(_Integer|_List)}								         :> GetImageMetaData["TIFF","Author", f],
		{"BitDepth", f:(_Integer|_List)}							           :> GetImageMetaData["TIFF","BitDepth", f],
		{"CameraTopOrientation", f:(_Integer|_List)}				           :> GetImageMetaData["TIFF","CameraTopOrientation", f],
		{"ColorMap", f:(_Integer|_List)}							           :> GetRawDataAndColorMapElements["TIFF", f],
		{"ColorProfile", f:(_Integer|_List)}						           :> GetImageMetaData["TIFF","ColorProfile", f],
		{"ColorSpace", f:(_Integer|_List)}							         :> GetImageMetaData["TIFF","ColorSpace", f],
		{"Comments", f:(_Integer|_List)}							           :> GetImageMetaData["TIFF","Comments", f],
		{"CopyrightNotice", f:(_Integer|_List)}						        :> GetImageMetaData["TIFF","CopyrightNotice", f],
		{"Data", f:(_Integer|_List)}								           :> GetDataElement["TIFF", f],
		{"Device", f:(_Integer|_List)}								         :> GetImageMetaData["TIFF","Device", f],
		{"DeviceManufacturer", f:(_Integer|_List)}					         :> GetImageMetaData["TIFF","DeviceManufacturer", f],
		{"GraphicsList", f:(_Integer|_List)}						           :> GetGraphicsListElement["TIFF", f],
		{"GrayLevels", f:(_Integer|_List)}							         :> GetGrayLevelsElement["TIFF", f],
		{"ImageCreationDate", f:(_Integer|_List)}					          :> GetImageMetaData["TIFF","ImageCreationDate", f],
		{"ImageEncoding", f:(_Integer|_List)}						          :> GetImageMetaData["TIFF","ImageEncoding", f],
		{"ImageList", f:(_Integer|_List)}							          :> GetImageListElement["TIFF", f],
		{"ImageResolution", f:(_Integer|_List)}						        :> GetImageMetaData["TIFF","ImageResolution", f],
		{"ImageSize", f:(_Integer|_List)}							           :> GetImageMetaData["TIFF","ImageSize", f],
		{"RawData", f:(_Integer|_List)}								        :> GetRawDataAndColorMapElements["TIFF", f],
		{"RGBColorArray", f:(_Integer|_List)}						          :> GetRGBColorArrayElement["TIFF", f],
		{"RowsPerStrip", f:(_Integer|_List)}						           :> GetImageMetaData["TIFF", "RowsPerStrip", f],
		{"TileSize", f:(_Integer|_List)}							           :> GetImageMetaData["TIFF", "TileSize", f],
		{"Thumbnail", f:(_Integer|_Symbol)}           				         :> GetThumbnailElement["TIFF", f],
		{"ThumbnailList", f:(_Integer|_List|All|"All")} 			           :> GetThumbnailListElement["TIFF", f],
		{"ThumbnailList", f:(_Integer|_List|All|"All"), s:(_Integer|_Symbol)} :> GetThumbnailListElement["TIFF", f, s],  

        {"Exif",  "Elements"}                                                    :> GetExifInformation["TIFF", True],
        {"IPTC",  "Elements"}                                                    :> GetIPTCInformation["TIFF", True],

        (*Metadata elements*)
		"Exif"					                                               :> GetExifInformation["TIFF"],
		"RawExif"					                                            :> GetExifInformationRaw["TIFF"],
		"IPTC"                                                                   :> GetIPTCInformation["TIFF"],
		"RawIPTC"                                                                :> GetIPTCInformationRaw["TIFF"],
		"XMP"					                                                :> GetXMPInformation["TIFF"],
		"RawXMP"				    	                                         :> GetXMPInformationRaw["TIFF"],
        tag: (Alternatives[Sequence@@System`ConvertersDump`$MetadataTags])     :> GetExifIndividualElement[tag],

        (*"DefaultElement" intentionally left out, converter decides whether to return Image or ImageList*)
		Automatic 													          :> GetDefaultImageElement["TIFF"],

        (*All elements*)
		"Elements"					                                           :> GetListOfElements["TIFF"],
		                                                                            GetListOfElements["TIFF"]
	},

    "AlphaChannel"      -> True,
    "BinaryFormat"      -> True,
    "Options"           -> {"BitDepth", "ColorSpace", "ImageSize", "Comments"},
    "Sources"           -> {"Convert`Exif`", "Convert`XMP`", "Convert`IPTC`", "Convert`CommonGraphics`"},
    "AvailableElements" -> Join[System`ConvertersDump`$MetadataTags, {"Channels", "Animation","Author","BitDepth","ColorMap", "ColorProfileData","ColorSpace","Comments","CopyrightNotice","Data","Device",
                                                                       "DeviceManufacturer","Graphics","GraphicsList","GrayLevels","Image","Image3D", "ImageCount","ImageCreationDate","ImageEncoding",
                                                                       "ImageList","ImageResolution", "ImageSize","RawData","RGBColorArray","RowsPerStrip","Thumbnail","ThumbnailList", "Summary", "SummarySlideView", "TileSize", 
                                                                       "Exif","RawExif","XMP","RawXMP","IPTC","RawIPTC"}]
]


End[]
