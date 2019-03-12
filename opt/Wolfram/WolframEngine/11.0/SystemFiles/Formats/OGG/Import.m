(* ::Package:: *)

Begin["System`Convert`AudioDump`"]

ImportExport`RegisterImport[
"OGG",
 {"Audio" 	  			  :> ImportAudio["OGG"],
  "AudioChannels" 		  :> ImportAudioMetaData["OGG", "AudioChannels"],
  "AudioEncoding" 		  :> ImportAudioMetaData["OGG", "AudioEncoding"],
  "AudioFile" 			  :> ImportAudioFile["OGG"],
  "Data" 	  			  :> ImportAudioData["OGG"],
  "Duration" 		  	  :> ImportAudioMetaData["OGG", "Duration"],
  "Length" 		  	  	  :> ImportAudioMetaData["OGG", "Length"],
  "MetaInformation"   	  :> ImportAudioMetaData["OGG", "MetaInformation"],
  "SampleDepth" 		  :> ImportAudioMetaData["OGG", "SampleDepth"],
  "SampledSoundList" 	  :> ImportSampledSoundList["OGG"],
  "SampleRate" 		  	  :> ImportAudioMetaData["OGG", "SampleRate"],
  "Sound" 	  			  :> ImportSound["OGG"],
  
  "Elements"  			  :> GetListOfAudioElements["OGG"],
  GetListOfAudioElements["OGG"]}
 ,
 "DefaultElement" -> "Audio",
 "Options" -> {"AudioChannels", "SampleRate"},
 "Sources" -> ImportExport`DefaultSources["Audio"],
 "AvailableElements" -> {"Audio", "AudioChannels", "AudioEncoding", "AudioFile", "Data", "Duration", "Length", "MetaInformation", "SampleDepth", "SampledSoundList", "SampleRate", "Sound"},
 "BinaryFormat" -> True
]


End[]
