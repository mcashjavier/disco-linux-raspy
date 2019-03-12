(* ::Package:: *)

Begin["System`Convert`AudioDump`"]

ImportExport`RegisterImport[
"FLAC",
 {"Audio" 	  			  :> ImportAudio["FLAC"],
  "AudioChannels" 		  :> ImportAudioMetaData["FLAC", "AudioChannels"],
  "AudioEncoding" 		  :> ImportAudioMetaData["FLAC", "AudioEncoding"],
  "AudioFile" 			  :> ImportAudioFile["FLAC"],
  "Data" 	  			  :> ImportAudioData["FLAC"],
  "Duration" 		  	  :> ImportAudioMetaData["FLAC", "Duration"],
  "Length" 		  	  	  :> ImportAudioMetaData["FLAC", "Length"],
  "MetaInformation"   	  :> ImportAudioMetaData["FLAC", "MetaInformation"],
  "SampleDepth" 		  :> ImportAudioMetaData["FLAC", "SampleDepth"],
  "SampledSoundList" 	  :> ImportSampledSoundList["FLAC"],
  "SampleRate" 		  	  :> ImportAudioMetaData["FLAC", "SampleRate"],
  "Sound" 	  			  :> ImportSound["FLAC"],
  
  "Elements"  			  :> GetListOfAudioElements["FLAC"],
  GetListOfAudioElements["FLAC"]}
 ,
 "DefaultElement" -> "Audio",
 "Options" -> {"AudioChannels", "SampleRate"},
 "Sources" -> ImportExport`DefaultSources["Audio"],
 "AvailableElements" -> {"Audio", "AudioChannels", "AudioEncoding", "AudioFile", "Data", "Duration", "Length", "MetaInformation", "SampleDepth", "SampledSoundList", "SampleRate", "Sound"},
 "BinaryFormat" -> True
]


End[]
