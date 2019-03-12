(* ::Package:: *)

Begin["System`Convert`AudioDump`"]

ImportExport`RegisterImport[
"AIFF",
 {"Audio" 	  			  :> ImportAudio["AIFF"],
  "AudioChannels" 		  :> ImportAudioMetaData["AIFF", "AudioChannels"],
  "AudioEncoding" 		  :> ImportAudioMetaData["AIFF", "AudioEncoding"],
  "AudioFile" 			  :> ImportAudioFile["AIFF"],
  "Data" 	  			  :> ImportAudioData["AIFF"],
  "Duration" 		  	  :> ImportAudioMetaData["AIFF", "Duration"],
  "Length" 		  	  	  :> ImportAudioMetaData["AIFF", "Length"],
  "MetaInformation"   	  :> ImportAudioMetaData["AIFF", "MetaInformation"],
  "SampleDepth" 		  :> ImportAudioMetaData["AIFF", "SampleDepth"],
  "SampledSoundList" 	  :> ImportSampledSoundList["AIFF"],
  "SampleRate" 		  	  :> ImportAudioMetaData["AIFF", "SampleRate"],
  "Sound" 	  			  :> ImportSound["AIFF"],
  
  "Elements"  			  :> GetListOfAudioElements["AIFF"],
  GetListOfAudioElements["AIFF"]}
 ,
 "DefaultElement" -> "Audio",
 "Options" -> {"AudioChannels", "SampleRate"},
 "Sources" -> ImportExport`DefaultSources["Audio"],
 "AvailableElements" -> {"Audio", "AudioChannels", "AudioEncoding", "AudioFile", "Data", "Duration", "Length", "MetaInformation", "SampleDepth", "SampledSoundList", "SampleRate", "Sound"},
 "BinaryFormat" -> True
]


End[]
