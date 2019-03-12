(* ::Package:: *)

Begin["System`Convert`AudioDump`"]

ImportExport`RegisterImport[
"WAV",
 {"Audio" 	  			  :> ImportAudio["WAV"],
  "AudioChannels" 		  :> ImportAudioMetaData["WAV", "AudioChannels"],
  "AudioEncoding" 		  :> ImportAudioMetaData["WAV", "AudioEncoding"],
  "AudioFile" 			  :> ImportAudioFile["WAV"],
  "Data" 	  			  :> ImportAudioData["WAV"],
  "Duration" 		  	  :> ImportAudioMetaData["WAV", "Duration"],
  "Length" 		  	  	  :> ImportAudioMetaData["WAV", "Length"],
  "MetaInformation"   	  :> ImportAudioMetaData["WAV", "MetaInformation"],
  "SampleDepth" 		  :> ImportAudioMetaData["WAV", "SampleDepth"],
  "SampledSoundList" 	  :> ImportSampledSoundList["WAV"],
  "SampleRate" 		  	  :> ImportAudioMetaData["WAV", "SampleRate"],
  "Sound" 	  			  :> ImportSound["WAV"],
  
  "Elements"  			  :> GetListOfAudioElements["WAV"],
  GetListOfAudioElements["WAV"]}
 ,
 "DefaultElement" -> "Audio",
 "Options" -> {"AudioChannels", "SampleRate"},
 "Sources" -> ImportExport`DefaultSources["Audio"],
 "AvailableElements" -> {"Audio", "AudioChannels", "AudioEncoding", "AudioFile", "Data", "Duration", "Length", "MetaInformation", "SampleDepth", "SampledSoundList", "SampleRate", "Sound"},
 "BinaryFormat" -> True
]


End[]
