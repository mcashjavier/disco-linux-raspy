(* ::Package:: *)

(* Mathematica Package *)

(* Created by the Wolfram Workbench Jan 30, 2012 *)

BeginPackage["AudioFileStreamTools`"]
(* Exported symbols added here with SymbolName::usage *)

AudioFileStreamTools`FileStreamOpenRead::usage = "AudioFileStreamTools`FileStreamOpenRead[ fileName] opens a read stream to the file specified. An AudioFileStreamTools`FileSteamObject is returned.";
AudioFileStreamTools`FileStreamReadN::usage = "AudioFileStreamTools`FileStreamReadN[ obj, n] reads n frames from the current position of obj and moves the read pointer accordingly.";
AudioFileStreamTools`FileStreamClose::usage = "AudioFileStreamTools`FileStreamClose[ obj] closes the specified AudioFileStreamTools`FileSteamObject.";
AudioFileStreamTools`FileStreamObject::usage = "AudioFileStreamTools`FileStreamObject represents an audio file stream.";
AudioFileStreamTools`FileStreamOpenWrite::usage = "AudioFileStreamTools`FileStreamOpenWrite[ fileName] opens a write stream to the file specified. An AudioFileStreamTools`FileSteamObject is returned.";
AudioFileStreamTools`FileStreamWrite::usage = "AudioFileStreamTools`FileStreamWrite[ obj, audioData] writes the information in the audioData to the specified file stream object.";
AudioFileStreamTools`FileStreamSetReadPosition::usage = "AudioFileStreamTools`FileStreamSetReadPosition[ obj, pos] sets the read position of obj. ";
AudioFileStreamTools`FileStreamGetReadPosition::usage = "AudioFileStreamTools`FileStreamGetReadPosition[ obj] returns the read position of obj.";
AudioFileStreamTools`FileStreamGetMetaInformation::usage = "AudioFileStreamTools`FileStreamGetMetaInformation[ obj] returns the meta information for a stream.";
AudioFileStreamTools`FileStreamGetTotalNumberOfFrames::usage = "DEPRECATED: AudioFileStreamTools`FileStreamGetTotalNumberOfFrames[ obj] returns the total number of audio frames in the audio stream.";
AudioFileStreamTools`FileStreamGetStreamInformation::usage = "AudioFileStreamTools`FileStreamGetStreamInformation[ obj] returns the stream information for a stream.";

AudioFileStreamTools`InternetStreamReadN::usage = "AudioFileStreamTools`InternetStreamReadN[ obj, n] reads n frames from the current position of obj and moves the read pointer accordingly.";
AudioFileStreamTools`InternetStreamClose::usage = "AudioFileStreamTools`InternetStreamClose[ obj] closes the specified AudioFileStreamTools`FileSteamObject.";
AudioFileStreamTools`InternetStreamSetReadPosition::usage = "AudioFileStreamTools`InternetStreamSetReadPosition[ obj, pos] sets the read position of obj. ";
AudioFileStreamTools`InternetStreamGetReadPosition::usage = "AudioFileStreamTools`InternetStreamGetReadPosition[ obj] returns the read position of obj.";
AudioFileStreamTools`InternetStreamGetMetaInformation::usage = "AudioFileStreamTools`InternetStreamGetMetaInformation[ obj] returns the meta information for a stream.";
AudioFileStreamTools`InternetStreamGetTotalNumberOfFrames::usage = "DEPRECATED: AudioFileStreamTools`InternetStreamGetTotalNumberOfFrames[ obj] returns the total number of audio frames in the audio stream.";
AudioFileStreamTools`InternetStreamGetStreamInformation::usage = "AudioFileStreamTools`InternetStreamGetStreamInformation[ obj] returns the stream information for a stream.";

AudioFileStreamTools`InternetStreamOpenRead::usage = "AudioFileStreamTools`InternetStreamOpenRead[ url] or AudioFileStreamTools`FileStreamOpenRead[ url, func] opens a read stream to the url specified. An AudioFileStreamTools`FileSteamObject is returned.";
AudioFileStreamTools`InternetStreamGetBufferedRange::usage = "AudioFileStreamTools`InternetStreamGetBufferedRange[ obj] returns the range of buffered audio frames as {startPosition, endPosition}";
AudioFileStreamTools`InternetStreamDownloadStatus::usage = "AudioFileStreamTools`InternetStreamDownloadStatus[ obj] returns the current status of the download, either \"InProgress\", \"Complete\", or \"Aborted\"";
AudioFileStreamTools`InternetStreamDownloadPercent::usage = "AudioFileStreamTools`InternetStreamDownloadPercent[ obj] returns the current percentage of the download as a Real from 0 to 1.";

AudioFileStreamTools`FileStreamGetTags::usage = "AudioFileStreamTools`FileStreamGetTags[ obj, type] opens a MetaTag object for the stream containing tags of the specified type.";
AudioFileStreamTools`InternetStreamGetTags::usage = "AudioFileStreamTools`InternetStreamGetTags[ obj, type] opens a MetaTag object for the stream containing tags of the specified type.";
AudioFileStreamTools`MetaTag::usage = "AudioFileStreamTools`MetaTag[] is an object containing tags of the specified type.";

General::nostream = "There is no stream open for `1`.";
General::invalidargs = "The arguments entered for this function are invalid.";
General::openreadfail = "The format of file \"`1`\" is not supported, or the file is empty.";
General::invalidcontainer = "The \"ContainerType\" \"`1`\" is invalid.";
General::nodirectory = "The directory \"`1`\" does not exist.";
General::deletionoptionsimmutable = "The file deletion options (\"DeleteFileOnClose\" and \"DeleteFileOnExit\") have already been set for this file.";
General::streamtypeconflict = "Cannot open \"`3`\" as a \"`1`\" stream. It is currently open as a \"`2`\" stream.";
General::nowriteperm = "Unable to open the file \"`1`\" for writing. Please check your access permissions for the file or directory.";
General::noreadperm = "Unable to open the file \"`1`\" for reading. Please check your access permissions for the file or directory.";

AudioFileStreamTools`FileStreamOpenRead::nofile = "The file \"`1`\" does not exist.";
AudioFileStreamTools`FileStreamOpenWrite::openwritefail = "Failed to open file for writing due to invalid arguments for the specified file format."; (* TODO: Need different error types returned from C *)
AudioFileStreamTools`FileStreamOpenWrite::currentlyopen = "Failed to open \"`1`\" for writing. The file is currently open with another stream.";
AudioFileStreamTools`FileStreamGetMetaInformation::noinfo = "\"`1`\" metainformation does not exist for this stream.";
AudioFileStreamTools`FileStreamGetStreamInformation::noinfo = "\"`1`\" stream information does not exist for this stream.";
AudioFileStreamTools`FileStreamWrite::dimensionmismatch = "The number of dimensions of the audio data does not match the number of channels for stream `1`.";
AudioFileStreamTools`FileStreamWrite::containermismatch = "Audio data container type does not match the stream's data container type, `1`.";
AudioFileStreamTools`FileStreamWrite::invalidtype = "The audio data type is not valid.";
AudioFileStreamTools`FileStreamSetReadPosition::invalidposition = "The position `1` is invalid. It must be between 1 and the EndOfFile inclusive, where the EndOfFile is the \"FrameCount\"+1.";
AudioFileStreamTools`FileStreamSetReadPosition::stmrng = "Cannot set the current point in stream `1` to position `2`. The requested position exceeds the current number of frames in the stream.";
AudioFileStreamTools`FileStreamReadN::reachedendoffile = "Attempted to read past the current EndOfFile. The data output has been truncated.";
AudioFileStreamTools`FileStreamReadN::positionpastendoffile = "The current read position is past EndOfFile. No data could be read.";
AudioFileStreamTools`FileStreamReadN::numframesoutofbounds = "Attempted to read an invalid number of frames. The number of frames must be between 1 and 2147483647 inclusive.";


AudioFileStreamTools`InternetStreamOpenRead::filenotfound = "The file located at \"`1`\" was not found. Check that the URL is valid and that you have permission to access it.";
AudioFileStreamTools`InternetStreamOpenRead::forbidden = "The server returned a \"403 Forbidden\" response to the request for \"`1`\". Check that the URL is valid and that you have permission to access it.";
AudioFileStreamTools`InternetStreamOpenRead::authenticationrequired = "The server returned a \"401 Unauthorized \" response to the request for \"`1`\". Check that you have supplied the necessary credentials, that the URL is valid, and that you have permission to access it.";
AudioFileStreamTools`InternetStreamOpenRead::servernotfound = "Failed to resolve the server for the request of \"`1`\". Check that the URL is valid.";
AudioFileStreamTools`InternetStreamOpenRead::invalidsslcertificate = "The SSL certificate for the server hosting \"`1`\" is invalid. To attempt to connect using the invalid certificate, set the following option: \"VerifySSLCertificate\" -> False";
AudioFileStreamTools`InternetStreamOpenRead::unknownerror = "An unknown error occurred when attempting to access \"`1`\" and the connection attempt was aborted. Check that the URL is valid and that you have permission to access it.";
AudioFileStreamTools`InternetStreamOpenRead::unsupportedprotocol = "The protocol specified by \"`1`\" is not supported. Supported procols are HTTP and HTTPS.";
AudioFileStreamTools`InternetStreamOpenRead::timedout = "The connection attempt timed out when attempting to access \"`1`\". Check that the URL is valid and that you have permission to access it.";
AudioFileStreamTools`InternetStreamOpenRead::couldnotconnect = "Could not connect to the server for the request \"`1`\". Check that the URL is valid and that your Internet connection is functioning correctly.";
AudioFileStreamTools`InternetStreamOpenRead::readerror = "An error occured when reading data from \"`1`\".";
AudioFileStreamTools`InternetStreamOpenRead::filepathignored = "The \"FilePath\" option, \"`1`\", when opening the URL, \"`2`\", was ignored since this URL is already opened with another stream. This stream is a reference to the original stream.";
AudioFileStreamTools`InternetStreamOpenRead::openreadfailfilepath = "Failed to open the stream to the URL, \"`2`\". The \"FilePath\", \"`1`\", is in use by another URL.";
AudioFileStreamTools`InternetStreamOpenRead::authfailed = "Authentication attempts failed.";

AudioFileStreamTools`InternetStreamOpenRead::proxyauthenticationrequired = "The proxy server, \"`1`\", returned a \"407 Proxy authentication required \" response. Check that you have supplied the necessary credentials, that the proxy server is valid, and that you have permission to access it.";
AudioFileStreamTools`InternetStreamOpenRead::proxyservernotfound = "Failed to resolve the proxy server \"`1`\". Check that the URL is valid.";
AudioFileStreamTools`InternetStreamOpenRead::proxyunknownerror = "An unknown error occurred when attempting to access the proxy server \"`1`\" and the connection attempt was aborted. Check that the URL is valid and that you have permission to access it.";
AudioFileStreamTools`InternetStreamOpenRead::proxyinvalidsslcertificate = "The SSL certificate for the proxy server \"`1`\" is invalid. To attempt to connect using the invalid certificate, set the following option: \"VerifySSLCertificate\" -> False";
AudioFileStreamTools`InternetStreamOpenRead::proxyunsupportedprotocol = "The connection attempt to the proxy server, \"`1`\", failed. The specified protocol in the request URL is not supported. Supported procols are HTTP and HTTPS.";
AudioFileStreamTools`InternetStreamOpenRead::proxytimedout = "The connection attempt timed out when attempting to access the proxy server, \"`1`\". Check that the URL is valid and that you have permission to access it.";
AudioFileStreamTools`InternetStreamOpenRead::proxycouldnotconnect = "Could not connect to the proxy server, \"`1`\". Check that the URL is valid and that your Internet connection is functioning correctly.";
AudioFileStreamTools`InternetStreamOpenRead::proxyreaderror = "An error occured when reading data from the proxy server, \"`1`\".";

AudioFileStreamTools`InternetStreamOpenRead::invalidsoundcloudurl = "The SoundCloud URL \"`1`\" is not valid and could not be parsed.";
AudioFileStreamTools`InternetStreamOpenRead::soundcloudauthfailed = "Failed to authenticate with SoundCloud.";
AudioFileStreamTools`InternetStreamOpenRead::usernamepasswordformat = "Both \"UserName\" and \"Password\" must be specified together, or not at all.";
AudioFileStreamTools`InternetStreamOpenRead::proxyusernamepasswordformat = "Both \"ProxyUserName\" and \"ProxyPassword\" must be specified together, or not at all.";
AudioFileStreamTools`InternetStreamOpenRead::usernamepasswordimmutable = "The \"UserName\" and \"Password\" options have already been set for this URL.";
AudioFileStreamTools`InternetStreamOpenRead::proxyusernamepasswordimmutable = "The \"ProxyUserName\" and \"ProxyPassword\" options have already been set for this URL.";
AudioFileStreamTools`InternetStreamOpenRead::nodeletionoptions = "Deletion options (\"DeleteFileOnClose\" and \"DeleteFileOnExit\") should not be specified if \"FilePath\" is not specified.";
AudioFileStreamTools`InternetStreamOpenRead::flacnotsupported = "The FLAC format is not currently supported for decoding as an InternetStream. If a \"FilePath\" was specified, the file will be downloaded and saved. It is not possible to decode without closing after the download completes and reopening with FileStreamOpenRead[].";

AudioFileStreamTools`InternetStreamGetBufferedRange::invalidtype = "The index type `1` is not valid, must be either \"Frames\" or \"Bytes\"";

AudioFileStreamTools`FileStreamGetTags::notsupported = "This audio format does not support `1` tags.";
AudioFileStreamTools`FileStreamGetTags::internetstreammemory = "MetaTag objects are not yet available for streams using a memory buffer (streams not using a file specified with the \"FilePath\" option).";
AudioFileStreamTools::missingdependency = "The dependency \"`1`\" was not found. AudioFileStreamTools may not function properly.";

AudioFileStreamTools`MetaTag::invalid = "The MetaTag object \"`1`\" does not belong to any open streams.";
AudioFileStreamTools`MetaTag::notsupported = "This operation is not supported for \"`1`\" tags.";
AudioFileStreamTools`MetaTag::invalidframe = "The specified frame was not found.";
AudioFileStreamTools`MetaTag::internetstream = "This MetaTag object belongs to an Internet stream and cannot be modified.";
AudioFileStreamTools`MetaTag::nowriteperm = "Unable to modify the MetaTag object. Please check your access permissions for the file or directory.";

Begin["`Private`"]
(* Implementation of the package *)

(*Set to True to have MetaTag ByteArrays always return empty. Used for testing, this should always be False in production. *)
$testByteArrays = False;

(*Set to False to disable getting Operating System configured proxies for Windows and OSX through CURLLink.
This should only be disabled if a change in CURLLink breaks functionality.
Environment variable proxies will still function.*)
$enableOperatingSystemProxies = True;

(*Set to False to disable SoundCloud ServiceConnect functionality.*)
$enableSoundCloudFunctionality = False;

$packageFile = $InputFileName;
$libraryExtension = Switch[$OperatingSystem, "Windows", ".dll", "MacOSX", ".dylib", "Unix", ".so"];
$CURLLinkDir = FileNameJoin[{$InstallationDirectory, "SystemFiles", "Links", "CURLLink"}];
$CURLLinkLibDir = FileNameJoin[{$CURLLinkDir, "LibraryResources", $SystemID}];
$MP3ToolsDir = FileNameJoin[{$InstallationDirectory, "SystemFiles", "Links", "MP3Tools"}];
$MP3ToolsLibDir = FileNameJoin[{$MP3ToolsDir, "LibraryResources", $SystemID}];
$CACERT = FileNameJoin[{$CURLLinkDir, "SSL", "cacert.pem"}];
$useCACERT = 1;

(*  Do not set CURLOPT_CAINFO, now that OSX version of libcurl uses
    darwinssl.
    This option was relevent when we were using openssl for https requests.
    https://stash.wolfram.com/projects/PAC/repos/curllink/commits/6c93dcd10c8823222963a2870010808edc4798d3

    For Mac we use OSX native Keychain, after switching to darwinssl, this is only relevent when using openssl.
*)
If[$SystemID=="MacOSX-x86-64",
    $useCACERT = 0;
];

$curlLibFileNames = Switch[$OperatingSystem, "Windows", {"libcurl.dll"}, "MacOSX", {"libcurl.dylib"}, "Unix", {"libcurl.so"}];
(* The libraries must be loaded in this order: Windows: "libeay32.dll", "ssleay32.dll", "libssh2.dll", "libcurl.dll" OSX: "libcrypto.dylib", "libssl.dylib", "libssh2.dylib", "libcurl.dylib"*)
(* CURLLink`CURLInitialize[]; should now take care of loading the libcurl dependencies. *)
$curlLibs = FileNameJoin[{$CURLLinkLibDir, #}]& /@ $curlLibFileNames;
$lameLibFileNames = Switch[$OperatingSystem, "Windows", {"libmp3lame.dll"}, "MacOSX", {"libmp3lame.dylib"}, "Unix", {"libmp3lame.so"}]; (* msvcr100.dll needs to be loaded before libmp3lame.dll *)
$lameLibs = FileNameJoin[{$MP3ToolsLibDir, #}]& /@ $lameLibFileNames;
$adapterLibFileName = Switch[$OperatingSystem, "Windows", "AudioFileStreamTools.dll", "MacOSX", "AudioFileStreamTools.dylib", "Unix", "AudioFileStreamTools.so"];
$otherLibFileNames = Switch[$OperatingSystem, "Windows", {}, "MacOSX", {}, "Unix", {}];
$adapterLibDir = FileNameJoin[{FileNameTake[$packageFile, {1,-2}], "LibraryResources", $SystemID}];
$adapterLib = FileNameJoin[{$adapterLibDir, $adapterLibFileName}];
$otherLibs = (FileNameJoin[{$adapterLibDir, #}])& /@ $otherLibFileNames;
$openStreams = Association[];
$$adapterInitialized = False;
$signedInt32BitMax = 2147483647;
$metaInformationFields = <|"BitDepth" -> 1, "ChannelCount" -> 2, "FrameCount" -> 3, "SampleRate" -> 4, "TotalFrameCount" -> 5|>;
$streamInformationFields = {"FilePath", "DataContainer", "InternetStream", "InternetStreamType", "URL"};
$proxyEnvVars = <|"http_proxy" -> 1, "https_proxy" -> 2, "ftp_proxy" -> 3, "socks_proxy" -> 4, "all_proxy" -> 5, "no_proxy" -> 6|>;
$tagsKey = "Tags";
$tagTypes = <|"ID3v2" -> 0, "ID3v1" -> 1, "Xiph" -> 2, "APE" -> 3|>

$eventTimingCodesAssociation = <|0 -> "Padding", 1 -> "EndOfInitialSilence", 2 -> "IntroStart",
    3 -> "MainPartStart", 4 -> "OutroStart", 5 -> "OutroEnd",
    6 -> "VerseStart", 7 -> "RefrainStart", 8 -> "InterludeStart",
    9 -> "ThemeStart", 10 -> "VariationStart", 11 -> "KeyChange",
    12 -> "TimeChange", 13 -> "MomentaryUnwantedNoise",
    14 -> "SustainedNoise", 15 -> "SustainedNoiseEnd", 16 -> "IntroEnd",
    17 -> "MainPartEnd", 18 -> "VerseEnd", 19 -> "RefrainEnd",
    20 -> "ThemeEnd", 21 -> "Profanity", 22 -> "ProfanityEnd",
    224 -> "NotPredefinedSynch0", 225 -> "NotPredefinedSynch1",
    226 -> "NotPredefinedSynch2", 227 -> "NotPredefinedSynch3",
    228 -> "NotPredefinedSynch4", 229 -> "NotPredefinedSynch5",
    230 -> "NotPredefinedSynch6", 231 -> "NotPredefinedSynch7",
    232 -> "NotPredefinedSynch8", 233 -> "NotPredefinedSynch9",
    234 -> "NotPredefinedSynchA", 235 -> "NotPredefinedSynchB",
    236 -> "NotPredefinedSynchC", 237 -> "NotPredefinedSynchD",
    238 -> "NotPredefinedSynchE", 239 -> "NotPredefinedSynchF",
    253 -> "AudioEnd", 254 -> "AudioFileEnds",
    "Padding" -> 0, "EndOfInitialSilence" -> 1, "IntroStart" -> 2,
    "MainPartStart" -> 3, "OutroStart" -> 4, "OutroEnd" -> 5,
    "VerseStart" -> 6, "RefrainStart" -> 7, "InterludeStart" -> 8,
    "ThemeStart" -> 9, "VariationStart" -> 10, "KeyChange" -> 11,
    "TimeChange" -> 12, "MomentaryUnwantedNoise" -> 13, "SustainedNoise"
    -> 14, "SustainedNoiseEnd" -> 15, "IntroEnd" -> 16, "MainPartEnd" ->
    17, "VerseEnd" -> 18, "RefrainEnd" -> 19, "ThemeEnd" -> 20,
    "Profanity" -> 21, "ProfanityEnd" -> 22, "NotPredefinedSynch0" ->
    224, "NotPredefinedSynch1" -> 225, "NotPredefinedSynch2" -> 226,
    "NotPredefinedSynch3" -> 227, "NotPredefinedSynch4" -> 228,
    "NotPredefinedSynch5" -> 229, "NotPredefinedSynch6" -> 230,
    "NotPredefinedSynch7" -> 231, "NotPredefinedSynch8" -> 232,
    "NotPredefinedSynch9" -> 233, "NotPredefinedSynchA" -> 234,
    "NotPredefinedSynchB" -> 235, "NotPredefinedSynchC" -> 236,
    "NotPredefinedSynchD" -> 237, "NotPredefinedSynchE" -> 238,
    "NotPredefinedSynchF" -> 239, "AudioEnd" -> 253, "AudioFileEnds" -> 254|>;

$pictureTypes = <|0 -> "Other", "Other" -> 0, 1 -> "FileIcon", "FileIcon" -> 1,
    2 -> "OtherFileIcon", "OtherFileIcon" -> 2, 3 -> "FrontCover",
    "FrontCover" -> 3, 4 -> "BackCover", "BackCover" -> 4,
    5 -> "LeafletPage", "LeafletPage" -> 5, 6 -> "Media", "Media" -> 6,
    7 -> "LeadArtist", "LeadArtist" -> 7, 8 -> "Artist", "Artist" -> 8,
    9 -> "Conductor", "Conductor" -> 9, 10 -> "Band", "Band" -> 10,
    11 -> "Composer", "Composer" -> 11, 12 -> "Lyricist",
    "Lyricist" -> 12, 13 -> "RecordingLocation",
    "RecordingLocation" -> 13, 14 -> "DuringRecording",
    "DuringRecording" -> 14, 15 -> "DuringPerformance",
    "DuringPerformance" -> 15, 16 -> "MovieScreenCapture",
    "MovieScreenCapture" -> 16, 17 -> "ColouredFish",
    "ColouredFish" -> 17, 18 -> "Illustration", "Illustration" -> 18,
    19 -> "BandLogo", "BandLogo" -> 19, 20 -> "PublisherLogo",
    "PublisherLogo" -> 20|>;

$lyricsTypes = <|0 -> "Other", "Other" -> 0, 1 -> "Lyrics", "Lyrics" -> 1,
    2 -> "TextTranscription", "TextTranscription" -> 2, 3 -> "Movement",
    "Movement" -> 3, 4 -> "Events", "Events" -> 4, 5 -> "Chord",
    "Chord" -> 5, 6 -> "Trivia", "Trivia" -> 6, 7 -> "WebpageUrls",
    "WebpageUrls" -> 7, 8 -> "ImageUrls", "ImageUrls" -> 8|>;

$eventTimestampFormat = <|0 -> "Unknown", 1 -> "AbsoluteMpegFrames", 2 -> "AbsoluteMilliseconds",
    "Unknown" -> 0, "AbsoluteMpegFrames" -> 1, "AbsoluteMilliseconds" -> 2|>;

$channelTypes = <|0 -> "Other", "Other" -> 0, 1 -> "MasterVolume",
    "MasterVolume" -> 1, 2 -> "FrontRight", "FrontRight" -> 2,
    3 -> "FrontLeft", "FrontLeft" -> 3, 4 -> "BackRight",
    "BackRight" -> 4, 5 -> "BackLeft", "BackLeft" -> 5,
    6 -> "FrontCentre", "FrontCentre" -> 6, 7 -> "BackCentre",
    "BackCentre" -> 7, 8 -> "Subwoofer", "Subwoofer" -> 8|>;

$metaTagElementsAssociation = <|
    "T***" -> <|"Elements" -> {"Values"}, "Alias" -> <||>,
        "Description" -> "Text Information"|>,

    "TXXX" -> <|"Elements" -> {"Values", "Description"}, "Alias" -> <||>,
        "Description" -> "User Defined Text Information"|>,

    "APIC" -> <|
        "Elements" -> {"Picture", "MimeType", "PictureType",
            "Description"}, "Alias" -> <||>,
        "Description" -> "Attached Picture"|>,

    "UFID" -> <|"Elements" -> {"Owner", "Identifier"}, "Alias" -> <||>,
        "Description" -> "Unique File Identifier"|>,

    "OWNE" -> <|"Elements" -> {"PricePaid", "PurchaseDate", "Seller"},
        "Alias" -> <||>, "Description" -> "Ownership Information"|>,

    "PRIV" -> <|"Elements" -> {"Owner", "Data"}, "Alias" -> <||>,
        "Description" -> "Private Information"|>,

    "POPM" -> <|"Elements" -> {"Email", "Rating", "Counter"},
        "Alias" -> <||>, "Description" -> "Popularimeter"|>,

    "GEOB" -> <|
        "Elements" -> {"Object", "MimeType", "FileName", "Description"},
        "Alias" -> <||>,
        "Description" -> "General Encapsulated Object"|>,

    "COMM" -> <|"Elements" -> {"Language", "Description", "Text"},
        "Alias" -> <||>, "Description" -> "Comments"|>,

    "CHAP" -> <|
        "Elements" -> {"StartTime", "EndTime", "StartOffset",
            "EndOffset", "EmbeddedFrames", "Identifier"}, "Alias" -> <||>,
        "Description" -> "Chapter"|>,

    "CTOC" -> <|
        "Elements" -> {"Ordered", "TopLevel", "EmbeddedFrames",
            "Identifier", "ChildElements"}, "Alias" -> <||>,
        "Description" -> "Table of Contents"|>,

    "ETCO" -> <|"Elements" -> {"TimestampFormat", "SynchedEvents"}, "Alias" -> <||>,
        "Description" -> "Event Timing Codes"|>,

    "RVA2" -> <|"Elements" -> {"Description", "Channels"}, "Alias" -> <||>,
        "Description" -> "Relative Volume Adjustment"|>,

    "SYLT" -> <|
        "Elements" -> {"Language", "Description", "TimestampFormat",
            "LyricsType", "SynchedText"}, "Alias" -> <||>,
        "Description" -> "Synchronised Lyrics or Text"|>,

    "USLT" -> <|"Elements" -> {"Language", "Description", "Text"},
        "Alias" -> <||>,
        "Description" -> "Unsynchronised Lyrics or Text"|>,

    "W***" -> <|"Elements" -> {"URL"}, "Alias" -> <||>,
        "Description" -> "URL Link"|>,

    "WXXX" -> <|"Elements" -> {"URL", "Description"}, "Alias" -> <||>,
        "Description" -> "User Defined URL Link"|>|>;

$metaTagElements = <|
    "Channels" -> <|"Type" -> Association, "AllowedValues" -> DeleteCases[Keys[$channelTypes], _Integer] -> <|"PeakVolume" -> ByteArray, "BitsRepresentingPeak" -> Integer, "VolumeAdjustment" -> Integer|>|>,
    "SynchedEvents" -> <|"Type" -> Association, "AllowedValues" -> Integer -> DeleteCases[Keys[$eventTimingCodesAssociation], _Integer]|>,
    "SynchedText" -> <|"Type" -> Association, "AllowedValues" -> Integer -> String|>,
    "ChildElements" -> <|"Type" -> List, "AllowedValues" -> ByteArray|>,
    "PictureType" -> <|"Type" -> String, "AllowedValues" -> DeleteCases[Keys[$pictureTypes], _Integer]|>,
    "LyricsType" -> <|"Type" -> String, "AllowedValues" -> DeleteCases[Keys[$lyricsTypes], _Integer]|>,
    "TimestampFormat" -> <|"Type" -> String, "AllowedValues" -> DeleteCases[Keys[$eventTimestampFormat], _Integer]|>,
    "Description" -> <|"Type" -> String, "AllowedValues" -> All|>,
    "Values" -> <|"Type" -> List, "AllowedValues" -> String|>,
    "Language" -> <|"Type" -> String, "AllowedValues" -> {Length, 3}|>,
    "FileName" -> <|"Type" -> String, "AllowedValues" -> All|>,
    "MimeType" -> <|"Type" -> String, "AllowedValues" -> All|>,
    "Picture" -> <|"Type" -> ByteArray, "AllowedValues" -> All|>,
    "Seller" -> <|"Type" -> String, "AllowedValues" -> All|>,
    "PurchaseDate" -> <|"Type" -> String, "AllowedValues" -> All|>,
    "PricePaid" -> <|"Type" -> String, "AllowedValues" -> All|>,
    "Email" -> <|"Type" -> String, "AllowedValues" -> All|>,
    "Object" -> <|"Type" -> ByteArray, "AllowedValues" -> All|>,
    "Owner" -> <|"Type" -> String, "AllowedValues" -> All|>,
    "Data" -> <|"Type" -> ByteArray, "AllowedValues" -> All|>,
    "Identifier" -> <|"Type" -> ByteArray, "AllowedValues" -> All|>,
    "URL" -> <|"Type" -> String, "AllowedValues" -> All|>,
    "Text" -> <|"Type" -> String, "AllowedValues" -> All|>,
    "EndOffset" -> <|"Type" -> Integer, "AllowedValues" -> All|>,
    "StartOffset" -> <|"Type" -> Integer, "AllowedValues" -> All|>,
    "EndTime" -> <|"Type" -> Integer, "AllowedValues" -> All|>,
    "StartTime" -> <|"Type" -> Integer, "AllowedValues" -> All|>,
    "Ordered" -> <|"Type" -> Booleans,
        "AllowedValues" -> {True, False}|>,
    "TopLevel" -> <|"Type" -> Booleans,
        "AllowedValues" -> {True, False}|>
    |>

toByteArray[dat_] := Module[{},
    If[$testByteArrays, Return[ByteArray[]];];
    Return[ByteArray[dat]];
];

assertDependency[depencencyPath_] := If[!FileExistsQ[depencencyPath], Message[AudioFileStreamTools::missingdependency, depencencyPath];];

loadAdapter[]:= If[ !$$adapterInitialized,
    Scan[(assertDependency[#])&, $otherLibs];
    (* $SystemID guard here and in LibraryLoad[...] below added to handle RPi platform's use of System library dependencies *)
    If[$SystemID =!= "Linux-ARM", Scan[(assertDependency[#])&, $lameLibs];];
    If[$SystemID =!= "Linux-ARM", Scan[(assertDependency[#])&, $curlLibs];];
    assertDependency[$CACERT];

    Scan[(LibraryLoad[#])&, $otherLibs];
    If[$SystemID =!= "Linux-ARM", Scan[(LibraryLoad[#])&, $lameLibs];];
    Needs["CURLLink`"];
    CURLLink`CURLInitialize[];

    lfFileStreamOpenRead = LibraryFunctionLoad[ $adapterLib, "FileStreamOpenRead", LinkObject, LinkObject];
    lfFileStreamOpenReadExtension = LibraryFunctionLoad[ $adapterLib, "FileStreamOpenReadExtension", LinkObject, LinkObject];
    lfFileStreamReadN = LibraryFunctionLoad[ $adapterLib, "FileStreamReadN", {Integer, Integer}, {Real, _}];
    lfFileStreamReadNRawArray = LibraryFunctionLoad[ $adapterLib, "FileStreamReadNRA", {Integer, Integer}, {"RawArray"}];
    lfFileStreamReadUntilEndOFFile= LibraryFunctionLoad[ $adapterLib, "FileStreamReadUntilEndOFFile", {Integer}, {Real, _}];
    lfFileStreamReadUntilEndOFFileRA= LibraryFunctionLoad[ $adapterLib, "FileStreamReadUntilEndOFFileRA", {Integer}, {"RawArray"}];
    lfFileStreamOpenWrite = LibraryFunctionLoad[ $adapterLib, "FileStreamOpenWrite", LinkObject, LinkObject];
    lfFileStreamWrite = LibraryFunctionLoad[$adapterLib,"FileStreamWrite",{Integer,{_Real,_,"Constant"}},{Integer}];
    lfFileStreamWriteRawArray = LibraryFunctionLoad[$adapterLib,"FileStreamWriteRA",{Integer,{"RawArray","Constant"}},{Integer}];
    lfFileStreamSetReadPosition = LibraryFunctionLoad[ $adapterLib, "FileStreamSetReadPosition", {Integer, Integer}, {Integer}];
    lfFileStreamGetReadPosition = LibraryFunctionLoad[ $adapterLib, "FileStreamGetReadPosition", {Integer}, {Integer}];
    lfFileStreamClose=LibraryFunctionLoad[$adapterLib,"FileStreamClose",{Integer},{Integer}];
    lfInternetStreamOpenReadMemory = LibraryFunctionLoad[ $adapterLib, "InternetStreamOpenReadMemory", LinkObject, LinkObject];
    lfInternetStreamOpenReadFile = LibraryFunctionLoad[ $adapterLib, "InternetStreamOpenReadFile", LinkObject, LinkObject];
    lfInternetStreamStartDownload=LibraryFunctionLoad[$adapterLib, "InternetStreamStartDownload", {Integer, Integer, Integer}, Integer];
    (*lfInternetStreamStopDownload=LibraryFunctionLoad[$adapterLib, "InternetStreamStopDownload", {Integer}, Integer];*)
    lfInternetStreamDownloadStatus=LibraryFunctionLoad[$adapterLib, "InternetStreamDownloadStatus", {Integer}, Integer];
    lfInternetStreamCurrentDownloadSize=LibraryFunctionLoad[$adapterLib, "InternetStreamCurrentDownloadSize", {Integer}, Real];
    lfInternetStreamFinalDownloadSize=LibraryFunctionLoad[$adapterLib, "InternetStreamFinalDownloadSize", {Integer}, Real];
    lfInternetStreamWaitForTransferInitialization=LibraryFunctionLoad[$adapterLib, "InternetStreamWaitForTransferInitialization", {Integer}, Integer];
    (*
	lfFileStreamGetWritePosition
	lfFileStreamSetWritePosition
	*)
    lfFileStreamGetMetaInformation = LibraryFunctionLoad[ $adapterLib, "FileStreamGetMetaInformation", LinkObject, LinkObject];

    lfGetEnvironmentProxySettings = LibraryFunctionLoad[ $adapterLib, "GetEnvironmentProxySettings", LinkObject, LinkObject];
    lfURLIsOpenedAsInternetStream = LibraryFunctionLoad[ $adapterLib, "URLIsOpenedAsInternetStream", LinkObject, LinkObject];
    lfFileIsOpenedAsInternetStream = LibraryFunctionLoad[ $adapterLib, "FileIsOpenedAsInternetStream", LinkObject, LinkObject];
    lfURLOpenedAsInternetStreamType = LibraryFunctionLoad[ $adapterLib, "URLOpenedAsInternetStreamType", LinkObject, LinkObject];
    lfFileOpenedAsInternetStreamType = LibraryFunctionLoad[ $adapterLib, "FileOpenedAsInternetStreamType", LinkObject, LinkObject];
    lfInternetStreamFilePathGetURL = LibraryFunctionLoad[ $adapterLib, "InternetStreamFilePathGetURL", LinkObject, LinkObject];
    lfInternetStreamURLGetFilePath = LibraryFunctionLoad[ $adapterLib, "InternetStreamURLGetFilePath", LinkObject, LinkObject];
    lfStreamHasOperatingSystemWritePermissions = LibraryFunctionLoad[ $adapterLib, "StreamHasOperatingSystemWritePermissions", {Integer}, Integer];

    lfFileStreamOpenTags = LibraryFunctionLoad[ $adapterLib, "FileStreamOpenTags", {Integer}, "Void"];
    lfFileStreamCloseTags = LibraryFunctionLoad[ $adapterLib, "FileStreamCloseTags", {Integer, Integer}, "Void"];

    lfFileStreamRemoveID3v2Frame = LibraryFunctionLoad[$adapterLib, "FileStreamRemoveID3v2Frame", {Integer, {_Integer,_,"Constant"}}, "Void"];
    lfFileStreamRemoveTag = LibraryFunctionLoad[$adapterLib, "FileStreamRemoveTag", {Integer, Integer}, "Void"];
    lfFileStreamAddID3v2Frame = LibraryFunctionLoad[$adapterLib, "FileStreamAddID3v2Frame", {Integer, {_Integer,_,"Constant"}, {_Integer,_,"Constant"}}, "Void"];

    lfFileStreamClearID3v2TableOFContentsFrameChildElements = LibraryFunctionLoad[$adapterLib, "FileStreamClearID3v2TableOFContentsFrameChildElements",{Integer, Integer, {_Integer,_,"Constant"}}, "Void"];
    lfFileStreamClearID3v2FrameSynchedText = LibraryFunctionLoad[$adapterLib, "FileStreamClearID3v2FrameSynchedText",{Integer, Integer, {_Integer,_,"Constant"}}, "Void"];
    lfFileStreamClearID3v2FrameChannels = LibraryFunctionLoad[$adapterLib, "FileStreamClearID3v2FrameChannels",{Integer, Integer, {_Integer,_,"Constant"}}, "Void"];

    lfFileStreamGetID3v2TableOFContentsFrameChildElementCount = LibraryFunctionLoad[ $adapterLib, "FileStreamGetID3v2TableOFContentsFrameChildElementCount", {Integer, Integer, {_Integer,_,"Constant"}}, Integer];
    lfFileStreamGetID3v2TableOFContentsFrameChildElementIdentifier = LibraryFunctionLoad[ $adapterLib, "FileStreamGetID3v2TableOFContentsFrameChildElementIdentifier", {Integer, Integer, Integer, {_Integer,_,"Constant"}}, {Integer, _}];

    lfFileStreamGetID3v2FramesList = LibraryFunctionLoad[ $adapterLib, "FileStreamGetID3v2FramesList", {Integer}, {Integer, _}];
    lfFileStreamGetID3v2FrameID = LibraryFunctionLoad[ $adapterLib, "FileStreamGetID3v2FrameID", {Integer, Integer, {_Integer,_,"Constant"}}, {Integer, _}];
    lfFileStreamGetID3v2FrameValues = LibraryFunctionLoad[ $adapterLib, "FileStreamGetID3v2FrameValues", {Integer, Integer, {_Integer,_,"Constant"}}, {Integer, _}];
    lfFileStreamGetID3v2FrameDescription = LibraryFunctionLoad[ $adapterLib, "FileStreamGetID3v2FrameDescription", {Integer, Integer, {_Integer,_,"Constant"}}, {Integer, _}];

    lfFileStreamGetID3v2ChapterFrameValues = LibraryFunctionLoad[ $adapterLib, "FileStreamGetID3v2ChapterFrameValues", {Integer, Integer, {_Integer,_,"Constant"}}, {Integer, _}];
    lfFileStreamGetID3v2ChapterFrameEmbeddedFramesList = LibraryFunctionLoad[ $adapterLib, "FileStreamGetID3v2ChapterFrameEmbeddedFramesList", {Integer, Integer, {_Integer,_,"Constant"}}, {Integer, _}];
    lfFileStreamGetID3v2FrameLanguage = LibraryFunctionLoad[ $adapterLib, "FileStreamGetID3v2FrameLanguage", {Integer, Integer, {_Integer,_,"Constant"}}, {Integer, _}];
    lfFileStreamGetID3v2FrameText = LibraryFunctionLoad[ $adapterLib, "FileStreamGetID3v2FrameText", {Integer, Integer, {_Integer,_,"Constant"}}, {Integer, _}];

    lfFileStreamGetID3v2FrameTimeStampFormat = LibraryFunctionLoad[ $adapterLib, "FileStreamGetID3v2FrameTimeStampFormat", {Integer, Integer, {_Integer,_,"Constant"}}, Integer];
    lfFileStreamGetID3v2FrameSynchedEvents = LibraryFunctionLoad[ $adapterLib, "FileStreamGetID3v2FrameSynchedEvents", {Integer, Integer, {_Integer,_,"Constant"}}, {Integer, _}];

    lfFileStreamGetID3v2FrameMimeType = LibraryFunctionLoad[ $adapterLib, "FileStreamGetID3v2FrameMimeType", {Integer, Integer, {_Integer,_,"Constant"}}, {Integer, _}];
    lfFileStreamGetID3v2FrameFileName = LibraryFunctionLoad[ $adapterLib, "FileStreamGetID3v2FrameFileName", {Integer, Integer, {_Integer,_,"Constant"}}, {Integer, _}];
    lfFileStreamGetID3v2FrameObject = LibraryFunctionLoad[ $adapterLib, "FileStreamGetID3v2FrameObject", {Integer, Integer, {_Integer,_,"Constant"}}, {Integer, _}];
    lfFileStreamGetID3v2FramePicture = LibraryFunctionLoad[ $adapterLib, "FileStreamGetID3v2FramePicture", {Integer, Integer, {_Integer,_,"Constant"}}, {Integer, _}];
    lfFileStreamGetID3v2FramePictureType = LibraryFunctionLoad[ $adapterLib, "FileStreamGetID3v2FramePictureType", {Integer, Integer, {_Integer,_,"Constant"}}, Integer];

    lfFileStreamGetID3v2FramePricePaid = LibraryFunctionLoad[ $adapterLib, "FileStreamGetID3v2FramePricePaid", {Integer, Integer, {_Integer,_,"Constant"}}, {Integer, _}];
    lfFileStreamGetID3v2FramePurchaseDate = LibraryFunctionLoad[ $adapterLib, "FileStreamGetID3v2FramePurchaseDate", {Integer, Integer, {_Integer,_,"Constant"}}, {Integer, _}];
    lfFileStreamGetID3v2FrameSeller = LibraryFunctionLoad[ $adapterLib, "FileStreamGetID3v2FrameSeller", {Integer, Integer, {_Integer,_,"Constant"}}, {Integer, _}];
    lfFileStreamGetID3v2FrameEmail = LibraryFunctionLoad[ $adapterLib, "FileStreamGetID3v2FrameEmail", {Integer, Integer, {_Integer,_,"Constant"}}, {Integer, _}];
    lfFileStreamGetID3v2FrameCounter = LibraryFunctionLoad[ $adapterLib, "FileStreamGetID3v2FrameCounter", {Integer, Integer, {_Integer,_,"Constant"}}, Integer];
    lfFileStreamGetID3v2FrameRating = LibraryFunctionLoad[ $adapterLib, "FileStreamGetID3v2FrameRating", {Integer, Integer, {_Integer,_,"Constant"}}, Integer];

    lfFileStreamGetID3v2FrameOwner = LibraryFunctionLoad[ $adapterLib, "FileStreamGetID3v2FrameOwner", {Integer, Integer, {_Integer,_,"Constant"}}, {Integer, _}];
    lfFileStreamGetID3v2FrameChannels = LibraryFunctionLoad[ $adapterLib, "FileStreamGetID3v2FrameChannels", {Integer, Integer, {_Integer,_,"Constant"}}, {Integer, _}];
    lfFileStreamGetID3v2FrameData = LibraryFunctionLoad[ $adapterLib, "FileStreamGetID3v2FrameData", {Integer, Integer, {_Integer,_,"Constant"}}, {Integer, _}];

    lfFileStreamGetID3v2FramePeakVolume = LibraryFunctionLoad[ $adapterLib, "FileStreamGetID3v2FramePeakVolume", {Integer, Integer, Integer, {_Integer,_,"Constant"}}, {Integer, _}];
    lfFileStreamGetID3v2FramePeakBits = LibraryFunctionLoad[ $adapterLib, "FileStreamGetID3v2FramePeakBits", {Integer, Integer, Integer, {_Integer,_,"Constant"}}, Integer];
    (*lfFileStreamGetID3v2FrameVolumeAdjustment = LibraryFunctionLoad[ $adapterLib, "FileStreamGetID3v2FrameVolumeAdjustment", {Integer, Integer, Integer, {_Integer,_,"Constant"}}, Real];*)
    lfFileStreamGetID3v2FrameVolumeAdjustmentIndex = LibraryFunctionLoad[ $adapterLib, "FileStreamGetID3v2FrameVolumeAdjustmentIndex", {Integer, Integer, Integer, {_Integer,_,"Constant"}}, Integer];

    lfFileStreamGetID3v2FrameLyricsType = LibraryFunctionLoad[ $adapterLib, "FileStreamGetID3v2FrameLyricsType", {Integer, Integer, {_Integer,_,"Constant"}}, Integer];
    lfFileStreamGetID3v2FrameTopLevel = LibraryFunctionLoad[ $adapterLib, "FileStreamGetID3v2FrameTopLevel", {Integer, Integer, {_Integer,_,"Constant"}}, Integer];
    lfFileStreamGetID3v2FrameOrdered = LibraryFunctionLoad[ $adapterLib, "FileStreamGetID3v2FrameOrdered", {Integer, Integer, {_Integer,_,"Constant"}}, Integer];

    lfFileStreamGetID3v2FrameSynchedTextTimes = LibraryFunctionLoad[ $adapterLib, "FileStreamGetID3v2FrameSynchedTextTimes", {Integer, Integer, {_Integer,_,"Constant"}}, {Integer, _}];
    lfFileStreamGetID3v2FrameSynchedTextList = LibraryFunctionLoad[ $adapterLib, "FileStreamGetID3v2FrameSynchedTextList", {Integer, Integer, {_Integer,_,"Constant"}}, {Integer, _}];
    lfFileStreamGetID3v2FrameIdentifier = LibraryFunctionLoad[ $adapterLib, "FileStreamGetID3v2FrameIdentifier", {Integer, Integer, {_Integer,_,"Constant"}}, {Integer, _}];
    lfFileStreamGetID3v2FrameURL = LibraryFunctionLoad[ $adapterLib, "FileStreamGetID3v2FrameURL", {Integer, Integer, {_Integer,_,"Constant"}}, {Integer, _}];

    (* ID3v2 Frame Setters *)

    lfFileStreamSetID3v2TableOFContentsFrameChildElements = LibraryFunctionLoad[$adapterLib,"FileStreamSetID3v2TableOFContentsFrameChildElements",{Integer, Integer, {_Integer,_,"Constant"}, {_Integer,_,"Constant"}},"Void"];
    lfFileStreamSetID3v2FrameSynchedText = LibraryFunctionLoad[$adapterLib,"FileStreamSetID3v2FrameSynchedText",{Integer, Integer, Integer, {_Integer,_,"Constant"}, {_Integer,_,"Constant"}},"Void"];
    lfFileStreamSetID3v2FrameSynchedEvents = LibraryFunctionLoad[$adapterLib,"FileStreamSetID3v2FrameSynchedEvents",{Integer, Integer, {_Integer,_,"Constant"}, {_Integer,_,"Constant"}},"Void"];

    lfFileStreamSetID3v2FrameChannel = LibraryFunctionLoad[$adapterLib,"FileStreamSetID3v2FrameChannel",{Integer, Integer, Integer, Integer, Integer, {_Integer,_,"Constant"}, {_Integer,_,"Constant"}},"Void"];

    lfFileStreamSetID3v2FramePictureType = LibraryFunctionLoad[ $adapterLib, "FileStreamSetID3v2FramePictureType", {Integer, Integer, Integer, {_Integer,_,"Constant"}}, "Void"];
    lfFileStreamSetID3v2FrameLyricsType = LibraryFunctionLoad[ $adapterLib, "FileStreamSetID3v2FrameLyricsType", {Integer, Integer, Integer, {_Integer,_,"Constant"}}, "Void"];
    lfFileStreamSetID3v2FrameTimeStampFormat = LibraryFunctionLoad[ $adapterLib, "FileStreamSetID3v2FrameTimeStampFormat", {Integer, Integer, Integer, {_Integer,_,"Constant"}}, "Void"];

    lfFileStreamSetID3v2FrameDescription = LibraryFunctionLoad[$adapterLib,"FileStreamSetID3v2FrameDescription",{Integer, Integer, {_Integer,_,"Constant"}, {_Integer,_,"Constant"}},"Void"];
    lfFileStreamSetID3v2FrameValues = LibraryFunctionLoad[$adapterLib,"FileStreamSetID3v2FrameValues",{Integer, Integer, {_Integer,_,"Constant"}, {_Integer,_,"Constant"}},"Void"];
    lfFileStreamSetID3v2FrameLanguage = LibraryFunctionLoad[$adapterLib,"FileStreamSetID3v2FrameLanguage",{Integer, Integer, {_Integer,_,"Constant"}, {_Integer,_,"Constant"}},"Void"];
    lfFileStreamSetID3v2FrameFileName = LibraryFunctionLoad[$adapterLib,"FileStreamSetID3v2FrameFileName",{Integer, Integer, {_Integer,_,"Constant"}, {_Integer,_,"Constant"}},"Void"];

    lfFileStreamSetID3v2FrameMimeType = LibraryFunctionLoad[$adapterLib,"FileStreamSetID3v2FrameMimeType",{Integer, Integer, {_Integer,_,"Constant"}, {_Integer,_,"Constant"}},"Void"];
    lfFileStreamSetID3v2FramePicture = LibraryFunctionLoad[$adapterLib,"FileStreamSetID3v2FramePicture",{Integer, Integer, {_Integer,_,"Constant"}, {_Integer,_,"Constant"}},"Void"];
    lfFileStreamSetID3v2FrameSeller = LibraryFunctionLoad[$adapterLib,"FileStreamSetID3v2FrameSeller",{Integer, Integer, {_Integer,_,"Constant"}, {_Integer,_,"Constant"}},"Void"];
    lfFileStreamSetID3v2FramePurchaseDate = LibraryFunctionLoad[$adapterLib,"FileStreamSetID3v2FramePurchaseDate",{Integer, Integer, {_Integer,_,"Constant"}, {_Integer,_,"Constant"}},"Void"];
    lfFileStreamSetID3v2FramePricePaid = LibraryFunctionLoad[$adapterLib,"FileStreamSetID3v2FramePricePaid",{Integer, Integer, {_Integer,_,"Constant"}, {_Integer,_,"Constant"}},"Void"];
    lfFileStreamSetID3v2FrameEmail = LibraryFunctionLoad[$adapterLib,"FileStreamSetID3v2FrameEmail",{Integer, Integer, {_Integer,_,"Constant"}, {_Integer,_,"Constant"}},"Void"];
    lfFileStreamSetID3v2FrameObject = LibraryFunctionLoad[$adapterLib,"FileStreamSetID3v2FrameObject",{Integer, Integer, {_Integer,_,"Constant"}, {_Integer,_,"Constant"}},"Void"];

    lfFileStreamSetID3v2FrameOwner = LibraryFunctionLoad[$adapterLib,"FileStreamSetID3v2FrameOwner",{Integer, Integer, {_Integer,_,"Constant"}, {_Integer,_,"Constant"}},"Void"];
    lfFileStreamSetID3v2FrameData = LibraryFunctionLoad[$adapterLib,"FileStreamSetID3v2FrameData",{Integer, Integer, {_Integer,_,"Constant"}, {_Integer,_,"Constant"}},"Void"];
    lfFileStreamSetID3v2FrameIdentifier = LibraryFunctionLoad[$adapterLib,"FileStreamSetID3v2FrameIdentifier",{Integer, Integer, {_Integer,_,"Constant"}, {_Integer,_,"Constant"}},"Void"];
    lfFileStreamSetID3v2FrameURL = LibraryFunctionLoad[$adapterLib,"FileStreamSetID3v2FrameURL",{Integer, Integer, {_Integer,_,"Constant"}, {_Integer,_,"Constant"}},"Void"];
    lfFileStreamSetID3v2FrameText = LibraryFunctionLoad[$adapterLib,"FileStreamSetID3v2FrameText",{Integer, Integer, {_Integer,_,"Constant"}, {_Integer,_,"Constant"}},"Void"];


    lfFileStreamSetID3v2FrameEndOffset = LibraryFunctionLoad[ $adapterLib, "FileStreamSetID3v2FrameEndOffset", {Integer, Integer, Integer, {_Integer,_,"Constant"}}, "Void"];
    lfFileStreamSetID3v2FrameStartOffset = LibraryFunctionLoad[ $adapterLib, "FileStreamSetID3v2FrameStartOffset", {Integer, Integer, Integer, {_Integer,_,"Constant"}}, "Void"];
    lfFileStreamSetID3v2FrameStartTime = LibraryFunctionLoad[ $adapterLib, "FileStreamSetID3v2FrameStartTime", {Integer, Integer, Integer, {_Integer,_,"Constant"}}, "Void"];
    lfFileStreamSetID3v2FrameEndTime = LibraryFunctionLoad[ $adapterLib, "FileStreamSetID3v2FrameEndTime", {Integer, Integer, Integer, {_Integer,_,"Constant"}}, "Void"];
    lfFileStreamSetID3v2FrameOrdered = LibraryFunctionLoad[ $adapterLib, "FileStreamSetID3v2FrameOrdered", {Integer, Integer, Integer, {_Integer,_,"Constant"}}, "Void"];
    lfFileStreamSetID3v2FrameTopLevel = LibraryFunctionLoad[ $adapterLib, "FileStreamSetID3v2FrameTopLevel", {Integer, Integer, Integer, {_Integer,_,"Constant"}}, "Void"];

    (* ID3v1 *)

    lfFileStreamHasID3v1Tag = LibraryFunctionLoad[ $adapterLib, "FileStreamHasID3v1Tag", LinkObject, LinkObject];
    lfFileStreamGetID3v1Element = LibraryFunctionLoad[ $adapterLib, "FileStreamGetID3v1Element", LinkObject, LinkObject];
    lfFileStreamSetID3v1Element = LibraryFunctionLoad[ $adapterLib, "FileStreamSetID3v1Element", LinkObject, LinkObject];

    (* APE *)

    lfFileStreamGetAPEItemKeys = LibraryFunctionLoad[ $adapterLib, "FileStreamGetAPEItemKeys", {Integer}, {Integer, _}];
    lfFileStreamGetAPEItemTypes = LibraryFunctionLoad[ $adapterLib, "FileStreamGetAPEItemTypes", {Integer}, {Integer, _}];
    lfFileStreamAddAPEItem = LibraryFunctionLoad[ $adapterLib, "FileStreamAddAPEItem", {Integer, Integer, {_Integer,_,"Constant"}}, "Void"];
    lfFileStreamRemoveAPEItem = LibraryFunctionLoad[ $adapterLib, "FileStreamRemoveAPEItem", {Integer, {_Integer,_,"Constant"}}, "Void"];
    lfFileStreamGetAPEItemValues = LibraryFunctionLoad[ $adapterLib, "FileStreamGetAPEItemValues", {Integer, {_Integer,_,"Constant"}}, {Integer, _}];
    lfFileStreamGetAPEItemData = LibraryFunctionLoad[ $adapterLib, "FileStreamGetAPEItemData", {Integer, {_Integer,_,"Constant"}}, {Integer, _}];
    lfFileStreamSetAPEItemValues = LibraryFunctionLoad[ $adapterLib, "FileStreamSetAPEItemValues", {Integer, {_Integer,_,"Constant"}, {_Integer,_,"Constant"}}, "Void"];
    lfFileStreamSetAPEItemData = LibraryFunctionLoad[ $adapterLib, "FileStreamSetAPEItemData", {Integer, {_Integer,_,"Constant"}, {_Integer,_,"Constant"}}, "Void"];

    (* Xiph *)

    lfFileStreamGetXiphKeys = LibraryFunctionLoad[ $adapterLib, "FileStreamGetXiphKeys", {Integer}, {Integer, _}];
    lfFileStreamGetXiphValues = LibraryFunctionLoad[ $adapterLib, "FileStreamGetXiphValues", {Integer, {_Integer,_,"Constant"}}, {Integer, _}];
    lfFileStreamSetXiphValues = LibraryFunctionLoad[ $adapterLib, "FileStreamSetXiphValues", {Integer, {_Integer,_,"Constant"}, {_Integer,_,"Constant"}}, "Void"];
    lfFileStreamAddXiphKey = LibraryFunctionLoad[ $adapterLib, "FileStreamAddXiphKey", {Integer, {_Integer,_,"Constant"}}, "Void"];
    lfFileStreamRemoveXiphKey = LibraryFunctionLoad[ $adapterLib, "FileStreamRemoveXiphKey", {Integer, {_Integer,_,"Constant"}}, "Void"];

    $$adapterInitialized = True;
]

loadAdapter[];

(****************************************************************************)
(* Password Dialog code modified from CURLLink's HTTP.m *)

(* Old default Wolfram System password dialog *)
If[!ValueQ[$allowDialogs], $allowDialogs = True]
hasFrontEnd[] := ToString[Head[$FrontEnd]] === "FrontEndObject"
$pwdDlgResult;
$pwdDlgStandaloneRetries = 0;

passwordDialogStandalone[prompt1_, prompt2_, prompt3_] :=
    (
        Print[prompt1];
        Print[prompt2];
        Print[prompt3];
        $pwdDlgResult = {InputString["username: "], InputString["password (will echo as cleartext): "]};
        $pwdDlgStandaloneRetries++;
    )

passwordDialogFE[title_, prompt1_, prompt2_, prompt3_] :=
    Module[{cells, uname = "", pwd = "", createDialogResult},
        cells = {
            TextCell[prompt1, NotebookDefault, "DialogStyle", "ControlStyle"],
            TextCell[prompt2, NotebookDefault, "DialogStyle", "ControlStyle"],
            ExpressionCell[Grid[{ {TextCell["UserName:  "], InputField[Dynamic[uname], String, ContinuousAction -> True,
                ImageSize -> 200, BoxID -> "UserNameField"]}, {TextCell["Password:  "],
                InputField[Dynamic[pwd], String, ContinuousAction -> True,
                    ImageSize -> 200, FieldMasked -> True]}}], "DialogStyle", "ControlStyle"],
            TextCell[prompt3, NotebookDefault, "DialogStyle", "ControlStyle"],

            ExpressionCell[ Row[{DefaultButton[$pwdDlgResult = {uname, pwd};
            DialogReturn[], ImageSize -> Dynamic[CurrentValue["DefaultButtonSize"]]], Spacer[{2.5`, 42, 16}],
                CancelButton[$pwdDlgResult = $Canceled; DialogReturn[],
                    ImageSize -> Dynamic[CurrentValue["DefaultButtonSize"]]]}], TextAlignment -> Right] };
        createDialogResult = DialogInput[DialogNotebook[cells],
            WindowTitle -> title, WindowSize -> {400, FitAll}, Evaluator -> CurrentValue["Evaluator"],
            LineIndent -> 0, PrivateFontOptions -> {"OperatorSubstitution" -> False} ];
        If[createDialogResult === $Failed,
            Null,
        (* else *)
            MathLink`CallFrontEnd[FrontEnd`BoxReferenceFind[ FE`BoxReference[createDialogResult, {{"UserNameField"}},
                FE`BoxOffset -> {FE`BoxChild[1]}]]];
            $pwdDlgResult
        ]
    ]

coreDialog[url_String, prompt2_String, urlPrompt_String] :=
    Module[{title, prompt1, prompt3},
        title = "Authentication Required";
        Clear[$pwdDlgResult];
        Which[
            !TrueQ[$allowDialogs],
            Null,
            hasFrontEnd[],
        (* Use FE dialog box *)
            prompt1 = Row[{urlPrompt, Hyperlink[url, BaseStyle -> "ControlStyle"]}];
            prompt3 = "(These values are kept for this connection only.)";
            passwordDialogFE[title, prompt1, prompt2, prompt3],
            True,
            prompt1 = urlPrompt <> url;
            prompt3 = "(These values are kept for this connection only.)";
            passwordDialogStandalone[prompt1, prompt2, prompt3]
        ]
    ]

passwordDialog[url_String, lastAuthFailed_Integer] := coreDialog[url, "The server is requesting authentication.", "You are attempting to read from the URL:\n"]
proxyDialog[url_String, lastAuthFailed_Integer] := coreDialog[url, "The proxy server is requesting authentication.", "You are attempting to connect to the proxy server:\n"]
passwordDialog[url_String, 1] := coreDialog[url, "Your last authentication attempt failed.\nThe server is requesting re-authentication.", "You are attempting to read from the URL:\n"]
proxyDialog[url_String, 1] := coreDialog[url, "Your last authentication attempt failed.\nThe proxy server is requesting re-authentication.", "You are attempting to connect to the proxy server:\n"]

(****************************************************************************)

$trueOpenedFileStreamsRefCount = <||>;
$deleteFileOnCloseAssoc = <||>;
$streamTypeAssoc = <||>;
$originalStreamTypes = <|1 -> "File", 2 -> "Memory"|>;
Internal`SetValueNoTrack[$trueOpenedFileStreamsRefCount, True];
Internal`SetValueNoTrack[$deleteFileOnCloseAssoc, True];
Internal`SetValueNoTrack[$streamTypeAssoc, True];

Options[AudioFileStreamTools`FileStreamOpenRead] = {"ContainerType" -> "RawArray", "DeleteFileOnClose" -> Automatic, "DeleteFileOnExit" -> Automatic};

AudioFileStreamTools`FileStreamOpenRead[fileName_String, OptionsPattern[]]:=
    Module[{streamID, containerType, filePath, format = "", deleteOnClose = 0, deleteOnExit = 0, originalStreamType = 0},
        containerType = OptionValue["ContainerType"];
        If[!(containerType === "RawArray") && !(containerType === "MTensor"), Message[FileStreamOpenRead::invalidcontainer, containerType];Return[$Failed];];
        loadAdapter[];
        filePath = ImportExport`FileUtilities`GetFilePath[fileName];
        If[filePath == $Failed, Message[FileStreamOpenRead::nofile, fileName]; Return[$Failed];];
        If[!FileExistsQ[filePath], Message[FileStreamOpenRead::nofile, filePath]; Return[$Failed];];
        If[$streamTypeAssoc[filePath] === "Write", Message[FileStreamOpenRead::streamtypeconflict, "Read", $streamTypeAssoc[filePath], filePath]; Return[$Failed];];

        If[!MissingQ[$trueOpenedFileStreamsRefCount[filePath]], deleteOnExit = -1];

        If[OptionValue["DeleteFileOnClose"] =!= Automatic || OptionValue["DeleteFileOnExit"] =!= Automatic,
            If[lfFileIsOpenedAsInternetStream[filePath] === 1 || !MissingQ[$trueOpenedFileStreamsRefCount[filePath]],
                Message[FileStreamOpenRead::deletionoptionsimmutable];
                ,
                If[(OptionValue["DeleteFileOnClose"] === False || OptionValue["DeleteFileOnClose"] === Automatic) && OptionValue["DeleteFileOnExit"] === True, deleteOnExit = 1;];
                If[OptionValue["DeleteFileOnClose"] == True, deleteOnClose = 1;];
            ];
        ];

        (* Using FileFormat here to assert that we have read permissions for the file, JUCE will hang stuck in a loop if we do not. *)
        If[Quiet[FileFormat[filePath]] == $Failed, Message[FileStreamOpenRead::noreadperm, filePath]; Return[$Failed];];
        originalStreamType = lfFileOpenedAsInternetStreamType[filePath];

	(* handle case where file has no extension, since AFST relies on extension to determine audio format *)
	(*If[Length[StringPosition[filePath, "."]] > 0,
		Print["file had extension: ", StringDrop[filePath,First[Last[StringPosition[filePath,"."]]]-1]];
		streamID = lfFileStreamOpenRead[filePath, deleteOnClose, deleteOnExit]; (* will this handle .WAV extension properly? *)
		,
		Print["file had NO extension; format is: ", "."<>ToLowerCase[FileFormat[filePath]]];
		format = "."<>ToLowerCase[FileFormat[filePath]];
		streamID = lfFileStreamOpenReadExtension[filePath, format, deleteOnClose, deleteOnExit];
	];*)
		(* use FileFormat[] for all files, to handle .tmp files from URLSave *)
		format = "."<>ToLowerCase[FileFormat[filePath]];
		streamID = lfFileStreamOpenReadExtension[filePath, format, deleteOnClose, deleteOnExit];
        If[Head[streamID] === LibraryFunctionError, Message[FileStreamOpenRead::openreadfail, filePath]; Return[$Failed]];
        setField[streamID, "FilePath", filePath];
        setField[streamID, "DataContainer", containerType];

        If[lfFileIsOpenedAsInternetStream[filePath] === 0,
            If[MissingQ[$trueOpenedFileStreamsRefCount[filePath]],
                $trueOpenedFileStreamsRefCount[filePath] = 1;
                $streamTypeAssoc[filePath] = "Read";
                If[OptionValue["DeleteFileOnClose"] === Automatic,
                    $deleteFileOnCloseAssoc[filePath] = False;
                    ,
                    $deleteFileOnCloseAssoc[filePath] = OptionValue["DeleteFileOnClose"];
                ];
                ,
                $trueOpenedFileStreamsRefCount[filePath] = $trueOpenedFileStreamsRefCount[filePath] + 1;
            ];
        ];

        If[!IntegerQ[originalStreamType] || originalStreamType === 0,
            setField[streamID, "InternetStream", False];
            setField[streamID, "InternetStreamType", None];
            setField[streamID, "URL", None];
            ,
            setField[streamID, "InternetStream", True];
            setField[streamID, "InternetStreamType", $originalStreamTypes[originalStreamType]];
            setField[streamID, "URL", lfInternetStreamFilePathGetURL[filePath]];
        ];

        Return[ AudioFileStreamTools`FileStreamObject[ streamID]];
    ]

AudioFileStreamTools`FileStreamReadN[ obj_AudioFileStreamTools`FileStreamObject, numFrames_Integer]:=
    Module[{ streamID, res, readPos, eof},
        loadAdapter[];
        streamID = getStreamID@obj;
        If[!KeyExistsQ[$openStreams, streamID], Message[FileStreamReadN::nostream, obj]; Return[$Failed]];
        If[numFrames <= 0, Message[FileStreamReadN::numframesoutofbounds]; Return[$Failed];];
        If[numFrames > $signedInt32BitMax, Message[FileStreamReadN::numframesoutofbounds]; Return[$Failed];];
        If[getField[streamID, "DataContainer"] == "RawArray", res = lfFileStreamReadNRawArray[streamID, numFrames];, res = lfFileStreamReadN[streamID, numFrames];];
        If[Head[res] === LibraryFunctionError, Message[FileStreamReadN::positionpastendoffile]; Return[EndOfFile]];
        readPos = FileStreamGetReadPosition[obj]; (* M-indexed *)
        (* from FileStreamSetReadPosition documentation, eof is defined as "FrameCount" + 1 *)
        eof = FileStreamGetMetaInformation[obj]["FrameCount"] + 1;
        If[IntegerQ[eof] && IntegerQ[readPos],
            If[readPos > eof, Message[FileStreamReadN::reachedendoffile]; FileStreamSetReadPosition[obj, eof];];
        ];
        Return[res];
    ]

AudioFileStreamTools`FileStreamReadN[___]:= (Message[FileStreamReadN::invalidargs]; Return[$Failed]);

removeTagReferences[streamID_] := Module[{tagList = {}},
    Scan[If[#[[2]][[1]] == streamID, AppendTo[tagList, #[[1]]]] &, Normal[$metaTagAssociation]];
    tagList = DeleteDuplicates[tagList];
    KeyDropFrom[$metaTagAssociation, tagList];
]

AudioFileStreamTools`FileStreamClose[ obj_AudioFileStreamTools`FileStreamObject]:=
    Module[{ streamID, filePath, refCount},
        loadAdapter[];
        streamID = getStreamID@obj;
        If[!KeyExistsQ[$openStreams, streamID], Message[FileStreamClose::nostream, obj]; Return[$Failed]];
        lfFileStreamClose[streamID]; (* TODO ERR *)
        filePath = getField[streamID, "FilePath"];
        If[!StringQ[filePath], Return[];];

        refCount = $trueOpenedFileStreamsRefCount[filePath];
        If[!MissingQ[refCount],
            refCount--;
            $trueOpenedFileStreamsRefCount[filePath] = refCount;
            If[refCount === 0,
                If[$deleteFileOnCloseAssoc[filePath] === True,
                    DeleteFile[filePath];
                ];
                KeyDropFrom[$trueOpenedFileStreamsRefCount, filePath];
                KeyDropFrom[$deleteFileOnCloseAssoc, filePath];
                KeyDropFrom[$streamTypeAssoc, filePath];
            ];
            ,
            If[lfFileIsOpenedAsInternetStream[filePath] === 0,
                KeyDropFrom[$streamTypeAssoc, filePath]; (* Remove on last virtual stream closed. *)
            ]
        ];

        removeTagReferences[streamID];
        KeyDropFrom[$openStreams, streamID];
    ]

AudioFileStreamTools`FileStreamClose[___]:= (Message[FileStreamClose::invalidargs]; Return[$Failed]);

AudioFileStreamTools`FileStreamGetReadPosition[ obj_AudioFileStreamTools`FileStreamObject]:=
    Module[{ streamID, position},
        loadAdapter[];
        streamID = getStreamID@obj;
        If[!KeyExistsQ[$openStreams, streamID], Message[FileStreamGetReadPosition::nostream, obj]; Return[$Failed]];

        position = lfFileStreamGetReadPosition[streamID];
        If[Head[position] == LibraryFunctionError, Message[FileStreamGetReadPosition::nostream, obj]; Return[$Failed]];
        Return[position];
    ]

AudioFileStreamTools`FileStreamGetReadPosition[___]:= (Message[FileStreamGetReadPosition::invalidargs]; Return[$Failed]);

AudioFileStreamTools`FileStreamSetReadPosition[ obj_AudioFileStreamTools`FileStreamObject, pos_Integer]:=
    Module[{ streamID, position},
        loadAdapter[];
        streamID = getStreamID@obj;
        If[!KeyExistsQ[$openStreams, streamID], Message[FileStreamSetReadPosition::nostream, obj]; Return[$Failed]];
        If[pos <= 0,
            Message[FileStreamSetReadPosition::invalidposition, pos]; Return[$Failed];
        ];
        If[pos > lfFileStreamGetMetaInformation[streamID, $metaInformationFields["FrameCount"]]+1,
            Message[FileStreamSetReadPosition::stmrng, obj, pos];
        ];
        position = lfFileStreamSetReadPosition[streamID, pos];

        If[position == LibraryFunctionError["LIBRARY_DIMENSION_ERROR", 3], Message[FileStreamSetReadPosition::invalidposition, pos]; Return[$Failed]];
		If[Head[position] == LibraryFunctionError, Message[FileStreamSetReadPosition::nostream, obj]; Return[$Failed]];
		Return[position];
    ]

AudioFileStreamTools`FileStreamSetReadPosition[___]:= (Message[FileStreamSetReadPosition::invalidargs]; Return[$Failed]);

AudioFileStreamTools`FileStreamGetMetaInformation[ obj_AudioFileStreamTools`FileStreamObject]:=
    Module[{ streamID},
        loadAdapter[];
        streamID = getStreamID@obj;
        If[!KeyExistsQ[$openStreams, streamID], Message[FileStreamGetMetaInformation::nostream, obj]; Return[$Failed]];
        Return[ AssociationMap[(lfFileStreamGetMetaInformation[ streamID, $metaInformationFields[#]])&, Keys[$metaInformationFields]]];
    ]

AudioFileStreamTools`FileStreamGetMetaInformation[ obj_AudioFileStreamTools`FileStreamObject, field_String]:=
    Module[{ streamID},
        loadAdapter[];
        streamID = getStreamID@obj;
        If[!KeyExistsQ[$openStreams, streamID], Message[FileStreamGetMetaInformation::nostream, obj]; Return[$Failed]];
        If[!KeyExistsQ[$metaInformationFields, field], Message[FileStreamGetMetaInformation::noinfo, field]; Return[$Failed]];
        Return[ lfFileStreamGetMetaInformation[ streamID, $metaInformationFields[field]]];
    ]

AudioFileStreamTools`FileStreamGetMetaInformation[ obj_AudioFileStreamTools`FileStreamObject, fields_List]:=
    Module[{streamID, results},
        loadAdapter[];
        streamID = getStreamID@obj;
        If[!KeyExistsQ[$openStreams, streamID], Message[FileStreamGetMetaInformation::nostream, obj]; Return[$Failed]];
        results = Map[(If[!KeyExistsQ[$metaInformationFields, #], Message[FileStreamGetMetaInformation::noinfo, #]; 1, 0])&, fields];
        If[Total[results] =!= 0, Return[$Failed]];
        Return[ AssociationMap[(lfFileStreamGetMetaInformation[ streamID, $metaInformationFields[#]])&, fields]];
    ]

AudioFileStreamTools`FileStreamGetMetaInformation[___]:= (Message[FileStreamGetMetaInformation::invalidargs]; Return[$Failed]);

AudioFileStreamTools`FileStreamGetTotalNumberOfFrames[ obj_AudioFileStreamTools`FileStreamObject]:= AudioFileStreamTools`FileStreamGetMetaInformation[obj, "FrameCount"];

AudioFileStreamTools`FileStreamGetTotalNumberOfFrames[___]:= (Message[FileStreamGetTotalNumberOfFrames::invalidargs]; Return[$Failed]);

AudioFileStreamTools`FileStreamGetStreamInformation[ obj_AudioFileStreamTools`FileStreamObject]:=
    Module[{streamID},
        loadAdapter[];
        streamID = getStreamID@obj;
        If[!KeyExistsQ[$openStreams, streamID], Message[FileStreamGetStreamInformation::nostream, obj]; Return[$Failed]];
        Return[ AssociationMap[(getField[streamID, #])&, $streamInformationFields]];
    ]

AudioFileStreamTools`FileStreamGetStreamInformation[ obj_AudioFileStreamTools`FileStreamObject, field_String]:=
    Module[{streamID},
        loadAdapter[];
        streamID = getStreamID@obj;
        If[!KeyExistsQ[$openStreams, streamID], Message[FileStreamGetStreamInformation::nostream, obj]; Return[$Failed]];
        If[!MemberQ[$streamInformationFields, field], Message[FileStreamGetStreamInformation::noinfo, field]; Return[$Failed]];
        Return[getField[streamID, field]];
	]

AudioFileStreamTools`FileStreamGetStreamInformation[ obj_AudioFileStreamTools`FileStreamObject, fields_List]:=
    Module[{streamID, results},
        loadAdapter[];
        streamID = getStreamID@obj;
        If[!KeyExistsQ[$openStreams, streamID], Message[FileStreamGetStreamInformation::nostream, obj]; Return[$Failed]];
        results = Map[(If[!MemberQ[$streamInformationFields, #], Message[FileStreamGetStreamInformation::noinfo, #]; 1, 0])&, fields];
        If[Total[results] =!= 0, Return[$Failed]];
        Return[ AssociationMap[(getField[streamID, #])&, fields]];
    ]

AudioFileStreamTools`FileStreamGetStreamInformation[___]:= (Message[FileStreamGetStreamInformation::invalidargs]; Return[$Failed]);

Options[AudioFileStreamTools`FileStreamOpenWrite] = {"ContainerType" -> "RawArray", "DeleteFileOnClose" -> Automatic, "DeleteFileOnExit" -> Automatic};

AudioFileStreamTools`FileStreamOpenWrite[fileName_String, sampleFreq_, dim_, bitdepth_, OptionsPattern[]]:=
    Module[{ streamID, filePath, directory, deleteOnClose = 0, deleteOnExit = 0, hasWritePermissions},
        loadAdapter[];

        directory = FileNameTake[fileName, {1, -2}];
        If[directory === "", directory = Directory[];];
        filePath = ImportExport`FileUtilities`GetFilePath[directory];
        If[!DirectoryQ[directory] || filePath === $Failed,
            Message[FileStreamOpenWrite::nodirectory, directory];
            Return[$Failed];
        ];
        filePath = filePath <> FileNameTake[fileName];

        If[!MissingQ[$streamTypeAssoc[filePath]], Message[FileStreamOpenWrite::currentlyopen, filePath]; Return[$Failed];];

        If[!MissingQ[$trueOpenedFileStreamsRefCount[filePath]], deleteOnExit = -1];

        If[OptionValue["DeleteFileOnClose"] =!= Automatic || OptionValue["DeleteFileOnExit"] =!= Automatic,
            If[lfFileIsOpenedAsInternetStream[filePath] === 1 || !MissingQ[$trueOpenedFileStreamsRefCount[filePath]],
                Message[FileStreamOpenWrite::deletionoptionsimmutable];
                ,
                If[(OptionValue["DeleteFileOnClose"] === False || OptionValue["DeleteFileOnClose"] === Automatic) && OptionValue["DeleteFileOnExit"] === True, deleteOnExit = 1;];
                If[OptionValue["DeleteFileOnClose"] == True, deleteOnClose = 1;];
            ];
        ];

        streamID = lfFileStreamOpenWrite[filePath, sampleFreq, dim, bitdepth, deleteOnClose, deleteOnExit]; (* TODO ERR *)
        If[Head[streamID] === LibraryFunctionError, Message[FileStreamOpenWrite::openwritefail]; Return[$Failed];];
        setField[streamID, "FilePath", filePath];
        hasWritePermissions = lfStreamHasOperatingSystemWritePermissions[streamID];
        If[Head[hasWritePermissions] === LibraryFunctionError, Message[FileStreamOpenWrite::openwritefail]; lfFileStreamClose[streamID]; Return[$Failed]];
        If[hasWritePermissions =!= 1, Message[FileStreamOpenWrite::nowriteperm, filePath, directory]; lfFileStreamClose[streamID]; Return[$Failed]];

        setField[streamID, "DataContainer", OptionValue["ContainerType"]];
        setField[streamID, "Channels", dim];

        If[lfFileIsOpenedAsInternetStream[filePath] === 0,
            If[MissingQ[$trueOpenedFileStreamsRefCount[filePath]],
                $trueOpenedFileStreamsRefCount[filePath] = 1;
                $streamTypeAssoc[filePath] = "Write";
                If[OptionValue["DeleteFileOnClose"] === Automatic,
                    $deleteFileOnCloseAssoc[filePath] = False;
                    ,
                    $deleteFileOnCloseAssoc[filePath] = OptionValue["DeleteFileOnClose"];
                ];
                ,
                $trueOpenedFileStreamsRefCount[filePath] = $trueOpenedFileStreamsRefCount[filePath] + 1;
            ];
        ];

        Return[AudioFileStreamTools`FileStreamObject[streamID]];
    ]

AudioFileStreamTools`FileStreamOpenWrite[___]:= (Message[FileStreamOpenWrite::invalidargs]; Return[$Failed]);

AudioFileStreamTools`FileStreamWrite[obj_AudioFileStreamTools`FileStreamObject, audioDataMatrix_]:=
    Module[{streamID, res, dataIsRawArray, streamDataContainer, streamIsRawArray, dataIsMTensor, streamIsMTensor, numChannels, dimensions, rank, typeMismatch},
        loadAdapter[];
        streamID = getStreamID@obj;
        If[!KeyExistsQ[$openStreams, streamID], Message[FileStreamWrite::nostream, obj]; Return[$Failed]];dataIsRawArray = Developer`RawArrayQ[audioDataMatrix];
        dataIsMTensor = Head[audioDataMatrix] == List;
        streamDataContainer = getField[streamID, "DataContainer"];
        streamIsRawArray = streamDataContainer == "RawArray";
        streamIsMTensor = streamDataContainer == "MTensor";
        If[dataIsRawArray != streamIsRawArray, Message[FileStreamWrite::containermismatch, streamDataContainer]; Return[$Failed]];
        If[dataIsMTensor != streamIsMTensor, Message[FileStreamWrite::containermismatch, streamDataContainer]; Return[$Failed]];
        numChannels = getField[streamID, "Channels"];
        dimensions = Dimensions[audioDataMatrix];
        rank = Length[dimensions];
        If[(numChannels == 1 && rank != 1) || (numChannels != 1 && (rank != 2 || numChannels != dimensions[[1]])), Message[AudioFileStreamTools`FileStreamWrite::dimensionmismatch, obj]; Return[$Failed]];

        typeMismatch = If[dataIsRawArray, Developer`RawArrayType[audioDataMatrix] != "Real32",
            If[numChannels == 1,
                Scan[If[!(Developer`RealQ[#] || IntegerQ[#]), Return[True]]&]
                ,
                !MatrixQ[audioDataMatrix,(Developer`RealQ@#||IntegerQ@#)&]
            ]
        ];
        If[typeMismatch, Message[AudioFileStreamTools`FileStreamWrite::invalidtype]; Return[$Failed]];
        If[dataIsRawArray, res = lfFileStreamWriteRawArray[streamID, audioDataMatrix];, res = Check[lfFileStreamWrite[streamID,audioDataMatrix], Message[AudioFileStreamTools`FileStreamWrite::dimensionmismatch, obj]; $Failed];];
        If[res === 0, Return[Null], Return[$Failed]];
    ]

AudioFileStreamTools`FileStreamWrite[___]:= (Message[FileStreamWrite::invalidargs]; Return[$Failed]);

(* AudioFileStreamTools`GetWritePosition[]:= foo
 AudioFileStreamTools`SetWritePosition[]:= foo *)

(*Internet Stream Functions:*)

(* Following URLFetchAsynchronous convention http://reference.wolfram.com/language/ref/URLFetchAsynchronous.html?q=URLFetchAsynchronous&lang=en *)
internetStreamInternalAsyncCallback[streamID_, asynchObj_, eventType_, data_] :=
    Module[{func, dlnow, dltotal},
        func = getField[streamID, "CallbackFunction"];
        If[eventType == "progress",
            dltotal = data[[1]];
            dlnow = dltotal - getField[streamID, "DownloadProgress"];
            setField[streamID, "DownloadProgress", dltotal];
            If[!(func === None),
                func[AudioFileStreamTools`FileStreamObject[streamID], eventType, {dlnow, dltotal}];
            ];
        ];
        If[eventType == "data",
            If[!(func === None),
                func[AudioFileStreamTools`FileStreamObject[streamID], eventType, data];
            ];
        ];
    ]

(****************************************************************************)
(* Proxy code modified from CURLLink's HTTP.m *)

extractScheme[url_String] := Module[{scheme, positions},
    positions = StringPosition[url, "://"];
    scheme = StringTake[url, {1, positions[[1]][[2]]}];
    Return[scheme];
];
addProxies[allProxies_, newProxies_] :=
    Module[{newProxyList = allProxies},
        Scan[If[! MemberQ[allProxies, #], AppendTo[newProxyList, #]] &,
            newProxies];
        Return[newProxyList];
    ];
getProxiesWrapper[url_String, flag_] := Module[{allProxies, scheme},
    If[flag == False,
        Return[{""}];
    ];

    scheme = extractScheme[url];
    If[flag == Automatic,
        allProxies = getProxies[url, flag];
        If[allProxies[[1]] == All,
            Return[{allProxies[[2]], removeDuplicateScheme["socks://" <> allProxies[[2]]]}];
        ];
        allProxies = addProxies[allProxies, getProxies["socks://a", flag]];
        If[allProxies == {}, Return[{""}];];
        Return[allProxies];
    ];
    If[flag == True,
        allProxies = getProxies[url, flag];
        If[allProxies == {}, Return[{""}];];
        Return[allProxies];
    ];

];

removeDuplicateScheme[url_String] :=
    Module[{positions, newurl = url},
        positions = StringPosition[newurl, "://"];
        While[Length[positions] >= 2,
            newurl = StringReplacePart[newurl, "", {1, positions[[1]][[2]]}];
            positions = StringPosition[newurl, "://"];
        ];
        Return[newurl];
    ];

URISplit[uri_String] :=
    Flatten[StringCases[uri,
        RegularExpression[
            "^(([^:/?#]+):)?(//([^/?#]*))?([^?#]*)(\\?([^#]*))?(#(.*))?"] ->
            {"Scheme" -> "$2", "Authority" -> "$4"}]]

URIJoin[uri : List[_Rule, _Rule]] :=
    Module[{scheme, authority},
        If[! Fold[And, True, Map[MatchQ[#, Rule[_String, _String]] &, uri]],
            Return[$Failed]];
        {scheme, authority} = Map[Last, uri];
        StringJoin[
            Cases[{If[scheme =!= "", StringJoin[scheme, ":"]],
                Which[authority =!= "" && scheme =!= "",
                    StringJoin["//", authority], authority === "" && scheme =!= "",
                    authority]}, Except[Null]]]]
getProxies[url_String, False] = {""}


tourl[{url_String, port_Integer}] := url <> ":" <> ToString[port]
tourl[{url_String}] := url
tourl[___] := Nothing
wlproxy[proto_String] :=
    Module[{url, scheme, fullurl}, fullurl = proto /. $InternetProxyRules;
    url = tourl[fullurl];
    If[fullurl =!= {}, scheme = URLParse[fullurl[[1]], "Scheme"]];
    If[scheme === None, url = ToLowerCase[proto] <> "://" <> url, url]]

(*when UseProxy->True,in $InternetProxyRules, and scheme isn't https or ftp,
 getProxies defaults to HTTP and Socks proxies given by $InternetProxyRules *)
getProxies[url_String, True] :=
    Module[{proxies, scheme = URLParse[url, "Scheme"]},
        proxies =
            Which[scheme === "https", {wlproxy["HTTPS"], wlproxy["Socks"]},
                scheme === "ftp", {wlproxy["FTP"], wlproxy["Socks"]},
                True, {wlproxy["HTTP"], wlproxy["Socks"]}];
        If[proxies === {}, {""}, proxies]]

getProxies[url_String, Automatic] := Module[{proxies, scheme},
    proxies = getSystemProxies[url, $OperatingSystem];
    If[Length[proxies] === 2 && proxies[[1]] === All,
    	scheme = extractScheme[url];
        proxies = {All, removeDuplicateScheme[scheme <> proxies[[2]]]};
        Return[proxies];
    ];
    proxies = Map[removeDuplicateScheme[#] &, proxies];
    Return[proxies];
];

getSystemProxies[url_String, "Windows"] :=
    Module[{rawProxies, proxies, proxyList = {}},
    	If[$enableOperatingSystemProxies === False, Return[{}];];
        rawProxies =
            If[(StringMatchQ[url, "http://*"] ||
                StringMatchQ[url, "https://*"] || StringMatchQ[url, "ftp://*"] ||
                StringMatchQ[url, "ftps://*"] ||
                StringMatchQ[url, "socks*://*"]),
                Quiet[Check[CURLLink`CURLGetProxies[url], {}],
                    LibraryFunction::strnull]
            (*else*),
                Quiet[Check[CURLLink`CURLGetProxies["http://" <> url], {}],
                    LibraryFunction::strnull]];
                    
        If[rawProxies == $Failed, Return[{All, {}}];];
        If[(!StringMatchQ[rawProxies, "http=*"]) && (!StringMatchQ[rawProxies, "https=*"]) && (!StringMatchQ[rawProxies, "ftp=*"]) && (!StringMatchQ[rawProxies, "socks=*"]),
        	rawProxies = "http=" <> rawProxies <> ";http=https://" <> rawProxies <>";https=" <> rawProxies <> ";https=http://" <> rawProxies <> ";ftp=" <> rawProxies <> ";socks=" <> rawProxies;
        ];
        rawProxies = StringSplit[rawProxies, ";"];
        If[StringMatchQ[url, "http://*"],
        	proxies = Select[rawProxies, StringMatchQ[#, "http=*"] &];
        	proxies = Map[StringReplace[#, "http=" -> "", 1] &, proxies];
			proxies = Map[("http://" <> #) &, proxies];
			Scan[AppendTo[proxyList, #] &, proxies];
        ];
        If[StringMatchQ[url, "https://*"],
        	proxies = Select[rawProxies, StringMatchQ[#, "https=*"] &];
        	proxies = Map[StringReplace[#, "https=" -> "", 1] &, proxies];
			proxies = Map[("https://" <> #) &, proxies];
			Scan[AppendTo[proxyList, #] &, proxies];
        ];
        If[StringMatchQ[url, "ftp://*"],
        	proxies = Select[rawProxies, StringMatchQ[#, "ftp=*"] &];
        	proxies = Map[StringReplace[#, "ftp=" -> "", 1] &, proxies];
			proxies = Map[("ftp://" <> #) &, proxies];
			Scan[AppendTo[proxyList, #] &, proxies];
        ];
        proxies = Select[rawProxies, StringMatchQ[#, "socks=*"] &];
    	proxies = Map[StringReplace[#, "socks=" -> "", 1] &, proxies];
		proxies = Map[("socks://" <> #) &, proxies];
		Scan[AppendTo[proxyList, #] &, proxies];
        Return[proxyList];
];

getSystemProxies[url_String, "MacOSX"] :=
    Module[{},
    	If[$enableOperatingSystemProxies === False, Return[{}];];
        If[(StringMatchQ[url, "http://*"] ||
            StringMatchQ[url, "https://*"] || StringMatchQ[url, "ftp://*"] ||
            StringMatchQ[url, "ftps://*"] ||
            StringMatchQ[url, "socks*://*"]),
            Flatten@{Quiet[
                Check[CURLLink`CURLGetProxies[
                    URIJoin[Flatten@{URISplit[url]}]], {}]]}
        (*else*),
            Flatten@{Quiet[
                Check[CURLLink`CURLGetProxies[
                    URIJoin[Flatten@{URISplit["http://" <> url]}]], {}]]}]]

getSystemProxies[url_String, _] := {}
buildProxy[{scheme_String, url_String}] :=
    If[StringMatchQ[url, scheme <> "://*"], url, scheme <> "://" <> url]
buildProxy[{url_String}] := url
buildProxy[url_String] := url


(****************************************************************************)

internetStreamChooseProxy[url_String, proxyType_] :=
    Module[{proxies = {}},
        If[proxyType == Automatic,
            proxies = DeleteDuplicates[getProxiesWrapper[ToLowerCase[url], ("UseProxy" /. $InternetProxyRules)]];
            proxies = Map[removeDuplicateScheme[#] &, proxies];
            Return[proxies];
        ];
        If[proxyType == "HTTP",
            proxies = DeleteDuplicates[getProxiesWrapper["http://a", ("UseProxy" /. $InternetProxyRules)]];
            proxies = Map[removeDuplicateScheme[#] &, proxies];
            Return[proxies];
        ];
        If[proxyType == "HTTPS",
            proxies = DeleteDuplicates[getProxiesWrapper["https://a", ("UseProxy" /. $InternetProxyRules)]];
            proxies = Map[removeDuplicateScheme[#] &, proxies];
            Return[proxies];
        ];
        If[proxyType == "Socks",
            proxies = DeleteDuplicates[getProxiesWrapper["socks://a", ("UseProxy" /. $InternetProxyRules)]];
            proxies = Map[removeDuplicateScheme[#] &, proxies];
            Return[proxies];
        ];
        If[proxyType == "FTP",
            proxies = DeleteDuplicates[getProxiesWrapper["ftp://a", ("UseProxy" /. $InternetProxyRules)]];
            proxies = Map[removeDuplicateScheme[#] &, proxies];
            Return[proxies];
        ];
        Return[$Failed];
    ]

addToAvailableProxies[availableProxies_, proxy_] :=
    If[proxy =!= "",
        Append[availableProxies, proxy]
        ,
        availableProxies
    ]

proxyErrorQ[error_Integer] = If[error < -100, True, False]

$downloadStatusMessagesAssociation = <|-1 -> (Message[InternetStreamOpenRead::unknownerror, #1]&), -2 -> (Message[InternetStreamOpenRead::invalidsslcertificate, #1]&),
    -3 -> (Message[InternetStreamOpenRead::authenticationrequired, #1]&), -4 -> (Message[InternetStreamOpenRead::filenotfound, #1]&),
    -5 -> (Message[InternetStreamOpenRead::forbidden, #1]&), -6 -> (Message[InternetStreamOpenRead::servernotfound, #1]&),
    -7 -> (Message[InternetStreamOpenRead::unsupportedprotocol, #1]&), -8 -> (Message[InternetStreamOpenRead::timedout, #1]&),
    -9 -> (Message[InternetStreamOpenRead::couldnotconnect, #1]&), -10 -> (Message[InternetStreamOpenRead::readerror, #1]&),

    -103 -> (Message[InternetStreamOpenRead::proxyunknownerror, #1]&) , -108 -> (Message[InternetStreamOpenRead::proxyinvalidsslcertificate, #1]&),
    -102 -> (Message[InternetStreamOpenRead::proxyauthenticationrequired, #1]&), -101 -> (Message[InternetStreamOpenRead::proxyservernotfound, #1]&),
    -107 -> (Message[InternetStreamOpenRead::proxyunsupportedprotocol, #1]&), -105 -> (Message[InternetStreamOpenRead::proxytimedout, #1]&),
    -104 -> (Message[InternetStreamOpenRead::proxycouldnotconnect, #1]&), -106 -> (Message[InternetStreamOpenRead::proxyreaderror, #1]&)|>;

$downloadStatusAssociation = <|0 -> "InProgress", 1 -> "Complete",
    2 -> "PartiallyComplete", -1 -> "Aborted",
    -2 -> "AbortedInvalidCertificate", -3 -> "AuthenticationRequired",
    -4 -> "FileNotFound", -5 -> "Forbidden", -6 -> "ServerNotFound",
    -7 ->"UnsupportedProtocol", -8 -> "TimedOut",
    -9 -> "CouldNotConnect", -10 -> "ErrorReadingData",

    -101 -> "ProxyServerNotFound", -102 -> "ProxyAuthenticationRequired",
    -103 -> "ProxyAborted", -104 -> "ProxyCouldNotConnect",
    -105 -> "ProxyTimedOut", -106 -> "ProxyErrorReadingData",
    -107 -> "ProxyUnsupportedProtocol", -108 -> "ProxyAbortedInvalidCertificate"|>;

(* TODO: Assert file output format is the same as stream read-in format *)
(* "AudioFormat" option will be used if "FilePath" is not specified, and the format was not found from the formatServiceURL[] parser. *)
Options[AudioFileStreamTools`InternetStreamOpenRead] =
    {   "ContainerType" -> "RawArray", "FilePath" -> None,
        "AudioFormat" -> None, "DeleteFileOnClose" -> Automatic, "DeleteFileOnExit" -> Automatic,
        "DataUpdates" -> False, "VerifySSLCertificate" -> False,
        "UserName" -> None, "Password" -> None, "ConnectTimeout" -> 0,
        "ProxyUserName" -> None, "ProxyPassword" -> None, "ProxyType" -> Automatic, "ProxyOverride" -> None
    }; (* NOTE: "UserName" is correct, URLFetch uses "Username", but this is going to be changed to "UserName"?*)

$lfcLLproxyTypeAssociation = <|None -> 0, "GivenProxy" -> 1, 0 -> None, 1 -> "GivenProxy"|>;

AudioFileStreamTools`InternetStreamOpenRead[url_String, opts:OptionsPattern[]]:= AudioFileStreamTools`InternetStreamOpenRead[url, None, opts];
AudioFileStreamTools`InternetStreamOpenRead[url_String, func:Except[_Rule], opts:OptionsPattern[]]:=
    Module[{streamID, res, containerType, deleteOnClose = 1, deleteOnExit = 0, dataUpdates = 1, verifySSLCert = 1, useUsernamePassword = 0, callbackDataType,
        filePath, directory, formats, audioFormat, formattedURL, existingURL, hasWritePermissions, userName = "", password = "", timeout = 0,
        useProxyUsernamePassword = 0, proxyAddress = "", proxyUserName = "", proxyPassword = "", lfcLLproxyType = $lfcLLproxyTypeAssociation[None],
        availableProxies = {}, retriedStream, failureMessage, originalStreamType = 0},

        If[$pwdDlgStandaloneRetries >= 3,
            $pwdDlgStandaloneRetries = 0;
            Message[InternetStreamOpenRead::authfailed];
            Return[$Failed];
        ];

        containerType = OptionValue["ContainerType"];
        formats = {".mp3", ".wav", ".ogg", ".oga", ".aif", ".aiff", ".flac"};
        If[!(containerType === "RawArray") && !(containerType === "MTensor"), Message[InternetStreamOpenRead::invalidcontainer, containerType];Return[$Failed];];
        loadAdapter[];

        (* Append http:// to URL if url does not have a protocol specified. *)
        If[StringPosition[url, "://"] === {},
            formattedURL = "http://" <> url;
            ,
            formattedURL = url;
        ];

        {formattedURL, audioFormat} = formatServiceURL[formattedURL];
        If[formattedURL == $Failed,
            Return[$Failed];
        ];

        If[IntegerQ[OptionValue["ConnectTimeout"]] && OptionValue["ConnectTimeout"] > 0,
            timeout = OptionValue["ConnectTimeout"];
        ];

        If[OptionValue["VerifySSLCertificate"] == False, verifySSLCert = 0;];

        If[OptionValue["UserName"] =!= None || OptionValue["Password"] =!= None,
            If[OptionValue["UserName"] =!= None && OptionValue["Password"] =!= None,
                If[lfURLIsOpenedAsInternetStream[formattedURL] === 1,
                    Message[InternetStreamOpenRead::usernamepasswordimmutable];
                    ,
                    userName = OptionValue["UserName"];
                    password = OptionValue["Password"];
                    useUsernamePassword = 1;
                ];
                ,
                Message[InternetStreamOpenRead::usernamepasswordformat];
                Return[$Failed];
            ];
        ];

        If[OptionValue["ProxyUserName"] =!= None || OptionValue["ProxyPassword"] =!= None,
            If[OptionValue["ProxyUserName"] =!= None && OptionValue["ProxyPassword"] =!= None,
                If[lfURLIsOpenedAsInternetStream[formattedURL] === 1,
                    Message[InternetStreamOpenRead::proxyusernamepasswordimmutable];
                    ,
                    proxyUserName = OptionValue["ProxyUserName"];
                    proxyPassword = OptionValue["ProxyPassword"];
                    useProxyUsernamePassword = 1;
                ];
                ,
                Message[InternetStreamOpenRead::proxyusernamepasswordformat];
                Return[$Failed];
            ];
        ];

        If[OptionValue["ProxyOverride"] === None,
            Quiet[availableProxies = internetStreamChooseProxy[formattedURL, OptionValue["ProxyType"]];];

            If[availableProxies == $Failed,
                Message[InternetStreamOpenRead::proxyconfigurationerror];
                Return[$Failed];
            ];
            
            availableProxies = DeleteCases[availableProxies, "http://"];
            availableProxies = DeleteCases[availableProxies, "https://"];
            availableProxies = DeleteCases[availableProxies, "socks*://"];

            If[availableProxies == {},
                availableProxies = {""};
            ];
            proxyAddress = availableProxies[[1]];
            ,
            availableProxies = {OptionValue["ProxyOverride"]};
            proxyAddress = availableProxies[[1]];
        ];

        If[("UseProxy" /. $InternetProxyRules) == Automatic && proxyAddress == "",
            availableProxies = {};

            If[extractScheme[formattedURL] == "http://",
                availableProxies = addToAvailableProxies[availableProxies, lfGetEnvironmentProxySettings[$proxyEnvVars["http_proxy"]]];
                availableProxies = addToAvailableProxies[availableProxies, lfGetEnvironmentProxySettings[$proxyEnvVars["socks_proxy"]]];
                availableProxies = addToAvailableProxies[availableProxies, lfGetEnvironmentProxySettings[$proxyEnvVars["all_proxy"]]];
            ];
            If[extractScheme[formattedURL] == "https://",
                availableProxies = addToAvailableProxies[availableProxies, lfGetEnvironmentProxySettings[$proxyEnvVars["https_proxy"]]];
                availableProxies = addToAvailableProxies[availableProxies, lfGetEnvironmentProxySettings[$proxyEnvVars["socks_proxy"]]];
                availableProxies = addToAvailableProxies[availableProxies, lfGetEnvironmentProxySettings[$proxyEnvVars["all_proxy"]]];
            ];

            If[availableProxies == {},
                availableProxies = {""};
            ];
            proxyAddress = availableProxies[[1]];

        ];

        If[proxyAddress != "",
            lfcLLproxyType = $lfcLLproxyTypeAssociation["GivenProxy"];
        ];

        (*Print["availableProxies: " <> ToString[availableProxies]];*)
        (*Print["proxyAddress: " <> proxyAddress];*)
        (*Print["lfcLLproxyType: " <> ToString[$lfcLLproxyTypeAssociation[lfcLLproxyType]]];*)

        originalStreamType = lfURLOpenedAsInternetStreamType[formattedURL];

        If[OptionValue["FilePath"] === None,
            If[audioFormat == None,
                audioFormat = StringTake[formattedURL, Last[StringPosition[formattedURL, formats], 0]];
                If[audioFormat == ".flac",
                    Message[InternetStreamOpenRead::flacnotsupported];
                    Return[$Failed];
                ];
                If[OptionValue["AudioFormat"] === None,
                    If[audioFormat == "", audioFormat = ".mp3"];
                    ,
                    audioFormat = OptionValue["AudioFormat"];
                ];
            ];

            If[(OptionValue["DeleteFileOnClose"] =!= Automatic || OptionValue["DeleteFileOnExit"] =!= Automatic),
                Message[InternetStreamOpenRead::nodeletionoptions];
            ];
            streamID = lfInternetStreamOpenReadMemory[ formattedURL, audioFormat, $CACERT, $useCACERT, verifySSLCert, useUsernamePassword, userName, password, timeout, useProxyUsernamePassword, proxyUserName, proxyPassword, proxyAddress, lfcLLproxyType];
            If[Head[streamID] === LibraryFunctionError, Message[InternetStreamOpenRead::openreadfail, formattedURL]; Return[$Failed]];
            setField[streamID, "FilePath", None];

            If[!IntegerQ[originalStreamType] || originalStreamType === 0,
                setField[streamID, "InternetStreamType", "Memory"];
                ,
                setField[streamID, "InternetStreamType", $originalStreamTypes[originalStreamType]];
            ];
            ,
            directory = FileNameTake[OptionValue["FilePath"], {1, -2}];
            If[directory === "", directory = Directory[];];
            filePath = ImportExport`FileUtilities`GetFilePath[directory];
            If[!DirectoryQ[directory] || filePath === $Failed,
                Message[InternetStreamOpenRead::nodirectory, directory];
                Return[$Failed];
            ];
            filePath = filePath <> FileNameTake[OptionValue["FilePath"]];
            audioFormat = "." <> Last[StringSplit[filePath, "."]];
            If[audioFormat == ".flac",
                Message[InternetStreamOpenRead::flacnotsupported];
            ];

            If[$streamTypeAssoc[filePath] === "Write" || $streamTypeAssoc[filePath] === "Read", Message[InternetStreamOpenRead::streamtypeconflict, "Internet", $streamTypeAssoc[filePath], filePath]; Return[$Failed];];

            If[(OptionValue["DeleteFileOnClose"] =!= Automatic || OptionValue["DeleteFileOnExit"] =!= Automatic) && (lfFileIsOpenedAsInternetStream[filePath] === 1 || lfURLIsOpenedAsInternetStream[formattedURL] === 1),
                Message[InternetStreamOpenRead::deletionoptionsimmutable];
                ,
                If[OptionValue["DeleteFileOnClose"] == False,
                    deleteOnClose = 0;
                    If[OptionValue["DeleteFileOnExit"] == True, deleteOnExit = 1;];
                ];
            ];

            existingURL = lfInternetStreamFilePathGetURL[filePath];
            If[existingURL =!= "" && existingURL =!= formattedURL,
                Message[InternetStreamOpenRead::openreadfailfilepath, filePath, formattedURL];
                Return[$Failed];
            ];

            streamID = lfInternetStreamOpenReadFile[ formattedURL, audioFormat, $CACERT, $useCACERT, filePath, deleteOnClose, deleteOnExit, verifySSLCert, useUsernamePassword, userName, password, timeout, useProxyUsernamePassword, proxyUserName, proxyPassword, proxyAddress, lfcLLproxyType];

            If[Head[streamID] === LibraryFunctionError, Message[InternetStreamOpenRead::openreadfail, formattedURL]; Return[$Failed]];
            $streamTypeAssoc[filePath] = "Internet";

            If[!IntegerQ[originalStreamType] || originalStreamType === 0,
                setField[streamID, "InternetStreamType", "File"];
                setField[streamID, "FilePath", filePath];
                ,
                setField[streamID, "InternetStreamType", $originalStreamTypes[originalStreamType]];
                setField[streamID, "FilePath", lfInternetStreamURLGetFilePath[formattedURL]];
                Message[InternetStreamOpenRead::filepathignored, filePath, formattedURL];
            ];
        ];

        hasWritePermissions = lfStreamHasOperatingSystemWritePermissions[streamID];
        If[Head[hasWritePermissions] === LibraryFunctionError, Message[InternetStreamOpenRead::openreadfail, formattedURL]; lfFileStreamClose[streamID]; Return[$Failed]];
        If[hasWritePermissions =!= 1, Message[InternetStreamOpenRead::nowriteperm, filePath, directory]; lfFileStreamClose[streamID]; Return[$Failed]];

        setField[streamID, "DataContainer", OptionValue["ContainerType"]];
        setField[streamID, "CallbackFunction", func];
        setField[streamID, "DataUpdates", OptionValue["DataUpdates"]];
        setField[streamID, "DownloadProgress", 0];
        setField[streamID, "InternetStream", True];
        setField[streamID, "URL", formattedURL];

        If[!(func === None) && OptionValue["DataUpdates"] == True, dataUpdates = 2];
        If[containerType == "MTensor", callbackDataType = 0];
        If[containerType == "RawArray", callbackDataType = 1];

        asyncObj = Internal`CreateAsynchronousTask[lfInternetStreamStartDownload, {streamID, dataUpdates, callbackDataType}, internetStreamInternalAsyncCallback[streamID, #, #2, #3] & ];
        setField[streamID, "AsyncObject", asyncObj];
        res = lfInternetStreamWaitForTransferInitialization[streamID];

        If[res < 0,
            AudioFileStreamTools`InternetStreamClose[AudioFileStreamTools`FileStreamObject[ streamID]];

            If[SameQ[$downloadStatusAssociation[res], "AuthenticationRequired"],
                passwordDialog[url, useUsernamePassword];
                If[$pwdDlgResult === $Canceled,
                    $downloadStatusMessagesAssociation[res][url];
                    Return[$Failed];
                    ,
                    Return[
                        AudioFileStreamTools`InternetStreamOpenRead[url, func,
                        "ContainerType" -> OptionValue["ContainerType"], "FilePath" -> OptionValue["FilePath"],
                        "AudioFormat" -> OptionValue["AudioFormat"], "DeleteFileOnClose" -> OptionValue["DeleteFileOnClose"], "DeleteFileOnExit" -> OptionValue["DeleteFileOnExit"],
                        "DataUpdates" -> OptionValue["DataUpdates"], "VerifySSLCertificate" -> OptionValue["VerifySSLCertificate"],
                        "UserName" -> $pwdDlgResult[[1]], "Password" -> $pwdDlgResult[[2]], "ConnectTimeout" -> OptionValue["ConnectTimeout"],
                        "ProxyUserName" -> OptionValue["ProxyUserName"], "ProxyPassword" -> OptionValue["ProxyPassword"], "ProxyType" -> OptionValue["ProxyType"], "ProxyOverride" -> OptionValue["ProxyOverride"]
                        ]
                    ];
                ];
            ];

            If[SameQ[$downloadStatusAssociation[res], "ProxyAuthenticationRequired"],
                proxyDialog[proxyAddress, useProxyUsernamePassword];
                If[$pwdDlgResult === $Canceled,
                    $downloadStatusMessagesAssociation[res][proxyAddress];
                    Return[$Failed];
                    ,
                    Return[
                        AudioFileStreamTools`InternetStreamOpenRead[url, func,
                            "ContainerType" -> OptionValue["ContainerType"], "FilePath" -> OptionValue["FilePath"],
                            "AudioFormat" -> OptionValue["AudioFormat"], "DeleteFileOnClose" -> OptionValue["DeleteFileOnClose"], "DeleteFileOnExit" -> OptionValue["DeleteFileOnExit"],
                            "DataUpdates" -> OptionValue["DataUpdates"], "VerifySSLCertificate" -> OptionValue["VerifySSLCertificate"],
                            "UserName" -> OptionValue["UserName"], "Password" -> OptionValue["Password"], "ConnectTimeout" -> OptionValue["ConnectTimeout"],
                            "ProxyUserName" -> $pwdDlgResult[[1]], "ProxyPassword" -> $pwdDlgResult[[2]], "ProxyType" -> OptionValue["ProxyType"], "ProxyOverride" -> OptionValue["ProxyOverride"]
                        ]
                    ];
                ];
            ];

            If[proxyErrorQ[res], (* Retry with other available proxy methods *)
                $downloadStatusMessagesAssociation[res][proxyAddress];
                availableProxies = Delete[availableProxies, 1];
                While[OptionValue["ProxyOverride"] === None && availableProxies =!= {},
                    (*Print["Retrying with " <> availableProxies[[1]]];*)
                    retriedStream = AudioFileStreamTools`InternetStreamOpenRead[url, func,
                        "ContainerType" -> OptionValue["ContainerType"], "FilePath" -> OptionValue["FilePath"],
                        "AudioFormat" -> OptionValue["AudioFormat"], "DeleteFileOnClose" -> OptionValue["DeleteFileOnClose"], "DeleteFileOnExit" -> OptionValue["DeleteFileOnExit"],
                        "DataUpdates" -> OptionValue["DataUpdates"], "VerifySSLCertificate" -> OptionValue["VerifySSLCertificate"],
                        "UserName" -> OptionValue["UserName"], "Password" -> OptionValue["Password"], "ConnectTimeout" -> OptionValue["ConnectTimeout"],
                        "ProxyUserName" -> OptionValue["ProxyUserName"], "ProxyPassword" -> OptionValue["ProxyPassword"], "ProxyType" -> OptionValue["ProxyType"], "ProxyOverride" -> availableProxies[[1]]
                    ];
                    If[retriedStream =!= $Failed, Return[retriedStream];];
                    availableProxies = Delete[availableProxies, 1];
                ];
                Return[$Failed];
            ];
        ];

        failureMessage = $downloadStatusMessagesAssociation[res];
        If[MissingQ[failureMessage], Return[AudioFileStreamTools`FileStreamObject[streamID]];];

        failureMessage[url];
        Return[$Failed];
    ]

AudioFileStreamTools`InternetStreamOpenRead[___]:= (Message[InternetStreamOpenRead::invalidargs]; Return[$Failed]);

AudioFileStreamTools`InternetStreamGetBufferedRange[obj_AudioFileStreamTools`FileStreamObject] :=
    Module[{streamID, startPos, endPos, bitDepth},
        streamID = getStreamID@obj;
        If[!KeyExistsQ[$openStreams, streamID], Message[InternetStreamGetBufferedRange::nostream, obj]; Return[$Failed]];

        startPos = 1;
        endPos = getField[streamID, "DownloadProgress"];
        If[endPos == Missing["NotAvailable"], endPos = 1];

        Return[{startPos, endPos}]
    ]


AudioFileStreamTools`InternetStreamGetBufferedRange[___]:= (Message[InternetStreamGetBufferedRange::invalidargs]; Return[$Failed]);

AudioFileStreamTools`InternetStreamDownloadStatus[obj_AudioFileStreamTools`FileStreamObject] :=
    Module[{streamID, res, resStr},
        streamID = getStreamID@obj;
        If[!KeyExistsQ[$openStreams, streamID], Message[InternetStreamDownloadStatus::nostream, obj]; Return[$Failed]];
        res = lfInternetStreamDownloadStatus[streamID];
        If[Head[res] == LibraryFunctionError, Message[InternetStreamDownloadStatus::nostream, obj]; Return[$Failed]];

        resStr = $downloadStatusAssociation[res];
        If[MissingQ[resStr], Return[$Failed]; (* Need message *)];
        Return[resStr];
    ]

AudioFileStreamTools`InternetStreamDownloadStatus[___]:= (Message[InternetStreamOpenRead::invalidargs]; Return[$Failed]);

AudioFileStreamTools`InternetStreamDownloadPercent[obj_AudioFileStreamTools`FileStreamObject] :=
    Module[{streamID, size, finalSize, frameCount, finalFrameCount, sizeDiv = -1, frameCountDiv = -1},
        streamID = getStreamID@obj;
        If[!KeyExistsQ[$openStreams, streamID], Message[InternetStreamDownloadPercent::nostream, obj]; Return[$Failed]];
        If[AudioFileStreamTools`InternetStreamDownloadStatus[obj] == "Complete", Return[N[1/1]];];
        size = lfInternetStreamCurrentDownloadSize[streamID];
        finalSize = lfInternetStreamFinalDownloadSize[streamID];
        If[size =!= LibraryFunctionError["LIBRARY_FUNCTION_ERROR", 6] && finalSize =!= LibraryFunctionError["LIBRARY_FUNCTION_ERROR", 6],
            If[finalSize >= size && finalSize > 0,
                (*Print[size];*)
                (*Print[finalSize];*)
                sizeDiv = N[size/finalSize];
            ]
        ];

        If[sizeDiv > 0, Return[sizeDiv];];

        frameCount = lfFileStreamGetMetaInformation[streamID, $metaInformationFields["FrameCount"]];
        finalFrameCount = lfFileStreamGetMetaInformation[streamID, $metaInformationFields["TotalFrameCount"]];
        If[frameCount =!= LibraryFunctionError["LIBRARY_FUNCTION_ERROR", 6] && finalFrameCount =!= LibraryFunctionError["LIBRARY_FUNCTION_ERROR", 6] && !MissingQ[frameCount] && !MissingQ[finalFrameCount],
            If[finalFrameCount >= frameCount && finalFrameCount > 0,
                frameCountDiv = N[frameCount/finalFrameCount];
            ]
        ];

        If[frameCountDiv >= 0, Return[frameCountDiv];];
        If[sizeDiv == 0, Return[sizeDiv];];

        Return[Missing["Indeterminate"]];
    ]

AudioFileStreamTools`InternetStreamDownloadStatus[___]:= (Message[InternetStreamOpenRead::invalidargs]; Return[$Failed]);

AudioFileStreamTools`InternetStreamReadN[args___] := AudioFileStreamTools`FileStreamReadN[args];
AudioFileStreamTools`InternetStreamClose[args___] := AudioFileStreamTools`FileStreamClose[args];
AudioFileStreamTools`InternetStreamGetReadPosition[args___] := AudioFileStreamTools`FileStreamGetReadPosition[args];
AudioFileStreamTools`InternetStreamSetReadPosition[args___] := AudioFileStreamTools`FileStreamSetReadPosition[args];
AudioFileStreamTools`InternetStreamGetMetaInformation[args___] := AudioFileStreamTools`FileStreamGetMetaInformation[args];
AudioFileStreamTools`InternetStreamGetTotalNumberOfFrames[args___] := AudioFileStreamTools`FileStreamGetTotalNumberOfFrames[args];
AudioFileStreamTools`InternetStreamGetStreamInformation[args___] := AudioFileStreamTools`FileStreamGetStreamInformation[args];

AudioFileStreamTools`InternetStreamGetTags[args___] := AudioFileStreamTools`FileStreamGetTags[args];

AudioFileStreamTools`FileStreamGetTags[obj_AudioFileStreamTools`FileStreamObject, "ID3v2"] :=
    Module[{streamID, ret (*currentMetaTagCount*), prevKey},
        streamID = getStreamID@obj;
        If[getField[streamID, "InternetStreamType"] == "Memory", Message[FileStreamGetTags::internetstreammemory]; Return[$Failed]];
        If[!MissingQ[$fsiMetaTagKeysID3v2[streamID]], KeyDropFrom[$metaTagAssociation, $fsiMetaTagKeysID3v2[streamID]];];
        $fsiMetaTagKeysID3v2[streamID] = {};
        If[!KeyExistsQ[$openStreams, streamID], Message[FileStreamGetTags::nostream, obj]; Return[$Failed]];
        lfFileStreamOpenTags[streamID];
        ret = getID3v2TagsAssociation[streamID, {}];
        If[ret === $Failed,
            Message[FileStreamGetTags::notsupported, "ID3v2"];
            Return[$Failed];
        ];
        If[MissingQ[$rootMetaTagAssociationID3v2[streamID]],
            $rootMetaTagAssociationID3v2[streamID] = ret;
            ,
            prevKey = ret;
            ret = $rootMetaTagAssociationID3v2[streamID];
            $metaTagAssociation[{"ID3v2", ret}] = $metaTagAssociation[{"ID3v2", prevKey}];
            KeyDropFrom[$metaTagAssociation, {"ID3v2", prevKey}];
        ]
        lfFileStreamCloseTags[streamID, 0];
        (*Return[AudioFileStreamTools`MetaTag[currentMetaTagCount]]*)
        Return[AudioFileStreamTools`MetaTag["ID3v2", ret]]
    ]

AudioFileStreamTools`FileStreamGetTags[obj_AudioFileStreamTools`FileStreamObject, "ID3v1"] :=
    Module[{streamID, ret (*currentMetaTagCount*), prevKey},
        streamID = getStreamID@obj;
        If[getField[streamID, "InternetStreamType"] == "Memory", Message[FileStreamGetTags::internetstreammemory]; Return[$Failed]];
        If[!MissingQ[$fsiMetaTagKeysID3v1[streamID]], KeyDropFrom[$metaTagAssociation, $fsiMetaTagKeysID3v1[streamID]];];
        $fsiMetaTagKeysID3v1[streamID] = {};
        If[!KeyExistsQ[$openStreams, streamID], Message[FileStreamGetTags::nostream, obj]; Return[$Failed]];
        lfFileStreamOpenTags[streamID];
        If[SameQ[Head[lfFileStreamHasID3v1Tag[streamID]], LibraryFunctionError],
            Message[FileStreamGetTags::notsupported, "ID3v1"];
            Return[$Failed];
        ];
        ret = getID3v1TagsAssociation[streamID];
        If[MissingQ[$rootMetaTagAssociationID3v1[streamID]],
            $rootMetaTagAssociationID3v1[streamID] = ret;
            ,
            prevKey = ret;
            ret = $rootMetaTagAssociationID3v1[streamID];
            $metaTagAssociation[{"ID3v1", ret}] = $metaTagAssociation[{"ID3v1", prevKey}];
            KeyDropFrom[$metaTagAssociation, {"ID3v1", prevKey}];
        ]
        lfFileStreamCloseTags[streamID, 0];
        (*Return[AudioFileStreamTools`MetaTag[currentMetaTagCount]]*)
        Return[AudioFileStreamTools`MetaTag["ID3v1", ret]]
    ]

AudioFileStreamTools`FileStreamGetTags[obj_AudioFileStreamTools`FileStreamObject, "APE"] :=
    Module[{streamID, ret (*currentMetaTagCount*), prevKey},
        streamID = getStreamID@obj;
        If[getField[streamID, "InternetStreamType"] == "Memory", Message[FileStreamGetTags::internetstreammemory]; Return[$Failed]];
        If[!MissingQ[$fsiMetaTagKeysAPE[streamID]], KeyDropFrom[$metaTagAssociation, $fsiMetaTagKeysAPE[streamID]];];
        $fsiMetaTagKeysAPE[streamID] = {};
        If[!KeyExistsQ[$openStreams, streamID], Message[FileStreamGetTags::nostream, obj]; Return[$Failed]];
        lfFileStreamOpenTags[streamID];
        ret = getAPETagsAssociation[streamID];
        If[ret === $Failed,
            Message[FileStreamGetTags::notsupported, "APE"];
            Return[$Failed];
        ];
        If[MissingQ[$rootMetaTagAssociationAPE[streamID]],
            $rootMetaTagAssociationAPE[streamID] = ret;
            ,
            prevKey = ret;
            ret = $rootMetaTagAssociationAPE[streamID];
            $metaTagAssociation[{"APE", ret}] = $metaTagAssociation[{"APE", prevKey}];
            KeyDropFrom[$metaTagAssociation, {"APE", prevKey}];
        ]
        lfFileStreamCloseTags[streamID, 0];
        (*Return[AudioFileStreamTools`MetaTag[currentMetaTagCount]]*)
        Return[AudioFileStreamTools`MetaTag["APE", ret]]
    ]


AudioFileStreamTools`FileStreamGetTags[obj_AudioFileStreamTools`FileStreamObject, "Xiph"] :=
    Module[{streamID, ret (*currentMetaTagCount*), prevKey},
        streamID = getStreamID@obj;
        If[getField[streamID, "InternetStreamType"] == "Memory", Message[FileStreamGetTags::internetstreammemory]; Return[$Failed]];
        If[!MissingQ[$fsiMetaTagKeysXiph[streamID]], KeyDropFrom[$metaTagAssociation, $fsiMetaTagKeysXiph[streamID]];];
        $fsiMetaTagKeysXiph[streamID] = {};
        If[!KeyExistsQ[$openStreams, streamID], Message[FileStreamGetTags::nostream, obj]; Return[$Failed]];
        lfFileStreamOpenTags[streamID];
        ret = getXiphTagsAssociation[streamID];
        If[ret === $Failed,
            Message[FileStreamGetTags::notsupported, "Xiph"];
            Return[$Failed];
        ];
        If[MissingQ[$rootMetaTagAssociationXiph[streamID]],
            $rootMetaTagAssociationXiph[streamID] = ret;
            ,
            prevKey = ret;
            ret = $rootMetaTagAssociationXiph[streamID];
            $metaTagAssociation[{"Xiph", ret}] = $metaTagAssociation[{"Xiph", prevKey}];
            KeyDropFrom[$metaTagAssociation, {"Xiph", prevKey}];
        ]
        lfFileStreamCloseTags[streamID, 0];
        (*Return[AudioFileStreamTools`MetaTag[currentMetaTagCount]]*)
        Return[AudioFileStreamTools`MetaTag["Xiph", ret]]
    ]

AudioFileStreamTools`MetaTag[tagType_, tagNo_][tagID_, "EntryCount"] := Module[{count = 0, tagPair},
    tagPair = $metaTagAssociation[{tagType, tagNo}];
    If[MissingQ[tagPair], Message[AudioFileStreamTools`MetaTag::invalid, AudioFileStreamTools`MetaTag[tagType, tagNo]]; Return[$Failed];];
    Scan[(If[SameQ[tagID, #["ID"]], count++]) &, $rawTagContainer[tagType, tagPair[[1]],tagPair[[2]]]];
    Return[count]
]

AudioFileStreamTools`MetaTag[tagType_, tagNo_][Keys] := Module[{l, tagPair},
    tagPair = $metaTagAssociation[{tagType, tagNo}];
    If[MissingQ[tagPair], Message[AudioFileStreamTools`MetaTag::invalid, AudioFileStreamTools`MetaTag[tagType, tagNo]]; Return[$Failed];];
    l = {};
    Map[(AppendTo[l,#["ID"]])&, $rawTagContainer[tagType, tagPair[[1]],tagPair[[2]]]];
    Return[DeleteDuplicates[l]]
]

AudioFileStreamTools`MetaTag[tagType_, tagNo_][tagID_] := Module[{retVal = None, tagPair},
    tagPair = $metaTagAssociation[{tagType, tagNo}];
    If[MissingQ[tagPair], Message[AudioFileStreamTools`MetaTag::invalid, AudioFileStreamTools`MetaTag[tagType, tagNo]]; Return[$Failed];];
    Scan[(If[SameQ[tagID, #["ID"]], retVal = #["Frame"]; Return[]]) &, $rawTagContainer[tagType, tagPair[[1]],tagPair[[2]]]];
    Return[retVal]
]

AudioFileStreamTools`MetaTag[tagType_, tagNo_][tagID_, n_Integer] := Module[{count = 0, retVal = None, tagPair},
    tagPair = $metaTagAssociation[{tagType, tagNo}];
    If[MissingQ[tagPair], Message[AudioFileStreamTools`MetaTag::invalid, AudioFileStreamTools`MetaTag[tagType, tagNo]]; Return[$Failed];];
    Scan[(
        If[SameQ[tagID, #["ID"]], count++];
        If[SameQ[count, n], retVal = #["Frame"]; Return[]]
    ) &, $rawTagContainer[tagType, tagPair[[1]],tagPair[[2]]]];
    Return[retVal]
]

AudioFileStreamTools`MetaTag[tagType_, tagNo_][tagID_, All] := Module[{retVal = {}, tagPair},
    tagPair = $metaTagAssociation[{tagType, tagNo}];
    If[MissingQ[tagPair], Message[AudioFileStreamTools`MetaTag::invalid, AudioFileStreamTools`MetaTag[tagType, tagNo]]; Return[$Failed];];
    Scan[(
        If[SameQ[tagID, #["ID"]], AppendTo[retVal, #["Frame"]]];
    ) &, $rawTagContainer[tagType, tagPair[[1]],tagPair[[2]]]];
    Return[retVal]
]

AudioFileStreamTools`MetaTag[tagType_, tagNo_][All] := Module[{retVal = {}, tagPair},
    tagPair = $metaTagAssociation[{tagType, tagNo}];
    If[MissingQ[tagPair], Message[AudioFileStreamTools`MetaTag::invalid, AudioFileStreamTools`MetaTag[tagType, tagNo]]; Return[$Failed];];
    Return[AssociationMap[(AudioFileStreamTools`MetaTag[tagType, tagNo][#, All])&,AudioFileStreamTools`MetaTag[tagType, tagNo][Keys]]];
]

reloadTag[tag_] := Quiet[AudioFileStreamTools`FileStreamGetTags[AudioFileStreamTools`FileStreamObject[tag[[1]]], tag[[2]]]];

reloadAllTags[exceptTag_] := Module[{tagList},
    tagList = Map[({#[[2]][[1]], #[[1]][[1]]}) &,  Normal[$metaTagAssociation]];
    tagList = DeleteDuplicates[tagList];
    tagList = DeleteCases[tagList, exceptTag];
    Scan[reloadTag[#] &, tagList];
]

AudioFileStreamTools`MetaTag[tagType_, tagNo_][All, "Remove"] := Module[{tagPair, res, tag},
    tagPair = $metaTagAssociation[{tagType, tagNo}];
    If[MissingQ[tagPair], Message[AudioFileStreamTools`MetaTag::invalid, AudioFileStreamTools`MetaTag[tagType, tagNo]]; Return[$Failed];];
    If[getField[tagPair[[1]], "InternetStream"] == True, Message[MetaTag::internetstream]; Return[$Failed]];
    If[lfStreamHasOperatingSystemWritePermissions[tagPair[[1]]] =!= 1, Message[AudioFileStreamTools`MetaTag::nowriteperm]; Return[$Failed];];

    lfFileStreamOpenTags[tagPair[[1]]];
    res = lfFileStreamRemoveTag[tagPair[[1]], $tagTypes[tagType]];
    lfFileStreamCloseTags[tagPair[[1]], 1];
    If[lfStreamHasOperatingSystemWritePermissions[tagPair[[1]]] =!= 1, Message[AudioFileStreamTools`MetaTag::nowriteperm]; Return[$Failed];];
    If[res == LibraryFunctionError["LIBRARY_FUNCTION_ERROR", 6],
        If[tagType == "ID3v2",
            tag = AudioFileStreamTools`MetaTag[tagType, tagNo];
            While[tag[Keys] =!= {}, Scan[(tag[#, "Remove"]) &, tag[Keys]]];
        ];
        If[tagType == "Xiph",
            tag = AudioFileStreamTools`MetaTag[tagType, tagNo];
            Scan[(tag[#, "Remove"]) &, tag[Keys]];
        ];
    ];
    reloadAllTags[{tagPair[[1]], tagType}];
    Return[AudioFileStreamTools`FileStreamGetTags[AudioFileStreamTools`FileStreamObject[tagPair[[1]]], tagType]];

]
AudioFileStreamTools`MetaTag[tagType_, tagNo_][tagID_, "Remove"] := AudioFileStreamTools`MetaTag[tagType, tagNo][tagID, "Remove", 1];
AudioFileStreamTools`MetaTag[tagType_, tagNo_][tagID_, "Remove", n_Integer] := Module[{count = 0, tagPair, index = None},
    If[tagType === "ID3v1", Message[AudioFileStreamTools`MetaTag::notsupported, tagType]; Return[$Failed];];
    tagPair = $metaTagAssociation[{tagType, tagNo}];
    If[MissingQ[tagPair], Message[AudioFileStreamTools`MetaTag::invalid, AudioFileStreamTools`MetaTag[tagType, tagNo]]; Return[$Failed];];
    If[getField[tagPair[[1]], "InternetStream"] == True, Message[MetaTag::internetstream]; Return[$Failed]];
    If[lfStreamHasOperatingSystemWritePermissions[tagPair[[1]]] =!= 1, Message[AudioFileStreamTools`MetaTag::nowriteperm]; Return[$Failed];];

    Scan[(
        If[SameQ[tagID, #[[2]]["ID"]], count++];
        If[SameQ[count, n], index = #[[1]]; Return[]];
    ) &, Normal[$rawTagContainer[tagType, tagPair[[1]],tagPair[[2]]]]];

    If[SameQ[index, None], Message[AudioFileStreamTools`MetaTag::invalidframe]; Return[$Failed]];
    lfFileStreamOpenTags[tagPair[[1]]];
    If[tagType == "ID3v2",
        lfFileStreamRemoveID3v2Frame[tagPair[[1]], AppendTo[tagPair[[2]], index]];
    ];
    If[tagType == "APE",
        lfFileStreamRemoveAPEItem[tagPair[[1]], ToCharacterCode[tagID]];
    ];
    If[tagType == "Xiph",
        lfFileStreamRemoveXiphKey[tagPair[[1]], ToCharacterCode[ToUpperCase[tagID]]];
    ];
    lfFileStreamCloseTags[tagPair[[1]], 1];
    If[lfStreamHasOperatingSystemWritePermissions[tagPair[[1]]] =!= 1, Message[AudioFileStreamTools`MetaTag::nowriteperm]; Return[$Failed];];
    reloadAllTags[{tagPair[[1]], tagType}];
    Return[AudioFileStreamTools`FileStreamGetTags[AudioFileStreamTools`FileStreamObject[tagPair[[1]]], tagType]];
]

AudioFileStreamTools`MetaTag[tagType_, tagNo_][tagID_, "Add", frameType_] := Module[{tagPair, typeEnum},
    If[tagType === "ID3v1", Message[AudioFileStreamTools`MetaTag::notsupported, tagType]; Return[$Failed];];
    If[tagType === "ID3v2", Message[AudioFileStreamTools`MetaTag::notsupported, tagType]; Return[$Failed];];
    If[tagType === "Xiph", Message[AudioFileStreamTools`MetaTag::notsupported, tagType]; Return[$Failed];];
    typeEnum = $APETypesAssociation[frameType];
    If[MissingQ[typeEnum], Message[AudioFileStreamTools`MetaTag::invalidframe]; Return[$Failed];];
    tagPair = $metaTagAssociation[{tagType, tagNo}];
    If[MissingQ[tagPair], Message[AudioFileStreamTools`MetaTag::invalid, AudioFileStreamTools`MetaTag[tagType, tagNo]]; Return[$Failed];];
    If[getField[tagPair[[1]], "InternetStream"] == True, Message[MetaTag::internetstream]; Return[$Failed]];
    If[lfStreamHasOperatingSystemWritePermissions[tagPair[[1]]] =!= 1, Message[AudioFileStreamTools`MetaTag::nowriteperm]; Return[$Failed];];

    lfFileStreamOpenTags[tagPair[[1]]];
    lfFileStreamAddAPEItem[tagPair[[1]], typeEnum, ToCharacterCode[tagID]];
    lfFileStreamCloseTags[tagPair[[1]], 1];
    If[lfStreamHasOperatingSystemWritePermissions[tagPair[[1]]] =!= 1, Message[AudioFileStreamTools`MetaTag::nowriteperm]; Return[$Failed];];
    reloadAllTags[{tagPair[[1]], tagType}];
    AudioFileStreamTools`FileStreamGetTags[AudioFileStreamTools`FileStreamObject[tagPair[[1]]], tagType];
]

AudioFileStreamTools`MetaTag[tagType_, tagNo_][tagID_, "Add"] := Module[{tagPair},
    If[tagType === "ID3v1", Message[AudioFileStreamTools`MetaTag::notsupported, tagType]; Return[$Failed];];
    If[tagType === "APE", Message[AudioFileStreamTools`MetaTag::notsupported, tagType]; Return[$Failed];];
    tagPair = $metaTagAssociation[{tagType, tagNo}];
    If[MissingQ[tagPair], Message[AudioFileStreamTools`MetaTag::invalid, AudioFileStreamTools`MetaTag[tagType, tagNo]]; Return[$Failed];];
    If[getField[tagPair[[1]], "InternetStream"] == True, Message[MetaTag::internetstream]; Return[$Failed]];
    If[lfStreamHasOperatingSystemWritePermissions[tagPair[[1]]] =!= 1, Message[AudioFileStreamTools`MetaTag::nowriteperm]; Return[$Failed];];

    lfFileStreamOpenTags[tagPair[[1]]];
    If[SameQ[tagType, "ID3v2"],
        lfFileStreamAddID3v2Frame[tagPair[[1]], ToCharacterCode[tagID], tagPair[[2]]];
    ];
    If[SameQ[tagType, "Xiph"],
        lfFileStreamAddXiphKey[tagPair[[1]], ToCharacterCode[ToUpperCase[tagID]]];
    ];
    lfFileStreamCloseTags[tagPair[[1]], 1];
    If[lfStreamHasOperatingSystemWritePermissions[tagPair[[1]]] =!= 1, Message[AudioFileStreamTools`MetaTag::nowriteperm]; Return[$Failed];];
    reloadAllTags[{tagPair[[1]], tagType}];
    AudioFileStreamTools`FileStreamGetTags[AudioFileStreamTools`FileStreamObject[tagPair[[1]]], tagType];
]

AudioFileStreamTools`MetaTag[tagType_, tagNo_][tagID_, "Set", property_String, value_] := AudioFileStreamTools`MetaTag[tagType, tagNo][tagID, "Set", 1, property, value];

AudioFileStreamTools`MetaTag[tagType_, tagNo_][tagID_, "Set", n_Integer, property_String, value_] := Module[{count = 0, tagPair, index = None, frameType, data, ret, elements},
    tagPair = $metaTagAssociation[{tagType, tagNo}];
    If[MissingQ[tagPair], Message[AudioFileStreamTools`MetaTag::invalid, AudioFileStreamTools`MetaTag[tagType, tagNo]]; Return[$Failed];];
    If[getField[tagPair[[1]], "InternetStream"] == True, Message[MetaTag::internetstream]; Return[$Failed]];
    If[lfStreamHasOperatingSystemWritePermissions[tagPair[[1]]] =!= 1, Message[AudioFileStreamTools`MetaTag::nowriteperm]; Return[$Failed];];

    Scan[(
        If[SameQ[tagID, #[[2]]["ID"]], count++];
        If[SameQ[count, n], index = #[[1]]; frameType = #[[2]]["Type"]; Return[]];
    ) &, Normal[$rawTagContainer[tagType, tagPair[[1]], tagPair[[2]]]]];

    If[SameQ[index, None], Message[AudioFileStreamTools`MetaTag::invalidframe]; Return[$Failed]];

    lfFileStreamOpenTags[tagPair[[1]]];

    If[SameQ[tagType, "ID3v2"],
        index = Append[tagPair[[2]], index];
        If[!MissingQ[$metaTagElementsAssociation[tagID]],
            elements = $metaTagElementsAssociation[tagID]["Elements"];
            ,
            Switch[Characters[tagID][[1]],
                "T",
                elements = $metaTagElementsAssociation["T***"]["Elements"],
                "W",
                elements = $metaTagElementsAssociation["W***"]["Elements"],
                _,
                lfFileStreamCloseTags[tagPair[[1]], 0];
                reloadAllTags[{tagPair[[1]], tagType}];
                AudioFileStreamTools`FileStreamGetTags[AudioFileStreamTools`FileStreamObject[tagPair[[1]]],"ID3v2"];
                Message[AudioFileStreamTools`MetaTag::invalidframe];
                Return[$Failed]
            ]
        ];

        If[!MemberQ[elements, property],
            lfFileStreamCloseTags[tagPair[[1]], 0];
            reloadAllTags[{tagPair[[1]], tagType}];
            AudioFileStreamTools`FileStreamGetTags[AudioFileStreamTools`FileStreamObject[tagPair[[1]]],"ID3v2"];
            Message[AudioFileStreamTools`MetaTag::invalidframe];
            Return[$Failed]
        ];

        Switch[property,
            "Channels",
            lfFileStreamClearID3v2FrameChannels[tagPair[[1]], frameType, index];
            Scan[(
                lfFileStreamSetID3v2FrameChannel[tagPair[[1]], frameType, $channelTypes[#[[1]]], #[[2]]["BitsRepresentingPeak"], #[[2]]["VolumeAdjustment"], Normal[#[[2]]["PeakVolume"]], index];
            )&, Normal[value]];
            ,
            "ChildElements",
            lfFileStreamClearID3v2TableOFContentsFrameChildElements[tagPair[[1]], frameType, index];
            Scan[(lfFileStreamSetID3v2TableOFContentsFrameChildElements[tagPair[[1]], frameType, Normal[#], index])&, value];
            ,
            "SynchedEvents",
            data = {};
            Scan[(AppendTo[data, #[[1]]]; AppendTo[data, $eventTimingCodesAssociation[#[[2]]]])&, Normal[value]];
            ret = lfFileStreamSetID3v2FrameSynchedEvents[tagPair[[1]], frameType, data, index];
            ,
            "SynchedText",
            lfFileStreamClearID3v2FrameSynchedText[tagPair[[1]], frameType, index];
            Scan[(lfFileStreamSetID3v2FrameSynchedText[tagPair[[1]], frameType, #[[1]], ToCharacterCode[#[[2]]], index])&, Normal[value]];
            ,
            "Values",
            data = ToCharacterCode[value];
            Scan[(AppendTo[data[[#]], 0]) &, Range[Length[data] - 1]];
            data = Flatten[data];
            ret = lfFileStreamSetID3v2FrameValues[tagPair[[1]], frameType, data, index];
            ,
            "Description",
            data = ToCharacterCode[value];
            ret = lfFileStreamSetID3v2FrameDescription[tagPair[[1]], frameType, data, index];
            ,
            "Language",
            data = ToCharacterCode[value];
            ret = lfFileStreamSetID3v2FrameLanguage[tagPair[[1]], frameType, data, index];
            ,
            "FileName",
            data = ToCharacterCode[value];
            ret = lfFileStreamSetID3v2FrameFileName[tagPair[[1]], frameType, data, index];
            ,
            "MimeType",
            data = ToCharacterCode[value];
            ret = lfFileStreamSetID3v2FrameMimeType[tagPair[[1]], frameType, data, index];
            ,
            "Picture",
            data = Normal[value];
            ret = lfFileStreamSetID3v2FramePicture[tagPair[[1]], frameType, data, index];
            ,
            "Seller",
            data = ToCharacterCode[value];
            ret = lfFileStreamSetID3v2FrameSeller[tagPair[[1]], frameType, data, index];
            ,
            "PurchaseDate",
            data = ToCharacterCode[value];
            ret = lfFileStreamSetID3v2FramePurchaseDate[tagPair[[1]], frameType, data, index];
            ,
            "PricePaid",
            data = ToCharacterCode[value];
            ret = lfFileStreamSetID3v2FramePricePaid[tagPair[[1]], frameType, data, index];
            ,
            "Email",
            data = ToCharacterCode[value];
            ret = lfFileStreamSetID3v2FrameEmail[tagPair[[1]], frameType, data, index];
            ,
            "Object",
            data = Normal[value];
            ret = lfFileStreamSetID3v2FrameObject[tagPair[[1]], frameType, data, index];
            ,
            "Owner",
            data = ToCharacterCode[value];
            ret = lfFileStreamSetID3v2FrameOwner[tagPair[[1]], frameType, data, index];
            ,
            "Data",
            data = Normal[value];
            ret = lfFileStreamSetID3v2FrameData[tagPair[[1]], frameType, data, index];
            ,
            "Identifier",
            data = Normal[value];
            ret = lfFileStreamSetID3v2FrameIdentifier[tagPair[[1]], frameType, data, index];
            ,
            "URL",
            data = ToCharacterCode[value];
            ret = lfFileStreamSetID3v2FrameURL[tagPair[[1]], frameType, data, index];
            ,
            "Text",
            data = ToCharacterCode[value];
            ret = lfFileStreamSetID3v2FrameText[tagPair[[1]], frameType, data, index];
            ,
            "EndOffset",
            ret = lfFileStreamSetID3v2FrameEndOffset[tagPair[[1]], frameType, value, index];
            ,
            "StartOffset",
            ret = lfFileStreamSetID3v2FrameStartOffset[tagPair[[1]], frameType, value, index];
            ,
            "EndTime",
            ret = lfFileStreamSetID3v2FrameEndTime[tagPair[[1]], frameType, value, index];
            ,
            "StartTime",
            ret = lfFileStreamSetID3v2FrameStartTime[tagPair[[1]], frameType, value, index];
            ,
            "PictureType",
            ret = lfFileStreamSetID3v2FramePictureType[tagPair[[1]], frameType, $pictureTypes[value], index];
            ,
            "LyricsType",
            ret = lfFileStreamSetID3v2FrameLyricsType[tagPair[[1]], frameType, $lyricsTypes[value], index];
            ,
            "TimestampFormat",
            ret = lfFileStreamSetID3v2FrameTimeStampFormat[tagPair[[1]], frameType, $eventTimestampFormat[value], index];
            ,
            "Ordered",
            data = If[TrueQ[value], 1, 0];
            ret = lfFileStreamSetID3v2FrameOrdered[tagPair[[1]], frameType, data, index];
            ,
            "TopLevel",
            data = If[TrueQ[value], 1, 0];
            ret = lfFileStreamSetID3v2FrameTopLevel[tagPair[[1]], frameType, data, index];

        ];


        lfFileStreamCloseTags[tagPair[[1]], 1];
        If[lfStreamHasOperatingSystemWritePermissions[tagPair[[1]]] =!= 1, Message[AudioFileStreamTools`MetaTag::nowriteperm]; Return[$Failed];];
        reloadAllTags[{tagPair[[1]], tagType}];
        AudioFileStreamTools`FileStreamGetTags[AudioFileStreamTools`FileStreamObject[tagPair[[1]]], "ID3v2"];
        Return[Null];
    ];

    If[SameQ[tagType, "ID3v1"],
        If[property != "Value",
            lfFileStreamCloseTags[tagPair[[1]], 0];
            reloadAllTags[{tagPair[[1]], tagType}];
            AudioFileStreamTools`FileStreamGetTags[AudioFileStreamTools`FileStreamObject[tagPair[[1]]], "ID3v1"];
            Message[AudioFileStreamTools`MetaTag::notsupported, tagType];
            Return[$Failed];
        ];

        data = value;
        If[tagID === "YEAR" || tagID === "TRACK" || tagID === "GENRE",
            If[!IntegerQ[data],
                lfFileStreamCloseTags[tagPair[[1]], 0];
                reloadAllTags[{tagPair[[1]], tagType}];
                AudioFileStreamTools`FileStreamGetTags[AudioFileStreamTools`FileStreamObject[tagPair[[1]]], "ID3v1"];
                Message[AudioFileStreamTools`MetaTag::notsupported, tagType];
                Return[$Failed];
            ];
            ,
            If[!StringQ[data],
                lfFileStreamCloseTags[tagPair[[1]], 0];
                reloadAllTags[{tagPair[[1]], tagType}];
                AudioFileStreamTools`FileStreamGetTags[AudioFileStreamTools`FileStreamObject[tagPair[[1]]], "ID3v1"];
                Message[AudioFileStreamTools`MetaTag::notsupported, tagType];
                Return[$Failed];
            ]
        ];
        ret = lfFileStreamSetID3v1Element[tagPair[[1]], $ID3v1Keys[tagID], data];
        lfFileStreamCloseTags[tagPair[[1]], 1];
        If[lfStreamHasOperatingSystemWritePermissions[tagPair[[1]]] =!= 1, Message[AudioFileStreamTools`MetaTag::nowriteperm]; Return[$Failed];];
        reloadAllTags[{tagPair[[1]], tagType}];
        AudioFileStreamTools`FileStreamGetTags[AudioFileStreamTools`FileStreamObject[tagPair[[1]]], "ID3v1"];
        Return[Null];
    ];

    If[SameQ[tagType, "APE"],
        data = value;
        If[frameType === $APETypesAssociation["Text"],

            If[property != "Values",
                lfFileStreamCloseTags[tagPair[[1]], 0];
                reloadAllTags[{tagPair[[1]], tagType}];
                AudioFileStreamTools`FileStreamGetTags[AudioFileStreamTools`FileStreamObject[tagPair[[1]]], "APE"];
                Message[AudioFileStreamTools`MetaTag::notsupported, tagType];
                Return[$Failed];
            ];

            If[!ListQ[data] || !AllTrue[data, StringQ],
                lfFileStreamCloseTags[tagPair[[1]], 0];
                reloadAllTags[{tagPair[[1]], tagType}];
                AudioFileStreamTools`FileStreamGetTags[AudioFileStreamTools`FileStreamObject[tagPair[[1]]], "APE"];
                Message[AudioFileStreamTools`MetaTag::notsupported, tagType];
                Return[$Failed];
            ];
            data = ToCharacterCode[value];
            Scan[(AppendTo[data[[#]], 0]) &, Range[Length[data] - 1]];
            data = Flatten[data];
            ret = lfFileStreamSetAPEItemValues[tagPair[[1]], data, ToCharacterCode[tagID]];
            ,
            If[property != "Data",
                lfFileStreamCloseTags[tagPair[[1]], 0];
                reloadAllTags[{tagPair[[1]], tagType}];
                AudioFileStreamTools`FileStreamGetTags[AudioFileStreamTools`FileStreamObject[tagPair[[1]]], "APE"];
                Message[AudioFileStreamTools`MetaTag::notsupported, tagType];
                Return[$Failed];
            ];

            If[!ByteArrayQ[data],
                lfFileStreamCloseTags[tagPair[[1]], 0];
                reloadAllTags[{tagPair[[1]], tagType}];
                AudioFileStreamTools`FileStreamGetTags[AudioFileStreamTools`FileStreamObject[tagPair[[1]]], "APE"];
                Message[AudioFileStreamTools`MetaTag::notsupported, tagType];
                Return[$Failed];
            ];
            ret = lfFileStreamSetAPEItemData[tagPair[[1]], data, ToCharacterCode[tagID]];
        ];
        lfFileStreamCloseTags[tagPair[[1]], 1];
        If[lfStreamHasOperatingSystemWritePermissions[tagPair[[1]]] =!= 1, Message[AudioFileStreamTools`MetaTag::nowriteperm]; Return[$Failed];];
        reloadAllTags[{tagPair[[1]], tagType}];
        AudioFileStreamTools`FileStreamGetTags[AudioFileStreamTools`FileStreamObject[tagPair[[1]]], "APE"];
        Return[Null];
    ];

    If[SameQ[tagType, "Xiph"],
        data = value;

        If[property != "Values",
            lfFileStreamCloseTags[tagPair[[1]], 0];
            reloadAllTags[{tagPair[[1]], tagType}];
            AudioFileStreamTools`FileStreamGetTags[AudioFileStreamTools`FileStreamObject[tagPair[[1]]], "Xiph"];
            Message[AudioFileStreamTools`MetaTag::notsupported, tagType];
            Return[$Failed];
        ];

        If[!ListQ[data] || !AllTrue[data, StringQ],
            lfFileStreamCloseTags[tagPair[[1]], 0];
            reloadAllTags[{tagPair[[1]], tagType}];
            AudioFileStreamTools`FileStreamGetTags[AudioFileStreamTools`FileStreamObject[tagPair[[1]]], "Xiph"];
            Message[AudioFileStreamTools`MetaTag::notsupported, tagType];
            Return[$Failed];
        ];
        data = ToCharacterCode[value];
        Scan[(AppendTo[data[[#]], 0]) &, Range[Length[data] - 1]];
        data = Flatten[data];
        ret = lfFileStreamSetXiphValues[tagPair[[1]], data, ToCharacterCode[ToUpperCase[tagID]]];

        lfFileStreamCloseTags[tagPair[[1]], 1];
        If[lfStreamHasOperatingSystemWritePermissions[tagPair[[1]]] =!= 1, Message[AudioFileStreamTools`MetaTag::nowriteperm]; Return[$Failed];];
        reloadAllTags[{tagPair[[1]], tagType}];
        AudioFileStreamTools`FileStreamGetTags[AudioFileStreamTools`FileStreamObject[tagPair[[1]]], "Xiph"];
        Return[Null];
    ];
    
    Message[AudioFileStreamTools`MetaTag::notsupported, tagType];
    Return[$Failed];
]

(* Stream Handle Functions *)

check1DListType[element_] := If[!(Developer`RealQ[#] || IntegerQ[#]), Return[True]]&

getStreamID[ obj_AudioFileStreamTools`FileStreamObject]:= Return[ obj[[1]]]

setField[key_,field_,value_]:=Module[{innerAssoc},
    innerAssoc=Lookup[$openStreams,key];
    If[Head@innerAssoc === Missing, innerAssoc=Association[];];
    AssociateTo[innerAssoc, field->value];
    AssociateTo[$openStreams,key->innerAssoc]
]

getField[key_,field_]:=Module[{innerAssoc},
    innerAssoc=Lookup[$openStreams,key];
    If[Head@innerAssoc===Missing,Return[Missing["NotAvailable"]]];
    Lookup[innerAssoc,field]
]

getField[key_]:=Module[{innerAssoc},
    innerAssoc=Lookup[$openStreams,key];
    If[Head@innerAssoc===Missing,Return[Missing["NotAvailable"]]];
    Return[innerAssoc]
]

(* Helper functions *)

$metaTagCountID3v2 = 1;
$metaTagCountID3v1 = 1;
$metaTagCountAPE = 1;
$metaTagCountXiph = 1;

$metaTagAssociation = Association[];
$rootMetaTagAssociationID3v2 = Association[];
$fsiMetaTagKeysID3v2 = Association[];

$rootMetaTagAssociationID3v1 = Association[];
$fsiMetaTagKeysID3v1 = Association[];

$rootMetaTagAssociationAPE = Association[];
$fsiMetaTagKeysAPE = Association[];

$rootMetaTagAssociationXiph = Association[];
$fsiMetaTagKeysXiph = Association[];

$ID3v1Keys = <|"TITLE" -> 0, "ARTIST" -> 1, "ALBUM" -> 2, "YEAR" -> 3, "COMMENT" -> 4, "GENRE" -> 5, "TRACK" -> 6|>;
$APETypesAssociation = <|"Text" -> 0, "Binary" -> 1, 0 -> "Text", 1 -> "Binary", 2 -> "Binary"|>;

getID3v2TagsAssociation[fsi_, level_] :=
    Quiet[Module[{i, data, newLevel, currentMetaTagCount, type, data2},
        currentMetaTagCount = $metaTagCountID3v2;
        $fsiMetaTagKeysID3v2[fsi] = Append[$fsiMetaTagKeysID3v2[fsi], {"ID3v2", $metaTagCountID3v2}];
        $metaTagAssociation[{"ID3v2", $metaTagCountID3v2++}] = {fsi, level};
        frameIndexListID3v2[{}] = lfFileStreamGetID3v2FramesList[fsi];
        If[frameIndexListID3v2[level] === LibraryFunctionError["LIBRARY_DIMENSION_ERROR", 3],
            $rawTagContainer["ID3v2", fsi, level] = <||>;
            Return[currentMetaTagCount];
        ];

        If[Head[frameIndexListID3v2[level]] === LibraryFunctionError,
            Return[$Failed];
        ];

        i = 0; $rawTagContainer["ID3v2", fsi, level] = <|
            Map[i -> <|"Type" -> #,
                "Frame" -> DeleteCases[<|

                    (data =

                            lfFileStreamGetID3v2ChapterFrameValues[fsi, #, Append[level, i]];
                    If[SameQ[Head[data],
                        LibraryFunctionError],
                        "StartTime" -> Nothing[]
                        ,
                        {"StartTime" -> data[[1]],
                        If[data[[2]] =!= -1, "StartOffset" -> data[[2]], "StartOffset" -> Nothing[]]
                        ,
                        "EndTime" -> data[[3]],
                        If[data[[4]] =!= -1, "EndOffset" -> data[[4]], "EndOffset" -> Nothing[]]
                    }]),


                    "Language" -> (data =

                            lfFileStreamGetID3v2FrameLanguage[fsi, #, Append[level, i]];
                    If[SameQ[Head[data],
                        LibraryFunctionError],
                        Nothing[], FromCharacterCode[data]]),

                    "ChildElements" -> (data =

                        lfFileStreamGetID3v2TableOFContentsFrameChildElementCount[fsi, #, Append[level, i]];
                    If[SameQ[Head[data],
                        LibraryFunctionError],
                        Nothing[],
                        type = #;
                        Map[(toByteArray[lfFileStreamGetID3v2TableOFContentsFrameChildElementIdentifier[fsi, type, #, Append[level, i]]])&, Range[data] - 1]
                    ]),


                    "Data" -> (data =

                            lfFileStreamGetID3v2FrameData[fsi, #, Append[level, i]];
                    If[SameQ[Head[data],
                        LibraryFunctionError],
                        Nothing[], toByteArray[data]]),


                    "Owner" -> (data =

                            lfFileStreamGetID3v2FrameOwner[fsi, #, Append[level, i]];
                    If[SameQ[Head[data],
                        LibraryFunctionError],
                        Nothing[], FromCharacterCode[data]]),


                    "URL" -> (data =
                        lfFileStreamGetID3v2FrameURL[
                            fsi, #, Append[level, i]];
                    If[SameQ[Head[data],
                        LibraryFunctionError],
                        Nothing[], FromCharacterCode[data]]),


                    "Identifier" -> (data =

                            lfFileStreamGetID3v2FrameIdentifier[fsi, #, Append[level, i]];
                    If[SameQ[Head[data],
                        LibraryFunctionError],
                        Nothing[], toByteArray[data]]),


                    "Ordered" -> (data =

                            lfFileStreamGetID3v2FrameOrdered[fsi, #, Append[level, i]];
                    If[SameQ[Head[data],
                        LibraryFunctionError],
                        Nothing[],
                        If[SameQ[data, 0],
                            False,
                            True
                        ]
                    ]),



                    "TopLevel" -> (data =

                            lfFileStreamGetID3v2FrameTopLevel[fsi, #, Append[level, i]];
                    If[SameQ[Head[data],
                        LibraryFunctionError],
                        Nothing[],
                        If[SameQ[data, 0],
                            False,
                            True
                        ]
                    ]),


                    "SynchedText" -> (
                        data = lfFileStreamGetID3v2FrameSynchedTextList[fsi, #, Append[level, i]];
                        data2 = lfFileStreamGetID3v2FrameSynchedTextTimes[fsi, #, Append[level, i]];
                        If[SameQ[Head[data], LibraryFunctionError] || SameQ[Head[data2], LibraryFunctionError],
                            Nothing[],
                            data = FromCharacterCode[SplitBy[data, SameQ[#, 0] &][[1 ;; ;; 2]]];
                            If[!SameQ[Length[data], Length[data2]],
                                Nothing[],
                                <|Map[(data2[[#]] -> data[[#]]) &, Range[Length[data]]]|>
                            ]
                        ]
                    ),


                    "LyricsType" -> (data =

                            lfFileStreamGetID3v2FrameLyricsType[fsi, #, Append[level, i]];
                    If[SameQ[Head[data],
                        LibraryFunctionError],
                        Nothing[], $lyricsTypes[data]]),


                    "Channels" -> (data =

                            lfFileStreamGetID3v2FrameChannels[fsi, #, Append[level, i]];
                    If[SameQ[Head[data],
                        LibraryFunctionError],
                        Nothing[],
                        type = #;
                        <|Map[($channelTypes[#]-><|
                            "PeakVolume" -> toByteArray[lfFileStreamGetID3v2FramePeakVolume[fsi, type, #, Append[level, i]]],
                            "BitsRepresentingPeak" -> lfFileStreamGetID3v2FramePeakBits[fsi, type, #, Append[level, i]],
                            "VolumeAdjustment" -> lfFileStreamGetID3v2FrameVolumeAdjustmentIndex[fsi, type, #, Append[level, i]]
                        |>)&, data]|>

                    ]),

                    "Rating" -> (data =

                            lfFileStreamGetID3v2FrameRating[fsi, #, Append[level, i]];
                    If[SameQ[Head[data],
                        LibraryFunctionError],
                        Nothing[], data]),


                    "Counter" -> (data =

                            lfFileStreamGetID3v2FrameCounter[fsi, #, Append[level, i]];
                    If[SameQ[Head[data],
                        LibraryFunctionError],
                        Nothing[], data]),


                    "Email" -> (data =

                            lfFileStreamGetID3v2FrameEmail[fsi, #, Append[level, i]];
                    If[SameQ[Head[data],
                        LibraryFunctionError],
                        Nothing[], FromCharacterCode[data]]),


                    "Seller" -> (data =

                            lfFileStreamGetID3v2FrameSeller[fsi, #, Append[level, i]];
                    If[SameQ[Head[data],
                        LibraryFunctionError],
                        Nothing[], FromCharacterCode[data]]),


                    "PurchaseDate" -> (data =

                            lfFileStreamGetID3v2FramePurchaseDate[fsi, #, Append[level, i]];
                    If[SameQ[Head[data],
                        LibraryFunctionError],
                        Nothing[], FromCharacterCode[data]]),


                    "PricePaid" -> (data =

                            lfFileStreamGetID3v2FramePricePaid[fsi, #, Append[level, i]];
                    If[SameQ[Head[data],
                        LibraryFunctionError],
                        Nothing[], FromCharacterCode[data]]),


                    "PictureType" -> (data =

                            lfFileStreamGetID3v2FramePictureType[fsi, #, Append[level, i]];
                    If[SameQ[Head[data],
                        LibraryFunctionError],
                        Nothing[], $pictureTypes[data]]),


                    "Picture" -> (data =

                            lfFileStreamGetID3v2FramePicture[fsi, #, Append[level, i]];
                    If[SameQ[Head[data],
                        LibraryFunctionError],
                        Nothing[], toByteArray[data]]),


                    "Object" -> (data =

                            lfFileStreamGetID3v2FrameObject[fsi, #, Append[level, i]];
                    If[SameQ[Head[data],
                        LibraryFunctionError],
                        Nothing[], toByteArray[data]]),


                    "MimeType" -> (data =

                            lfFileStreamGetID3v2FrameMimeType[fsi, #, Append[level, i]];
                    If[SameQ[Head[data],
                        LibraryFunctionError],
                        Nothing[], FromCharacterCode[data]]),


                    "FileName" -> (data =

                            lfFileStreamGetID3v2FrameFileName[fsi, #, Append[level, i]];
                    If[SameQ[Head[data],
                        LibraryFunctionError],
                        Nothing[], FromCharacterCode[data]]),


                    "TimestampFormat" -> (data =

                            lfFileStreamGetID3v2FrameTimeStampFormat[fsi, #, Append[level, i]];
                    If[SameQ[Head[data],
                        LibraryFunctionError],
                        Nothing[], $eventTimestampFormat[data]]),


                    "SynchedEvents" -> (data =

                            lfFileStreamGetID3v2FrameSynchedEvents[fsi, #, Append[level, i]];
                    If[SameQ[Head[data],
                        LibraryFunctionError],
                        Nothing[],
                        <|Map[(data[[(#*2) - 1]] -> $eventTimingCodesAssociation[data[[#*2]]]) &, Range[Length[data]/2]]|>
                    ]),



                    "Text" -> (data =

                            lfFileStreamGetID3v2FrameText[fsi, #, Append[level, i]];
                    If[SameQ[Head[data],
                        LibraryFunctionError],
                        Nothing[], FromCharacterCode[data]]),


                    "Description" -> (data =

                            lfFileStreamGetID3v2FrameDescription[fsi, #, Append[level, i]];
                    If[SameQ[Head[data],
                        LibraryFunctionError],
                        Nothing[], FromCharacterCode[data]]),


                    "EmbeddedFrames" -> (data =

                            lfFileStreamGetID3v2ChapterFrameEmbeddedFramesList[fsi, #,
                                Append[level, i]];
                    If[SameQ[data,
                        LibraryFunctionError["LIBRARY_FUNCTION_ERROR", 6]], (* This check needs to remain a specific check for LF Function Error, since this is later checked in the next recursive level to see if it is LF error 3: frameIndexListID3v2[level] *)
                        Nothing[], newLevel = Append[level, i];
                    frameIndexListID3v2[newLevel] = data;
                    AudioFileStreamTools`MetaTag["ID3v2", getID3v2TagsAssociation[fsi, newLevel]]]),



                    "Values" -> (data =
                            lfFileStreamGetID3v2FrameValues[fsi, #, Append[level, i]];
                    If[SameQ[Head[data],
                        LibraryFunctionError],
                        Nothing[],
                        FromCharacterCode[
                            SplitBy[data, SameQ[#, 0] &][[1 ;; ;; 2]]]]),
                    "Foo" -> Nothing[]|>, Nothing],
                "ID" -> FromCharacterCode[
                    lfFileStreamGetID3v2FrameID[
                        fsi, #, Append[level, i++]]]|> &, frameIndexListID3v2[level]]|>;
        $rawTagContainer["ID3v2", fsi, level]["Parent"] = None;
        currentMetaTagCount
    ], {Association::setps, LibraryFunction::dimerr}]


getID3v1TagsAssociation[fsi_] :=
    Quiet[Module[{i, data, currentMetaTagCount},
        currentMetaTagCount = $metaTagCountID3v1;
        $fsiMetaTagKeysID3v1[fsi] = Append[$fsiMetaTagKeysID3v1[fsi], {"ID3v1", $metaTagCountID3v1}];
        $metaTagAssociation[{"ID3v1", $metaTagCountID3v1++}] = {fsi, {}};


        i = 0; $rawTagContainer["ID3v1", fsi, {}] = <|
            Map[i++ -> <|
                "Frame" -> DeleteCases[<|

                    "Value"-> (data =

                        lfFileStreamGetID3v1Element[fsi, $ID3v1Keys[#]];
                    If[SameQ[Head[data],
                        LibraryFunctionError],
                        Nothing[],
                        data
                        ])|>, Nothing],

                "ID" -> #|> &, Keys[$ID3v1Keys]]|>;
        currentMetaTagCount
    ], {Association::setps}]

getAPETagsAssociation[fsi_] :=
    Quiet[Module[{i, data, currentMetaTagCount, APEKeys, APETypes},
        currentMetaTagCount = $metaTagCountAPE;
        $fsiMetaTagKeysAPE[fsi] = Append[$fsiMetaTagKeysAPE[fsi], {"APE", $metaTagCountAPE}];
        $metaTagAssociation[{"APE", $metaTagCountAPE++}] = {fsi, {}};
        APEKeys = lfFileStreamGetAPEItemKeys[fsi];

        If[APEKeys === LibraryFunctionError["LIBRARY_DIMENSION_ERROR", 3],
            $rawTagContainer["APE", fsi, {}] = <||>;
            Return[currentMetaTagCount];
        ];

        If[Head[APEKeys] === LibraryFunctionError,
            Return[$Failed];
        ];

        APEKeys = FromCharacterCode[SplitBy[APEKeys, SameQ[#, 0] &][[1 ;; ;; 2]]];
        APETypes = lfFileStreamGetAPEItemTypes[fsi];
        i = 0; $rawTagContainer["APE", fsi, {}] = <|
            Map[i++ -> <|"Type" -> #,
                "Frame" -> DeleteCases[<|"Type" -> $APETypesAssociation[#],
                    If[# === $APETypesAssociation["Text"],
                        "Values" -> (
                            data = lfFileStreamGetAPEItemValues[fsi, ToCharacterCode[APEKeys[[i]]]];
                        If[SameQ[Head[data],
                            LibraryFunctionError],
                            Nothing[],
                            FromCharacterCode[
                                SplitBy[data, SameQ[#, 0] &][[1 ;; ;; 2]]]])
                        ,
                        data = lfFileStreamGetAPEItemData[fsi, ToCharacterCode[APEKeys[[i]]]];
                        "Data" -> If[SameQ[Head[data], LibraryFunctionError],
                            Nothing[],
                            toByteArray[data]
                        ]
                    ]
                |>, Nothing],
                "ID" -> APEKeys[[i]]|> &, APETypes]|>;
        currentMetaTagCount
    ], {Association::setps, LibraryFunction::dimerr}]


getXiphTagsAssociation[fsi_] :=
    Quiet[Module[{i, data, currentMetaTagCount, XiphKeys},
        currentMetaTagCount = $metaTagCountXiph;
        $fsiMetaTagKeysXiph[fsi] = Append[$fsiMetaTagKeysXiph[fsi], {"Xiph", $metaTagCountXiph}];
        $metaTagAssociation[{"Xiph", $metaTagCountXiph++}] = {fsi, {}};

        XiphKeys = lfFileStreamGetXiphKeys[fsi];
        If[XiphKeys === LibraryFunctionError["LIBRARY_DIMENSION_ERROR", 3],
            $rawTagContainer["Xiph", fsi, {}] = <||>;
            Return[currentMetaTagCount];
        ];

        If[Head[XiphKeys] === LibraryFunctionError,
            Return[$Failed];
        ];

        XiphKeys = FromCharacterCode[SplitBy[XiphKeys, SameQ[#, 0] &][[1 ;; ;; 2]]];
        i = 0; $rawTagContainer["Xiph", fsi, {}] = <|
            Map[i++ -> <|
                "Frame" -> DeleteCases[<|
                        "Values" -> (
                            data = lfFileStreamGetXiphValues[fsi, ToCharacterCode[#]];
                            If[SameQ[Head[data],
                                LibraryFunctionError],
                                Nothing[],
                                FromCharacterCode[
                                    SplitBy[data, SameQ[#, 0] &][[1 ;; ;; 2]]]])

                    |>, Nothing],
                "ID" -> #|> &, XiphKeys]|>;
        currentMetaTagCount
    ], {Association::setps, LibraryFunction::dimerr}]


(* TODO: Need to decide on where this parser should live, a seperate paclet? *)
formatServiceURL[url_] :=
    Quiet[Module[{parsedURL, accessToken, queryAssoc, soundCloudAccessToken, sc, trackURL,
        urlToResolve, resolveURL, rawURL},
        parsedURL = URLParse[url];
        If[$enableSoundCloudFunctionality && StringEndsQ[ToLowerCase[parsedURL["Domain"]] , "soundcloud.com"],
            Quiet[sc = ServiceConnect["SoundCloud"];];

            If[Check[ServiceConnections`ServiceInformation[sc], $Failed, {ServiceConnections`Private`ServiceInformation::nolink}] == $Failed,
            (* Throw non connected message *)
                Message[InternetStreamOpenRead::soundcloudauthfailed];
                Return[{$Failed, None}];
            ];

            Check[
                soundCloudAccessToken = Select[Internal`CheckCache[{"OAuthTokens", "SoundCloud"}] /. OAuthSigning`OAuthToken -> List, SameQ[Head[#], OAuthSigning`Private`Token20] &][[1]][[1]];
                ,
                Message[InternetStreamOpenRead::soundcloudauthfailed];
                Return[{$Failed, None}];
            ];
            If[!StringQ[soundCloudAccessToken],
                Message[InternetStreamOpenRead::soundcloudauthfailed];
                Return[{$Failed, None}];
            ];

            (* If domain is not api, attempt to resolve *)

            If[! SameQ[ToLowerCase[parsedURL["Domain"]] , "api.soundcloud.com"],
                parsedURL["Query"] = {};
                parsedURL["Fragment"] = None;
                urlToResolve = URLBuild[parsedURL];
                Check[
                    rawURL = sc["RawResolve", "url" -> urlToResolve];
                    ,
                    (*Throw non resolve message *)
                    Message[InternetStreamOpenRead::filenotfound, url];
                    Return[{$Failed, None}];
                ];
                If[!StringQ[rawURL],
                (*Throw non resolve message *)
                    Message[InternetStreamOpenRead::filenotfound, url];
                    Return[{$Failed, None}];
                ];
                Check[
                    parsedURL = URLParse[rawURL];
                    parsedURL["Path"] = Append[parsedURL["Path"], "stream"];
                    ,
                    (*Throw non resolve message *)
                    Message[InternetStreamOpenRead::filenotfound, url];
                    Return[{$Failed, None}];
                ];
            ];
            (* Valid API stream or download URL *)
            If[SameQ[parsedURL["Path"][[2]], "tracks"] && (SameQ[parsedURL["Path"][[4]], "stream"] || SameQ[parsedURL["Path"][[4]], "download"]),
                queryAssoc = <|parsedURL["Query"]|>;
                KeyDropFrom[queryAssoc, "access_token"];
                queryAssoc["oauth_token"] = soundCloudAccessToken;
                parsedURL["Query"] = Normal[queryAssoc];
                If[SameQ[parsedURL["Path"][[4]], "stream"], (* Stream URL *)
                    Return[{URLBuild[parsedURL], ".mp3"}];
                ];
                If[SameQ[parsedURL["Path"][[4]], "download"], (* Download URL *)
                    Return[{URLBuild[parsedURL], None}];
                ];
            ];

            Message[InternetStreamOpenRead::invalidsoundcloudurl, url];
            Return[{$Failed, None}];

        ];
        Return[{url, None}];
    ];
    ,
    {Part::partw}
];

End[]

EndPackage[]
