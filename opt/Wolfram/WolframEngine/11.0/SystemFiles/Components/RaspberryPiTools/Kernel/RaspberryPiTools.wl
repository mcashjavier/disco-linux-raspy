(* ::Package:: *)

(* RaspberryPiTools Paclet *)
(* Brett Haines, July 2015 *)
(* This paclet adds functionality for Raspberry Pi addons.  It currently supports the Pi Camera and 
the Sense HAT addons. *)

BeginPackage["RaspberryPiTools`"]

Begin["`Private`"]

(* Change the definition of CurrentImage to use the device driver *)
Unprotect[System`CurrentImage];
Attributes[System`CurrentImage] = {};
ClearAll[System`CurrentImage];
System`CurrentImage[ args___ ] := currentImagePi[ args ];
Protect[System`CurrentImage];

(* Needs Statements *)
Needs["PacletManager`"]
Needs["MRAALink`"];
Needs["Quaternions`"]

(* Contstants *)
defaultRaspiCamWidth = 2592;
defaultRaspiCamHeight = 1944;
minRaspiCamWidth = 300;
defaultRaspiCamRatio = 0.75;

(* Messages *)
DeviceOpen::cameraDisabled="The camera is disabled.  Please enable it via raspi-config.";
DeviceOpen::cameraDisconnected="The camera is not detected.  Please ensure the camera module is connected properly.";
DeviceOpen::cameraUnknown="An unknown error has occurred.";
DeviceRead::badCameraParams="Too many parameters were given.";
DeviceRead::invalidCameraSizes="Height and width must both be positive integers, where width is less than 2593 and height is less than 1945.";
DeviceOpen::noDevice="No SenseHAT device was detected.";
DeviceRead::unknownFunction="Unknown sensor function given, please enter one of the following:\ntemperature, humidity, pressure, acceleration, rotation, magnetic field, orientation";
DeviceRead::orientationErr="Error reading orientation.  Please ensure your Sense HAT is connected properly.";
DeviceWrite::writePixelErr="Error writing to LED array."
OpenRead::configFailure="Could not open or read from configuration file."

(* Persistent system variables *)
$isPiCameraConnected = 0;
Protect[$isPiCameraConnected];

(*this function uses apt-cache to check if an aptitude package exists or not*)
(*it is guaranteed to always return true or false for any given string*)
(*note that apt-cache may print off some stuff to stderr if the package doesn't exist at all*)
aptitudePackageInstalledQ[package_String] := StringTrim[
	First[
		StringSplit[
			StringTrim[
				Join[
					StringSplit[
						(*we use apt-cache policy to see if a package is installed or not*)
						Import["!apt-cache policy " <> package, "Text"],
						"\n"
					],
					(*we join this with the following strings because if the package doesn't exist at all, the empty string is returned*)
					{"","Installed:(none)"}
				][[2]]
			],
			"Installed:"
		]
	]
] != "(none)";


(*configFileParse parses out from a file the lines that don't start with a comment, and also cleans out comments from lines with other valid data in them*)
(*it returns a list of the lines, all cleaned of trailing and beginning whitespace characters*)
configFileParse[fName_,commentCharacter_String:"#"]:=Module[
	{
		line,
		modules = {},
		fileName = AbsoluteFileName[fName],
		file
	},
	(
		(*open the file with BinaryFormat set to True so we can use ReadLine properly*)
		file=OpenRead[fileName,BinaryFormat->True];
		If[file===$Failed, Message[OpenRead::configFailure]; Return[$Failed]];
		(*initially, read a line before entering the while loop*)
		line = ReadLine[file];
		(*now read the entirety of the file, one line at a time, and if the line is a comment or empty,*)
		(*ignore it, else parse it out*)
		While[line =!= EndOfFile,
			If[line =!= "" && StringTake[StringTrim[line], 1] =!= commentCharacter,
				(*THEN*)
				(*the line isn't a comment*)
				If[MemberQ[Characters[line], commentCharacter],
					(*THEN*)
					(*there is a comment somewhere im this line, so we have to trim it*)
					AppendTo[modules,StringTrim[StringDrop[#,First@StringPosition[#,commentCharacter~~___]]&[line]]],
					(*ELSE*)
					(*there isn't a comment, so we can just trim out the whitespace it*)
					AppendTo[modules, StringTrim[line]]
				]
				(*ELSE*)
				(*this line is either empty or a comment, so ignore it*)
			];
			line = ReadLine[file];
		];
		Return[modules];
	)
]/;StringLength[commentCharacter]===1
(*only match the pattern if commentCharacter is actually a single character*)


(*============================================================================*)
(* RaspiCam Code *)
(*============================================================================*)

(* Create DeviceRead error messages *)
DeviceRead::raspiCamError="`1`";
raspiCamMessages=<|
	0->"No error",
	1->"Error creating camera component",
	2->"Error setting image format",
	3->"Error creating preview",
	4->"Error setting preview port parameters",
	5->"Error creating camera-preview connection",
	6->"Error enabling camera-preview",
	7->"Error creating encoder",
	8->"Error setting output format of encoder",
	9->"Error enabling encoder",
	10->"Error creating camera-encoder connection",
	11->"Error enabling camera-encoder connection",
	12->"Error enabling encoder output port",
	13->"Error getting buffer from pool queue",
	14->"Error sending buffer to encoder output",
	15->General::nomem
|>;

(* Load the dynamic library RaspiSillLink *)
LibraryLoad["libRaspberryPiTools"];
lib1="libRaspberryPiTools";

(* Link to function in library that gets an image from the camera *)
getImageBytes = LibraryFunctionLoad[lib1, "GetByteData", {Integer, Integer}, {_Integer, 1, "Automatic"}];

(* Link to functions that return default (max) image width and height, then read them in *)
getMaxWidth = LibraryFunctionLoad[lib1, "GetMaxWidth", {}, Integer];
getMaxHeight = LibraryFunctionLoad[lib1, "GetMaxHeight", {}, Integer];
maxRaspiCamWidth = getMaxWidth[];
maxRaspiCamHeight = getMaxHeight[];

(* If the max values cannot be read, default to the pre-defined constants *)
If[maxRaspiCamWidth==Null, maxRaspiCamWidth=defaultRaspiCamWidth];
If[maxRaspiCamHeight==Null, maxRaspiCamHeight=defaultRaspiCamHeight];

(* Create wrapper function for library function *)
raspiCam[{ihandle_,dhandle_},args___]:=Module[
	{
		argsList=Flatten[{args}], 
		mult, 
		width, 
		height, 
		bytes, 
		string, 
		img
	},

	Switch[ Length[argsList],
		2, {width,height} = argsList,
		1, {width,height} = {argsList[[1]],argsList[[1]]*defaultRaspiCamRatio},
		0, {width,height} = {maxRaspiCamWidth,maxRaspiCamHeight},
		_, Message[DeviceRead::badCameraParams]; Return[$Failed];
	];
	
	(* Ensure height and width aren't negative, zero, or too large *)
	If[ width <= 0 || width > maxRaspiCamWidth || height <= 0 || height > maxRaspiCamHeight,
		(*THEN*)
		Message[DeviceRead::invalidCameraSizes]; $Failed
	];

	(* Deal with too-small images bug by scaling up, taking the image, then scaling back down *)
	mult = 1;
	If[ width < minRaspiCamWidth || height < minRaspiCamWidth, 
		(*THEN*)
		mult = minRaspiCamWidth / Min[ width, height ];
	];
	
	(*Get the returned bytes for the image*)
	bytes = getImageBytes[ Floor[mult*width], Floor[mult*height] ];
	If[Length[bytes]===1,
		(*THEN*)
		(*the image acuisition failed, and the tensor is just the error code*)
		(
			Message[DeviceRead::raspiCamError,raspiCamMessages[First[bytes]]];
			$Failed
		),
		(*ELSE*)
		(*valid image, just take the rest of the bytes and interpret that*)
		(
			(*First coerce the bytes into a string*)
			string = FromCharacterCode[Rest[bytes]];
			(*now import the string into an image*)
			img = ImportString[string,"JPEG"];
			(*finally resize it to whatever the user requested*)
			ImageResize[img,width]
		)
	]
];

(* Ensure the camera is connected to the Pi. This function runs when DeviceOpen is called. *)
checkCamera[args___] := Module[ {vcout},
	
	(*  $isPiCameraConnected has 3 states:
		0 -> No check has been performed on the camera
		1 -> The camera is not present
		2 -> The camera is present
	*)
	If[ $isPiCameraConnected === 0 ,

		(* Unprotect the camera connection variable so it can be set *)
		Unprotect[$isPiCameraConnected];

		vcout = TimeConstrained[RunProcess[{"/opt/vc/bin/vcgencmd", "get_camera"}, "StandardOutput"], 3];
		Switch[ vcout,

			"supported=1 detected=1\n",
			$isPiCameraConnected = 2;
			Protect[$isPiCameraConnected];
			Return[{{}}],

			"supported=1 detected=0\n",
			Message[DeviceOpen::cameraDisconnected]; 
			$isPiCameraConnected = 1;
			Protect[$isPiCameraConnected];
			Return[$Failed],

			"supported=0 detected=0\n",
			Message[DeviceOpen::cameraDisabled];
			$isPiCameraConnected = 1;
			Protect[$isPiCameraConnected];
			Return[$Failed],
		
			_,
			Message[DeviceOpen::cameraUnknown];
			$isPiCameraConnected = 1;
			Protect[$isPiCameraConnected];
			Return[$Failed]
		],
		
		(* If $isPiCameraConnected isn't 0, return accordingly *)
		If[ $isPiCameraConnected === 2, 
			(* If the camera is connected... *)
			Return[{{}}],
			(* ...else, *)
			Message[DeviceOpen::cameraDisconnected]; Return[$Failed]
		];
	]
];
   
(* Function to use with DeviceFind driver function. *)
findCamera[]:= checkCamera[];

(* Options for the CurrentImage wrapper function *)
Options[currentImagePi]={"ImageSize"->Medium, "RasterSize"->{maxRaspiCamWidth,maxRaspiCamHeight}};

(* Wrapper functions for CurrentImage *)
currentImagePi[ opts:OptionsPattern[] ] := Module[ 
	{res},
	If[ (res = DeviceRead["RaspiCam", OptionValue["RasterSize"]]) =!= $Failed,
		Image[res,ImageSize->OptionValue["ImageSize"]],
		$Failed
	]
];

currentImagePi[ num_Integer, opts:OptionsPattern[] ] := 
	Table[ currentImagePi[ FilterRules[{opts},Options[currentImagePi]] ], num ];



(*============================================================================*)
(* SenseHAT Code *)
(*============================================================================*)

symbolsBinary=Binarize[Import[PacletResource["RaspberryPiTools","TextImage"],"PNG"]];

(* Helper function to get pixel map to display a character that can be Joined with other chars*)
getCharData[char_String]:=( Module[ 
	{
		charList=" +-*/!\"#$><0123456789.=)(ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz?,;:|@%[&_']\\~^",
		pos
	},

	(* Get character images from Resources folder and isolate the desired symbol*)
	img=Binarize[Import[PacletResource["RaspberryPiTools","TextImage"],"PNG"]];
	
	(* Search for desired character's position in list.  If it isn't in the list, return Failed.  *)
	If[Length[StringPosition[charList,char]]==0,Return[$Failed]];
	pos=StringPosition[charList,char][[1,1]];
	Take[ImageData[symbolsBinary,"Binary"],{(pos-1)*5+1,pos*5}]
]);

(* Helper function to turn 3 color values from 0-255 into 2 bytes that will be written to the output stream *)
encodePixel[color_List]:=(Module[{r,g,b,bits},
	(* Ensure 3 values are given for R,G,B *)
	If[Length[color]!=3,Return[$Failed]];

	(* Condense the 3 values to a single 16 bit integer *)
	r=BitAnd[BitShiftRight[color[[1]],3],FromDigits["1F",16]];
	g=BitAnd[BitShiftRight[color[[2]],2],FromDigits["3F",16]];
	b=BitAnd[BitShiftRight[color[[3]],3],FromDigits["1F",16]];
	bits=BitShiftLeft[r,11]+BitShiftLeft[g,5]+b;

	(* Convert integer into 2 byte values *)
	bits=Reverse[IntegerDigits[bits,256,2]];
	Return[bits];
]);

(* Helper function to turn off all LEDs *)
clearLEDArray[]:=(Module[{stream},
	stream=OpenWrite["/dev/fb1","BinaryFormat"->True];
	If[stream === $Failed,
		Message[DeviceWrite::writePixelErr]; Return[]
	];
	BinaryWrite[stream,Table[encodePixel[{0,0,0}],{64}]];
	Close[stream];
]);

(* Display an image with 64 pixels; used in DisplayMessage *)
displayImage[img_Image,color_List]:=(Module[
	{
		data=Flatten[ImageData[img]],
		display,
		stream
	},
	
	(* Error checking *)
	If[Length[data]!=64,Return[$Failed]];
	If[Length[color]!=3,Return[$Failed]];

	(* Color 1's, don't color 0's, then encode colored pixels *)
	display=If[#==1,color,{0,0,0}]&/@data;
	display=Flatten[encodePixel/@display];

	(* Open stream to LED array *)
	stream=OpenWrite["/dev/fb1","BinaryFormat"->True];
	If[stream === $Failed,
		Message[DeviceWrite::writePixelErr]; Return[]
	];

	(* Display pixels *)
	BinaryWrite[stream,display];

	(* Close the stream *)
	Close[stream];
]);

(* Display an image with 192 pixels; used in DisplayMessage *)
displayImage[img_List]:=(Module[
	{display=Flatten[encodePixel/@img],stream},

	(* Open stream to LED array *)
	stream=OpenWrite["/dev/fb1","BinaryFormat"->True];
	If[stream === $Failed,
		Message[DeviceWrite::writePixelErr]; $Failed
	];

	(* Display pixels *)
	BinaryWrite[stream,display];

	(* Close the stream *)
	Close[stream];
]);

(* Display a given pattern on the LED array *)
Options[displayMessage]={"Color"->{255,255,255}, "ScrollSpeed"->0.05};

displayMessage[{ihandle_,dhandle_},args___,opts:OptionsPattern[]]:=(Module[
	{
		argsList={Flatten[{args}]},
		color,
		chars,
		fullimg,
		frames,
		data,
		scroll
	},

	(* Error Checking *)
	If[Length[OptionValue["Color"]]!=3, color={255,255,255}, color=OptionValue["Color"]];
	If[Head[OptionValue["ScrollSpeed"]]=!=Real, scroll=0.05, scroll=OptionValue["ScrollSpeed"] ];

	(* If input is a list of length 64, display the image.  Else, isolate the message string. *)
	If[ Head[argsList]===List, 
		(* If the list has 64 elements (one for each light)... *)
		If[Length[Flatten[argsList]]==64, 
			(* Binarize and display the list, using color to light 1s and not lighting 0s  *)
			data=Partition[First[argsList], 8];
			displayImage[Binarize[Image[data]], color];
		];
		(* Else, if the list has 192 elements (3 for each light)... *)
		If[Length[Flatten[argsList]]==192, 
			(* Display the list, lighting each light using the color defined in the corresponding list position  *)
			displayImage[Partition[ argsList[[1]], 3]];
		];
	];
	(* Finally, the only other option is a string. *)
	Quiet[If[ Head[args]=!=String, Return[$Failed] ]];

	(* If string is empty or all whitespace, just clear the LED array *)
	If[StringTrim[args]=="", clearLEDArray[]; Return[]; ];

	(* Else, display the message... *)
	(* Get pixel arrays for each character in message and combine them *)
	chars=getCharData/@Characters[args];

	(* Join character images and add buffer space to left/right of message *)
	data=ArrayPad[Partition[Flatten[Join[chars]],8], {{8,8}}, {{0,0,0,0,0,0,0,0}} ];

	(* Determine height of array for use in loop *)
	frames=Dimensions[data][[1]]-8;

	(* Display top 8 rows, rotated to correct orientation, then delete first row to simulate scrolling motion *)
	Do[ displayImage[ImageRotate[Image[Take[data,8]]], color];
		Pause[scroll];
		data=Drop[data,1],
		frames (* Execute loops as many times as there are frames *)
	];

	(* Clear out the array *)
	clearLEDArray[];
]);


(* SENSOR FUNCTIONS AND HELPERS *)

(* Helper function to convert a binary string to a number using 2's complement *)
twosComplement[digits_List]:=(Module[
	{
		len=Length[digits],
		sum=0,
		i=0
	},
	
	sum = Total[Table[
		digits[[i]]*2^(len-i),
		{i,len}
	]];
	
	If[digits[[1]]==1, sum=sum-(2^len)];
	Return[sum];
]);

(* Helper function to cleanly read from a specific register *)
i2CRegisterRead[sensor_DeviceObject, register_String]:=(
	DeviceWrite[sensor,FromDigits[register,16]];
	Return[DeviceRead[sensor]];
);

(* Helper function to project a 3D vector to a 2D space *)
accelToEuler[vector_List] := Module[{x,y,z},
	{x,y,z}=vector;
	Return[{ ArcTan[y,z], ArcTan[x,Sqrt[y^2 + z^2]], 0 }];
];

senseHatReadTemperature[]:=Module[
	{binaryDigits, pressureSensor, temp, tempOutHigh, tempOutLow},
	
	pressureSensor=DeviceOpen["I2C",92];
	
	(* Initialize the sensor *)
	DeviceWrite[pressureSensor,{FromDigits["20",16],FromDigits["C4",16]}];
	DeviceWrite[pressureSensor,{FromDigits["10",16],FromDigits["05",16]}];
	DeviceWrite[pressureSensor,{FromDigits["2E",16],FromDigits["C0",16]}];
	DeviceWrite[pressureSensor,{FromDigits["21",16],FromDigits["40",16]}];

	(* Read data from the temperature registers *)
	tempOutLow=i2CRegisterRead[pressureSensor,"2B"];
	tempOutHigh=i2CRegisterRead[pressureSensor,"2C"];

	binaryDigits=Join[IntegerDigits[tempOutHigh,2,8],IntegerDigits[tempOutLow,2,8]];
	
	(* Sometimes this fails on the first reading, so double check *)
	If[ twosComplement[binaryDigits]==0,
		tempOutLow=i2CRegisterRead[pressureSensor,"2B"];
		tempOutHigh=i2CRegisterRead[pressureSensor,"2C"];
		binaryDigits=Join[IntegerDigits[tempOutHigh,2,8],IntegerDigits[tempOutLow,2,8]];
	];
	
	(* Convert this binary string to degrees Celsius, formula taken form sensor manual, page 34 *)
	temp=(twosComplement[binaryDigits]/480)+42.5;

	(* Close the device to prevent memory leaks *)
	DeviceClose[pressureSensor];

	(* Return the temperature *)
	Return[temp];
];

senseHatReadPressure[]:=Module[
	{binaryDigits, pressureSensor, pres, presOutHigh, presOutLow, presOutXLow},
	pressureSensor=DeviceOpen["I2C",92];
	
	(* Initialize the sensor *)
	DeviceWrite[pressureSensor,{FromDigits["20",16],FromDigits["C4",16]}];
	DeviceWrite[pressureSensor,{FromDigits["10",16],FromDigits["05",16]}];
	DeviceWrite[pressureSensor,{FromDigits["2E",16],FromDigits["C0",16]}];
	DeviceWrite[pressureSensor,{FromDigits["21",16],FromDigits["40",16]}];

	(* Read data from the pressure registers *)
	presOutXLow=i2CRegisterRead[pressureSensor,"28"];
	presOutLow=i2CRegisterRead[pressureSensor,"29"];
	presOutHigh=i2CRegisterRead[pressureSensor,"2A"];

	(* Convert this binary string to millibars, formula taken form sensor manual, page 33 *)
	binaryDigits=Join[IntegerDigits[presOutHigh,2,8],IntegerDigits[presOutLow,2,8],IntegerDigits[presOutXLow,2,8]];
	pres=twosComplement[binaryDigits]/4096;

	(* Close the device to prevent memory leaks *)
	DeviceClose[pressureSensor];

	(* Return the pressure *)
	Return[N[pres]];
];

senseHatReadHumidity[]:=Module[
	{binaryDigits, humiditySensor, hum, humOutHigh, humOutLow, modifiers},
	humiditySensor=DeviceOpen["I2C",95];
	
	(* Initialize the sensor *)
	DeviceWrite[humiditySensor,{FromDigits["20",16],FromDigits["87",16]}];
	DeviceWrite[humiditySensor,{FromDigits["10",16],FromDigits["1B",16]}];

	(* Calibrate humidity sensor *)
	modifiers=calibrateHumidity[humiditySensor];

	(* Read data from the humidity registers *)
	humOutLow=i2CRegisterRead[humiditySensor,"28"];
	humOutHigh=i2CRegisterRead[humiditySensor,"29"];

	(* Convert this binary string to pressure using two's complement *)
	binaryDigits=Join[IntegerDigits[humOutHigh,2,8],IntegerDigits[humOutLow,2,8]];
	hum=twosComplement[binaryDigits];

	(* Use calibration modifiers to correct humidity *)
	hum=hum*modifiers[[1]]+modifiers[[2]];

	(* Close the device to prevent memory leaks *)
	DeviceClose[humiditySensor];

	(* Return the humidity *)
	Return[N[hum]];
];

(* Get calibration data for the humidity sensor, modeled after RTHumidityHTS221.cpp *)
calibrateHumidity[humiditySensor_DeviceObject]:=Module[{h0,h1,h0t0out,h1t0out,humm,humc},
	h0=i2CRegisterRead[humiditySensor,"30"]/2;
	h1=i2CRegisterRead[humiditySensor,"31"]/2;
	h0t0out=twosComplement[Join[IntegerDigits[i2CRegisterRead[humiditySensor,"36"],2,8],IntegerDigits[i2CRegisterRead[humiditySensor,"37"],2,8]]];
	h1t0out=twosComplement[Join[IntegerDigits[i2CRegisterRead[humiditySensor,"3A"],2,8],IntegerDigits[i2CRegisterRead[humiditySensor,"3B"],2,8]]];
	humm=(h1-h0)/(h1t0out-h0t0out);
	humc=h0-(humm*h0t0out);
	Return[{humm,humc}];
];

senseHatReadGyroscope[]:=Module[
	{conv, ndofSensor, pitch, roll, yaw, xHigh, xLow, yHigh, yLow, zHigh, zLow},
	
	ndofSensor=DeviceOpen["I2C",106];

	(* Initialize the sensor *)
	DeviceWrite[ndofSensor,{FromDigits["22",16],FromDigits["80",16]}];

	(* Set up control registers *)
	DeviceWrite[ndofSensor,{FromDigits["10",16],FromDigits["89",16]}];
	DeviceWrite[ndofSensor,{FromDigits["12",16],FromDigits["44",16]}];
	DeviceWrite[ndofSensor,{FromDigits["20",16],FromDigits["7B",16]}];
	DeviceWrite[ndofSensor,{FromDigits["21",16],FromDigits["00",16]}];

	(* Read data from the x,y,z angle registers *)
	xLow=i2CRegisterRead[ndofSensor,"18"];
	xHigh=i2CRegisterRead[ndofSensor,"19"];
	yLow=i2CRegisterRead[ndofSensor,"1A"];
	yHigh=i2CRegisterRead[ndofSensor,"1B"];
	zLow=i2CRegisterRead[ndofSensor,"1C"];
	zHigh=i2CRegisterRead[ndofSensor,"1D"];

	(* Convert the raw data to pitch, roll, and yaw in revolutions per second *)
	roll=twosComplement[Join[IntegerDigits[xHigh,2,8],IntegerDigits[xLow,2,8]]];
	pitch=twosComplement[Join[IntegerDigits[yHigh,2,8],IntegerDigits[yLow,2,8]]];
	yaw=twosComplement[Join[IntegerDigits[zHigh,2,8],IntegerDigits[zLow,2,8]]];	

	(* Conversions taken from RTIMULSM9DS1.cpp and RTIMULib.ini *)
	(* Divided by 360 to convert degrees to revolutions *)
	conv=(0.00875/360);
	pitch*=conv; roll*=-conv; yaw*=conv;

	(* Correct for noise when not moving *)
	If[Abs[pitch]<0.01,pitch=0]; If[Abs[roll]<0.01,roll=0]; If[Abs[yaw]<0.01,yaw=0];

	(* Close the device to prevent memory leaks *)
	DeviceClose[ndofSensor];

	(* Return the gyroscope data *)
	Return[{roll,pitch,yaw}];
];

senseHatReadAccelerometer[]:=Module[
	{conv, ndofSensor, pitch, roll, yaw, xHigh, xLow, yHigh, yLow, zHigh, zLow},
	
	ndofSensor=DeviceOpen["I2C",106];
	
	(* Initialize the sensor *)
	DeviceWrite[ndofSensor,{FromDigits["22",16],FromDigits["80",16]}];

	(* Set up control registers *)
	DeviceWrite[ndofSensor,{FromDigits["10",16],FromDigits["89",16]}];
	DeviceWrite[ndofSensor,{FromDigits["12",16],FromDigits["44",16]}];
	DeviceWrite[ndofSensor,{FromDigits["20",16],FromDigits["7B",16]}];
	DeviceWrite[ndofSensor,{FromDigits["21",16],FromDigits["00",16]}];

	(* Read data from the x,y,z angle registers *)
	xLow=i2CRegisterRead[ndofSensor,"28"];
	xHigh=i2CRegisterRead[ndofSensor,"29"];
	yLow=i2CRegisterRead[ndofSensor,"2A"];
	yHigh=i2CRegisterRead[ndofSensor,"2B"];
	zLow=i2CRegisterRead[ndofSensor,"2C"];
	zHigh=i2CRegisterRead[ndofSensor,"2D"];

	(* Convert the raw data to pitch, roll, and yaw in G's (Earth's gravity) *)
	roll=twosComplement[Join[IntegerDigits[xHigh,2,8],IntegerDigits[xLow,2,8]]];
	pitch=twosComplement[Join[IntegerDigits[yHigh,2,8],IntegerDigits[yLow,2,8]]];
	yaw=twosComplement[Join[IntegerDigits[zHigh,2,8],IntegerDigits[zLow,2,8]]];	

	conv=0.000244;  (* Conversion taken from RTIMULSM9DS1.cpp *)
	pitch*=-conv; roll*=-conv; yaw*=conv;

	(* Close the device to prevent memory leaks *)
	DeviceClose[ndofSensor];

	(* Return the accelerometer data *)
	Return[{roll,pitch,yaw}];
];

senseHatReadMagnetometer[]:=Module[
	{
		conv,
		magSensor,
		roll,
		pitch,
		yaw,
		xHigh,
		xLow,
		yHigh,
		yLow,
		zHigh,
		zLow
	},
	
	magSensor=DeviceOpen["I2C",28];

	(* Set up control registers *)
	DeviceWrite[magSensor,{FromDigits["20",16],FromDigits["14",16]}];
	DeviceWrite[magSensor,{FromDigits["21",16],FromDigits["00",16]}];
	DeviceWrite[magSensor,{FromDigits["22",16],FromDigits["00",16]}];	

	(* Read data from the x,y,z angle registers *)
	xLow=i2CRegisterRead[magSensor,"28"];
	xHigh=i2CRegisterRead[magSensor,"29"];
	yLow=i2CRegisterRead[magSensor,"2A"];
	yHigh=i2CRegisterRead[magSensor,"2B"];
	zLow=i2CRegisterRead[magSensor,"2C"];
	zHigh=i2CRegisterRead[magSensor,"2D"];

	(* Convert the raw data to roll,pitch,yaw in degrees *)
	roll=twosComplement[Join[IntegerDigits[xHigh,2,8],IntegerDigits[xLow,2,8]]];
	pitch=twosComplement[Join[IntegerDigits[yHigh,2,8],IntegerDigits[yLow,2,8]]];
	yaw=twosComplement[Join[IntegerDigits[zHigh,2,8],IntegerDigits[zLow,2,8]]];	

	conv=0.014;  (* Conversion taken from RTIMULSM9DS1.cpp *)
	pitch*=conv; roll*=-conv; yaw*=-conv;

	(* Close the device to prevent memory leaks *)
	DeviceClose[magSensor];

	(* Return the compass data *)
	Return[{roll,pitch,yaw}];
];

senseHatCalculateOrientation[]:=Module[
	{vector, mvec, x, y, z, m, q},
	
	(* Turn accelerometer data into a normalized 2D vector (z=0) *)
	{x,y,z} = accelToEuler[senseHatReadAccelerometer[]];
	
	(* Check for accelerometer reading all 0s - this will cause an error later *)
	If[(x==y==z==0),Return[$Failed]];

	(* Create the quaternion q to rotate m *)
	q=Quaternion[ Cos[x/2]*Cos[y/2], Sin[x/2]*Cos[y/2], Cos[x/2]*Sin[y/2], -Sin[x/2]*Sin[y/2] ];
	
	(* Turn magnetometer readings into a quaternion *)
	mvec=senseHatReadMagnetometer[];
	m=Quaternion[ 0, mvec[[1]], mvec[[2]], mvec[[3]] ];

	(* Rotation of m to correct for accelerations *)
	m=q**m**Conjugate[q];

	(* Use m to give vector a Z component *)
	If[
		( m[[3]]==0 && m[[2]]==0 ) || ( m[[3]]==0.0 || m[[2]]==0.0 ),
		Message[DeviceRead::orientationErr]; Return[$Failed]
	];

	(* Convert radians to degrees and correct offsets *)
	{x,y} = Mod[#*(360/2*Pi)-90, 360]&/@{x,y};
	z = Mod[ArcTan[ m[[3]],m[[2]] ]*(360/(2*Pi))-180,360];

	Return[{x,y,z}];
];

(* Device Read function *)
raspiSenseHat[{ihandle_,dhandle_},args___]:=( Module[ 
	{datatype=ToLowerCase[args]},

	(* Call internal functions *)
	If[StringMatchQ[datatype,"temperature"], Return[ Quantity[senseHatReadTemperature[],"Celsius"] ] ];
	If[StringMatchQ[datatype,"humidity"], Return[ Quantity[senseHatReadHumidity[],"%"] ] ];
	If[StringMatchQ[datatype,"pressure"], Return[ Quantity[senseHatReadPressure[],"mbar"] ] ];
	If[StringMatchQ[datatype,"rotation"], Return[ Map[Quantity[#,"Revolutions"/"Seconds"]&, senseHatReadGyroscope[]] ] ];
	If[StringMatchQ[datatype,"magnetic field"], Return[ Map[Quantity[#,"Microteslas"]&, senseHatReadMagnetometer[]] ] ];
	If[StringMatchQ[datatype,"orientation"], Return[ senseHatCalculateOrientation[] ] ];
	If[StringMatchQ[datatype,"acceleration"], Return[ Map[Quantity[#,"StandardAccelerationOfGravity"]&, senseHatReadAccelerometer[]] ] ];
	
	(* If the arg doesn't match anything else, return an error message *)
	Message[DeviceRead::unknownFunction];
	Return[$Failed];
]);

(* DRIVER FUNCTION *)

deviceOpenDriver[ihandle_]:=Module[
	{osfile, pres, hmdy, gyro, magn},

	(* Open connections to the 4 I2C addresses used by the SenseHAT *)
	pres=DeviceOpen["I2C",92];
	hmdy=DeviceOpen["I2C",95];
	gyro=DeviceOpen["I2C",106];
	magn=DeviceOpen["I2C",28];

	(* If any failed... *)
	If[AnyTrue[{pres,hmdy,gyro,magn}, #==$Failed &], 
		Message[DeviceOpen::noDevice];Return[$Failed],        (* ...return $Failed *)
		Return[ihandle]                                       (* ...else, return the device handle *)
	];
];


End[]


EndPackage[]
