(* ::Package:: *)

(* Wolfram Language Package *)

BeginPackage["MQTTLink`"]
(* Exported symbols added here with SymbolName::usage *)  

MQTTClient

CreateClient::usage="CreateClient creates a new MQTTClient";
CreateClient::exists="The client with ID `1` already exists";
CreateClient::cleanSession="The value of \"CleanSession\", `1`, is not True or False";
CreateClient::invalidHost="The host `1` is invalid";

ConnectClient::usage="ConnectClient connects the given MQTTClient to a broker";
ConnectClient::alreadyConnected="The client `1` is already connected";
ConnectClient::nonexist="The client `1` doesn't exist";
ConnectClient::noPort="No port for broker specified, using 1883";
ConnectClient::noIP="No IP address for broker specified, using localhost";
ConnectClient::blocking="The value of \"Blocking\", `1`, is not True or False";
ConnectClient::timeout="The value of \"Timeout\", `1`, should be an Integer";
ConnectClient::threadFail="An unknown error occured connecting the client, try again later";
ConnectClient::connFail="`1`";
ConnectClient::unexpectDisconn="The client `1` was unexpectedly disconnected with code `2`";
ConnectClient::errno="A system error was encountered trying to connect to `1`; the connection failed";
ConnectClient::noBroker="Could not connect to the broker at  `1`";
ConnectClient::invalidHost=CreateClient::invalidHost;

DisconnectClient::unknownError="An unknown error occured while disconnecting";
DisconnectClient::nonexist=ConnectClient::nonexist;

TopicPublish::usage="TopicPublish publishs a message on a topic using the specified client";
TopicPublish::messageascii="The message `1` should be a list of integers 0-255 or a string of ASCII characters";
TopicPublish::topicascii="The topic pattern `1` contains non-ASCII characters and cannot be used";
TopicPublish::nonexist=ConnectClient::nonexist;
TopicPublish::notConnected="The client `1` isn't connected";
TopicPublish::noConnection="The client `1` failed to connect to broker";
TopicPublish::protocolError="The system failed to send the message due to a protocol error";
TopicPublish::qos="The QualityOfService level `1` is invalid, valid levels are 0, 1, and 2";
TopicPublish::blocking=ConnectClient::blocking;
TopicPublish::unknownError="An unknown error occured while publishing";
TopicPublish::retain="The \"Retain\" option must be either True or False";
TopicPublish::tooLarge="The message `1` is too large, messages must be smaller than 256 MB";

TopicSubscribe::usage="TopicSubscribe subscribes to the given topic";
TopicSubscribe::blocking=ConnectClient::blocking;
TopicSubscribe::qos=TopicPublish::qos;
TopicSubscribe::noConnection=TopicPublish::noConnection;
TopicSubscribe::alreadySubscribed="The client `1` is already subscribed to the topic pattern `2`";
TopicSubscribe::topicascii=TopicPublish::topicascii;
TopicSubscribe::diffQos="The topic `1` was requested with a Quality of Service level `2`, but the broker granted level `3`";
TopicSubscribe::unknownMessageCallbackError="An unknown error occured when a message was received (`1`)";
TopicSubscribe::unknownError="An unknown error occured while subscribing (`1`)";
TopicSubscribe::subFailed="Subscribing to the topic `1` with a Quality of Service level `2` failed with error code `3`"

TopicUnsubscribe::usage="TopicUnsubscribe unsubscribes from the specified topic";
TopicUnsubscribe::noSubscribe="The client `1` isn't subscribed to the topic `2`";
TopicUnsubscribe::blocking=ConnectClient::blocking;
TopicUnsubscribe::noConnection=TopicPublish::noConnection;
TopicUnsubscribe::unknownError="An unknown error occured while unsubscribing (`1`)";
TopicUnsubscribe::nonexist=ConnectClient::nonexist

MQTTClientSetOptions::nokey="The option `1` doesn't exist";
MQTTClientSetOptions::nonexist = ConnectClient::nonexist;
MQTTClientSetOptions::protectedKey="The option `1` is not user directly configurable with MQTTClientSetOptions";

StartBroker::usage="Starts a local MQTT broker on the specified port";
StartBroker::running="There is already a broker running on port `1`";
StartBroker::startError="The broker failed to run exiting with : `1` (exit code `2`)";

MQTTBrokers::usage="Returns an Association of running Brokers"

Begin["`Private`"] (* Begin Private Context *) 

(*paclet manager is for accessing paclet resources*)
Needs["PacletManager`"];


(*for tracking the various brokers*)
$Brokers = <||>;


(*------RETURN CODE CONSTANTS------*)
(*MQTTLink specific return values*)
$ClientNotFound = -42;
$ClientExists = -44;
$TopicNotFound = -45;
$ThreadError = -46;
(*mosquitto return values*)
$MosquittoConnectionPending = -1;
$MosquittoSuccess = 0;
$MosquittoOutOfMemory = 1;
$MosquittoProtocolError = 2;
$MosquittoInvalidFunctionArguments = 3;
$MosquittoNoConnection = 4;
$MosquittoConnectionRefused = 5;
$MosquittoErrorNotFound = 6;
$MosquittoConnectionLost = 7;
$MosquittoTLSError = 8;
$MosquittoPayloadTooLarge = 9;
$MosquittoNotSupported = 10;
$MosquittoAuthenticationError = 11;
$MosquittoACLDenied= 12;
$MosquittoUnknownError = 13;
$MosquittoErrnoError = 14;
$MosquittoLookupError = 15;
$MosquittoProxyError = 16;
$MosquittoConnectionRefusedProtoVersion = 1;
$MosquittoConnectionRefusedIdentifierRejected = 2;
$MosquittoBrokerUnavailable = 3;

(*lock and protect all the constants*)
SetAttributes[$ClientNotFound,{Locked,Protected}];
SetAttributes[$ClientExists,{Locked,Protected}];
SetAttributes[$TopicNotFound,{Locked,Protected}];
SetAttributes[$ThreadError,{Locked,Protected}];
SetAttributes[$MosquittoConnectionPending,{Locked,Protected}];
SetAttributes[$MosquittoSuccess,{Locked,Protected}];
SetAttributes[$MosquittoOutOfMemory,{Locked,Protected}];
SetAttributes[$MosquittoProtocolError,{Locked,Protected}];
SetAttributes[$MosquittoInvalidFunctionArguments,{Locked,Protected}];
SetAttributes[$MosquittoNoConnection,{Locked,Protected}];
SetAttributes[$MosquittoConnectionRefused,{Locked,Protected}];
SetAttributes[$MosquittoErrorNotFound,{Locked,Protected}];
SetAttributes[$MosquittoConnectionLost,{Locked,Protected}];
SetAttributes[$MosquittoTLSError,{Locked,Protected}];
SetAttributes[$MosquittoPayloadTooLarge,{Locked,Protected}];
SetAttributes[$MosquittoNotSupported,{Locked,Protected}];
SetAttributes[$MosquittoAuthenticationError,{Locked,Protected}];
SetAttributes[$MosquittoACLDenied,{Locked,Protected}];
SetAttributes[$MosquittoUnknownError,{Locked,Protected}];
SetAttributes[$MosquittoErrnoError,{Locked,Protected}];
SetAttributes[$MosquittoLookupError,{Locked,Protected}];
SetAttributes[$MosquittoProxyError,{Locked,Protected}];
SetAttributes[$MosquittoConnectionRefusedProtoVersion,{Locked,Protected}];
SetAttributes[$MosquittoConnectionRefusedIdentifierRejected,{Locked,Protected}];
SetAttributes[$MosquittoBrokerUnavailable,{Locked,Protected}];


(*these are properties we don't want the user setting themselves using MQTTClientSetOptions, as they aren't really options*)
$ProtectedMQTTClientProperties = {"Connected","UUID","BrokerIP","BrokerPort","SubscribedTopics","ID"};
SetAttributes[$ProtectedMQTTClientProperties,{Locked,Protected}];


(*loads the mosquitto library and all the relevant functions from the linked library*)
loadLibrary[]:=Module[
	{
		mqttlinkLib=FindLibrary["MQTTLink"],
		ssleay32,
		libeay32,
		libmosquitto
	},
	(
		(*load this library first*)
		Switch[$SystemID,
			"Windows-x86-64",
			(
				(*on windows, need to load the ssl libraries, then the mosquitto library itself*)
				ssleay32 = FileNameJoin[{$InstallationDirectory, "SystemFiles", "Libraries", $SystemID, "ssleay32.dll"}];
				libeay32 = FileNameJoin[{$InstallationDirectory, "SystemFiles", "Libraries", $SystemID, "libeay32.dll"}];
				LibraryLoad[ssleay32];
				LibraryLoad[libeay32];
				LibraryLoad[libmosquitto];
			),
			"MacOSX-x86-64",
			(
				(*no other libraries need to be loaded for OSX*)
				Null
			)
		];
		LibraryLoad[mqttlinkLib];
		(*load the LibraryLink functions*)
		(*first arg is uuid, second is whether to make this client use a clean session or not*)
		iCreateClient = LibraryFunctionLoad[mqttlinkLib, "CreateClient", {String, "Boolean"}, Integer];
		(*first arg is uuid, second is host ip, third is port, fourth is timeout to use*)
		iConnectClient = LibraryFunctionLoad[mqttlinkLib, "ClientConnect", {String, String, Integer, Integer}, {Integer, 1}];
		(*only argument is the uuid to disconnect*)
		iDisconnectClient = LibraryFunctionLoad[mqttlinkLib,"ClientDisconnect",{String},Integer];
		(*first arg is uuid, second is topic pattern, third is message data, fourth is quality of service, fifth is whether this is to be a retained message*)
		iTopicPublish = LibraryFunctionLoad[mqttlinkLib, "TopicPublish", {String, String, {Integer,1}, Integer, "Boolean"}, {Integer, 1}];
		(*first arg is uuid, second is topic pattern to subscribe to, third is quality of service*)
		iTopicSubscribe = LibraryFunctionLoad[mqttlinkLib, "SubscribeTopic", {String, String, Integer}, {Integer, 1}];
		(*first arg is uuid, second is topic pattern to unsubscribe from*)
		iTopicUnsubscribe = LibraryFunctionLoad[mqttlinkLib, "UnsubscribeTopic", {String, String}, {Integer, 1}];
		(*first arg is uuid, second is topic pattern to unsubscribe from*)
		iCreateTopicSubscriptionTask = LibraryFunctionLoad[mqttlinkLib,"CreateSubscriptionTask",{String,String},{Integer,1}];
	)
];

(*make sure to load the library whenever this package is loaded*)
loadLibrary[];


(*-----MQTTClient content box / summary box code------*)

(*set upvalues for Normal*)
MQTTClient/:Normal[client_MQTTClient]:=First[client];

(*set UpValues for MakeBoxes*)
(*StandardForm MakeBoxes*)
MQTTClient/:MakeBoxes[MQTTClient[data_Association],StandardForm]:=BoxForm`ArrangeSummaryBox[
		(*first argument is the head to use*)
		MQTTClient,
		(*second argument is the expression*)
		MQTTClient[data],
		(*third argument is the icon to use*)
		clientImage,
		(*the next argument is the always visisble properties*)
		BoxForm`SummaryItem[{ToString[#1]<>": ",#2}]&@@@Normal[KeyTake[data,{"ID","Connected"}]],
		(*the next argument is the optional items that come down when the plus button is pressed*)
		BoxForm`SummaryItem[{ToString[#1]<>": ",Normal@#2}]&@@@Normal[KeyDrop[data,{"ID","Connected"}]],
		(*lastly,the display form we want to display this with*)
		StandardForm,
		(*making it interpretable allows users to take the displayed object and query things about it directly without having to save it in a variable*)
		"Interpretable"->True
];
(*FullForm MakeBoxes*)
MQTTClient/:MakeBoxes[MQTTClient[data_Association],FullForm]:=MQTTClient[Append[data,"Connected"->$Clients[data["UUID"],"Connected"]]];

(*when properties from the object are queried, just used the internal association*)
MQTTClient[data_Association][key_] := $Clients[data["UUID"],key];

MQTTClient[data_Association]["Properties"]:=Keys[$Clients[data["UUID"]]];

MQTTClient /: Set[MQTTClient[data_Association][key_], val_] := ($Clients[data["UUID"],key] = val)	

(*can't seem to get this form to work yet*)
(*MQTTClient /: SetDelayed[MQTTClient[data_Association][key_], val_] := ($Clients[data["UUID"],key] := val)*)

(*this is for pattern matching, so we only evaluate the arguments once if using form for Rule, and never if we are using RuleDelayed*)
SetAttributes[MQTTClientSetOptions,{HoldRest}];

(*MQTTClientSetOptions sets options for the client, mainly used for changing Callback functions after the corresponding function has been called*)
(*for example, calling 
	ClientConnect[client, "ClientConnectedFunction" :> Print["got connected"]] 
and then 
	MQTTClientSetOptions[client, "ClientConnectedFunction" :> Print["different message]]
allows the callback function for connecting to be changed after ClientConnect
*)
MQTTClientSetOptions[client_MQTTClient,settings_]:=Module[
	{
		uuid = client["UUID"],
		(*for the option, we want to try and extract the option name, with the expectation that settings is of the form opt->val or opt:>val*)
		opt = Quiet[Check[Extract[HoldComplete[settings],{1,1}],$Failed],Extract::partd]
	},
	(
		(*make sure that the client exists*)
		If[KeyExistsQ[$Clients,uuid],
			(*THEN*)
			(*client exists, so check the option*)
			(
				If[KeyExistsQ[$Clients[uuid],opt],
					(*THEN*)
					(*key exists, check if it's settable*)
					(
						If[Not[MemberQ[$ProtectedMQTTClientProperties,opt]],
							(*THEN*)
							(*it's a user configurable option*)
							(
								(*see if we want to SetDelayed it or Set it*)
								Switch[HoldComplete[settings],
									HoldComplete[RuleDelayed[___]]|HoldComplete[List[___]],
									(*form for f[ client,"CallbackFunction" :> Print["callback evaled"] ] or for f[ client, { "opt" , val } ]*)
									(
										$Clients[uuid,opt] := Extract[HoldComplete[settings],{1,2}]
									),
									HoldComplete[Rule[___]], (*form for f[ client, opt -> val] where it's okay to eval the rhs*)
									(
										$Clients[uuid,opt] = Extract[HoldComplete[settings],{1,2}]
									),
									_, (*anything else message and exit*)
									(
										Message[General::argx,MQTTClientSetOptions];
										$Failed
									)
								]
							),
							(*ELSE*)
							(*it's a protected option, we don't want the user's messing with it*)
							(
								Message[MQTTClientSetOptions::protectedKey,opt];
								$Failed
							)
						]
					),
					(*ELSE*)
					(*key doesn't exist, can't set it*)
					(
						Message[MQTTClientSetOptions::nokey,opt];
						$Failed
					)
				]
			),
			(*ELSE*)
			(*client doesn't exist*)
			(
				Message[MQTTClientSetOptions::nonexist,client];
				$Failed
			)
		]
	)
];

(*convenience syntax for f[ client , opt, val ], as opposed to f[ client, { opt, val} ]*)
MQTTClientSetOptions[client_MQTTClient,opt_,val_]:=MQTTClientSetOptions[client,{opt,val}];


(*internal association for keeping track of all clients*)
$Clients=<||>;

(*the id is used internally for keeping track of the objects on the C code side*)
id=0;

(*client image is the image to use for the summary box / content box*)
clientImage=ImageResize[Import[PacletResource["MQTTLink","ClientImage"]],50];

Options[CreateClient]=
{
	"CleanSession"->False,
	"Timeout"->10,
	"ClientConnectedFunction"->Null,
	"ClientDisconnectedFunction"->Null
};


CreateClient[host_?(MatchQ[#,_IPAddress|_URL]&),OptionsPattern[]]:=With[
	{
		stringsFromHost = Cases[host,_String,Infinity]
	},
	If[Length[stringsFromHost]>0,
		(*THEN*)
		(*we found a string from the host*)
		CreateClient[
			(*just use the first string*)
			First[stringsFromHost],
			"CleanSession"->OptionValue["CleanSession"],
			"ClientConnectedFunction":>OptionValue["ClientConnectedFunction"],
			"ClientDisconnectedFunction":>OptionValue["ClientDisconnectedFunction"]
		],
		(*ELSE*)
		(*no string inside it, error message*)
		(
			Message[CreateClient::invalidHost,Short[host]];
			$Failed
		)
	]	
];

CreateClient[host_?(MatchQ[#,_IPAddress|_URL]&),port_Integer,OptionsPattern[]]:=With[
	{
		stringsFromHost = Cases[host,_String,Infinity]
	},
	If[Length[stringsFromHost]>0,
		(*THEN*)
		(*we found a string from the host*)
		CreateClient[
			(*just use the first string and the specified port*)
			First[stringsFromHost],
			port,
			"CleanSession"->OptionValue["CleanSession"],
			"ClientConnectedFunction":>OptionValue["ClientConnectedFunction"],
			"ClientDisconnectedFunction":>OptionValue["ClientDisconnectedFunction"]
		],
		(*ELSE*)
		(*no string inside it, error message*)
		(
			Message[CreateClient::invalidHost,Short[host]];
			$Failed
		)
	]	
];


CreateClient[OptionsPattern[]]:=Module[
	{
		uuid=CreateUUID[],
		cleanSession,
		createStatus
	},
	(
		(*first check the value of persistent session and use that for CleanSession (they are synonymous)*)
		cleanSession=If[MemberQ[{True,False},OptionValue["CleanSession"]],
			(*THEN*)
			(*we can just use the option value set*)
			OptionValue["CleanSession"],
			(*ELSE*)
			(*invalid option value, so just return $Failed with a message*)
			(
				Message[CreateClient::cleanSession,OptionValue["CleanSession"]];
				Return[$Failed];
			)
		];
		(*make a client in the C library*)
		createStatus=iCreateClient[uuid,cleanSession];
		(*now we need to check the result to make sure that the client was created correctly*)
		Which[
			createStatus===$MosquittoSuccess,(*no errors, successfully made*)
			(
				(*nothing to do here...*)
				Null
			),
			createStatus===$MosquittoOutOfMemory,(*not enough memory to create the client*)
			(
				Message[General::nomem];
				Return[$Failed];
			),
			createStatus===$ClientExists,(*client already exists*)
			(
				(*TODO: maybe see if we can find this client already and return that as if it exists in C library, it should also probably exist in WL*)
				Message[CreateClient::exists];
				Return[$Failed];
			),
			True,(*if we get here just return $Failed, somehow the function returned an illegal return value*)
			(
				Return[$Failed];
			)
		];
		AppendTo[
			$Clients,
			uuid->
				<|
					"Connected"->False,
					"UUID"->uuid,
					"ID"->id,
					"BrokerIP"->None,
					"BrokerPort"->None,
					"SubscribedTopics"-><||>,
					"CleanSession"->cleanSession,
					"MessageReceivedFunction"-><||>,
					"SubscriptionCompleteFunction"-><||>,
					"UnsubscribeCompleteFunction"-><||>,
					"PublishCompleteFunction"-><||>,
					"ClientConnectedFunction":>OptionValue["ClientConnectedFunction"],
					"ClientDisconnectedFunction":>OptionValue["ClientDisconnectedFunction"]
				|>
		];
		id++;
		Return[MQTTClient[
				<|
					"Connected"->With[
							{uuidVal=uuid},
							Deploy@Graphics[
								{
									Dynamic[
										If[ValueQ[$Clients]&&KeyExistsQ[uuidVal]@$Clients&&$Clients[uuidVal,"Connected"],
											(*THEN*)
											(*we have an association and this client exists in that association*)
											Darker[Green],
											(*ELSE*)
											(*we don't have an association or this client doesn't exist in that association*)
											Red
										]
									],
									Disk[]
								},
								ImageSize->8
							]
						],
					"UUID"->uuid,
					"ID"->With[{idVal=id},idVal],
					"BrokerIP"->With[{uuidVal=uuid},Dynamic[If[ValueQ[$Clients]&&KeyExistsQ[uuidVal]@$Clients,$Clients[uuidVal,"BrokerIP"],None]]],
					"BrokerPort"->With[{uuidVal=uuid},Dynamic[If[ValueQ[$Clients]&&KeyExistsQ[uuidVal]@$Clients,$Clients[uuidVal,"BrokerPort"],None]]],
					"SubscribedTopics"->With[{uuidVal=uuid},Dynamic[If[ValueQ[$Clients]&&KeyExistsQ[uuidVal]@$Clients,$Clients[uuidVal,"SubscribedTopics"],None]]]
				|>
			]
		];
	)
];

CreateClient[ip_String,port_Integer,OptionsPattern[]]:=Module[
	{client},
	(
		client=CreateClient[
			"CleanSession"->OptionValue["CleanSession"],
			"ClientConnectedFunction":>OptionValue["ClientConnectedFunction"],
			"ClientDisconnectedFunction":>OptionValue["ClientDisconnectedFunction"]
		];
		Return[ConnectClient[client,ip,port,"Timeout"->OptionValue["Timeout"]]];
	)
];

CreateClient[ip_String,OptionsPattern[]]:=Module[
	{client},
	(
		client=CreateClient[
			"CleanSession"->OptionValue["CleanSession"],
			"ClientConnectedFunction":>OptionValue["ClientConnectedFunction"],
			"ClientDisconnectedFunction":>OptionValue["ClientDisconnectedFunction"]
		];
		Return[ConnectClient[client,ip,"Timeout"->OptionValue["Timeout"]]];
	)
];

Options[ConnectClient]=
{
	"Timeout"->10
};

ConnectClient[client_MQTTClient,ip_String,port_Integer,OptionsPattern[]]:=Module[
	{
		clientUUID=client["UUID"],
		timeout,
		connectResult
	},
	(*validate the options first*)
	timeout=If[IntegerQ[OptionValue["Timeout"]],
		(*THEN*)
		(*use whatever was passed in*)
		OptionValue["Timeout"],
		(*ELSE*)
		(*invalid option, return $Failed*)
		(
			Message[ConnectClient::timeout,OptionValue["Timeout"]];
			Return[$Failed];
		)
	];
	Which[
		MemberQ[Keys[$Clients],clientUUID]&&Not[$Clients[clientUUID,"Connected"]],
		(*then the client isn't connected and exists, so we can connect it*)
		(
			(*connect the client using an asynchronous task*)
			Quiet[
				Internal`CreateAsynchronousTask[
					(Function[{first,last},connectResult=first;last]@@iConnectClient[clientUUID,ip,port,timeout])&,
					{},
					Function[{taskObject,msg,data},
						Which[
							msg === "connection",(*successfully connected*)
							(
								(*normal connection, set the appropriate property to true*)
								$Clients[clientUUID]["Connected"] = True;
								
								(*update the value of this client's BrokerIP and BrokerPort*)
								$Clients[clientUUID,"BrokerIP"]=ip;
								$Clients[clientUUID,"BrokerPort"]=port;

								(*finally the client's connected callback*)
								$Clients[clientUUID,"ClientConnectedFunction"][$AsynchronousTask,client,data];
							),
							msg === "connection_failure",
							(
								(*failed to connect properly, the data will contain reason why, so issue a message*)
								$Clients[clientUUID]["Connected"] = False;
								Switch[First[data],
									$MosquittoConnectionRefusedProtoVersion,
									(
										Message[ConnectClient::connFail,"The broker rejected the connection request due to an incompatible protocol version"];
									),
									$MosquittoConnectionRefusedIdentifierRejected,
									(
										Message[ConnectClient::connFail,"The broker rejected the connection request due to an invalid identifier"];
									),
									$MosquittoBrokerUnavailable,
									(
										Message[ConnectClient::connFail,"The broker specified is unavailable"];
									),
									_,
									(
										Message[ConnectClient::connFail,"Unable to connect to the specified broker"];
									)
								];
							),
							msg === "expected_disconnect",
							(
								(*normal expected disconnect, so just set the connected status to false*)
								$Clients[clientUUID]["Connected"] = False;
							),
							msg === "unexpected_disconnect",
							(
								(*unexpected disconnect, so raise a message about reason why*)
								$Clients[clientUUID]["Connected"] = False;
								Message[ConnectClient::unexpectDisconn,client,First[data]];
							),
							True,
							(
								(*unknown event type*)
								$Clients[clientUUID]["Connected"] = False;
								Message[ConnectClient::unexpectDisconn,client,data];
							)
						];
						(*now call the user's callback for disconnection as appropriate*)
						If[msg =!= "connection",
							(*THEN*)
							(*call the disconnection callback with the code*)
							(
								(*finally, because we got disconnected, check to see if this client had a clean session or not so we can update*)
								(*the subscriptions appropriately*)
								If[Not[$Clients[uuid,"CleanSession"]],
									(*THEN*)
									(*only need to remove QoS level 0 subscriptions*)
									(
										(*delete all the qos 0 subscriptions*)
										KeyDropFrom[$Clients[clientUUID,"SubscribedTopics"], Keys[Select[#===0&]@$Clients[clientUUID,"SubscribedTopics"]]];
									),
									(*ELSE*)
									(*we need to clear all subscription state*)
									(
										$Clients[clientUUID,"SubscribedTopics"]=<||>;
									)
								];
								
								(*now call the user's specified callback for disconnection if it exists*)
								$Clients[clientUUID,"ClientDisconnectedFunction"][$AsynchronousTask,client,data];
							)
						];
					],
					"TaskDetail"->clientUUID,
					"Visible"->False
				],
				{Internal`CreateAsynchronousTask::noid,StringForm::sfr}
			];
			Switch[connectResult,
				$MosquittoSuccess,(*no error*)
				(
					(*just return the client back to the user*)
					Return[client]
				),
				$ThreadError, (*the thread failed to start for some reason*)
				(
					Message[ConnectClient::threadFail];
					Return[$Failed];
				),
				$MosquittoLookupError, (*failed to find host or some other EAI error in getaddrinfo*)
				(
					Message[ConnectClient::noBroker,ip];
					Return[$Failed];
				),
				$MosquittoErrnoError|_, (*any other case use the errno message*)
				(
					Message[ConnectClient::errno,ip];
					Return[$Failed];
				)
			];
		),
		(*the client is already connected and exists, so don't do anything*)
		$Clients[clientUUID,"Connected"],
		(
			Message[ConnectClient::alreadyConnected,client];
			$Failed
		),
		Not[MemberQ[Keys[$Clients],clientUUID]],
		(*client doesn't exist*)
		(
			(*TODO: create the client with these properties, as this object was probably copied over or an old one from a notebook that was just opened*)
			(*so it doesn't make sense to the user that the client doesn't exist*)
			(*for now just fail and complain about the client not existing*)
			Message[ConnectClient::nonexist,client];
			$Failed
		)
	]
];

(*when the user just specifies the ip address and no string, check for port, if not found use 1883*)
ConnectClient[client_MQTTClient,ip_String,OptionsPattern[]]:=With[
	{
		port = StringCases[":" ~~ DigitCharacter ..]@ip
	},
	(
		(*check for a port spec in the ip string*)
		If[Length[port]>0 && IntegerQ[First[port]],
			(*THEN*)
			(*the port was in the ip string*)
			(
				ConnectClient[client,ip,First[port],"Timeout"->OptionValue["Timeout"]]
			),
			(*ELSE*)
			(*port is missing from the string - use default and issue message*)
			(
				Message[ConnectClient::noPort];
				ConnectClient[client,ip,1883,"Timeout"->OptionValue["Timeout"]]
			)
		]
	)
];

ConnectClient[client_MQTTClient,host_?(MatchQ[#,_IPAddress|_URL]&),OptionsPattern[]]:=With[
	{
		urlPort = StringCases[":" ~~ DigitCharacter ..]@First[host],
		optsList = With[{keys=Keys[<|Options[ConnectClient]|>]},
			AssociationThread[keys,OptionValue/@keys]
		]
	},
	If[Length[urlPort]>0 && IntegerQ[urlPort[[1]]],
		(*THEN*)
		(*we have a port from the IPAddress, use it*)
		ConnectClient[
			client,
			host,
			First[urlPort],
			Sequence@@optsList
		],
		(*ELSE*)
		(*we don't have a port - issue a message and call it with 1883*)
		(
			Message[ConnectClient::noPort];
			ConnectClient[
				client,
				host,
				1883,
				Sequence@@optsList
			]
		)
		
	]
];

ConnectClient[client_MQTTClient,host_?(MatchQ[#,_IPAddress|_URL]&),port_Integer,OptionsPattern[]]:=With[
	{
		stringsFromHost = Cases[host,_String,Infinity]
	},
	If[Length[stringsFromHost]>0,
		(*THEN*)
		(*we found a string from the host*)
		ConnectClient[
			client,
			First[Cases[host,_String,Infinity]],
			port,
			Sequence@@With[{keys=Keys[<|Options[ConnectClient]|>]},
				Normal[AssociationThread[keys,OptionValue/@keys]]
			]
		],
		(*ELSE*)
		(*no string inside it, error message*)
		(
			Message[ConnectClient::invalidHost,Short[host]];
			$Failed
		)
	]	
]


ConnectClient[client_MQTTClient,OptionsPattern[]]:=Module[
	{
		hostIP=client["BrokerIP"],
		hostPort=client["BrokerPort"]
	},
	Which[
		MissingQ[$Clients[client["UUID"]]],(*then the client passed in doesn't exist*)
		(
			(*TODO: create the client with these properties, as this object was probably copied over or an old one from a notebook that was just opened*)
			(*so it doesn't make sense to the user that the client doesn't exist*)
			(*for now just fail and complain about the client not existing*)
			Message[ConnectClient::nonexist,client];
			Return[$Failed];
		),
		hostIP===None&&hostPort===None, (*then this one doesn't have any ports specified, assume localhost and 1883*)
		(
			Message[ConnectClient::noIP];
			Message[ConnectClient::noPort];
			Return[ConnectClient[client,"localhost",1883,"Timeout"->OptionValue["Timeout"]]];
		),
		hostIP===None&&hostPort=!=None,(*port exists, ip doesn't*)
		(
			Message[ConnectClient::noIP];
			Return[ConnectClient[client,"localhost",hostPort,"Timeout"->OptionValue["Timeout"]]];
		),
		hostIP=!=None&&hostPort===None,(*ip exists, port doesn't*)
		(
			Message[ConnectClient::noPort];
			Return[ConnectClient[client,hostIP,1883,"Timeout"->OptionValue["Timeout"]]];
		),
		True, (*the last case is that both have appropriate values*)
		Return[ConnectClient[client,hostIP,hostPort,"Timeout"->OptionValue["Timeout"]]]
	]	
];

(*note that in all circumstances where the client is legitimately connected before this call, the ClientDisconnectedFunction callback option will be called during this*)
(*function call, possibly after due to how the kernel ends up handling the AsynchronousTask, but this function is guaranteed to trigger the callback to be queued*)
DisconnectClient[client_MQTTClient]:=Module[
	{
		uuid = client["UUID"],
		disconnectResult
	},
	(
		(*check to make sure the uuid is valid*)
		If[Not[MissingQ[$Clients[client["UUID"]]]],
			(*THEN*)
			(*client uuid exists, we can disconnect it*)
			(
				disconnectResult = iDisconnectClient[uuid];
				Switch[disconnectResult,
					$MosquittoSuccess, (*no error - set the state to be disconnected*)
					(
						(*when we disconnect, we need to check the subscriptions.*)
						(*if this client was connected with CleanSession->False, then if it is reconnected later on, it will keep QualityOfService level 2 or 1*)
						(*subscriptions, while all QualityOfService subscriptions are lost*)
						(*also note that no subscriptions are kept if CleanSession->True, as when it disconnects now, all state is lost on the broker about*)
						(*this particular client*)
						If[Not[$Clients[uuid,"CleanSession"]],
							(*THEN*)
							(*only need to remove QoS level 0 subscriptions*)
							(
								(*delete all the qos 0 subscriptions*)
								KeyDropFrom[$Clients[uuid,"SubscribedTopics"], Keys[Select[#===0&]@$Clients[uuid,"SubscribedTopics"]]];
							),
							(*ELSE*)
							(*we need to clear all subscription state*)
							(
								$Clients[uuid,"SubscribedTopics"]=<||>;
							)
						];
						$Clients[uuid,"Connected"] = False;
						client
					),
					$ClientNotFound,(*uuid is invalid*)
					(
						Message[DisconnectClient::nonexist,client];
						$Failed
					),
					_,
					(
						Message[DisconnectClient::unknownError,client];
						$Failed
					)
				]
			),
			(*ELSE*)
			(*uuid doesn't exist, issue message and return $Failed*)
			(
				Message[DisconnectClient::nonexist,client];
				$Failed
			)
		]
	)
];


Options[TopicPublish]=
{
	"QualityOfService"->2,
	"Retain"->False,
	"PublishCompleteFunction"->Null
}; 

TopicPublish[client_MQTTClient,topic_String->message_ByteArray,OptionsPattern[]]:=
	TopicPublish[
		client,
		Evaluate[topic],
		Evaluate[Normal[message]],
		"QualityOfService"->OptionValue["QualityOfService"],
		"Retain"->OptionValue["Retain"],
		"PublishCompleteFunction":>OptionValue["PublishCompleteFunction"]
	];
	
TopicPublish[client_MQTTClient,topic_String,message_ByteArray,OptionsPattern[]]:=
	TopicPublish[
		client,
		Evaluate[topic],
		Evaluate[Normal[message]],
		"QualityOfService"->OptionValue["QualityOfService"],
		"Retain"->OptionValue["Retain"],
		"PublishCompleteFunction":>OptionValue["PublishCompleteFunction"]
	];

TopicPublish[client_MQTTClient,topic_String->message_String,OptionsPattern[]]:=
	TopicPublish[
		client,
		Evaluate[topic],
		Evaluate[ToCharacterCode[message]],
		"QualityOfService"->OptionValue["QualityOfService"],
		"Retain"->OptionValue["Retain"],
		"PublishCompleteFunction":>OptionValue["PublishCompleteFunction"]
	];

TopicPublish[client_MQTTClient,topic_String,message_String,OptionsPattern[]]:=
	TopicPublish[
		client,
		Evaluate[topic],
		Evaluate[ToCharacterCode[message]],
		"QualityOfService"->OptionValue["QualityOfService"],
		"Retain"->OptionValue["Retain"],
		"PublishCompleteFunction":>OptionValue["PublishCompleteFunction"]
	];

TopicPublish[client_MQTTClient,topic_String->message_List,OptionsPattern[]]:=
	TopicPublish[
		client,
		topic,
		message,
		"QualityOfService"->OptionValue["QualityOfService"],
		"Retain"->OptionValue["Retain"],
		"PublishCompleteFunction":>OptionValue["PublishCompleteFunction"]
	];
	
TopicPublish[client_MQTTClient,topic_String,message_List,OptionsPattern[]]:=Module[
	{
		uuid = client["UUID"],
		topicBytes=ToCharacterCode[topic,"ASCII"],
		messageLength=Length[message],
		publishResult,
		qos,
		retain,
		asyncTask,
		messageID = CreateUUID[]
	},
	(
		Which[
			(*the client doesn't exist*)
			Not[KeyExistsQ[$Clients,uuid]],
			(
				Message[TopicPublish::nonexist,client];
				Return[$Failed];
			),
			(*the client isn't connected to a broker*)
			(*TODO: have a check in here that if we can reconnect the client, then do so before failing*)
			Not[$Clients[uuid,"Connected"]],
			(
				Message[TopicPublish::notConnected,client];
				Return[$Failed];
			),
			(*qos option is valid*)
			Not[MemberQ[{0,1,2},OptionValue["QualityOfService"]]],
			(
				Message[TopicPublish::qos,OptionValue["QualityOfService"]];
				Return[$Failed];
			),
			(*retain is valid*)
			Not[MemberQ[{True,False},OptionValue["Retain"]]],
			(
				Message[TopicPublish::retain,OptionValue["Retain"]];
				Return[$Failed];
			),
			(*message isn't too long*)
			messageLength>268435455,
			(
				Message[TopicPublish::messageTooLarge,Short[message]];
				Return[$Failed];
			),
			(*the topic has non-ascii values*)
			MemberQ[topicBytes,None],
			(
				Message[TopicPublish::topicascii,topic];
				Return[$Failed];
			),
			(*finally, see if the message itself has non-ascii values*)
			Not[ByteArrayQ[ByteArray[message]]],
			(
				Message[TopicPublish::messageascii,Short[message]];
				Return[$Failed];
			),
			(*everything is valid*)
			True,
			(
				qos = OptionValue["QualityOfService"];
				retain = OptionValue["Retain"];
				(*also set the callback for this message*)
				$Clients[uuid,"PublishCompleteFunction",messageID] := OptionValue["PublishCompleteFunction"];
				If[qos==0,
					(*THEN*)
					(*quality of service is 0, so there won't be any response from the broker*)
					(
						(*just run the librarylink function normally*)
						publishResult=First[
							iTopicPublish[
								uuid,
								topic,
								message,
								qos,
								retain
							]
						];
						
						(*now just call the PublishCompleteFunction callback now, as there won't be any response from the broker*)
						$Clients[uuid,"PublishCompleteFunction",messageID][$AsynchronousTask,client,messageID];
						
						(*we return the task, so just set it to Null in this case*)
						Return[<|"MessageID"->messageID|>];
					),
					(*ELSE*)
					(*quality of service is more than 0, so there will be a response from the broker*)
					(
						(*run the publish via CreateAsynchronousTask*)
						asyncTask = Quiet[
							Internal`CreateAsynchronousTask[
								(Function[{first,last},publishResult=first;last]@@iTopicPublish[
									uuid,
									topic,
									message,
									qos,
									retain
								])&,
								{},
								Function[{taskObject,msg,data},
									Which[
										msg=="publish_success",(*successfully published*)
										(
											(*successfully published on the topic - call the PublishCompleteFunction callback*)
											$Clients[uuid,"PublishCompleteFunction",messageID][$AsynchronousTask,client,messageID]
										),
										True,
										(
											Message[TopicPublish::unknownError,data];
										)
									];
									
									(*finally remove this asynchronous task*)
									RemoveAsynchronousTask[$AsynchronousTask];
								],
								"TaskDetail"->topic,
								"Visible"->False
							],
							{Internal`CreateAsynchronousTask::noid,StringForm::sfr}
						]
					)
				];

				(*check the result to make sure it was sent and we didn't get an error*)
				Switch[publishResult,
					$ClientNotFound,
					(
						(*if this ever returns then the client was killed somehow*)
						Return[$Failed];
					),
					$MosquittoSuccess,
					(
						(*success*)
						Return[<|"MessageID"->messageID|>];
					),
					$MosquittoInvalidFunctionArguments,
					(
						Return[$Failed];
					),
					$MosquittoOutOfMemory,
					(
						(*not enough memory to create the message packet*)
						Message[General::nomem];
						Return[$Failed];
					),
					$MosquittoNoConnection,
					(
						Message[TopicPublish::noConnection,client];
						Return[$Failed];
					),
					$MosquittoProtocolError,
					(
						Message[TopicPublish::protocolError];
						Return[$Failed];
					),
					$MosquittoPayloadTooLarge,
					(
						Message[TopicPublish::messageTooLarge,Short[message]];
						Return[$Failed];
					),
					_,(*any other error*)
					(
						(*shouldn't be any other error, but just in case*)
						Return[$Failed];
					)
				]
			)
		]
	)
];


Options[TopicSubscribe]=
{
	"QualityOfService"->2,
	"SubscriptionCompleteFunction"->Null,
	"MessageReceivedFunction"->Null
};
TopicSubscribe[client_MQTTClient,topicPattern_String,OptionsPattern[]]:=Module[
	{
		clientUUID=client["UUID"],
		qos,
		subscribeResult,
		asyncTask,
		taskResult
	},
	(
		(*validate the options first*)
		qos=If[MemberQ[{0,1,2},OptionValue["QualityOfService"]],
			(*THEN*)
			(*option is valid, sue whatever it's current value is*)
			OptionValue["QualityOfService"],
			(*ELSE*)
			(*option is invalid, raise message and return $Failed*)
			(
				Message[TopicSubscribe::qos,OptionValue["QualityOfService"]];
				Return[$Failed];
			)
		];
		(*set the options in $Clients for the MessageReceivedFunction and SubscriptionConfirmed callbacks*)
		$Clients[clientUUID,"SubscriptionCompleteFunction",topicPattern] := OptionValue["SubscriptionCompleteFunction"];
		$Clients[clientUUID,"MessageReceivedFunction",topicPattern] := OptionValue["MessageReceivedFunction"];
		(*check on whether the topic pattern is already connected or if it contains non-ascii characters*)
		If[Not[MemberQ[ToCharacterCode[topicPattern,"ASCII"],None]],
			(*THEN*)
			(*the pattern is good to use, check to make sure we aren't already subscribed to it*)
			(
				If[Not[KeyExistsQ[$Clients[clientUUID,"SubscribedTopics"],topicPattern]],
					(*THEN*)
					(*not subscribed to yet*)
					(
						asyncTask = Quiet[
							Internal`CreateAsynchronousTask[
								(Function[{first,last},subscribeResult=first;last]@@iTopicSubscribe[clientUUID,topicPattern,qos])&,
								{},
								Function[{taskObject,msg,data},
									Which[
										msg === "subscribe_success",(*successfully published*)
										(
											(*successfully subscribed to the topic, check the quality of service that we requested and make sure it matches what we were given*)
											If[qos=!=First[data],
												(*THEN*)
												(*different qos was granted then requested, post a message*)
												(
													(*it's not what was requested check if it's 128, which is code for an error*)
													If[First[data] === 128,
														(*THEN*)
														(*error subscribing, this client isn't subscribed*)
														(
															(*most likely case is insufficient priveledges, but could be other reasons*)
															Message[TopicSubscribe::subFailed,topicPattern,qos,First[data]];
															(*returning $Failed doesn't do anything here, so just return*)
															Return[];
														),
														(*ELSE*)
														(*just different qos level, raise message and continue*)
														(
															Message[TopicSubscribe::diffQos,topicPattern,qos,First[data]];
														)
													];
												)
											];
											(*save the level qos in the SubscribedTopics association*)
											AppendTo[$Clients[clientUUID,"SubscribedTopics"],topicPattern->First[data]];
											
											(*now create the topic subscription task to handle messages on this topic*)
											Quiet[
												Internal`CreateAsynchronousTask[
													(Function[{first,last},taskResult=first;last]@@iCreateTopicSubscriptionTask[clientUUID,topicPattern])&,
													{},
													Function[{taskObject2,msg2,data2},
														(*we got a message on the topic*)
														Which[
															msg2==="message_recieved",(*message received successfully*)
															(
																(*call the user's specified callback function*)
																$Clients[clientUUID,"MessageReceivedFunction",topicPattern][
																	$AsynchronousTask,
																	client,
																	<|
																		(*for the Timestamp option, use the timeInterpret function*)
																		"Timestamp"->timeInterpret["TimestampLow"/.data2,"TimestampHigh"/.data2],
																		(*for Topic, turn the MRawArray into a String*)
																		"Topic"->FromCharacterCode[Normal["Topic"/.data2]],
																		(*for Data, make it into a ByteArray*)
																		"Data"->ByteArray[Normal["Data"/.data2]],
																		(*for Retained, make it a boolean*)
																		"Retained"->("Retained"/.data2)===1,
																		"MessageID"->("MessageID"/.data2),
																		"QualityOfService"->("QualityOfService"/.data2)
																	|>
																]
															),
															True, (*all other cases - issue message*)
															(
																Message[TopicSubscribe::unknownMessageCallbackError,data2];
															)
														];
													],
													"TaskDetail"->topicPattern,
													"Visible"->False
												],
												{Internal`CreateAsynchronousTask::noid,StringForm::sfr}
											];
											
											(*now check the taskResult*)
											If[taskResult =!= $MosquittoSuccess,
												(*THEN*)
												(*client id is invalid somehow*)
												Message[TopicSubscribe::unknownError,taskResult];
											];
											
											(*call the user's SubscriptionCompleteFunction callback*)
											$Clients[clientUUID,"SubscriptionCompleteFunction",topicPattern][$AsynchronousTask,client];
										),
										True,
										(
											Message[TopicSubscribe::unknownError,data];
										)
									];
									
									(*finally remove this asynchronous task*)
									RemoveAsynchronousTask[$AsynchronousTask];
								],
								"Visible"->False
							],
							{Internal`CreateAsynchronousTask::noid,StringForm::sfr}
						];
						Switch[subscribeResult,
							$MosquittoSuccess,
							(
								(*return back the client*)
								Return[client];
							),
							$MosquittoInvalidFunctionArguments,
							(
								Return[$Failed];
							),
							$MosquittoOutOfMemory,
							(
								(*not enough memory to create the message packet*)
								Message[General::nomem];
								Return[$Failed];
							),
							$MosquittoNoConnection,
							(
								Message[TopicSubscribe::noConnection,client];
								Return[$Failed];
							),
							_,(*any other error*)
							(
								(*shouldn't be any other error, but just in case*)
								Return[$Failed];
							)
						]
					),
					(*ELSE*)
					(*already subscribed*)
					(
						Message[TopicSubscribe::alreadySubscribed,client,topicPattern];
						Return[$Failed];
					)
				];
			),
			(*ELSE*)
			(*the pattern contains non-ascii characters and can't be used*)
			(
				Message[TopicSubscribe::topicascii,topicPattern];
				Return[$Failed];
			)
		];
	)
];


Options[TopicUnsubscribe]=
{
	"UnsubscribeCompleteFunction"->Null
};

TopicUnsubscribe[client_MQTTClient,topicPattern_String,OptionsPattern[]]:=Module[
	{
		clientUUID=client["UUID"],
		unsubscribeResult,
		asyncTask
	},
	(
		(*make sure the client exists first*)
		If[KeyExistsQ[$Clients,clientUUID],
			(*THEN*)
			(*check if we are connected next*)
			(
				If[$Clients[clientUUID,"Connected"],
					(*THEN*)
					(*client is connected, and thus could hypothetically be disconnected*)
					(
						(*check whether we are actually subscribed to the topic*)
						If[KeyExistsQ[$Clients[clientUUID,"SubscribedTopics"],topicPattern],
							(*THEN*)
							(*subscribed to, so we can unsubscribe*)
							(
								(*first set the callback function for unsubscribing from this topic*)
								$Clients[clientUUID,"UnsubscribeCompleteFunction",topicPattern] := OptionValue["UnsubscribeCompleteFunction"];
								(*now actually perform the task*)
								asyncTask = Quiet[
									Internal`CreateAsynchronousTask[
										(Function[{first,last},unsubscribeResult=first;last]@@iTopicUnsubscribe[clientUUID,topicPattern])&,
										{},
										Function[{taskObject,msg,data},
											Which[
												msg=="unsubscribe_success",(*successfully unsubscribed*)
												(
													(*success, remove this topic from the list of subscribed topics*)
													$Clients[clientUUID,"SubscribedTopics"]=KeyDrop[$Clients[clientUUID,"SubscribedTopics"],topicPattern];
													
													(*now also call the callback for this unsubscribe*)
													$Clients[clientUUID,"UnsubscribeCompleteFunction",topicPattern][$AsynchronousTask,client,topicPattern];
												),
												True,
												(
													Message[TopicUnsubscribe::unknownError,data];
												)
											];
											
											(*finally remove this asynchronous task*)
											RemoveAsynchronousTask[$AsynchronousTask];
										]
									],
									{Internal`CreateAsynchronousTask::noid,StringForm::sfr}
								];
								Switch[unsubscribeResult,
									$MosquittoSuccess,
									(
										Return[asyncTask];
									),
									$MosquittoInvalidFunctionArguments,
									(
										$Failed
									),
									$MosquittoOutOfMemory,
									(
										(*not enough memory to create the message packet*)
										Message[General::nomem];
										$Failed
									),
									$MosquittoNoConnection,
									(
										Message[TopicUnsubscribe::noConnection,client];
										$Failed
									),
									_,(*any other error*)
									(
										(*shouldn't be any other error, but just in case*)
										$Failed
									)
								]
							),
							(*ELSE*)
							(*already subscribed*)
							(
								Message[TopicUnsubscribe::noSubscribe,client,topicPattern];
								$Failed
							)
						]
					),
					(*ELSE*)
					(*not connected*)
					(
						$Failed
					)
				]
			),
			(*ELSE*)
			(*client doesn't exist*)
			(
				Message[TopicUnsubscribe::nonexist,client];
				$Failed
			)
		]
	)
];

(*time interpret takes the time integers from the built-in OS functions and converts them to valid DateObjects*)
timeInterpret[low_Integer,high_Integer] := Module[{},
	(
		(*this takes two integers and turns them into a proper time object with correct TimeZone and such*)
		Switch[$OperatingSystem,
			"MacOSX"|"Unix",
			(
				(*on unix systems, the two integers are the number of seconds since the unix epoch and the fractional number of microseconds*)
				TimeZoneConvert[
					FromUnixTime[0,TimeZone->"UTC"]+UnitConvert[Quantity[low,"Microseconds"]+Quantity[high,"Seconds"],"Seconds"],
					$TimeZone
				]
			),
			"Windows",
			(
				(*on windows, just take bitshift the high int up 32 and OR it with the low to get the number of 100 nanosecond intervals since Jan 1 1601 00:00 UTC*)
				TimeZoneConvert[
					Quantity[BitOr[BitShiftLeft[high,32],low]*100,"Nanoseconds"]+DateObject[{1601,1,1,0,0},TimeZone->"UTC"],
					$TimeZone
				]
			),
			_,(*if for some reason $OperatingSystem isn't one of the above...*)
			(
				$Failed
			)
		]
	)
];

StartBroker[port_Integer]:=Block[
	{
		(*get the mosquitto binary for this system*)
		executable=First[FileNames["*",FileNameJoin[{PacletResource["MQTTLink","Binaries"], $SystemID}]]],
		stderr,
		proc,
		exitcode
	},
	(
		(*check to see if there is already a broker running on this port*)
		If[Not[KeyExistsQ[$Brokers,port]],
			(*THEN*)
			(*can start up a new broker on this port*)
			(
				proc = StartProcess[{executable,"-p",ToString[port],"-c",PacletResource["MQTTLink","BrokerConfig"]}];
				If[ProcessStatus[proc]=!="Running",
					(*THEN*)
					(*error trying to start the broker*)
					(
						stderr = ReadString @ ProcessConnection[proc, "StandardError"];
						exitcode = ProcessInformation[proc, "ExitCode"];
						Message[StartBroker::startError,stderr,exitcode];
						$Failed
					),
					(*ELSE*)
					(*no error, return fine*)
					$Brokers[port] = proc
				]
			),
			(*ELSE*)
			(*can't start it up, already have a broker on the port*)
			(
				Message[StartBroker::running,port];
				$Brokers[port]
			)
		]
	)
];

(*the With here ensures that $Brokers can't ever be modified with something like MQTTBrokers[][1883] = blah *)
MQTTBrokers[] := With[{assoc=$Brokers},assoc]


End[] (* End Private Context *)

EndPackage[]
