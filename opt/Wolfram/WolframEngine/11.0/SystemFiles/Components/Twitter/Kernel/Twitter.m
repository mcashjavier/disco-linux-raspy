Get["TwitterFunctions.m"]

Begin["TwitterOAuth`"] (* Begin Private Context *)

ServiceExecute::maximg = "Maximum number of images is 4.";

Begin["`Private`"](* Begin Private Context *)

(******************************* Twitter *************************************)

(* Authentication information *)

twitterdata[]=If[
	TrueQ[OAuthClient`Private`$AllowNonBlockingDialogsQ],{
		"OAuthVersion"		->"1.0a",
		"ServiceName" 		-> "Twitter",
		"RequestEndpoint" 	-> "https://api.twitter.com/oauth/request_token",
	  	"AccessEndpoint" 	-> "https://api.twitter.com/oauth/access_token",
	 	"AuthorizeEndpoint" -> "https://api.twitter.com/oauth/authorize",
	 	"RedirectURI"       -> "WolframConnectorChannelListen",
	 	"Blocking"           ->False,
	 	"VerifierParsing"   -> "oauth_verifier",
	 	"ClientInfo"		-> {"Wolfram","Token"},
	 	"AuthenticationDialog" -> "WolframConnectorChannel",
	 	"Gets"				-> {"GetTweet",OAuthClient`Private`gridRequests["TweetGrid"],"LastTweet","FollowerIDs","FriendIDs","UserData","UserMentions","UserReplies","FollowerMentionNetwork",
	 		"FriendMentionNetwork","FollowerReplyToNetwork","FriendReplyToNetwork","FriendNetwork","FollowerNetwork","Friends","Followers",
	 		"RateLimit","UserIDSearch","SearchNetwork","SearchReplyToNetwork","SearchMentionNetwork","UserHashtags","TweetSearch",
	 		"TweetEventSeries","TweetTimeline","TweetList"},
	 	"Posts"				-> {"Tweet","ImageTweet"},
	 	"RawGets"			-> {"RawMentionsTimeline","RawUserTimeline","RawHomeTimeline","RawRetweetTimeline",
	 				"RawStatus","RawRetweets","RawRetweeterIDs","RawTweetSearch",
	 				"RawDirectMessages","RawDirectMessagesSent","RawDirectMessage",
	 				"RawNoRetweetUserIDs", "RawFriendIDs", "RawFollowerIDs",
					"RawMyFriendship", "RawIncomingFriendships",
					"RawOutgoingFriendships", "RawFriendship", "RawFriends",
					"RawFollowers","RawUserSettings","RawVerifyCredentials","RawBlockList","RawBlockIDs",
					"RawUsers","RawUser","RawUserSearch","RawContributees","RawContributors",
					"RawSuggestedUsers","RawSuggestedUserCategories","RawSuggestedUserStatuses","RawFavorites","RawAccountStatus"},
	 	"RawPosts"			-> {
	 				"RawDeleteTweet",
	 				"RawUpdate","RawRetweet","RawMediaUpload","RawUpload",
	 				"RawDeleteDirectMessage","RawSendDirectMessage",
	 				"RawUpdateFollowing", "RawStopFollowing", "RawStartFollowing",
	 				"RawSetUserSettings","RawUpdateDevice","RawUpdateProfile","RawUpdateBackgroundImage",
	 				"RawUpdateProfileColors",
	 				(* "RawUpdateProfileImage",*)"RawCreateBlock","RawRemoveBlock","RawAddFavorite","RawRemoveFavorite"},
	 	"LogoutURL"			-> "https://twitter.com/logout",
 		"Information"		-> "A service for sending and receiving tweets from a Twitter account"

    }
    ,
    {
        "OAuthVersion"      ->"1.0a",
        "ServiceName"       -> "Twitter",
        "RequestEndpoint"   -> "https://api.twitter.com/oauth/request_token",
        "AccessEndpoint"    -> "https://api.twitter.com/oauth/access_token",
        "AuthorizeEndpoint" -> "https://api.twitter.com/oauth/authorize",
        "ClientInfo"        -> {"Wolfram","Token"},
        "AuthenticationDialog" :> (OAuthClient`tokenOAuthDialog[#, "Twitter",bird]&),
        "Gets"              -> {"GetTweet",OAuthClient`Private`gridRequests["TweetGrid"],"LastTweet","FollowerIDs","FriendIDs","UserData","UserMentions","UserReplies","FollowerMentionNetwork",
            "FriendMentionNetwork","FollowerReplyToNetwork","FriendReplyToNetwork","FriendNetwork","FollowerNetwork","Friends","Followers",
            "RateLimit","UserIDSearch","SearchNetwork","SearchReplyToNetwork","SearchMentionNetwork","UserHashtags","TweetSearch",
            "TweetEventSeries","TweetTimeline","TweetList"},
        "Posts"             -> {"Tweet","ImageTweet"},
        "RawGets"           -> {"RawMentionsTimeline","RawUserTimeline","RawHomeTimeline","RawRetweetTimeline",
                    "RawStatus","RawRetweets","RawRetweeterIDs","RawTweetSearch",
                    "RawDirectMessages","RawDirectMessagesSent","RawDirectMessage",
                    "RawNoRetweetUserIDs", "RawFriendIDs", "RawFollowerIDs",
                    "RawMyFriendship", "RawIncomingFriendships",
                    "RawOutgoingFriendships", "RawFriendship", "RawFriends",
                    "RawFollowers","RawUserSettings","RawVerifyCredentials","RawBlockList","RawBlockIDs",
                    "RawUsers","RawUser","RawUserSearch","RawContributees","RawContributors",
                    "RawSuggestedUsers","RawSuggestedUserCategories","RawSuggestedUserStatuses","RawFavorites","RawAccountStatus"},
        "RawPosts"          -> {
                    "RawDeleteTweet",
                    "RawUpdate","RawRetweet","RawMediaUpload","RawUpload",
                    "RawDeleteDirectMessage","RawSendDirectMessage",
                    "RawUpdateFollowing", "RawStopFollowing", "RawStartFollowing",
                    "RawSetUserSettings","RawUpdateDevice","RawUpdateProfile","RawUpdateBackgroundImage",
                    "RawUpdateProfileColors",
                    (* "RawUpdateProfileImage",*)"RawCreateBlock","RawRemoveBlock","RawAddFavorite","RawRemoveFavorite"},
        "LogoutURL"         -> "https://twitter.com/logout",
        "Information"       -> "A service for sending and receiving tweets from a Twitter account"
    }
]

(* a function for importing the raw data - usually json or xml - from the service *)
twitterimport[$Failed]:=Throw[$Failed]
twitterimport[json_]:=With[{res=ImportString[json,"RawJSON"]},
	If[KeyExistsQ[res,"errors"],
		Message[ServiceExecute::apierr,#["message"]]&/@(res["errors"]);
		Throw[$Failed]
	,
		If[KeyExistsQ[res,"error"],
			If[res["error"] === "Not authorized.",
				{}
			,
				Message[ServiceExecute::apierr,res["error"]];
				Throw[$Failed]
			]
		,
			res
		]
	]
]


(****** Raw Properties ******)
(* information:
 Each entry includes the api endpoint, the HTTP method ("GET" or "POST") as well as the different types of parameters that are used
 "Parameters" - standard URL parameters that appear in the query string as ?param1=val1&param2=val2...
 "PathParameters" - parameters that are included in the url path scheme://domain/path1/`1`/`2`...  make the URL (ToString[StringForm[...,#1,#2]]&)
 "BodyData" - parameters in HTTP Post requests that are includes as body data
 "MultipartData" - parameters in HTTP Post requests that are includes as multip part data,
 		usually large files or images are multipart data, each parameter should be given as {"parametername","datatype"}
 "RequiredParameters" - all above parameters are assumed to be optional, list required parameters a second time here
 "Headers" - additional headers to be included in the HTTP request
 "RequiredPermissions" - If we support incrementally adding permission for a service, list the required permissions for the request here*)

(*** Raw ***)

(** Timelines **)
twitterdata["RawMentionsTimeline"] = {
        "URL"				-> "https://api.twitter.com/1.1/statuses/mentions_timeline.json",
        "Parameters" 		-> {"count","since_id","max_id","trim_user","contributor_details","include_entities"},
        "RequiredParameters"-> {},
        "HTTPSMethod"		-> "GET",
        "ResultsFunction"	-> twitterimport
    }

twitterdata["RawUserTimeline"] = {
        "URL"				-> "https://api.twitter.com/1.1/statuses/user_timeline.json",
        "Parameters" 		-> {"user_id","screen_name","count","since_id","max_id","trim_user","exclude_replies","contributor_details","include_rts"},
        "RequiredParameters"-> {"user_id"|"screen_name"},
        "HTTPSMethod"		-> "GET",
        "ResultsFunction"	-> twitterimport
    }

twitterdata["RawHomeTimeline"] = {
        "URL"				-> "https://api.twitter.com/1.1/statuses/home_timeline.json",
        "Parameters" 		-> {"count","since_id","max_id","trim_user","exclude_replies","contributor_details","include_entities"},
        "HTTPSMethod"		-> "GET",
        "ResultsFunction"	-> twitterimport
    }

twitterdata["RawRetweetTimeline"] = {
        "URL"				-> "https://api.twitter.com/1.1/statuses/retweets_of_me.json",
        "Parameters" 		-> {"count","since_id","max_id","trim_user","include_entities","include_user_entities"},
        "HTTPSMethod"		-> "GET",
        "ResultsFunction"	-> twitterimport
    }

(** Tweets **)

twitterdata["RawRetweets"] = {
     	"URL"				-> "https://api.twitter.com/1.1/statuses/retweets.json",
        "Parameters" 		-> {"id","count","trim_user"},
        "RequiredParameters"-> {"id"},
        "HTTPSMethod"		-> "GET",
        "ResultsFunction"	-> twitterimport
    }

twitterdata["RawStatus"] = {
     	"URL"				-> "https://api.twitter.com/1.1/statuses/show.json",
        "Parameters" 		-> {"id","include_my_retweet","trim_user","include_entities"},
        "RequiredParameters"-> {"id"},
        "HTTPSMethod"		-> "GET",
        "ResultsFunction"	-> twitterimport
    }

twitterdata["RawDeleteTweet"] = {
        "URL"				-> (ToString@StringForm["https://api.twitter.com/1.1/statuses/destroy/`1`.json", #]&),
        "PathParameters"	-> {"id"},
        "BodyData" 			-> {"trim_user"},
        "RequiredParameters"-> {"id","trim_user"(* Should not be required but URLFetch fails when the BodyData is empty *)},
        "HTTPSMethod"		-> "POST",
        "ResultsFunction"	-> twitterimport
    }

twitterdata["RawUpdate"] = {
        "URL"				-> "https://api.twitter.com/1.1/statuses/update.json",
        "BodyData"			-> {"status","in_reply_to_status_id","lat","long","place_id","display_coordinates","trim_user","media_ids"},
        "RequiredParameters"-> {"status"},
        "HTTPSMethod"		-> "POST",
        "ResultsFunction"	-> twitterimport
    }

twitterdata["RawRetweet"] = {
        "URL"				-> (ToString@StringForm["https://api.twitter.com/1.1/statuses/retweet/`1`.json", #]&),
        "PathParameters"	-> {"id"},
        "BodyData" 			-> {"trim_user"},
        "RequiredParameters"-> {"id","trim_user"(* Should not be required but URLFetch fails when the BodyData is empty *)},
        "HTTPSMethod"		-> "POST",
        "ResultsFunction"	-> twitterimport
    }

twitterdata["RawMediaUpload"] = {
        "URL"				-> "https://api.twitter.com/1.1/statuses/update_with_media.json",
        "MultipartData"		-> {{"media[]","image/jpeg"},{"status","text/plain"},
        	{"possibly_sensitive","text/plain"},{"in_reply_to_status_id","text/plain"},
        	{"lat","text/plain"},{"long","text/plain"},{"place_id","text/plain"},{"display_coordinates","text/plain"} },
        "RequiredParameters"-> {"media[]","status"},
        "HTTPSMethod"		-> "POST",
        "ResultsFunction"	-> twitterimport
    }

twitterdata["RawUpload"] = {
		"URL"				-> "https://upload.twitter.com/1.1/media/upload.json",
		"MultipartData"		-> {{"media","image/jpeg"}},
        "RequiredParameters"-> {"media"},
        "HTTPSMethod"		-> "POST",
        "ResultsFunction"	-> twitterimport
	}

(* TODO: statuses/oembed *)

twitterdata["RawRetweeterIDs"] = {(* redundant with RawRetweets ?*)
        "URL"				-> "https://api.twitter.com/1.1/statuses/retweeters/ids.json",
        "Parameters" 		-> {"id","cursor","stringify_ids"},
        "RequiredParameters"-> {"id"},
        "HTTPSMethod"		-> "Get",
        "ResultsFunction"	-> twitterimport
    }

twitterdata["RawTweetSearch"] = {
        "URL"				-> "https://api.twitter.com/1.1/search/tweets.json",
        "Parameters" 		-> {"q","geocode","lang","locale","result_type","count","until","since_id","max_id","include_entities"},
        "RequiredParameters"-> {"q"},
        "HTTPSMethod"		-> "Get",
        "ResultsFunction"	-> twitterimport
    }

(* Stream API requires server support *)

(** Direct messages **)


twitterdata["RawDirectMessages"] = {
        "URL"				-> "https://api.twitter.com/1.1/direct_messages.json",
        "Parameters" 		-> {"count","since_id","max_id","include_entities","skip_status"},
        "HTTPSMethod"		-> "Get",
        "ResultsFunction"	-> twitterimport
    }

twitterdata["RawDirectMessagesSent"] = {
        "URL"				-> "https://api.twitter.com/1.1/direct_messages/sent.json",
        "Parameters" 		-> {"count","since_id","max_id","include_entities","page"},
        "HTTPSMethod"		-> "Get",
        "ResultsFunction"	-> twitterimport
    }

twitterdata["RawDirectMessage"] = {
        "URL"				-> "https://api.twitter.com/1.1/direct_messages/show.json",
        "Parameters" 		-> {"id"},
        "RequiredParameters"-> {"id"},
        "HTTPSMethod"		-> "Get",
        "ResultsFunction"	-> twitterimport
    }

twitterdata["RawDeleteDirectMessage"] = {
        "URL"				-> "https://api.twitter.com/1.1/direct_messages/destroy.json",
        "BodyData" 			-> {"id","include_entities"},
        "RequiredParameters"-> {"id"},
        "HTTPSMethod"		-> "POST",
        "ResultsFunction"	-> twitterimport
    }

twitterdata["RawSendDirectMessage"] = {
        "URL"				-> "https://api.twitter.com/1.1/direct_messages/new.json",
        "BodyData"			-> {"user_id","screen_name","text"},
        "RequiredParameters"-> {"user_id"|"screen_name","text"},
        "HTTPSMethod"		-> "POST",
        "ResultsFunction"	-> twitterimport
    }

(* Friends and Followers *)

twitterdata["RawNoRetweetUserIDs"] = {
     	"URL"				-> "https://api.twitter.com/1.1/friendships/no_retweets/ids.json",
        "Parameters" 		-> {"stringify_ids"},
        "HTTPSMethod"		-> "GET",
        "ResultsFunction"	-> twitterimport
    }

twitterdata["RawFriendIDs"] = { (* who the authenticated user is following *)
     	"URL"				-> "https://api.twitter.com/1.1/friends/ids.json",
        "Parameters" 		-> {"user_id","screen_name","cursor","stringify_ids","count"},
        "HTTPSMethod"		-> "GET",
        "ResultsFunction"	-> twitterimport
    }

twitterdata["RawFollowerIDs"] = { (* who is following the specified user *)
     	"URL"				-> "https://api.twitter.com/1.1/followers/ids.json",
        "Parameters" 		-> {"user_id","screen_name","cursor","stringify_ids","count"},
        "RequiredParameters"-> {"user_id"|"screen_name"},
        "HTTPSMethod"		-> "GET",
        "ResultsFunction"	-> twitterimport
    }

twitterdata["RawMyFriendship"] = { (* for the specified user, takes a comma separated list *)
     	"URL"				-> "https://api.twitter.com/1.1/friendships/lookup.json",
        "Parameters" 		-> {"user_id","screen_name"},
        "RequiredParameters"-> {"user_id"|"screen_name"},
        "HTTPSMethod"		-> "GET",
        "ResultsFunction"	-> twitterimport
    }

twitterdata["RawIncomingFriendships"] = { (* for the specified user *)
     	"URL"				-> "https://api.twitter.com/1.1/friendships/incoming.json",
        "Parameters" 		-> {"cursor","stringify_ids"},
        "HTTPSMethod"		-> "GET",
        "ResultsFunction"	-> twitterimport
    }

twitterdata["RawOutgoingFriendships"] = { (* for the specified user *)
     	"URL"				-> "https://api.twitter.com/1.1/friendships/outgoing.json",
        "Parameters" 		-> {"cursor","stringify_ids"},
        "HTTPSMethod"		-> "GET",
        "ResultsFunction"	-> twitterimport
    }

twitterdata["RawStartFollowing"] = {
        "URL"				-> "https://api.twitter.com/1.1/friendships/create.json",
        "BodyData"			-> {"user_id","screen_name","follow"},
        "RequiredParameters"-> {"user_id"|"screen_name"},
        "HTTPSMethod"		-> "POST",
        "ResultsFunction"	-> twitterimport
    }

twitterdata["RawStopFollowing"] = {
        "URL"				-> "https://api.twitter.com/1.1/friendships/destroy.json",
        "BodyData"			-> {"user_id","screen_name"},
        "RequiredParameters"-> {"user_id"|"screen_name"},
        "HTTPSMethod"		-> "POST",
        "ResultsFunction"	-> twitterimport
    }

twitterdata["RawUpdateFollowing"] = { (* turn devive or retweet notifications on/off *)
        "URL"				-> "https://api.twitter.com/1.1/friendships/update.json",
        "BodyData"			-> {"user_id","screen_name","device","retweets"},
        "RequiredParameters"-> {"user_id"|"screen_name"},
        "HTTPSMethod"		-> "POST",
        "ResultsFunction"	-> twitterimport
    }

twitterdata["RawFriendship"] = { (* between any two users *)
     	"URL"				-> "https://api.twitter.com/1.1/friendships/show.json",
        "Parameters" 		-> {"source_id","source_screen_name","target_id","target_screen_name"},
        "RequiredParameters"-> {"source_id"|"source_screen_name","target_id"|"target_screen_name"},
        "HTTPSMethod"		-> "GET",
        "ResultsFunction"	-> twitterimport
    }

twitterdata["RawFriends"] = { (* who the specified user is following *)
     	"URL"				-> "https://api.twitter.com/1.1/friends/list.json",
        "Parameters" 		-> {"user_id","screen_name","cursor","skip_status","include_user_entities","count"},
        "RequiredParameters"-> {"user_id"|"screen_name"},
        "HTTPSMethod"		-> "GET",
        "ResultsFunction"	-> twitterimport
    }

twitterdata["RawFollowers"] = { (* who is following the specified user *)
     	"URL"				-> "https://api.twitter.com/1.1/followers/list.json",
        "Parameters" 		-> {"user_id","screen_name","cursor","skip_status","include_user_entities","count"},
        "RequiredParameters"-> {"user_id"|"screen_name"},
        "HTTPSMethod"		-> "GET",
        "ResultsFunction"	-> twitterimport
    }

(** Users **)
twitterdata["RawUserSettings"] = {
     	"URL"				-> "https://api.twitter.com/1.1/account/settings.json",
        "HTTPSMethod"		-> "GET",
        "ResultsFunction"	-> twitterimport
    }

twitterdata["RawVerifyCredentials"] = {
     	"URL"				-> "https://api.twitter.com/1.1/account/verify_credentials.json",
        "HTTPSMethod"		-> "GET",
        "Parameters" 		-> {"include_entities","skip_status"},
        "ResultsFunction"	-> twitterimport
    }

twitterdata["RawSetUserSettings"] = {
     	"URL"				-> "https://api.twitter.com/1.1/account/settings.json",
        "HTTPSMethod"		-> "POST",
        "BodyData" 			-> {"trend_location_woeid","sleep_time_enabled","start_sleep_time","end_sleep_time","time_zone","lang"},
        "RequiredParameters"-> {"trend_location_woeid"|"sleep_time_enabled"|"start_sleep_time"|"end_sleep_time"|"time_zone"|"lang"},
        "ResultsFunction"	-> twitterimport
    }

twitterdata["RawUpdateDevice"] = {
     	"URL"				-> "https://api.twitter.com/1.1/account/settings.json",
        "HTTPSMethod"		-> "POST",
        "BodyData" 			-> {"device","include_entities"},
        "RequiredParameters"-> {"device"},
        "ResultsFunction"	-> twitterimport
    }

twitterdata["RawUpdateProfile"] = {
     	"URL"				-> "https://api.twitter.com/1.1/account/update_profile.json",
        "HTTPSMethod"		-> "POST",
        "BodyData" 			-> {"name","url","location","description","include_entities","skip_status"},
        "RequiredParameters"-> {"name"|"url"|"location"|"description"|"include_entities"|"skip_status"},
        "ResultsFunction"	-> twitterimport
    }

twitterdata["RawUpdateBackgroundImage"] = {
     	"URL"				-> "https://api.twitter.com/1.1/account/update_profile_background_image.json",
        "HTTPSMethod"		-> "POST",
        "MultipartData"		-> {{"image","image/jpeg"},{"tile","text/plain"},{"status","text/plain"},{"include_entities","text/plain"},
        	{"include_entities","text/plain"},{"skip_status","text/plain"},{"use","text/plain"}},
        "RequiredParameters"-> {"image"|"tile"|"use"},
        "ResultsFunction"	-> twitterimport
    }

twitterdata["RawUpdateProfileColors"] = {
     	"URL"				-> "https://api.twitter.com/1.1/account/update_profile_colors.json",
        "HTTPSMethod"		-> "POST",
        "BodyData" 			-> {"profile_background_color","profile_link_color","profile_sidebar_border_color",
        	"profile_sidebar_fill_color","profile_text_color","include_entities","skip_status"},
        "ResultsFunction"	-> twitterimport
    }

twitterdata["RawUpdateProfileImage"] = {
     	"URL"				-> "https://api.twitter.com/1.1/account/update_profile.json",
        "HTTPSMethod"		-> "POST",
        "BodyData"			-> {"image","include_entities","skip_status"},
       (* "MultipartData"		-> {{"image","image/jpeg"},{"include_entities","text/plain"},{"skip_status","text/plain"}},*)
        "RequiredParameters"-> {"image"},
        "ResultsFunction"	-> twitterimport
    }


(* Blocks *)
twitterdata["RawBlockList"] = {
     	"URL"				-> "https://api.twitter.com/1.1/blocks/list.json",
        "HTTPSMethod"		-> "GET",
        "Parameters" 		-> {"include_entities","skip_status","cursor"},
        "ResultsFunction"	-> twitterimport
    }

twitterdata["RawBlockIDs"] = {
     	"URL"				-> "https://api.twitter.com/1.1/blocks/ids.json",
        "HTTPSMethod"		-> "GET",
        "Parameters" 		-> {"stringify_ids","cursor"},
        "ResultsFunction"	-> twitterimport
    }

twitterdata["RawCreateBlock"] = {
     	"URL"				-> "https://api.twitter.com/1.1/blocks/create.json",
        "HTTPSMethod"		-> "POST",
        "BodyData" 			-> {"screen_name","user_id","include_entities","skip_status"},
        "RequiredParameters"-> {"screen_name"|"user_id"},
        "ResultsFunction"	-> twitterimport
    }

twitterdata["RawRemoveBlock"] = {
     	"URL"				-> "https://api.twitter.com/1.1/blocks/destroy.json",
        "HTTPSMethod"		-> "POST",
        "BodyData" 			-> {"screen_name","user_id","include_entities","skip_status"},
        "RequiredParameters"-> {"screen_name"|"user_id"},
        "ResultsFunction"	-> twitterimport
    }
(* Users *)
twitterdata["RawUsers"] = { (* comma separated list of users *)
     	"URL"				-> "https://api.twitter.com/1.1/users/lookup.json",
        "HTTPSMethod"		-> "GET",
        "Parameters" 		-> {"screen_name","user_id","include_entities"},
        "RequiredParameters"-> {"screen_name"|"user_id"},
        "ResultsFunction"	-> twitterimport
    }

twitterdata["RawUser"] = { (* a single user, with more information *)
     	"URL"				-> "https://api.twitter.com/1.1/users/show.json",
        "HTTPSMethod"		-> "GET",
        "Parameters" 		-> {"screen_name","user_id","include_entities"},
        "RequiredParameters"-> {"screen_name"|"user_id"},
        "ResultsFunction"	-> twitterimport
    }

twitterdata["RawUserSearch"] = {
     	"URL"				-> "https://api.twitter.com/1.1/users/search.json",
        "HTTPSMethod"		-> "GET",
        "Parameters" 		-> {"q","page","count","include_entities"},
        "RequiredParameters"-> {"q"},
        "ResultsFunction"	-> twitterimport
    }

twitterdata["RawContributees"] = {
     	"URL"				-> "https://api.twitter.com/1.1/users/contributees.json",
        "HTTPSMethod"		-> "GET",
        "Parameters" 		-> {"screen_name","user_id","include_entities","skip_status"},
        "RequiredParameters"-> {"screen_name"|"user_id"},
        "ResultsFunction"	-> twitterimport
    }

twitterdata["RawContributors"] = {
     	"URL"				-> "https://api.twitter.com/1.1/users/contributors.json",
        "HTTPSMethod"		-> "GET",
        "Parameters" 		-> {"screen_name","user_id","include_entities","skip_status"},
        "RequiredParameters"-> {"screen_name"|"user_id"},
        "ResultsFunction"	-> twitterimport
    }

(* profile banners omitted for now *)

(** Suggested Users **)
twitterdata["RawSuggestedUsers"] = {
     	"URL"				-> (ToString@StringForm["https://api.twitter.com/1.1/users/suggestions/`1`.json", #]&),
        "HTTPSMethod"		-> "GET",
        "PathParameters"	-> {"slug"},
        "Parameters" 		-> {"lang"},
        "RequiredParameters"-> {"slug"},
        "ResultsFunction"	-> twitterimport
    }

twitterdata["RawSuggestedUserCategories"] = {
     	"URL"				-> "https://api.twitter.com/1.1/users/suggestions.json",
        "HTTPSMethod"		-> "GET",
        "Parameters" 		-> {"lang"},
        "ResultsFunction"	-> twitterimport
    }

twitterdata["RawSuggestedUserStatuses"] = {
     	"URL"				-> (ToString@StringForm["https://api.twitter.com/1.1/users/suggestions/`1`/members.json", #]&),
        "HTTPSMethod"		-> "GET",
        "PathParameters"	-> {"slug"},
        "Parameters" 		-> {"lang"},
        "RequiredParameters"-> {"slug"},
        "ResultsFunction"	-> twitterimport
    }

(** Favorites **)
twitterdata["RawFavorites"] = {
     	"URL"				-> "https://api.twitter.com/1.1/favorites/list.json",
        "HTTPSMethod"		-> "GET",
        "Parameters" 		-> {"screen_name","user_id","count","include_entities","since_id","max_id"},
        "ResultsFunction"	-> twitterimport
    }

twitterdata["RawRemoveFavorite"] = {
     	"URL"				-> "https://api.twitter.com/1.1/favorites/destroy.json",
        "HTTPSMethod"		-> "POST",
        "BodyData" 			-> {"id","include_entities"},
        "RequiredParameters"-> {"id"},
        "ResultsFunction"	-> twitterimport
    }

twitterdata["RawAddFavorite"] = {
     	"URL"				-> "https://api.twitter.com/1.1/favorites/create.json",
        "HTTPSMethod"		-> "POST",
        "BodyData" 			-> {"id","include_entities"},
        "RequiredParameters"-> {"id"},
        "ResultsFunction"	-> twitterimport
    }



(*** App ***)
twitterdata["RawAccountStatus"] = {
        "URL"				-> "https://api.twitter.com/1.1/application/rate_limit_status.json",
        "HTTPSMethod"		-> "GET",
        "ResultsFunction"	-> twitterimport
    }

(** Lists **)
(** Saved Searches **)
(** Places and Geo **)
(** Trends **)
(** Spam reporting **)

twitterdata["icon"]:=bird

twitterdata[___]:=$Failed
(****** Cooked Properties ******)

(* cooked data queries
	Cooked queries call at least one of the raw queried underneath but add pre and post processing to make the function easier to use and the result clearer.
*)


(* SocialMediaData
{"FollowerIDs", "FollowerMentionNetwork", "FollowerNetwork",
"FollowerReplyToNetwork", "Followers", "FriendIDs",
"FriendMentionNetwork", "FriendNetwork", "FriendReplyToNetwork",
"Friends", "RateLimit", "SearchMentionNetwork", "SearchNetwork",
"SearchReplyToNetwork", "UserData"}
*)

getAuthenticatedTwitterID[id_]:=If[ValueQ[authtwitterid[id]]&&authtwitterid[id]=!=$Failed,
	authtwitterid[id],
	authtwitterid[id]=Block[{rawdata=OAuthClient`rawoauthdata[id,"RawVerifyCredentials"],importeddata},
	importeddata=twitterimport[rawdata];
		Lookup[importeddata,"id_str",Throw[$Failed]]
	]
]


twittercookeddata[prop_,id_,rules___Rule]:=twittercookeddata[prop,id,{rules}]
twittercookeddata[prop_,id_,rule_Rule, rest___]:=twittercookeddata[prop,id,{rule}, rest]
twittercookeddata[prop_,id_]:=twittercookeddata[prop,id,{}]

twittercookeddata[prop:("FollowerIDs"|"Followers"|"FriendIDs"|"Friends"),id_,args_]:=Module[
	{invalidParameters, rawdata,params, ids, prop1,data},
	invalidParameters = Select[Keys[args],!MemberQ[{"user_id","UserID","screen_name","ScreenName","Username","Count","MaxItems",MaxItems,"stringify_ids","Elements"},#]&];
	If[Length[invalidParameters]>0,
		Message[ServiceObject::noget,#,"Twitter"]&/@invalidParameters;
		Throw[$Failed]
	];

	prop1=Switch[prop,
		"FollowerIDs"|"Followers","RawFollowerIDs",
		"FriendIDs"|"Friends","RawFriendIDs"
	];
	params = Normal@Map[TwitterToString,KeyMap[# /. {MaxItems | "MaxItems" | "Count" -> "count", "UserID" -> "user_id", "ScreenName"|"Username" -> "screen_name"} &, Association[args]]];
	params=If[FreeQ[params,("user_id"|"screen_name")->_],
		Flatten[{params,"user_id"->getAuthenticatedTwitterID[id]}],
		Flatten[{params}]
	];
	rawdata=OAuthClient`rawoauthdata[id,prop1,params];
	data=twitterimport[rawdata];
	(* Bug 255746 work around
	Lookup[twitterimport[rawdata],"ids",$Failed]
	*)
	(*
    ids=Flatten[Map[StringCases[#, DigitCharacter ..] & ,
        StringCases[rawdata, RegularExpression["(?ms)\"ids\":\\s*\\[\\s*(.+)\\s*\\]"] -> "$1"]]];
	*)
	ids = TwitterToString /@ Lookup[data, "ids", {}];
    If[MatchQ[prop,("Followers"|"Friends")],
    	TwitterGetscreennames[id,ids],
    	ids
    	]

]

twittercookeddata["GetTweet",id_,args_]:=Module[
	{invalidParameters, rawdata, params, data},
	invalidParameters = Select[Keys[args],!MemberQ[{"TweetID","Elements","ShowThumbnails","MediaResolution","ShowIDs"},#]&];
	If[Length[invalidParameters]>0,
		Message[ServiceObject::noget,#,"Twitter"]&/@invalidParameters;
		Throw[$Failed]
	];

	If[!KeyExistsQ[args,"TweetID"],
		Message[ServiceExecute::nparam,"TweetID"];
		Throw[$Failed]
	];

	params = Map[TwitterToString,Association[args]];

	If[!KeyExistsQ[params, "Elements"] || FreeQ[{"Text", "Images", "Data", "Default", Default, "FullData"}, params["Elements"]],
		params["Elements"] = Default;
	];

	rawdata=OAuthClient`rawoauthdata[id,"RawStatus",Flatten[Normal@params/."TweetID"->"id"]];

	data = TwitterFormatByElementType[twitterimport[rawdata],params];
	If[Length[data] > 0,
		Switch[params["Elements"],
			"Text",
				First[data]
			,

			"Images",
				First[data]
			,

			"FullData"|"Default"|Default|"Data"|_,
				Dataset[First[data]]
		]
	,
		data
	]
]

twittercookeddata[prop:("TweetGrid"|"TweetEventSeries"|"TweetTimeline"|"TweetList"),id_,args_]:=Module[
	{invalidParameters, rawdata,params,data,dates, tweets},
	invalidParameters = Select[Keys[args],!MemberQ[{"UserID","ScreenName","Username","MaxItems",MaxItems,"SinceID","MaxID","Count","Elements","ShowThumbnails","MediaResolution","ShowIDs"},#]&];
	If[Length[invalidParameters]>0,
		Message[ServiceObject::noget,#,"Twitter"]&/@invalidParameters;
		Throw[$Failed]
	];

	params = KeyMap[# /. {"TweetID"->"id", MaxItems | "MaxItems" | "Count" -> "count","Query"->"q","ResultType"->"result_type","UserID"->"user_id","ScreenName"|"Username"->"screen_name","SinceID"->"since_id","MaxID"->"max_id"} &, Association[args]];
	params = Normal@params;

	(*params=TwitterFilterParameters[params,TwitterGetallparameters["RawUserTimeline"]];*)
	(*params=params/.HoldPattern[Rule[a_,b_?(!StringQ[#]&)]]:>Rule[a,ToString[b]];*)
	params=If[FreeQ[params,("user_id"|"screen_name")->_],
		Flatten[{params,"user_id"->getAuthenticatedTwitterID[id]}],
		Flatten[{params}]
	]/."TweetID"->"id";

	params = Map[TwitterToString,Association[params]];
	If[ Xnor[!KeyExistsQ[params,"count"],!KeyExistsQ[params,MaxItems]],
    	params = Join[params,<|"count"->20|>]
	];

	If[prop =!= "TweetList" || !KeyExistsQ[params, "Elements"] || FreeQ[{"Text", "Images", "Data", "Default", Default, "FullData"}, params["Elements"]],
		params["Elements"] = Default;
	];

	params["count"]=ToString[params["count"]];
	count= FromDigits[params["count"]];
	data = TwitterPaginationCalls[id,"RawUserTimeline",params,200];
	Switch[prop,
		"TweetGrid",
		TwitterPrettygrid[Join[{{"CreatedAt","Text","TweetID"}},
			{"created_at", "text","id_str"}/.((Normal/@data)/.TwitterFormatvalue["created_at"->(TwitterReadDate[#]&)])]],
		"TweetList",
			data = TwitterFormatByElementType[data,params];
			If[Length[data] > 0,
				Switch[params["Elements"],
					"Text",
						data
					,

					"Images",
						data
					,

					"FullData"|"Default"|Default|"Data"|_,
						Dataset[data]
				]
			,
				data
			]

		(*Association[Replace[FilterRules[Normal[#],{"created_at", "text","id_str"}],
			HoldPattern[Rule][a_String,b_]:>Rule[TwitterCamelCase[a],b],Infinity]/.{
				"IDStr"->"TweetID",TwitterFormatvalue["CreatedAt"->TwitterReadDate]}]&/@data*),
		"TweetEventSeries",
		EventSeries[{"created_at", "text"}/.((Normal/@data)/.TwitterFormatvalue["created_at"->(TwitterReadDate[#]&)])],
		"TweetTimeline",
		{dates, tweets}=Transpose[{"created_at", "text"}/.Reverse@((Normal/@data)/.TwitterFormatvalue["created_at"->(TwitterReadDate[#]&)])];
		TwitterEventTimeline[tweets, dates]
	]
]

twittercookeddata["LastTweet",id_,args_]:=Module[
	{invalidParameters, rawdata, params, screennames, data},
	invalidParameters = Select[Keys[args],!MemberQ[{"user_id","UserID","screen_name","ScreenName","Username"},#]&];
	If[Length[invalidParameters]>0,
		Message[ServiceObject::noget,#,"Twitter"]&/@invalidParameters;
		Throw[$Failed]
	];

	params = KeyMap[# /. {"ScreenName"|"Username"->"screen_name", "UserID"->"user_id"} &, Association@args];
	params["count"] = "1";
	If[!KeyExistsQ[params, "user_id"] && !KeyExistsQ[params, "screen_name"],
		params = Join[params,<|"user_id"->getAuthenticatedTwitterID[id]|>];
	];

	rawdata=OAuthClient`rawoauthdata[id,"RawUserTimeline",Normal@params];
	data = twitterimport[rawdata];
	If[Length[data] > 0,
		First[data]["text"],
		""
	]
]

twittercookeddata["TweetSearch",id_,args_]:=Module[
	{enoughQ,distance,params=If[MatchQ[args,_Rule],Association[List[args]],Association[args]],newParams,invalidParameters},
  invalidParameters = Select[Keys@params,!MemberQ[{"Location",MaxItems,"Distance","MaxItems","Latitude","Longitude","Elements","Query","ResultType"},#]&];

	If[Length[invalidParameters]>0,
		Message[ServiceObject::noget,#,"Twitter"]&/@invalidParameters;
		Throw[$Failed]
	];

	If[!KeyExistsQ[args,"Query"],
		Message[ServiceExecute::nparam,"Query"];
		Throw[$Failed]
	];

	If[ !KeyExistsQ[params,"Elements"],
    	params=Join[params,<|"Elements"->Default|>]
 	];

	If[ Xnor[!KeyExistsQ[params,"MaxItems"],!KeyExistsQ[params,MaxItems]],
    	newParams = Join[params,<|"count"->20|>],
    	newParams = params;
	];

	newParams = KeyMap[# /. {MaxItems | "MaxItems" -> "count","Query"->"q","ResultType"->"result_type"} &, newParams];
	newParams["count"]=ToString[newParams["count"]];
	(*
	count= FromDigits[newParams["count"]];
	*)
	items = TwitterPaginationCalls[id,"RawTweetSearch",newParams,100];
	items = TwitterFormatValueDate/@items;
	(*items = Normal@items;*)

	Dataset[items][All, <|"Text"->"text","CreationDate"->"created_at",
													"Entities"->"entities","Language"->"lang",
													"Favorite Count"->"favorite_count","Retweet Count"->"retweet_count"|>]
]


twittercookeddata["UserData",id_,args_]:=Module[
	{invalidParameters, rawdata,params,users,res, users1},
	invalidParameters = Select[Keys[args],!MemberQ[{"UserID","ScreenName","user_id","screen_name","Username"},#]&];
	If[Length[invalidParameters]>0,
		Message[ServiceObject::noget,#,"Twitter"]&/@invalidParameters;
		Throw[$Failed]
	];

	params = Normal@Map[TwitterToString,KeyMap[# /. {"UserID" -> "user_id", "ScreenName"|"Username" -> "screen_name"} &, Association[args]]];
	(*params=TwitterFilterParameters[params,TwitterGetallparameters["RawUsers"]];*)
	params=If[FreeQ[params,("user_id"|"screen_name")->_],
		Flatten[{params,"user_id"->getAuthenticatedTwitterID[id]}],
		Flatten[{params}]
		];

	(*params = Normal@Map[ToString,Association[params]];*)
	users=Cases[params,HoldPattern[Rule|RuleDelayed]["user_id"|"screen_name",u_]:>u];
	(* comma separate multiple users *)
	If[MatchQ[users,{{___}}],
		users=First[users];
		users1=ToString/@users;
		If[Length[users1]>100, (* split it up *)
			Return[
				Flatten[twittercookeddata["UserData",id,
					(params/.(users->StringJoin[Riffle[#,","]]))]&/@Partition[users1,100,100,1,{}]]
			]
		];
		params=(params/.(users->StringJoin[Riffle[users1,","]]))];
	(* strip @ from screen name *)
	params=params/.("screen_name"->x_):>("screen_name"->StringReplace[x,"@"->""]);
	rawdata=OAuthClient`rawoauthdata[id,"RawUsers",params];
	res=twitterimport[rawdata];
	If[!MatchQ[res,{_Association...}],Throw[$Failed]];
	Association[Replace[
			Replace[FilterRules[Normal[#],{"id", "screen_name", "name", "location", "favourites_count", "followers_count", "friends_count"}],
				HoldPattern[Rule[a_String, b_]] :> Rule[a,TwitterUserdataparse[b,a]],Infinity],
		HoldPattern[Rule[a_,b_]]:>Rule[TwitterCamelCase[a],b],Infinity]]&/@res
]

twittercookeddata["UserHashtags",id_,args_]:=Module[
	{invalidParameters, rawdata, params, users, res, users1, temp, expectedSize=0, retries=0, maxRetries=10, size=0, result={}, strResult=""},
	invalidParameters = Select[Keys[args],!MemberQ[{"user_id","UserID","screen_name","ScreenName","Username","Count","MaxItems",MaxItems},#]&];
	If[Length[invalidParameters]>0,
		Message[ServiceObject::noget,#,"Twitter"]&/@invalidParameters;
		Throw[$Failed]
	];

	params = KeyMap[# /. {MaxItems | "MaxItems" | "Count" -> "count", "UserID" -> "user_id", "ScreenName"|"Username" -> "screen_name"} &, Association[args]];
	(*params=TwitterFilterParameters[params,TwitterGetallparameters["RawUserTimeline"]];*)
	If[!KeyExistsQ[params, "user_id"] && !KeyExistsQ[params, "screen_name"],
		params = Join[params,<|"user_id"->getAuthenticatedTwitterID[id]|>];
	];

	If[!KeyExistsQ[params,"count"],
    	params = Join[params,<|"count"->20|>]
	];

	expectedSize = Switch[Head[params["count"]], Integer, params["count"], String, FromDigits[params["count"]], _, 0];
	params["count"] = 200;
	params = Normal@Map[TwitterToString,params];

	users=Cases[params,HoldPattern[Rule|RuleDelayed]["user_id"|"screen_name",u_]:>u];
	If[MatchQ[users,{{___}}],
		users=First[users];
		users1=ToString/@users;
		params=(params/.(users->StringJoin[Riffle[users1,","]]))
	];

	While[size < expectedSize && retries < maxRetries,
		rawdata=OAuthClient`rawoauthdata[id,"RawUserTimeline",params];
		res=twitterimport[rawdata];
		If[Length[res] === 0, (*No more tweets available*)
			Break[]
		];

		tmp = Flatten[#["text"] & /@ Flatten[#["entities"]["hashtags"] & /@ res]];
		result = Join[result, Take[tmp,Min[expectedSize - size, Length[tmp]]]];
		size = Length[result];

		params = Association@params;
		params["max_id"] = ToString[Last[res]["id"] - 1];
		params = Normal@params;

		retries++;
	];

	result
(*
	rawdata=OAuthClient`rawoauthdata[id,"RawUserTimeline",params];
	res=twitterimport[rawdata];

	DeleteDuplicates[#["text"] & /@ (Flatten[#["entities"]["hashtags"] & /@ res])]*)
]

twittercookeddata["UserMentions",id_,args_]:=Module[
	{invalidParameters, rawdata, params, users, res, users1, temp, expectedSize=0, retries=0, maxRetries=10, size=0, result={}, strResult=""},
	invalidParameters = Select[Keys[args],!MemberQ[{"user_id","UserID","screen_name","ScreenName","Username","Count","MaxItems",MaxItems,"Elements"},#]&];
	If[Length[invalidParameters]>0,
		Message[ServiceObject::noget,#,"Twitter"]&/@invalidParameters;
		Throw[$Failed]
	];

	params = KeyMap[# /. {MaxItems | "MaxItems" | "Count" -> "count", "UserID" -> "user_id", "ScreenName"|"Username" -> "screen_name"} &, Association[args]];
	(*params=TwitterFilterParameters[params,TwitterGetallparameters["RawUserTimeline"]];*)
	If[!KeyExistsQ[params, "user_id"] && !KeyExistsQ[params, "screen_name"],
		params = Join[params,<|"user_id"->getAuthenticatedTwitterID[id]|>];
	];

	If[!KeyExistsQ[params,"count"],
    	params = Join[params,<|"count"->20|>]
	];

	expectedSize = Switch[Head[params["count"]], Integer, params["count"], String, FromDigits[params["count"]], _, 0];
	params["count"] = 200;
	params = Normal@Map[TwitterToString,params];

	users=Cases[params,HoldPattern[Rule|RuleDelayed]["user_id"|"screen_name",u_]:>u];
	If[MatchQ[users,{{___}}],
		users=First[users];
		users1=ToString/@users;
		params=(params/.(users->StringJoin[Riffle[users1,","]]))
	];

	While[size < expectedSize && retries < maxRetries,
		rawdata=OAuthClient`rawoauthdata[id,"RawUserTimeline",params];
		res=twitterimport[rawdata];
		If[Length[res] === 0, (*No more tweets available*)
			Break[]
		];

		tmp = Flatten[#["screen_name"] & /@ Flatten[#["entities"]["user_mentions"] & /@ res]];
		result = Join[result, Take[tmp,Min[expectedSize - size, Length[tmp]]]];
		size = Length[result];

		params = Association@params;
		params["max_id"] = ToString[Last[res]["id"] - 1];
		params = Normal@params;

		retries++;
	];

	result
]

twittercookeddata["UserReplies",id_,args_]:=Module[
	{invalidParameters, rawdata, params, users, res, users1, pos, temp, expectedSize=0, retries=0, maxRetries=10, size=0, result={}},
	invalidParameters = Select[Keys[args],!MemberQ[{"user_id","UserID","screen_name","ScreenName","Elements","Username","Count","MaxItems",MaxItems},#]&];
	If[Length[invalidParameters]>0,
		Message[ServiceObject::noget,#,"Twitter"]&/@invalidParameters;
		Throw[$Failed]
	];

	params = KeyMap[# /. {MaxItems | "MaxItems" | "Count" -> "count", "UserID" -> "user_id", "ScreenName"|"Username" -> "screen_name"} &, Association[args]];

	If[!KeyExistsQ[params, "user_id"] && !KeyExistsQ[params, "screen_name"],
		params = Join[params,<|"user_id"->getAuthenticatedTwitterID[id]|>];
	];

	If[!KeyExistsQ[params,"count"],
    	params = Join[params,<|"count"->20|>]
	];

	If[!KeyExistsQ[params,"Elements"],
    	params = Join[params,<|"Elements"->Default|>]
	];

	expectedSize = Switch[Head[params["count"]], Integer, params["count"], String, FromDigits[params["count"]], _, 0];
	params["count"] = 200;
	params = Normal@Map[TwitterToString,params];
	users=Cases[params,HoldPattern[Rule|RuleDelayed]["user_id"|"screen_name",u_]:>u];
	If[MatchQ[users,{{___}}],
		users=First[users];
		users1=ToString/@users;
		params=(params/.(users->StringJoin[Riffle[users1,","]]))
	];

	While[size < expectedSize && retries < maxRetries,
		rawdata=OAuthClient`rawoauthdata[id,"RawUserTimeline",params];
		res=twitterimport[rawdata];
		If[Length[res] === 0, (*No more tweets available*)
			Break[]
		];

		temp=(Lookup[#,"in_reply_to_user_id_str",Null]&/@res);
		pos=Flatten[Position[temp,Except[Null],{1},Heads->False]];
		temp=(<|"ID"->Lookup[#,"id_str",Null], "Text"->Lookup[#,"text",Null]|>&/@res[[pos]])/.Null:>Sequence@@{};

		result = Join[result, Take[temp,Min[expectedSize - size, Length[temp]]]];
		size = Length[result];

		params = Association@params;
		params["max_id"] = ToString[Last[res]["id"] - 1];
		params = Normal@params;

		retries++;
	];

	params = Association@params;
	Switch[params["Elements"],
		"RawData",
			result
		,

		Default|_,
			Dataset[result]
	]
	(* StringCases[rawdata, RegularExpression["(?ms)in_reply_to_user_id_str\":\"(\\d+)\""] -> "$1"]*)
]

twittercookeddata["Tweet",id_,args_]:=Module[
	{invalidParameters, rawdata,finalparams, mediaResponses, params},
	invalidParameters = Select[Keys[args],!MemberQ[{"Message",GeoLocation,"GeoPosition","InReplyToStatusID","Image"},#]&];
	If[Length[invalidParameters]>0,
		Message[ServiceObject::noget,#,"Twitter"]&/@invalidParameters;
		Throw[$Failed]
	];

	params = Association[args];
	If[!KeyExistsQ[params,"Message"],
		params["Message"] = "";
	];
	If[!KeyExistsQ[params,"Image"],
		params["Image"] = {},
		params["Image"] = Flatten[{params["Image"]}];
	];
	If[Length[params["Image"]] > 4,
		Message[ServiceExecute::maximg,"Twitter"];
		Throw[$Failed]
	];

	mediaResponses = TwitterUploadImages[id, params["Image"]];
	
	finalparams = FilterRules[params,GeoLocation|"GeoPosition"];
	finalparams = finalparams/.HoldPattern[Rule[GeoLocation|"GeoPosition",pos_]]:>(Sequence@@TwitterFormatgeoposition[pos]);
	params = Map[TwitterToString,KeyMap[# /. {"Message"->"status", "InReplyToStatusID"->"in_reply_to_status_id"} &, params]];
	If[Length[mediaResponses] > 0,
		params["media_ids"] = StringJoin[Riffle[#["media_id_string"] & /@ mediaResponses, ","]];
	,
		If[StringLength[params["status"]] === 0,
			Message[ServiceExecute::nparam,"Message"];
			Throw[$Failed]
		];
	];

	finalparams = Join[finalparams, FilterRules[params, "status"|"media_ids"|"in_reply_to_status_id"]];
	rawdata=OAuthClient`rawoauthdata[id,"RawUpdate",Flatten[finalparams]];
	twitterimport[rawdata]["text"]

]

twittercookeddata["RateLimit",id_,args_]:=Module[
	{invalidParameters, rawdata, res},
	invalidParameters = Select[Keys[args],!MemberQ[{},#]&];
	If[Length[invalidParameters]>0,
		Message[ServiceObject::noget,#,"Twitter"]&/@invalidParameters;
		Throw[$Failed]
	];

	rawdata=OAuthClient`rawoauthdata[id,"RawAccountStatus"];
	res=twitterimport[rawdata];
	If[KeyExistsQ[res, "resources"],
		Dataset[Association[Replace[res["resources"], Rule[a_String, b_] :> (StringReplace[a, {StartOfString ~~ "/" -> "", ":" -> "", "/" -> " "}] -> b), {2, 5}]]],
		$Failed
	]
]

twittercookeddata["UserIDSearch",id_,args_]:=Module[
	{invalidParameters, rawdata,res},
	invalidParameters = Select[Keys[args],!MemberQ[{"Query","Elements","MaxItems",MaxItems,"Count"},#]&];
	If[Length[invalidParameters]>0,
		Message[ServiceObject::noget,#,"Twitter"]&/@invalidParameters;
		Throw[$Failed]
	];

	If[!KeyExistsQ[args,"Query"],
		Message[ServiceExecute::nparam,"Query"];
		Throw[$Failed]
	];

	params = KeyMap[# /. {MaxItems | "MaxItems" | "Count" -> "count", "Query"->"q"} &, Association[args]];
	If[!KeyExistsQ[params,"q"],
		params["q"] = 20;
	];

	params = Normal@Map[TwitterToString,params];
	rawdata=OAuthClient`rawoauthdata[id,"RawUserSearch",Flatten[params]];
	res=twitterimport[rawdata];
	(Lookup[#,"id_str",Null]&/@res)/.Null:>Sequence@@{}
]

twittercookeddata[prop:"FollowerMentionNetwork"|"FollowerReplyToNetwork"|"FriendMentionNetwork"|"FriendReplyToNetwork"|"FriendNetwork"|"FollowerNetwork",id_,args_]:=Module[
	{invalidParameters,params,twitterid},
	invalidParameters = Select[Keys[args],!MemberQ[{"user_id","UserID","screen_name","ScreenName","Username","stringify_ids"},#]&];
	If[Length[invalidParameters]>0,
		Message[ServiceObject::noget,#,"Twitter"]&/@invalidParameters;
		Throw[$Failed]
	];
	params = Normal@Map[TwitterToString,KeyMap[# /. {"UserID" -> "user_id", "ScreenName"|"Username" -> "screen_name"} &, Association[args]]];
	params=If[FreeQ[params,("user_id"|"screen_name")->_],
		Flatten[{params,"user_id"->getAuthenticatedTwitterID[id]}],
		Flatten[{params}]
		];
	(*
	twitterid="user_id"/.params;
	If[twitterid==="user_id",
		twitterid=First[TwitterGetuserids[id,{"screen_name"/.params}]]
	];*)
	(*{"user_id"->twitterid,"stringify_ids"->"true"}*)
	TwitterBuildnetwork[prop,id,params]
]

twittercookeddata[prop:("SearchNetwork"|"SearchReplyToNetwork"|"SearchMentionNetwork"),id_,args_]:=TwitterBuildnetwork[prop,id,args]

twittercookeddata["ImageTweet",id_,args___]:=Module[
	{invalidParameters, rawdata, status,media,statusBytes,mediaBytes,params,otherparams, message},
	invalidParameters = Select[Keys[args],!MemberQ[{"Message","Image",GeoLocation,"GeoPosition","InReplyToStatusID"},#]&];
	If[Length[invalidParameters]>0,
		Message[ServiceObject::noget,#,"Twitter"]&/@invalidParameters;
		Throw[$Failed]
	];

	If[!KeyExistsQ[args,"Image"],
		Message[ServiceExecute::nparam,"Image"];
		Throw[$Failed]
	];

	message = ("Message"/.Flatten[{args}])/."Message"->"";

	{status,media}=Switch[{args},
		{_Rule..}|{{_Rule..}},
			{message,
				("Image"/.Flatten[{args}])/."Image":>(Message[ServiceExecute::nparam,"Image"];Throw[$Failed])},
		{_},{"",args},
		_,(Message[ServiceExecute::nparam,"Image"];Throw[$Failed])
	];
	statusBytes=Switch[status,
		{_?IntegerQ...},status,
		_String,ImportString[status, "Byte"],
		_,ImportString[ToString[status], "Byte"]
	]/.{}->{32} (* Create a space, because our MultiPartData does not accept an empty list *);

	mediaBytes=Which[
		MatchQ[media,{_?IntegerQ...}],media,
		TrueQ[Quiet[FileExistsQ[media],FileExistsQ::fstr]],Import[media, "Byte"],
		ImageQ[media],ImportString[ExportString[media, "JPG"], "Byte"],
		True,
			Check[ImportString[ExportString[media, "JPG"], "Byte"],Throw[$Failed]]
	];
	otherparams=FilterRules[args,Except["Message"|"Image"]];
	otherparams=otherparams/.HoldPattern[Rule[GeoLocation|"GeoPosition",pos_]]:>(Sequence@@TwitterFormatgeoposition[pos]);

	otherparams = Normal@Map[TwitterToString,Association[otherparams]];
	otherparams=TwitterFilterParameters[otherparams,TwitterGetallparameters["RawMediaUpload"]];
	If[otherparams=!={},
		otherparams[[All,2]]=ImportString[#, "Byte"]&/@otherparams[[All,2]];
	];

	params=Flatten[{"media[]"->mediaBytes,"status"->statusBytes,otherparams}];
	rawdata=OAuthClient`rawoauthdata[id,"RawMediaUpload",params];
	twitterimport[rawdata]["text"]
]


(* Send Message *)
twittersendmessage[id_,message_String]:=twittercookeddata["Tweet",id,"Message"->message]
twittersendmessage[id_,message_]:=twittercookeddata["ImageTweet",id,{"Image"->message}]/;ImageQ[message]||MatchQ[message,(_Graphics|_Graphics3D)]
twittersendmessage[___]:=$Failed


bird=Image[RawArray["Byte", {{{0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0,
  0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0},
  {0, 0, 0, 0}, {40, 170, 225, 9}, {40, 170, 225, 113}, {40, 170, 225, 207}, {40, 170, 225, 249}, {40, 170, 225, 252},
  {40, 170, 225, 217}, {40, 170, 225, 131}, {40, 170, 225, 16}, {0, 0, 0, 0}, {0, 0, 0, 0}, {40, 170, 225, 1}, {40,
  170, 225, 74}, {40, 170, 225, 19}}, {{0, 0, 0, 0}, {40, 170, 225, 62}, {40, 170, 225, 124}, {0, 0, 0, 0}, {0, 0, 0,
  0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0},
  {0, 0, 0, 0}, {0, 0, 0, 0}, {40, 170, 225, 28}, {40, 170, 225, 210}, {40, 170, 225, 255}, {40, 170, 225, 255}, {40,
  170, 225, 255}, {40, 170, 225, 255}, {40, 170, 225, 255}, {40, 170, 225, 255}, {40, 170, 225, 230}, {40, 170, 225,
  86}, {40, 170, 225, 130}, {40, 170, 225, 212}, {40, 170, 225, 187}, {0, 0, 0, 0}}, {{0, 0, 0, 0}, {40, 170, 225,
  158}, {40, 170, 225, 255}, {40, 170, 225, 119}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0,
  0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {40, 170, 225, 5}, {40, 170, 225, 207},
  {40, 170, 225, 255}, {40, 170, 225, 255}, {40, 170, 225, 255}, {40, 170, 225, 255}, {40, 170, 225, 255}, {40, 170,
  225, 255}, {40, 170, 225, 255}, {40, 170, 225, 255}, {40, 170, 225, 255}, {40, 170, 225, 255}, {40, 170, 225, 228},
  {40, 170, 225, 31}, {40, 170, 225, 33}}, {{0, 0, 0, 0}, {40, 170, 225, 201}, {40, 170, 225, 255}, {40, 170, 225,
  255}, {40, 170, 225, 153}, {40, 170, 225, 8}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0},
  {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {40, 170, 225, 102}, {40, 170, 225, 255}, {40, 170, 225, 255}, {40, 170,
  225, 255}, {40, 170, 225, 255}, {40, 170, 225, 255}, {40, 170, 225, 255}, {40, 170, 225, 255}, {40, 170, 225, 255},
  {40, 170, 225, 255}, {40, 170, 225, 255}, {40, 170, 225, 252}, {40, 170, 225, 175}, {40, 170, 225, 206}, {40, 170,
  225, 117}}, {{0, 0, 0, 0}, {40, 170, 225, 201}, {40, 170, 225, 255}, {40, 170, 225, 255}, {40, 170, 225, 255}, {40,
  170, 225, 213}, {40, 170, 225, 61}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0,
  0}, {0, 0, 0, 0}, {40, 170, 225, 189}, {40, 170, 225, 255}, {40, 170, 225, 255}, {40, 170, 225, 255}, {40, 170, 225,
  255}, {40, 170, 225, 255}, {40, 170, 225, 255}, {40, 170, 225, 255}, {40, 170, 225, 255}, {40, 170, 225, 255}, {40,
  170, 225, 255}, {40, 170, 225, 255}, {40, 170, 225, 255}, {40, 170, 225, 139}, {0, 0, 0, 0}}, {{0, 0, 0, 0}, {40,
  170, 225, 153}, {40, 170, 225, 255}, {40, 170, 225, 255}, {40, 170, 225, 255}, {40, 170, 225, 255}, {40, 170, 225,
  255}, {40, 170, 225, 177}, {40, 170, 225, 60}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0},
  {40, 170, 225, 221}, {40, 170, 225, 255}, {40, 170, 225, 255}, {40, 170, 225, 255}, {40, 170, 225, 255}, {40, 170,
  225, 255}, {40, 170, 225, 255}, {40, 170, 225, 255}, {40, 170, 225, 255}, {40, 170, 225, 255}, {40, 170, 225, 255},
  {40, 170, 225, 255}, {40, 170, 225, 111}, {0, 0, 0, 0}, {0, 0, 0, 0}}, {{0, 0, 0, 0}, {40, 170, 225, 59}, {40, 170,
  225, 255}, {40, 170, 225, 255}, {40, 170, 225, 255}, {40, 170, 225, 255}, {40, 170, 225, 255}, {40, 170, 225, 255},
  {40, 170, 225, 255}, {40, 170, 225, 217}, {40, 170, 225, 138}, {40, 170, 225, 75}, {40, 170, 225, 26}, {40, 170,
  225, 1}, {40, 170, 225, 212}, {40, 170, 225, 255}, {40, 170, 225, 255}, {40, 170, 225, 255}, {40, 170, 225, 255},
  {40, 170, 225, 255}, {40, 170, 225, 255}, {40, 170, 225, 255}, {40, 170, 225, 255}, {40, 170, 225, 255}, {40, 170,
  225, 255}, {40, 170, 225, 255}, {40, 170, 225, 16}, {0, 0, 0, 0}, {0, 0, 0, 0}}, {{0, 0, 0, 0}, {0, 0, 0, 0}, {40,
  170, 225, 154}, {40, 170, 225, 255}, {40, 170, 225, 255}, {40, 170, 225, 255}, {40, 170, 225, 255}, {40, 170, 225,
  255}, {40, 170, 225, 255}, {40, 170, 225, 255}, {40, 170, 225, 255}, {40, 170, 225, 255}, {40, 170, 225, 255}, {40,
  170, 225, 251}, {40, 170, 225, 252}, {40, 170, 225, 255}, {40, 170, 225, 255}, {40, 170, 225, 255}, {40, 170, 225,
  255}, {40, 170, 225, 255}, {40, 170, 225, 255}, {40, 170, 225, 255}, {40, 170, 225, 255}, {40, 170, 225, 255}, {40,
  170, 225, 255}, {40, 170, 225, 255}, {40, 170, 225, 8}, {0, 0, 0, 0}, {0, 0, 0, 0}}, {{0, 0, 0, 0}, {40, 170, 225,
  170}, {40, 170, 225, 120}, {40, 170, 225, 215}, {40, 170, 225, 255}, {40, 170, 225, 255}, {40, 170, 225, 255}, {40,
  170, 225, 255}, {40, 170, 225, 255}, {40, 170, 225, 255}, {40, 170, 225, 255}, {40, 170, 225, 255}, {40, 170, 225,
  255}, {40, 170, 225, 255}, {40, 170, 225, 255}, {40, 170, 225, 255}, {40, 170, 225, 255}, {40, 170, 225, 255}, {40,
  170, 225, 255}, {40, 170, 225, 255}, {40, 170, 225, 255}, {40, 170, 225, 255}, {40, 170, 225, 255}, {40, 170, 225,
  255}, {40, 170, 225, 255}, {40, 170, 225, 239}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}}, {{0, 0, 0, 0}, {40, 170,
  225, 177}, {40, 170, 225, 255}, {40, 170, 225, 255}, {40, 170, 225, 255}, {40, 170, 225, 255}, {40, 170, 225, 255},
  {40, 170, 225, 255}, {40, 170, 225, 255}, {40, 170, 225, 255}, {40, 170, 225, 255}, {40, 170, 225, 255}, {40, 170,
  225, 255}, {40, 170, 225, 255}, {40, 170, 225, 255}, {40, 170, 225, 255}, {40, 170, 225, 255}, {40, 170, 225, 255},
  {40, 170, 225, 255}, {40, 170, 225, 255}, {40, 170, 225, 255}, {40, 170, 225, 255}, {40, 170, 225, 255}, {40, 170,
  225, 255}, {40, 170, 225, 255}, {40, 170, 225, 198}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}}, {{0, 0, 0, 0}, {40,
  170, 225, 91}, {40, 170, 225, 255}, {40, 170, 225, 255}, {40, 170, 225, 255}, {40, 170, 225, 255}, {40, 170, 225,
  255}, {40, 170, 225, 255}, {40, 170, 225, 255}, {40, 170, 225, 255}, {40, 170, 225, 255}, {40, 170, 225, 255}, {40,
  170, 225, 255}, {40, 170, 225, 255}, {40, 170, 225, 255}, {40, 170, 225, 255}, {40, 170, 225, 255}, {40, 170, 225,
  255}, {40, 170, 225, 255}, {40, 170, 225, 255}, {40, 170, 225, 255}, {40, 170, 225, 255}, {40, 170, 225, 255}, {40,
  170, 225, 255}, {40, 170, 225, 255}, {40, 170, 225, 140}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}}, {{0, 0, 0, 0},
  {40, 170, 225, 3}, {40, 170, 225, 197}, {40, 170, 225, 255}, {40, 170, 225, 255}, {40, 170, 225, 255}, {40, 170,
  225, 255}, {40, 170, 225, 255}, {40, 170, 225, 255}, {40, 170, 225, 255}, {40, 170, 225, 255}, {40, 170, 225, 255},
  {40, 170, 225, 255}, {40, 170, 225, 255}, {40, 170, 225, 255}, {40, 170, 225, 255}, {40, 170, 225, 255}, {40, 170,
  225, 255}, {40, 170, 225, 255}, {40, 170, 225, 255}, {40, 170, 225, 255}, {40, 170, 225, 255}, {40, 170, 225, 255},
  {40, 170, 225, 255}, {40, 170, 225, 255}, {40, 170, 225, 67}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}}, {{0, 0, 0,
  0}, {0, 0, 0, 0}, {40, 170, 225, 21}, {40, 170, 225, 201}, {40, 170, 225, 255}, {40, 170, 225, 255}, {40, 170, 225,
  255}, {40, 170, 225, 255}, {40, 170, 225, 255}, {40, 170, 225, 255}, {40, 170, 225, 255}, {40, 170, 225, 255}, {40,
  170, 225, 255}, {40, 170, 225, 255}, {40, 170, 225, 255}, {40, 170, 225, 255}, {40, 170, 225, 255}, {40, 170, 225,
  255}, {40, 170, 225, 255}, {40, 170, 225, 255}, {40, 170, 225, 255}, {40, 170, 225, 255}, {40, 170, 225, 255}, {40,
  170, 225, 255}, {40, 170, 225, 226}, {40, 170, 225, 3}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}}, {{0, 0, 0, 0},
  {0, 0, 0, 0}, {0, 0, 0, 0}, {40, 170, 225, 12}, {40, 170, 225, 104}, {40, 170, 225, 217}, {40, 170, 225, 255}, {40,
  170, 225, 255}, {40, 170, 225, 255}, {40, 170, 225, 255}, {40, 170, 225, 255}, {40, 170, 225, 255}, {40, 170, 225,
  255}, {40, 170, 225, 255}, {40, 170, 225, 255}, {40, 170, 225, 255}, {40, 170, 225, 255}, {40, 170, 225, 255}, {40,
  170, 225, 255}, {40, 170, 225, 255}, {40, 170, 225, 255}, {40, 170, 225, 255}, {40, 170, 225, 255}, {40, 170, 225,
  255}, {40, 170, 225, 118}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}}, {{0, 0, 0, 0}, {0, 0, 0, 0}, {0,
  0, 0, 0}, {40, 170, 225, 144}, {40, 170, 225, 255}, {40, 170, 225, 255}, {40, 170, 225, 255}, {40, 170, 225, 255},
  {40, 170, 225, 255}, {40, 170, 225, 255}, {40, 170, 225, 255}, {40, 170, 225, 255}, {40, 170, 225, 255}, {40, 170,
  225, 255}, {40, 170, 225, 255}, {40, 170, 225, 255}, {40, 170, 225, 255}, {40, 170, 225, 255}, {40, 170, 225, 255},
  {40, 170, 225, 255}, {40, 170, 225, 255}, {40, 170, 225, 255}, {40, 170, 225, 255}, {40, 170, 225, 231}, {40, 170,
  225, 11}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}}, {{0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {40,
  170, 225, 21}, {40, 170, 225, 232}, {40, 170, 225, 255}, {40, 170, 225, 255}, {40, 170, 225, 255}, {40, 170, 225,
  255}, {40, 170, 225, 255}, {40, 170, 225, 255}, {40, 170, 225, 255}, {40, 170, 225, 255}, {40, 170, 225, 255}, {40,
  170, 225, 255}, {40, 170, 225, 255}, {40, 170, 225, 255}, {40, 170, 225, 255}, {40, 170, 225, 255}, {40, 170, 225,
  255}, {40, 170, 225, 255}, {40, 170, 225, 255}, {40, 170, 225, 255}, {40, 170, 225, 89}, {0, 0, 0, 0}, {0, 0, 0, 0},
  {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}}, {{0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {40, 170, 225,
  48}, {40, 170, 225, 230}, {40, 170, 225, 255}, {40, 170, 225, 255}, {40, 170, 225, 255}, {40, 170, 225, 255}, {40,
  170, 225, 255}, {40, 170, 225, 255}, {40, 170, 225, 255}, {40, 170, 225, 255}, {40, 170, 225, 255}, {40, 170, 225,
  255}, {40, 170, 225, 255}, {40, 170, 225, 255}, {40, 170, 225, 255}, {40, 170, 225, 255}, {40, 170, 225, 255}, {40,
  170, 225, 255}, {40, 170, 225, 165}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0,
  0}}, {{0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {40, 170, 225, 16}, {40, 170, 225, 131},
  {40, 170, 225, 216}, {40, 170, 225, 252}, {40, 170, 225, 255}, {40, 170, 225, 255}, {40, 170, 225, 255}, {40, 170,
  225, 255}, {40, 170, 225, 255}, {40, 170, 225, 255}, {40, 170, 225, 255}, {40, 170, 225, 255}, {40, 170, 225, 255},
  {40, 170, 225, 255}, {40, 170, 225, 255}, {40, 170, 225, 255}, {40, 170, 225, 201}, {40, 170, 225, 10}, {0, 0, 0,
  0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}}, {{0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0,
  0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {40, 170, 225, 59}, {40, 170, 225, 215}, {40, 170, 225,
  255}, {40, 170, 225, 255}, {40, 170, 225, 255}, {40, 170, 225, 255}, {40, 170, 225, 255}, {40, 170, 225, 255}, {40,
  170, 225, 255}, {40, 170, 225, 255}, {40, 170, 225, 255}, {40, 170, 225, 255}, {40, 170, 225, 255}, {40, 170, 225,
  201}, {40, 170, 225, 15}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0,
  0, 0}}, {{0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {40, 170, 225, 8}, {40, 170, 225, 74}, {40, 170,
  225, 183}, {40, 170, 225, 255}, {40, 170, 225, 255}, {40, 170, 225, 255}, {40, 170, 225, 255}, {40, 170, 225, 255},
  {40, 170, 225, 255}, {40, 170, 225, 255}, {40, 170, 225, 255}, {40, 170, 225, 255}, {40, 170, 225, 255}, {40, 170,
  225, 255}, {40, 170, 225, 255}, {40, 170, 225, 172}, {40, 170, 225, 11}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0},
  {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}}, {{40, 170, 225, 81}, {40, 170, 225, 147},
  {40, 170, 225, 167}, {40, 170, 225, 195}, {40, 170, 225, 248}, {40, 170, 225, 255}, {40, 170, 225, 255}, {40, 170,
  225, 255}, {40, 170, 225, 255}, {40, 170, 225, 255}, {40, 170, 225, 255}, {40, 170, 225, 255}, {40, 170, 225, 255},
  {40, 170, 225, 255}, {40, 170, 225, 255}, {40, 170, 225, 255}, {40, 170, 225, 255}, {40, 170, 225, 235}, {40, 170,
  225, 95}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0,
  0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}}, {{0, 0, 0, 0}, {40, 170, 225, 62}, {40, 170, 225, 179}, {40, 170, 225, 251},
  {40, 170, 225, 255}, {40, 170, 225, 255}, {40, 170, 225, 255}, {40, 170, 225, 255}, {40, 170, 225, 255}, {40, 170,
  225, 255}, {40, 170, 225, 255}, {40, 170, 225, 255}, {40, 170, 225, 255}, {40, 170, 225, 255}, {40, 170, 225, 255},
  {40, 170, 225, 224}, {40, 170, 225, 120}, {40, 170, 225, 12}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0,
  0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}}, {{0, 0, 0,
  0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {40, 170, 225, 22}, {40, 170, 225, 101}, {40, 170, 225, 156}, {40, 170, 225, 211},
  {40, 170, 225, 235}, {40, 170, 225, 253}, {40, 170, 225, 255}, {40, 170, 225, 246}, {40, 170, 225, 224}, {40, 170,
  225, 184}, {40, 170, 225, 130}, {40, 170, 225, 60}, {40, 170, 225, 2}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0,
  0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0,
  0, 0}, {0, 0, 0, 0}}}], "Byte", ColorSpace -> "RGB", Interleaving -> True];


End[] (* End Private Context *)

End[]


SetAttributes[{},{ReadProtected, Protected}];

(* Return three functions to define oauthservicedata, oauthcookeddata, oauthsendmessage  *)
{TwitterOAuth`Private`twitterdata,TwitterOAuth`Private`twittercookeddata,TwitterOAuth`Private`twittersendmessage}
