(* Paclet Info File *)

(* created 2015/11/20*)

Paclet[
    Name -> "ServiceConnection_Twilio",
    Version -> "1.0.6",
    MathematicaVersion -> "10.2+",
    Loading -> Automatic,
    Extensions -> 
        {
            {"Kernel", Symbols -> 
                {"System`$PhoneNumber", "System`$SMSDestination"}
            , Root -> "Kernel", Context -> 
                {"Twilio`", "TwilioFunctions`", "TwilioLoad`"}
            }, 
            {"Documentation", MainPage -> "ReferencePages/Symbols/Twilio", Language -> All}
        }
]


