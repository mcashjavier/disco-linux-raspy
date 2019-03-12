(* ::Package:: *)

(* Paclet Info File *)

(* created 2015/06/05*)

Paclet[
    Name -> "RaspberryPiTools",
    Version -> "1.2.0",
    MathematicaVersion -> "10+",
    Creator -> "Brett Haines <bhaines@wolfram.com>",
    Loading -> Automatic,
    Extensions -> {
		{"LibraryLink",
			SystemID->"Linux-ARM"
		},
        {"Kernel", 
			Root->"Kernel", 
			Context->{"RaspberryPiTools`"},
			Symbols->{
				"System`CurrentImage"
			}
		},
        {"Documentation",
			MainPage->"ReferencePages/Devices/SenseHAT", 
			Language->"English"
		},
        {"Resource",
			Root->"Resources", 
			Resources->{
				{"TextImage", "Bitmap/astro_pi_text.png"}
			}
		}
    }
]
