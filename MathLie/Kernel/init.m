(* Mathematica Init File *)

General::mmaversion = "MathLie is incompatible with Mathematica `1`.";

(* Check for MMA version. *)
If[System`$VersionNumber < 7.,
	Message[General::mmaversion, IntegerPart @ System`$VersionNumber];
	Abort[]
];

Get[ "MathLie`MathLie`"]