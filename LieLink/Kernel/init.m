(* Mathematica Init File *)

General::mmaversion = "LieLink is incompatible with Mathematica `1`.";

(* Check for MMA version. *)
If[System`$VersionNumber < 7.,
	Message[General::mmaversion, IntegerPart @ System`$VersionNumber];
	Abort[]
];

(* Get the version number. *)
LieLink`$LieLinkVersion = Get["LieLink`VersionNumber`"];

(* Print version info etc. *)
Print["=================================================================="];
Print["Package LieLink` version ", LieLink`$LieLinkVersion[[1]],", " ,LieLink`$LieLinkVersion[[2]]];
Print["CopyRight (C) 2013, Teake Nutma, under the General Public License."];
Print["Based on LiE, by Arjen M. Cohen, Marc van Leeuwen, and Bert Lisser."];
Print["=================================================================="];

(* Load the package. *)
Get[ "LieLink`LieLink`"]