(* Mathematica Init File *)

(* LieLink: a Mathematica interface for LiE *)

(* Init file. *) 

(* https://github.com/teake/LieLink *)
 
(*

	LieLink roughly works as follows:
	
	1. Convert input query to LiE syntax.
	2. Write out a temporary file with the converted query to the filesystem.
	3. Run LiE on this file, and have LiE output its results to another
	   temporary file.
	4. Read in the second temporary file.
	5. Convert the result to Mathematica syntax.
	
	This workflow allows us to use an unmodified version of LiE (to be precise,
	the GAP-build of LiE, which can be build with "make Liegap.exe").
	
	A better approach would be to enable MathLink communication between LiE
	and Mathematica. But this would require significantly more work than this
	simple wrapper package.

*)


General::mmaversion = "LieLink is incompatible with Mathematica `1`.";

(* Check for MMA version. *)
If[System`$VersionNumber < 7.,
	Message[General::mmaversion, IntegerPart @ System`$VersionNumber];
	Abort[]
];

(* Get the version number. *)
LieLink`$LieLinkVersion = Get["LieLink`VersionNumber`"];

(* Print version info etc. *)
Print["================================================================="];
Print["Package LieLink` version ", LieLink`$LieLinkVersion[[1]],", " ,LieLink`$LieLinkVersion[[2]]];
Print["CopyRight (C) 2013, Teake Nutma, under the General Public License."];
Print["Based upon LiE version 2.2.2,"]; 
Print["CopyRight (C) 1992-2002, Arjeh M. Cohen, Marc van Leeuwen, "];
Print["and Bert Lisser, under the Lesser General Public License."];
Print["================================================================="];



BeginPackage["LieLink`"]


(* Load the package. *)
Get[ "LieLink`LieLink`"];
Get[ "LieLink`Extensions`"];

EndPackage[]