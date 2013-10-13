(* Mathematica package *)

(* LieLink: a Mathematica interface for LiE *)

(* Extensions file. *) 

Begin["`Private`"]



(*************************************
 *                                   *
 *    Extra restriction matrices     *
 *                                   *
 *************************************)

(*
 * This is a list of lists, each of the form
 * {"subgroup type", "group type", rank test, restriction matrix}.
 * The first two entries are strings, the last two are pure functions.
 *)
restrictionMatrixList = 
	{
		(* The maximal D_n subgroup of A_(2n-1) *)
		{
			"D", 
			"A", 
			(2 #1 === #2 + 1 && #2 > 8) &, 
			Table[
				KroneckerDelta[i, j] + KroneckerDelta[i, 2 # - j] + 
				KroneckerDelta[j, #] KroneckerDelta[i, # - 1] + 
				KroneckerDelta[j, #] KroneckerDelta[i, # + 1], 
				{i, 2 # - 1}, {j, #}
			]&
		},
		(* The maximal B_n subgroup of A_(2n) *)
		{
			"B", 
			"A", 
			(2 #1 === #2 && #2 > 5) &, 
			Table[
				KroneckerDelta[i, j] + KroneckerDelta[i, 2 # + 1 - j] + 
				KroneckerDelta[j, #] KroneckerDelta[i, #] + 
				KroneckerDelta[j, #] KroneckerDelta[i, # + 1], 
				{i, 2 #}, {j, #}
			]&
		}
	};

(* Function that takes an element of the list above, and sets the
   RestrictionMatrix downvalue. *)
SetRestrictionMatrix[{h_, g_, ranktest_, matrix_}] := 
	With[
		{
			H 	= h, 
			G 	= g, 
			M 	= matrix,
			RT 	= ranktest
		},
		RestrictionMatrix[hstring_String, gstring_String] /; 
			StringMatchQ[hstring, H ~~ NumberString] && 
			StringMatchQ[gstring, G ~~ NumberString] && 
			RT[
				ToExpression@StringDrop[hstring, 1], 
				ToExpression@StringDrop[gstring, 1]
		] := 
 			M[ ToExpression@StringDrop[hstring, 1] ]
	];

(* Map over the list to set the downvalues. *)
SetRestrictionMatrix /@ restrictionMatrixList;

End[]