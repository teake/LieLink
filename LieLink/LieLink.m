(* Mathematica Package *)

(* LieLink: a Mathematica interface for LiE *)

(* https://github.com/teake/LieLink *)

(*************************************
 *                                   *
 *          Introduction             *
 *                                   *
 *************************************)
 
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

BeginPackage["LieLink`"]


(*************************************
 *                                   *
 *          (Usage) messages         *
 *                                   *
 *************************************)
 

$LieDirectory::usage = 
	"$LieDirectory stores the location of the LiE executable.";

$LieExecutable::usage = 
	"$LieExecutable stores the filename of the LiE executable.";

$LieInFile::usage = 
	"$LieOutFile stores the location of the temporary file used for storing \
commands for LiE.";

$LieOutFile::usage = 
	"$LieOutFile stores the location of the temporary file used for retrieving \
results from LiE.";

LieQuery::exe = 
	"The external LiE executable cannot be found. Please check if \
$LieDirectory and $LieExecutable are correct.";

LieQuery::badreturn = 
	"Expected return code 0 from LiE but got `1` instead.";

LieQuery::invalid = 
	"LiE returned an invalid result.";

LieQuery::usage = 
	"LieQuery[string] queries LiE with the input string and doesn't \
parse the output.";

FromLieOutput::usage = 
	"FromLieOutput[string] parses the LiE string into Mathematica code.";

LieTerm::usage = 
	"LieTerm is the head for terms in LiE polynomials."

$LieTermColor::usage = 
	"$LieTermColor stores the color for the parentheses of LieTerm.";

ToLieInput::usage = 
	"ToLieInput[expr] converts expr to a string suited for LiE commands.";

CallLieFunction::usage = 
	"CallLieFunction[\"func\",args] calls the LiE function func with the given \
Mathematica arguments.";

SetDefaultAlgebra::usage = 
	"SetDefaultAlgebra[\"algebra\"] sets the default Lie algebra.";

LieFunction::usage =
	"LieFunction[\"func\"] gives the Mathematica name of the LiE  \
function func. It returns $Failed is the LiE function doesn't exist or \
hasn't been translated to a Mathematica name yet.";

LieFunctionTable::usage =
	"LieFunctionTable contains a list of LiE functions and their short-hand Mathematica \
counterparts.";

Begin["`Private`"]


(*************************************
 *                                   *
 *       Initialize variables        *
 *                                   *
 *************************************)


If[!ValueQ[$LieDirectory],
	$LieDirectory = 
		FileNameJoin @ Join [ 
			Drop[FileNameSplit@FindFile["LieLink`"], -2],
			{ "LiE" } 
		];
];

If[!ValueQ[$LieExecutable],
	$LieExecutable = 
		"Liegap.exe";
];

tempLieLinkDir = FileNameJoin @ { $TemporaryDirectory, "LieLinkInOutFiles" };
If[ !DirectoryQ @ tempLieLinkDir,
	CreateDirectory @ tempLieLinkDir;
];

$LieInFile 	= FileNameJoin @ { tempLieLinkDir, "LieLinkInFile" }
$LieOutFile = FileNameJoin @ { tempLieLinkDir, "LieLinkOutFile" }


(*************************************
 *                                   *
 *          LieQuery et. al.         *
 *                                   *
 *************************************)


(* Checks if the LiE exe is in the right folder. *)
CheckLieExecutable[] := 
	If[!FileExistsQ[ FileNameJoin @ {$LieDirectory, $LieExecutable} ],
		Message[LieQuery::exe];
		False,
		True
	];

(* Check on start up. *)
CheckLieExecutable[];

(* This checks the LiE exe and aborts if not found. *)
CheckLieExecutableAbort[] :=
	If[!CheckLieExecutable[],
		Abort[];
	];

(* Internal variable for the default algebra. *)
If[!ValueQ[$DefaultAlgebra],
	$DefaultAlgebra = None
];

(* Gives part of wrapped Lie query if the standard algebr is set. *)
AddDefaultAlgebraQuery[string_] := 
	If[
		$DefaultAlgebra =!= None,
		"setdefault(" <> $DefaultAlgebra <> ")\n",
		""
	] <> string;

(* Main function for querying LiE. *)
LieQuery[query_String] := 
	Module[{returncode},
		(* Check exe. *)
		CheckLieExecutableAbort[];
		(* Save the query to the in-file. *)
		Put[ 
			OutputForm @ AddDefaultAlgebraQuery @ query, 
			$LieInFile
		];
		(* Run LiE. *)
		returncode = Run @ StringJoin[
			"cd " <> $LieDirectory <> ";",
			"./" <> $LieExecutable <> " > " <> $LieOutFile <> " < " <> $LieInFile
		];
		(* Check the return code. *)
		If[ returncode =!= 0,
			Message[LieQuery::badreturn, returncode];
			Abort[];
		];
		(* Fetch result from the out-file. *)
		TrimLieResult @ CheckLieResult @ Import[$LieOutFile, "Text"]
	];

(* Issue a warning if the LiE result is not of the expected form. *)
CheckLieResult[result_] /; StringMatchQ[result,"(" ~~ ___ ~~ "of file stdin)"] := 
	(Message[LieQuery::invalid]; Abort[]);
CheckLieResult[result_String] :=
	result;

(* Trims the output of LiE to the relevant string. *)
TrimLieResult[result_String] /; StringMatchQ[result, "     " ~~ (nwsp_ /; !StringMatchQ[nwsp, Whitespace]) ~~ ___] := 
	StringTrim @ StringTake[result, {6, -1}];
TrimLieResult[result_String] :=
	StringTrim[result, "\n"];

(*************************************
 *                                   *
 *          FromLieOutput            *
 *                                   *
 *************************************)

(* Case 1: a number. *)
FromLieOutput[s_String] /; StringMatchQ[s, (Whitespace...) ~~ NumberString ~~ (Whitespace...)] := 
	ToExpression @ StringTrim @ s;

(* Case 2: a vector / matrix / Laurent polynomial. *)
FromLieOutput[s_String] /; StringMatchQ[s, (Whitespace...) ~~ "[" ~~ ___ ~~ "]" ~~ (Whitespace...)] := 
	ToLiePolynomial@ToExpression@StringReplace[StringTrim @ s, {"[" -> "{", "]" -> "}"}];

ToLiePolynomial[expr_] := expr;
ToLiePolynomial[list : {PatternSequence[_Integer, {___Integer}] ...}] := 
	Plus @@ ((First[#]*LieTerm @@ Last[#]) & /@ Partition[list, 2])

(* Case 3: a string. *)
FromLieOutput[s_String] :=
	s;



(*************************************
 *                                   *
 *              LieTerm              *
 *                                   *
 *************************************)

$LieTermColor = RGBColor[1, 0, 0];

(* Formatting. *)
MakeBoxes[LieTerm[args___], StandardForm] := 
	RowBox[
		Flatten[{
			StyleBox["{", FontColor -> $LieTermColor], 
			MakeBoxes[#, StandardForm] & /@ Riffle[{args},","], 
			StyleBox["}", FontColor -> $LieTermColor]
		}]
	];

(* Products. *)
LieTerm /: Times[LieTerm[args1___], LieTerm[args2___]] := 
	LieTerm @@ ({args1} + {args2})

(* Powers. *)
LieTerm /: Power[LieTerm[args___], n_Integer] := 
	LieTerm @@ (n {args})


(*************************************
 *                                   *
 *           ToLieInput              *
 *                                   *
 *************************************)

(* LieTerm times an integer: convert to "1X[1,2,3,4]" *)
ToLieInput[ Optional[int_Integer] * LieTerm[args___] ] := 
	ToString[int] <> "X[" <> StringJoin[Riffle[ToString /@ {args}, ","]] <> "]";

(* A sum. Take special care of minus / plus signs. *)
ToLieInput[sum_Plus] := 
	StringJoin[
		Join[{First[#]}, AddPlus /@ Rest[#]] &@(ToLieInput /@ List @@ sum)
	];

AddPlus[s_String] := 
	"+" <> s;
AddPlus[s_String] /; StringTake[s, 1] === "-" := 
	s;


(* A vector: convert to "[1,2,3,4]" *)
ToLieInput[list_List] := 
	"[" <> StringJoin[Riffle[ToLieInput /@ list, ","]] <> "]";

(* A string: don't do anything. *)	
ToLieInput[string_String] := 
	string;

(* Generic case: convert to string. *)
ToLieInput[expr_] := 
	ToString[expr];


(*************************************
 *                                   *
 *            Lie functions          *
 *                                   *
 *************************************)


CallLieFunction[func_String, args___] := 
	FromLieOutput @ LieQuery[func <> "(" <> Riffle[ToLieInput /@ {args}, ","] <> ")"];


SetDefaultAlgebra[string_] /; Head[string] === String || string === None := 
	$DefaultAlgebra = string
	
SetDefaultAlgebra[] := 
	$DefaultAlgebra


(* Helper function for setting short-hand Mathmetica symbols to CallLieFunction calls. *)
SetFunction[Rule[string_String, symbol_String]] :=
	With[
		{
			functionName 	= Symbol["LieLink`" <> symbol],
			symbolName		= symbol,
			lieName 		= string
		},
		(* Give a warning message if the new defintion shadows other symbols. *)
		If[Context[symbolName] =!= "LieLink`Private`" && Context[symbolName] =!= "LieLink`",
			Message[symbolName::shdw, symbolName, {Context[symbolName],"LieLink`"},"LieLink`"];
		];
		(* Make the definition. *)
		functionName[args___] := CallLieFunction[lieName, args];
		(* Set the usage message. *)
		functionName::usage = symbol <> " is the equivalent of the LiE function \"" <> lieName <> "\".";
	];


LieFunction[string_String] := 
	If[
		string =!= (string /. LieFunctionTable),
		Symbol["LieLink`" <> (string /. LieFunctionTable)],
		$Failed
	];


(* Main dictionary between LiE functions -> Mathematica short-hand symbols. *)
LieFunctionTable = 
	{
		"Adams" 		-> "Adams",
		"adjoint" 		-> "AdjointRepresentation",
		"alt_dom" 		-> "AlternatingDominant",
		"alt_tensor" 	-> "AlternatingTensorPower",
		"alt_W_sum" 	-> "AlternatingWeylSum",
		"block_mat" 	-> "BlockdiagonalMatrix",
		"branch" 		-> "Branch",
		"Bruhat_desc" 	-> "BruhatDescendant",
		"Bruhat_leq" 	-> "BruhatLeq",
		"canonical" 	-> "CanonicalWeylWord",
		"Cartan"		-> "CartanMatrix",
		"Cartan" 		-> "CartanProduct",
		"Cartan_type" 	-> "CartanType",
		"center" 		-> "GroupCenter",
		"cent_roots" 	-> "CentralizingRoots",
		"centr_type" 	-> "CentralizerType",
		"class_ord" 	-> "ConjugacyClassOrder",
		"closure" 		-> "Closure",
		"collect" 		-> "InverseBranch",
		"contragr" 		-> "Contragradient",
		"decomp" 		-> "Decomposition",
		"degree" 		-> "PolynomialDegree",
		"Demazure" 		-> "Demazure",
		"det_Cartan" 	-> "DetCartan",
		"diagram"		-> "DynkinDiagram",
		"dim" 			-> "Dim",
		"dom_char" 		-> "DominantCharacter",
		"dominant" 		-> "Dominant",
		"dom_weights" 	-> "DominantWeights",
		"exponents" 	-> "Exponents",
		"filter_dom" 	-> "FilterDominant",
		"from_part" 	-> "FromPartition",
		"fundam" 		-> "FundamentalRoots",
		"high_root" 	-> "HighestRoot",
		"i_Cartan" 		-> "InverseCartan",
		"inprod" 		-> "InnerProduct",
		"KL_poly" 		-> "KazhdanLusztigPolynomial",
		"Lie_code" 		-> "LieCode",
		"Lie_group" 	-> "LieGroup",
		"Lie_rank" 		-> "LieRank",
		"long_word" 	-> "LongestWord",
		"l_reduce" 		-> "LeftWeylReduce",
		"lr_reduce" 	-> "LeftRightWeylReduce",
		"LR_tensor" 	-> "LittlewoodRichardson",
		"max_sub" 		-> "MaximalSubgroups",
		"n_comp" 		-> "NumberOfSimpleComponents",
		"next_part" 	-> "NextPartition",
		"next_perm" 	-> "NextPermutation",
		"next_tabl" 	-> "NextTableau",
		"norm" 			-> "RootNorm",
		"n_tabl" 		-> "NumberOfTableaux",
		"n_vars" 		-> "NumberOfVariables",
		"orbit" 		-> "GroupOrbit",
		"plethysm" 		-> "Plethysm",
		"pos_roots" 	-> "PositiveRoots",
		"p_tensor" 		-> "TensorPower",
		"print_tab"		-> "PrintTableau",
		"reduce" 		-> "WeylReduce",
		"reflection" 	-> "Reflection",
		"res_mat" 		-> "RestrictionMatrix",
		"row_index" 	-> "RowIndex",
		"R_poly" 		-> "RPolynomial",
		"r_reduce" 		-> "RightWeylReduce",
		"RS" 			-> "RobinsonSchensted",
		"shape" 		-> "TableauShape",
		"sign_part" 	-> "PartitionSign",
		"spectrum" 		-> "ToralSpectrum",
		"support" 		-> "Support",
		"sym_char" 		-> "SymmetricCharacter",
		"sym_orbit" 	-> "SymmetricOrbit",
		"sym_tensor" 	-> "SymmetricTensorPower",
		"tableaux" 		-> "TableauxOfPartition",
		"tensor" 		-> "LieTensor",
		"to_part"	 	-> "ToPartition",
		"trans_part" 	-> "TransposePartition",
		"unique" 		-> "CanonicalMatrix",
		"v_decomp" 		-> "VirtualDecomposition",
		"W_action" 		-> "WeylAction",
		"W_orbit" 		-> "WeylOrbit",
		"W_orbit_graph"	-> "WeylOrbitGraph",
		"W_orbit_size" 	-> "WeylOrbitSize",
		"W_order" 		-> "WeylOrder",
		"W_rt_action" 	-> "WeylRootAction",
		"W_rt_orbit" 	-> "WeylRootOrbit",
		"W_word" 		-> "WeylWord"
	};

UnsupportedLieFunctionTable =
	{
		"all_one",
		"coef",
		"diag",
		"expon",
		"factor",
		"gcd",
		"id",
		"length",
		"mat_vec",
		"n_cols",
		"n_pos_roots",
		"r_rows",
		"null",
		"partitions",
		"poly_null",
		"poly_one",
		"polynom",
		"size",
		"sort",
		"used",
		"vec_mat"
	};

(* Set the short-hand symbols. *)
SetFunction /@ LieFunctionTable;


End[]

EndPackage[]

