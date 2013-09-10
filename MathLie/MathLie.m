(* Mathematica Package *)

(* MathLie: a Mathematica interface for LiE *)

(* https://github.com/teake/MathLie *)

(*************************************
 *                                   *
 *          Introduction             *
 *                                   *
 *************************************)
 
(*

	MathLie roughly works as follows:
	
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

BeginPackage["MathLie`"]


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

LieFunction::usage = 
	"LieFunction[\"func\",args] calls the LiE function func with the given \
Mathematica arguments.";

SetDefaultAlgebra::usage = 
	"SetDefaultAlgebra[\"algebra\"] sets the default Lie algebra.";

LookupLieFunction::usage =
	"LookupLieFunction[\"func\"] gives the Mathematica name of the LiE  \
function func. It returns $Failed is the LiE function doesn't exist or \
hasn't been translated to a Mathematica name yet.";

Begin["`Private`"]


(*************************************
 *                                   *
 *       Initialize variables        *
 *                                   *
 *************************************)


$LieDirectory = 
	FileNameJoin @ Join [ 
		Drop[FileNameSplit@FindFile["MathLie`"], -2],
		{ "LiE" } 
	];
$LieExecutable = 
	"Liegap.exe";

tempMathLieDir = FileNameJoin @ { $TemporaryDirectory, "MathLieInOutFiles" };
If[ !DirectoryQ @ tempMathLieDir,
	CreateDirectory @ tempMathLieDir;
];

$LieInFile 	= FileNameJoin @ { tempMathLieDir, "MathLieInFile" }
$LieOutFile = FileNameJoin @ { tempMathLieDir, "MathLieOutFile" }


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
$DefaultAlgebra = None;

(* Gives part of wrapped Lie query if the standard algebr is set. *)
DefaultAlgebraQuery[] := 
	If[
		$DefaultAlgebra =!= None,
		"setdefault(" <> $DefaultAlgebra <> ")\n",
		""
	];

(* Wraps a query for LiE such that the result is written to the out file. *)
WrapQuery[query_String] := 
	DefaultAlgebraQuery[] <> "LIE = ( "  <> query <> " )\n? LIE > " <> $LieOutFile <> "\n";

(* Trims the output of LiE to the relevant string. *)
TrimLieResult[result_String] /; StringMatchQ[result, "\nLIE:=" ~~ ___ ~~ ";"] :=
	StringTrim @ StringTake[result,{7,-2}];
(* Issue a warning if the LiE result is not of the expected form. *)
TrimLieResult[result_] := 
	(Message[LieQuery::invalid]; Abort[]);

(* Main function for querying LiE. *)
LieQuery[query_String] := 
	Module[{returncode},
		(* Check exe. *)
		CheckLieExecutableAbort[];
		(* Save the query to the in-file. *)
		Put[ 
			OutputForm @ WrapQuery @ query, 
			$LieInFile
		];
		(* Run LiE. *)
		returncode = Run @ StringJoin[
			"cd " <> $LieDirectory <> ";",
			"./" <> $LieExecutable <> " > /dev/null < " <> $LieInFile
		];
		(* Check the return code. *)
		If[ returncode =!= 0,
			Message[LieQuery::badreturn, returncode];
			Abort[];
		];
		(* Fetch result from the out-file. *)
		TrimLieResult@Import[$LieOutFile, "Text"]
	];


(*************************************
 *                                   *
 *          FromLieOutput            *
 *                                   *
 *************************************)

(* Case 1: a number. *)
FromLieOutput[s_String] /; StringMatchQ[s, NumberString] := 
	ToExpression[s];

(* Case 2: a vector / matrix / Laurent polynomial. *)
FromLieOutput[s_String] /; StringMatchQ[s, "[" ~~ ___ ~~ "]"] := 
	ToLiePolynomial@ToExpression@StringReplace[s, {"[" -> "{", "]" -> "}"}];

ToLiePolynomial[expr_] := expr;
ToLiePolynomial[list : {PatternSequence[_Integer, {___Integer}] ...}] := 
	Plus @@ ((First[#]*LieTerm @@ Last[#]) & /@ Partition[list, 2])

(* Case 3: a string. *)
FromLieOutput[s_String] /; StringMatchQ[s, "\"" ~~ ___ ~~ "\""] := 
	StringTake[s, {2, -2}];


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

(* Single LieTerm: convert to "[1,2,3,4]" *)
ToLieInput[LieTerm[args___]] := 
 "[" <> StringJoin[Riffle[ToString /@ {args}, ","]] <> "]"


(* LieTerm times an integer: convert to "1X[1,2,3,4]" *)
ToLieInput[int_Integer *  lt_LieTerm] := 
	ToString[int] <> "X" <> ToLieInput[lt];

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


LieFunction[func_String, args___] := 
	FromLieOutput @ LieQuery[func <> "(" <> Riffle[ToLieInput /@ {args}, ","] <> ")"];


SetDefaultAlgebra[string_] /; Head[string] === String || string === None := 
	$DefaultAlgebra = string
	
SetDefaultAlgebra[] := 
	$DefaultAlgebra


(* Helper function for setting short-hand Mathmetica symbols to LieFunction calls. *)
SetFunction[Rule[string_String, symbol_String]] :=
	With[
		{
			functionName 	= Symbol["MathLie`" <> symbol],
			lieName 		= string
		},
		ClearAll[symbol];
		functionName[args___] := LieFunction[lieName, args];
		functionName::usage = symbol <> " is the equivalent of the LiE function \"" <> lieName <> "\".";
	];


LookupLieFunction[string_String] := 
	If[
		string =!= (string /. LieFunctionTable),
		Symbol["MathLie`" <> (string /. LieFunctionTable)],
		$Failed
	];


(* Main dictionary between LiE functions -> Mathematica short-hand symbols. *)
LieFunctionTable = 
	{
		"dim" 			-> "Dim",
		"sym_tensor"	-> "SymTensor",
		"tensor" 		-> "LieTensor"
   };

(* Set the short-hand symbols. *)
SetFunction /@ LieFunctionTable;


End[]

EndPackage[]

