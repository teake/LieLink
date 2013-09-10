(* Mathematica Package *)

(* Created by the Wolfram Workbench Sep 9, 2013 *)

BeginPackage["MathLie`"]



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

$LieDirectory = 
	FileNameJoin @ Join [ 
		Drop[FileNameSplit@FindFile["MathLie`"], -2],
		{ "LiE" } 
	];
$LieExecutable = 
	"Liegap.exe";

$LieInFile 	= CreateTemporary[];
$LieOutFile = CreateTemporary[];


CheckLieExecutable[] := 
	If[!FileExistsQ[ FileNameJoin @ {$LieDirectory, $LieExecutable} ],
		Message[LieQuery::exe];
		False,
		True
	];

CheckLieExecutable[];

CheckLieExecutableAbort[] :=
	If[!CheckLieExecutable[],
		Abort[];
	];


$DefaultAlgebra = None;

DefaultAlgebraQuery[] := 
	If[
		$DefaultAlgebra =!= None,
		"setdefault(" <> $DefaultAlgebra <> ")\n",
		""
	];

WrapQuery[query_String] := 
	DefaultAlgebraQuery[] <> "LIE = ( "  <> query <> " )\n? LIE > " <> $LieOutFile <> "\n";


TrimLieResult[result_String] /; StringMatchQ[result, "\nLIE:=" ~~ ___ ~~ ";"] :=
	StringTrim @ StringTake[result,{7,-2}];

TrimLieResult[result_] := 
	(Message[LieQuery::invalid]; Abort[]);


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


FromLieOutput[s_String] /; StringMatchQ[s, NumberString] := 
	ToExpression[s];
	
FromLieOutput[s_String] /; StringMatchQ[s, "[" ~~ ___ ~~ "]"] := 
	ToLiePolynomial@ToExpression@StringReplace[s, {"[" -> "{", "]" -> "}"}];
	
FromLieOutput[s_String] /; StringMatchQ[s, "\"" ~~ ___ ~~ "\""] := 
	StringTake[s, {2, -2}];


ToLiePolynomial[expr_] := expr;
ToLiePolynomial[list : {PatternSequence[_Integer, {___Integer}] ...}] := 
	Plus @@ ((First[#]*LieTerm @@ Last[#]) & /@ Partition[list, 2])


$LieTermColor = RGBColor[1, 0, 0];

MakeBoxes[LieTerm[args___], StandardForm] := 
	RowBox[
		Flatten[{
			StyleBox["{", FontColor -> $LieTermColor], 
			MakeBoxes[#, StandardForm] & /@ Riffle[{args},","], 
			StyleBox["}", FontColor -> $LieTermColor]
		}]
	];

LieTerm /: Times[LieTerm[args1___], LieTerm[args2___]] := 
	LieTerm @@ ({args1} + {args2})

LieTerm /: Power[LieTerm[args___], n_Integer] := 
	LieTerm @@ (n {args})


ToLieInput[int_Integer *  lt_LieTerm] := 
	ToString[int] <> "X" <> ToLieInput[lt];

ToLieInput[sum_Plus] := 
	StringJoin[
		Join[{First[#]}, AddPlus /@ Rest[#]] &@(ToLieInput /@ List @@ sum)
	];

AddPlus[s_String] := 
	"+" <> s;
AddPlus[s_String] /; StringTake[s, 1] === "-" := 
	s;

ToLieInput[LieTerm[args___]] := 
 "[" <> StringJoin[Riffle[ToString /@ {args}, ","]] <> "]"

ToLieInput[list_List] := 
	"[" <> StringJoin[Riffle[ToLieInput /@ list, ","]] <> "]";

ToLieInput[expr_] := 
	ToString[expr];
	
ToLieInput[string_String] := 
	string;


LieFunction[func_String, args___] := 
	FromLieOutput @ LieQuery[func <> "(" <> Riffle[ToLieInput /@ {args}, ","] <> ")"];


SetDefaultAlgebra[string_] /; Head[string] === String || string === None := 
	$DefaultAlgebra = string
	
SetDefaultAlgebra[] := 
	$DefaultAlgebra


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


LieFunctionTable = 
	{
		"dim" 			-> "Dim",
		"sym_tensor"	-> "SymTensor",
		"tensor" 		-> "LieTensor"
   };

SetFunction /@ LieFunctionTable;


End[]

EndPackage[]

