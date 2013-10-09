(* Mathematica Package *)

(* LieLink: a Mathematica interface for LiE *)

(* Core file. *) 


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
			{ "LiE", $OperatingSystem } 
		];
	If[ $OperatingSystem === "Unix",
		$LieDirectory = $LieDirectory <>
			If[ StringMatchQ[$Version, "*64-bit*"],
				"64",
				"32"
			];
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
		Export[
			$LieInFile,
			AddDefaultAlgebraQuery @ query, 
			"Text"
		];
		(* Run LiE. *)
		returncode = Run @ StringJoin[
			"cd \"" <> $LieDirectory <> "\"",
			If[$OperatingSystem === "Windows",
				"&",
				"; chmod u+x " <> $LieExecutable <> "; ./"
			],
			$LieExecutable <> " > \"" <> $LieOutFile <> "\" < \"" <> $LieInFile <> "\""
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


(* Helper function for setting short-hand Mathematica symbols to CallLieFunction calls. *)
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
		functionName::usage = symbol <> " is the LieLink equivalent of the LiE function \"" 
			<> lieName <> "\", whose description is as follows:\n\n" 
			<> (lieName /. LieFunctionMessages)
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
	
LieFunctionMessages =
	{
		"Adams" 		-> "Adams(int,vec,grp)-> pol\nAdams(n,lambda,g) [lambda: weight, result: decomposition].   Returns the decomposition polynomial of the virtual module obtained by applying the n-th Adams operator to V_lambda. The result is the same as that computed by `v_decomp(dom_char(lambda,g)*n,g)'. This function is used in plethysm, sym_tensor, and alt_tensor.\n\nAdams(int,pol,grp)-> pol\nAdams(n,p,g) [p: decomposition, result: decomposition].   This is like Adams(n,lambda,g), but with the irreducible module V_lambda replaced by the module with decomposition polynomial p.",	
		"adjoint" 		-> "adjoint(grp)-> pol\nadjoint(g) [result: decomposition].   Returns the decomposition polynomial of the adjoint representation of g. For simple groups the adjoint representation is irreducible and the result therefore has a single term; the highest weight of the adjoint representation can then be obtained as expon(adjoint(g),1). Since the non-zero weights of the adjoint representation are precisely the roots, this highest weight is equal to high_root(g)*Cartan(g).",	
		"alt_dom" 		-> "alt_dom(pol,grp)-> pol\nalt_dom(p,g) [p,result: weights].   This is equivalent to (but somewhat faster than) `alt_dom(p,long_word(g),g)'. The resulting polynomial q can be charaterised as the unique polynomial with only dominant exponents which has alt_W_sum(q)==alt_W_sum(p). If p is a character polynomial, then q is the corresponding decomposition polynomial.\n\nalt_dom(vec,grp)-> pol\nalt_dom(lambda,g) [lambda: weight, result: weights].   Returns `alt_dom(X lambda,g)'.\n\nalt_dom(pol,vec,grp)-> pol\nalt_dom(p,w,g)  [p,result: weights, w: Weyl word].   (alternating dominant) Starting with the polynomial p, the following operation is repeatedly applied, taking for i the successive entries of the Weyl word w, reading from left to right.  For any term `n X^lambda' let lambda[i]=<lambda,alpha_i> be its coefficient of omega_i; the term is unaltered if lambda[i] >= 0, it is removed if lambda[i] = -1, and it is replaced by `n X(W_action(lambda+omega_i,[i])-omega_i)' if lambda[i] <= -2. (The exponent of the latter monomial could also have been written as `W_action(lambda,[i])-alpha_i' or as `lambda-(lambda[i]+1)*alpha_i'.) As a result of the operation for i, the coefficient lambda[i] is made non-negative without affecting the image Demazure(p,[i]) under the Demazure operator, and hence also without changing alt_W_sum(p). The final result of alt_dom should be the same when taking for w different reduced Weyl words for the same element of W .\n\nalt_dom(vec,vec,grp)-> pol\nalt_dom(lambda,w,g) [lambda: weight, w: Weyl word, result: weights]. Returns `alt_dom(X lambda,w,g)'.",	
		"alt_tensor" 	-> "alt_tensor(int,vec,grp)-> pol\nalt_tensor(n,lambda,g) [lambda: weight, result: decomposition]. (alternating tensor power)  Returns the decomposition polynomial of the n-th alternating tensor power (also called n-th exterior power) of V_lambda. See also sym_tensor and plethysm.\n\nalt_tensor(int,pol,grp)-> pol\nalt_tensor(n,p,g) [p,result: decomposition].   This is similar to alt_tensor(n,lambda,g), but with the irreducible module V_lambda replaced by the module with decomposition polynomial p.",	
		"alt_W_sum" 	-> "alt_W_sum(pol,grp)-> pol\nalt_W_sum(p,g) [p,result: weights].   (alternating Weyl sum) Returns the alternating Weyl sum J(p) of p, defined by\n     sum_{w in W} (-1)^length(w) W_action(X rho * p,w)*X(-rho)\n where rho=all_one(Lie_rank) (the half sum of the positive roots). The number of terms generated is a multiple of W_order(g), so it may not be wise to call this function if W_order(g) is very large.\n\nalt_W_sum(vec,grp)-> pol\nalt_W_sum(lambda,g) [lambda: weight, result: weights].  Returns `alt_W_sum(X lambda,g)'.",	
		"block_mat"		-> "block_mat(mat,mat)-> mat",	
		"branch" 		-> "branch(vec,grp,mat,grp)-> pol\nbranch(lambda,h,m,g) [lambda: weight, m: lin(weight,weight), result: decomposition].   Returns the decomposition polynomial of the restriction to h of V_lambda, with respect to the restriction matrix m. Here the matrix m is such that any weight mu (expressed on the basis of fundamental weights for g), when restricted to the maximal torus of h becomes the weight mu*m (expressed on the basis of fundamental weights for h). In many cases the restriction matrix can be obtained by use of res_mat. See also decomp for a warning for in case memory overflow should occur during branch.\n\nbranch(pol,grp,mat,grp)-> pol\nbranch(p,h,m,g) [p,result: decomposition, m: lin(weight,weight)].   This is like branch(lambda,h,m,g), but with the irreducible module V_lambda replaced by the module with decomposition polynomial p.",	
		"Bruhat_desc" 	-> "Bruhat_desc(vec,grp)-> mat\nBruhat_desc(w,g) [w: Weyl word, result: Weyl words]. Returns the set of Bruhat descendents of w, each one represented by a reduced Weyl word. The Weyl word chosen for a Bruhat descendent is the unique one which is obtainable by omitting one of the fundamental reflections occurring in the Weyl word reduce(w).\n\nBruhat_desc(vec,vec,grp)-> mat\nBruhat_desc(v,w,g) [v,w: Weyl word, result: Weyl words].   Returns the set of Bruhat descendents w' of w which lie above v in the Bruhat ordering. This is useful in generating all elements between v and w in the Bruhat ordering.",	
		"Bruhat_leq" 	-> "Bruhat_leq(vec,vec,grp)-> int\nBruhat_leq(v,w,g) [v,w: Weyl word].   Returns the value 1 if v<=w in the Bruhat order, and 0 otherwise.",	
		"canonical" 	-> "canonical(vec,grp)-> vec\ncanonical(w,g) [v,result: Weyl word].   Returns the canonical Weyl word representing the same Weyl group element as w, which is the lexicographically first reduced expression for that element.\n\ncanonical(mat,grp)-> mat\ncanonical(m,g) [m,result: Weyl words].   Returns the matrix obtained by replacing each row w by canonical(w,g), filling out the row with zeros if necessary. This is useful in combination with unique when handling sets of Weyl words.",	
		"Cartan" 		-> "Cartan(vec,vec,grp)-> int\nCartan(alpha,beta,g) [alpha,beta: root].   Returns the `Cartan product' <alpha,beta>, i.e., the integral value 2(alpha,beta)=(beta,beta), where beta must be a root, and alpha is any root vector. [This is is not an inner product because the function is not linear in beta. The function is linear in alpha however. See also inprod and norm.\n\nCartan(grp)-> mat\nCartan(g) [result: lin(root; weight)].   Returns the Cartan matrix of g, which is the transformation matrix from the root lattice to the weight lattice, using the bases of fundamental roots and fundamental weights respectively. Hence the i-th row of the Cartan matrix equals the i-th fundamental root, expressed as weight vector. The labeling of the fundamental roots is as indicated by diagram(g). When g is semisimple, the (i,j)-entry of the Cartan matrix is <alpha_i,alpha_j>. If g contains a central torus, so that the semisimple rank s of g is differs from the Lie rank r, then the Cartan matrix is not square, as it is an s x r matrix, but all entries beyond column s are zero.",	
		"Cartan" 		-> "Cartan(vec,vec,grp)-> int\nCartan(alpha,beta,g) [alpha,beta: root].   Returns the `Cartan product' <alpha,beta>, i.e., the integral value 2(alpha,beta)=(beta,beta), where beta must be a root, and alpha is any root vector. [This is is not an inner product because the function is not linear in beta. The function is linear in alpha however. See also inprod and norm.\n\nCartan(grp)-> mat\nCartan(g) [result: lin(root; weight)].   Returns the Cartan matrix of g, which is the transformation matrix from the root lattice to the weight lattice, using the bases of fundamental roots and fundamental weights respectively. Hence the i-th row of the Cartan matrix equals the i-th fundamental root, expressed as weight vector. The labeling of the fundamental roots is as indicated by diagram(g). When g is semisimple, the (i,j)-entry of the Cartan matrix is <alpha_i,alpha_j>. If g contains a central torus, so that the semisimple rank s of g is differs from the Lie rank r, then the Cartan matrix is not square, as it is an s x r matrix, but all entries beyond column s are zero.",	
		"Cartan_type" 	-> "Cartan_type(mat,grp)-> grp\nCartan_type(R,g) [R: roots].   Returns the type of the fundamental Lie subgroup whose root system is the minimal subsystem of the root system of g  containing all the roots in R.  A basis of fundamental roots of this subsystem may be obtained as fundam(R,g). See also closure and centr_type.",	
		"center" 		-> "center(grp)-> mat\ncenter(g) [result: torals].   Returns a matrix whose rows are semisimple elements or one parameter subgroups generating the center of g. The center of a semisimple Lie group g (always assumed to be simply connected in LiE) is a finite Abelian group isomorphic to the quotient of the weight lattice by the root lattice (for reductive groups the central torus is also included).",	
		"cent_roots" 	-> "cent_roots(vec,grp)-> mat\ncent_roots(t,g) [t: toral, result: roots].   Returns the matrix whose rows form the set of all positive roots centralising the semisimple element t of T (or the specified one parameter subgroup). Here a root alpha is said to centralise t if t commutes with all elements of the fundamental Lie subgroup of type A1 and closed subsystem of roots {alpha,-alpha} Equivalently, alpha centralises t if and only if alpha (which is a weight, and hence a map T->C ) maps t to 1.\n\ncent_roots(mat,grp)-> mat\ncent_roots(S,g) [S: torals, result: roots].   Returns the matrix whose rows form the set of all positive roots centralising the semisimple elements and/or one parameter subgroups represented by the rows of S. This set is the intersection of all sets cent_roots(t,g), with t traversing the rows of S. One may apply Cartan_type or fundam to the result to obtain the type, respectively the set of fundamental roots, of the centraliser. See also centr_type.",	
		"centr_type" 	-> "centr_type(vec,grp)-> grp\ncentr_type(t,g) [t: toral].   Returns the centraliser C_g(t) of the toral element t (or of the specified one parameter subgroup); effectively only the type is computed. See also cent_roots.\n\ncentr_type(mat,grp)-> grp\ncentr_type(S,g) [S: torals].   Returns the centraliser of the toral elements and/or one parameter subgroups of T represented by the rows of S, i.e., the intersection of the groups centr_type(t,g) for t traversing the rows of S.  This function can alternatively be computed as Cartan_type(cent_roots(S,g),g).",	
		"class_ord" 	-> "class_ord(vec)-> bin\nclass_ord(lambda) [lambda: partition].   Returns the order of the conjuga- tion class in S_n of permutations of cycle type lambda (for n = |lambda|, the sum of the parts of lambda).",	
		"closure" 		-> "closure(mat,grp)-> mat\nclosure(R,g) [R,result: roots].   Returns a basis of fundamental roots of the minimal closed subsystem of the root system of g that contains all the roots in R; the basis consisting of positive (for g) roots only is chosen.",	
		"collect" 		-> "collect(pol,grp,mat,grp)-> pol\ncollect(p,h,l,g) [p,result: decomposition, l: lin(weight,weight)].   This function attempts to perform the inverse operation of branch, namely to reconstruct a g-module from its restriction to h. This is not generally possible unless the restriction matrix is invertible, and in particular g and h have the same Lie rank. When a restriction matrix m has an inverse l, and the h-module with decomposition polynomial p is equal to some restriction branch(q,h,m,g) of a g-module via m, then the decomposition polynomial q can be computed as collect(p,h,l,g).\n\ncollect(pol,grp,mat,int,grp)-> pol\ncollect(p,h,l,n,g) [p,result: decomposition, l: lin(weight,weight)].   An obvious limitation of the previous version of collect is that it is only applicable for restriction matrices which are invertible over the integers; certain restriction matrices are invertible, but only over the rational numbers.  For these cases this extended version is provided.  Since LiE cannot handle matrices with rational entries, a common denominator n of all the entries of the inverse restriction matrix has to be factored out and passed as a separate argument, so that the scaled inverse matrix l has only integer coefficients. For all weights to which l is applied the image should be divisible by n, or else an error will be reported; apart from this, the extended version of collect operates in the same way as the previous one.",	
		"contragr" 		-> "contragr(vec,grp)-> vec\ncontragr(lambda,g)  [lambda,result: weight].   Yields the highest weight of the contragredient (or dual) representation V_lambda^* of V_lambda, which equals dominant (-lambda,g).\n\ncontragr(pol,grp)-> pol\ncontragr(p,g) [p,result: decomposition].   Returns the decomposition polynomial of the contragredient representation of the module with decomposition polynomial p.",	
		"decomp" 		-> "decomp(pol,grp)-> pol\ndecomp(d,g) [d: dominant, result: decomposition].   Returns the decomposition polynomial of the g-module with dominant character polynomial d; it is the inverse operation of dom_char.  See also v_decomp.",	
		"degree" 		-> "degree(pol)-> int\ndegree(p).   Returns the degree of a polynomial.\ndegree(p) = max { degree(p[i]) | 1 <= i <= length(p) }\ndegree(p[i]) = sum (j=1..n_vars(p)) expon(p,i)[j]\nExample\t\tdegree(X[1,2]+3X[3,4]) = 7",	
		"Demazure" 		-> "Demazure(pol,vec,grp)-> pol\nDemazure(p,w,g) [p,result: weights, w: Weyl word].   Starting with the polynomial p, repeatedly apply the Demazure operator M_{alpha_i}, taking for i the successive entries of the Weyl word w, from left to right. The final result of Demazure should be the same when taking for w different reduced Weyl words for the same element of W.\n\nDemazure(vec,vec,grp)-> pol\nDemazure(lambda,w,g) [lambda: weight, w: Weyl word, result: weights]. Returns `Demazure(X lambda,w,g)'.\n\nDemazure(pol,grp)-> pol\nDemazure(p,g) [p,result: weights].   This is an abbreviation for the call Demazure (p,long_word(g),g).  The resulting polynomial q can be characterised as the unique W-invariant polynomial which has J(q) = J(p). In fact, due to Demazure's character formula, q is the character polynomial of the module with decomposition polynomial p (provided all exponents of p were dominant).\n\nDemazure(vec,grp)-> pol\nDemazure(lambda,g) [lambda: weight, w: Weyl word, result: weights]. Returns `Demazure (X lambda,g)'.",	
		"det_Cartan" 	-> "det_Cartan(grp)-> int\ndet_Cartan(g).   Returns the determinant of Cartan (g). This number is the index of the root lattice in the weight lattice, and it is also the order of the center of g. See also i_Cartan.",	
		"diagram" 		-> "diagram(grp)-> vid\ndiagram(g).   Prints the Dynkin diagram of g, also indicating the type of each simple component printed, and labeling the nodes as done by Bourbaki (for the second and further simple components the labels are given an offset so as to make them disjoint from earlier labels). The labeling of the vertices of the Dynkin diagram prescribes the order of the coordinates of root- and weight vectors used in LiE.",	
		"dim" 			-> "dim(grp)-> int\ndim(g).   Returns the dimension of the Lie group g; equals dim(adjoint(g),g).\n\ndim(vec,grp)-> bin\ndim (lambda,g) [lambda: weight].   Returns the dimension of the representation V_lambda.\n\ndim(pol,grp)-> bin\ndim(p,g) [p: decomposition].   Returns the dimension of the g-module with decomposition polynomial p.",	
		"dom_char" 		-> "dom_char(vec,vec,grp)-> bin\ndom_char(lambda,mu,g) [lambda,mu: weight].   Returns the coefficient of `X mu' in the character polynomial of V_lambda.  The weight lambda should be dominant, but mu may be any weight.\n\ndom_char(pol,vec,grp)-> bin\ndom_char(p,mu,g) [p: decomposition, mu:weight, result: dominant].   Returns the coefficient of `X mu' in the character polynomial of the module with decomposition polynomial p.\n\ndom_char(vec,grp)-> pol\ndom_char(lambda,g) [lambda: weight, result: dominant].   (dominant character) Returns the polynomial representing the dominant part of the character of the g-module V_lambda.\n\ndom_char(pol,grp)-> pol\ndom_char(p,g) [p: decomposition, result: dominant].   This is like dom_char(lambda,g), but with the irreducible module V_lambda replaced by the module with decomposition polynomial p.",	
		"dominant" 		-> "dominant(vec,grp)-> vec\ndominant(lambda,g) [lambda,result: weight].   Returns the unique dominant  weight in the Weyl group orbit of the weight lambda.\n\ndominant(mat,grp)-> mat\ndominant(m,g) [m,result: weights].   Returns the matrix obtained by replacing each row of m by the unique dominant weight in its Weyl group orbit.\n\ndominant(pol,grp)-> pol\ndominant(p,g) [p,result: weights].   Returns the polynomial obtained by replacing each exponent of p by the unique dominant weight in its Weyl group orbit.",	
		"dom_weights" 	-> "dom_weights(vec,grp)-> mat\ndom_weights(lambda,g) [lambda: weight, result: weights].   Returns the set  of dominant weights lying under lambda. This is equal to the set of  weights occurring in dom_char (lambda,g).",	
		"exponents"		-> "exponents(grp)-> vec\nexponents(g) [result: ints].   Returns the exponents of the given Lie group. For composite groups the exponents are not necessarily increasing, as they are grouped according to the simple factors of the group, with the exponents for the central torus (all zeros) at the end.",	
		"filter_dom" 	-> "filter_dom(mat,grp)-> mat\nfilter_dom(m,g) [m,result: weights].   Returns the matrix obtained by casting away all rows of m that are not dominant weights.\n\nfilter_dom(pol,grp)-> pol\nfilter_dom(p,g)  [p,result: weights].   Returns the polynomial obtained by casting away all terms of p whose exponents are not dominant weights.",	
		"from_part" 	-> "from_part(vec)-> vec\nfrom_part(lambda) [lambda: partition, result: weight].   Let n be the number of parts of lambda (trailing zeros are significant here) then the function returns the weight for a group of type A_{n-1}  (i.e., for SL_n ) corresponding to lambda, expressed on the basis of fundamental weights. See also to_part.\n\nfrom_part(mat)-> mat\nfrom_part(m) [m: partitions, result: weights].   Replaces each row lambda of m by from_part(lambda).\n\nfrom_part(pol)-> pol\nfrom_part(p) [p: partitions, result: weights].   Replaces each exponent lambda occurring in p by from_part(lambda).",	
		"fundam" 		-> "fundam(mat,grp)-> mat\nfundam(R,g) [R,result: roots].   Returns a basis of fundamental roots of the minimal subsystem of the root system of g that contains all the roots in R; the basis consisting of positive (for g) roots only is chosen. The order in which the fundamental roots are returned is compatible with the standard labeling for a root system of type Cartan_type(R,g).",	
		"high_root" 	-> "high_root(grp)-> vec\nhigh_root(g) [result: root].   Returns the highest root of the root system of the group g, which must have exactly one simple component (for otherwise there exists no highest root).  This root is the last row of pos_roots(g). See also adjoint.",	
		"i_Cartan" 		-> "i_Cartan(grp)-> mat\ni_Cartan(g) [result: lin(weight,root)].   Returns det_Cartan(g) times the inverse of Cartan(g). The scalar factor det_Cartan(g) is required in order to keep all matrix entries integral.  To transform an element of the root lattice,  given as lambda in weight coordinates, to root coordinates, compute  lambda*i_Cartan(g)/det_Cartan(g).",	
		"inprod" 		-> "inprod(vec,vec,grp)-> int\ninprod(x,y,g) [x,y: root].   Returns the Weyl group invariant inner product of x and y.  The inner product is normalised in such a way that for each simple component of g the short roots x have inprod(x,x) = 2.",	
		"KL_poly" 		-> "KL_poly(vec,vec,grp)-> pol\nKL_poly(x,y,g) [x,y: Weyl word, result: polynomial].   Returns the Kazhdan-Lusztig polynomial P_{x,y}.",	
		"Lie_code" 		-> "Lie_code(grp)-> vec\nLie_code(g) [result: ints].   It is required that g be a simple group or a torus; the function returns a vector [t,n] of size 2, such that Lie_group(t,n)==g.",	
		"Lie_group" 	-> "Lie_group(int,int)-> grp\nLie_group(t,n).   Returns a torus or a simple group as follows:\n  Lie_group(0,n)= Tn  Lie_group(4,n)= Dn  (n>=3)  Lie_group(1,n)= An  (n>=1)    Lie_group(5,n)= En  (6<=n<=8)  Lie_group(2,n)= Bn  (n>=2)    Lie_group(6,4)= F4  Lie_group(3,n)= Cn  (n>=2)    Lie_group(7,2)= G2\n For any other numbers an error is indicated.  This function can be useful in order to run examples over many Lie groups using a for loop.",	
		"Lie_rank" 		-> "Lie_rank(grp)-> int\nLie_rank(g).   Returns the Lie rank of g; for simple groups and tori this equals Lie_code(g)[2], while for composite groups it is the sum of the Lie ranks of the component groups.",	
		"long_word" 	-> "long_word(grp)-> vec",	
		"l_reduce" 		-> "l_reduce(vec,vec,grp)-> vec\nl_reduce(l,w,g) [l: ints, w,result: Weyl word].   The set l determines a subgroup W_l of W generated by the set of fundamental reflections { r_i | i in l }. The function returns a Weyl word for the distinguished representative (element of minimal length) of the left coset `W_l w'. This Weyl word is obtained by deleting certain entries from w; in particular, if w is already a reduced expression for the distinguished representative, then w itself is returned.",	
		"lr_reduce" 	-> "lr_reduce(vec,vec,vec,grp)-> vec\nlr_reduce(l,w,r,g) [l,r: ints, w,result: Weyl word].   The sets l and r determine subgroups W_l and W_r of W generated by the sets of fundamental reflections { r_i | i in l } respectively { r_i | i in r }. The function returns a Weyl word for the distinguished representative (element of minimal length) of the double coset `W_l w W_r'.  This Weyl word is obtained by deleting certain entries from w; in particular, if w is already a reduced expression for the distinguished representative, then w itself is returned.",	
		"LR_tensor" 	-> "LR_tensor(vec,vec)-> pol\nLR_tensor(lambda,mu) [lambda,mu: partition result: decomposition]. (Littlewood-Richardson tensor) The partitions lambda and mu, which must have the same number of parts, say n, are interpreted as dominant weights for the group SL_n of type A_{n-1}, expressed in partition coordinates. The decomposition polynomial of the tensor product of the corresponding highest weight modules is computed using the Littlewood-Richardson rule, where the exponents in the result are again expressed in partition coordinates.  Note that extending lambda and mu by zeros can be significant: partitions with more than n non-zero parts may appear as exponents of new terms, while existing terms will reappear in zero- extended form. The total number of non-zero parts is bounded however by the number in lambda and mu taken together, so eventually the number of terms will stabilise; the limiting case corresponds to the decomposition of the Young product of the representations corresponding to lambda and mu in the representation theory of the symmetric groups.\n\nLR_tensor(pol,pol)-> pol\nLR_tensor(p,q) [p,q,result: decomposition].   Returns the decomposition poly- nomial of the tensor produce of the SL_n-modules with respective decompo- sition polynomials p and q, computed using the Littlewood-Richardson rule; all polynomials have their exponents in partition coordinates.",	
		"max_sub" 		-> "max_sub(grp)-> tex\nmax_sub(g).   Returns the types of the maximal proper subgroups of g, represented textually as comma separated list; the list is obtained from a small database. The group g must be simple and of rank <= 8. Types for which more than one conjugacy class of subgroups exist have repeated occurrences in the list. See also res_mat.\n\nmax_sub(int,grp)-> grp\nmax_sub(i,g).   Returns the type of the i-th maximal proper subgroup of g in the list max_sub(g).  The group g must be simple and of rank <= 8. See also res_mat.",	
		"n_comp" 		-> "n_comp(grp)-> int\nn_comp(g)   The number of simple components of g.",	
		"next_part" 	-> "next_part(vec)-> vec\nnext_part(lambda) [lambda,result: partition].   Returns the next partition of |lambda| in reverse lexicographic order. If lambda is the last one, i.e., if lambda[1,1,...,1], it will return lambda again. See also partitions.",	
		"next_perm" 	-> "next_perm(vec)-> vec\nnext_perm(p) [p,result: ints].   Returns the next permutation of the entries of p, in lexicographical order.  If p is the last such permutation, i.e., if the entries of p are decreasing, then p itself will be returned again. If there are repetitions among the entries of p, then this function will not attempt to permute identical entries, and in such cases it will take fewer applications of next_perm to go from the weakly decreasing order to the weakly increasing order. See also sym_orbit.",	
		"next_tabl" 	-> "next_tabl(vec)-> vec\nnext_tabl(T) [T,result: tableau].   Returns the lexicographically next Young tableau of the same shape as T. See also tableaux.",	
		"norm" 			-> "norm(vec,grp)-> int\nnorm(alpha,g) [alpha: root].   Returns the norm inprod(alpha,alpha) of the root vector alpha (it would be more accurate, but less convenient, to call this the \"squared norm\"). When alpha is a root, the value is one of {2, 4, 6}, and the inner product is chosen such that for each simple component the short roots have norm 2. Note that this normalisation differs from that used by Bourbaki in the case of groups of type Bn, as the short roots are given norm 1 there.",	
		"n_tabl" 		-> "n_tabl(vec)-> bin",	
		"n_vars" 		-> "n_vars(pol)-> int\nn_vars(p).   Returns the number of indeterminates of p.",	
		"orbit" 		-> "orbit(int,vec,mat)-> mat\norbit(n,v,M) [result: vectors].   This function operates in the same way as orbit(v,m), but n replaces the limit of 1000 elements in the orbit. Warning: orbit uses allocates space at the beginning for the maximal number n of vectors allowed in the orbit; therefore one shouldn't go overboard on choosing the limit n.\n\norbit(vec,mat)-> mat\norbit(v,M) [result: vectors].   Here v is a vector with an arbitrary interpretation, and M is a matrix whose column size c equals size(v), and whose row size is a multiple of c, say kc. We interpret M as a collection of k square matrices of size c x c, vertically concatenated. The function orbit attempts to compute the orbit of v under the group generated by the collection of matrices, i.e., a minimal set V of vectors containing v and closed under right multiplication by any of the matrices in the given collection. As the orbit might be infinite, and the algorithm has no means to detect this situation, it gives up when more than 1000 vectors in the orbit have been computed. For larger orbits, see orbit(n,v,M ), for Weyl group orbits see W_orbit.",	
		"plethysm" 		-> "plethysm(vec,vec,grp)-> pol\nplethysm(lambda,mu,g) [lambda: partition, mu: weight, result: decomposition]. Returns the decomposition polynomial of the g-module of the plethysm of of V_m  corresponding to the partition lambda.\n\nplethysm(vec,pol,grp)-> pol\nplethysm(lambda,p,g) [lambda: partition, p,result: decomposition]. This is similar to plethysm(lambda,mu,g), but with the irreducible module V_mu replaced by the module with decomposition polynomial p.",	
		"pos_roots" 	-> "pos_roots(grp)-> mat\npos_roots(g) [result: roots].   Returns a matrix whose rows are the positive roots of g. The first rows are the fundamental roots (i.e., the top r rows form the matrix id(r), and if g is simple the last row, which has index n_pos_roots(g), is high_root(g).",	
		"p_tensor" 		-> "p_tensor(int,vec,grp)-> pol\np_tensor(n,lambda,g)  [lambda: weight, result: decomposition]. Returns the decomposition polynomial of the n-th tensor power of V_lambda.\n\np_tensor(int,pol,grp)-> pol\np_tensor(n,p,g) [p,result: decomposition].   Returns the decomposition polynomial of the n-th tensor power of the g-module with decomposition polynomial p.",	
		"print_tab" 	-> "print_tab(vec)-> vid\nprint_tab(T) [T: tableau].   Displays the Young tableau encoded by T in 2-dimensional form.",	
		"reduce" 		-> "reduce(vec,grp)-> vec\nreduce(w,g) [w,result: Weyl word].   Returns a Weyl word of minimal length representing the same element of W as w. This Weyl word is obtained by deleting certain entries from w; in particular, if w is already a reduced expression, then w itself is returned. See also canonical, l_reduce, r_reduce and lr_reduce.",	
		"reflection" 	-> "reflection(vec,grp)-> mat\nreflection(vec alpha,g) [alpha: root, result: lin(weight,weight)].   Returns the matrix of the reflection of the weight lattice in the hyperplane perpendicular to the root ff, expressed on the basis of fundamental weights. See also W_action .",	
		"res_mat" 		-> "res_mat(grp,grp)-> mat\nres_mat(h,g) [result: lin(weight,weight)].   Returns the restriction matrix for the maximal proper subgroup with type h of g, which is obtained from a small database. The group g must be simple and of rank <= 8.  In case more than one non-conjugate subgroups of type h exist, the restriction matrix for the first one in the list is returned; in case no such subgroup exists, an error is reported.  See also max_sub.\n\nres_mat(grp,int,grp)-> mat\nres_mat(h,i,g) [result: lin(weight,weight)].   Returns the restriction matrix for the i-th maximal proper subgroup with type h of g,  which is obtained from a small database.  The group g must be simple and of rank <= 8. See also max_sub.\n\nres_mat(mat,grp)-> mat\nres_mat(R,g) [R: roots, result: lin(weight,weight)]. (restriction matrix) It is assumed that the set R consists of roots forming a fundamental basis for a closed subsystem Psi of the root system Phi of g (as for instance obtained by a call of closure).  The function returns the restriction matrix for the fundamental Lie subgroup of g with root system Psi. Although the function checks whether the rows of R are indeed roots, and whether they are linearly independent, it does not test whether they are positive roots and whether their mutual inner products are non-positive; these conditions should be met however in order to obtain a result suitable for use with branch and collect.  If the number m of roots is strictly less than the Lie rank r of g, and one is in fact interested in the semisimple subgroup with the given root system Psi, then it suffices to simply discard the final r-m columns.",	
		"row_index" 	-> "row_index(mat,vec,int,int)-> int\nrow_index(m,v,lb,ub)   Searches for i=lb, ..., ub whether m[i]==v.\nThe first such i is returned, or 0 if none was found.",	
		"R_poly" 		-> "R_poly(vec,vec,grp)-> pol\nR_poly(x,y,g):  pol  [x,y: Weyl word, result: polynomial].   Returns the value of the R-polynomial R_{x,y}.",	
		"r_reduce" 		-> "r_reduce(vec,vec,grp)-> vec\nr_reduce(w,r,g)  [w,result: Weyl word, r: ints].   The set r determines a subgroup Wr of W generated by the set of fundamental reflections { r_i | i in r }. The function returns a Weyl word for the distinguished representative of the right coset `w W_r'.  This Weyl word is obtained by deleting certain entries from w; in particular, if w is already a reduced expression for the distinguished representative, then w itself is returned.",	
		"RS" 			-> "RS(vec)-> mat\nRS(p) [p: permutation, result: tableaux].   Returns the pair of Young tableaux corresponding to the permutation p by the Robinson-Schensted correspondence; the result is represented as a 2-row matrix.\n\nRS(vec,vec)-> vec\nRS(P,Q)  [P,Q: tableau, result: permutation].   Returns the permutation corresponding to the pair of Young tableaux P,Q (which must have the same shape) by the Robinson-Schensted correspondence.",	
		"shape" 		-> "shape(vec)-> vec\nshape(T) [T : tableau, result: partition].   Returns the shape of the Young tableau T.",	
		"sign_part" 	-> "sign_part(vec)-> int\nsign_part(lambda) [lambda: partition].   Returns the sign (+1 or 1) of permutations of cycle type lambda.",	
		"spectrum" 		-> "spectrum(vec,vec,grp)-> pol\nspectrum(lambda,t,g) [lambda: weight, t: toral].   Let n be the last entry of t, then the toral element t will act in any representation of g as a diagonalisable transformation all of whose eigenvalues are n-th roots of unity. The function returns a polynomial in one indeterminate, in which the coefficient of the monomial `X i' is the multiplicity of the eigenvalue zeta^i in the action of the toral element t on the irreducible g-module V_lambda, where i is the complex number e^{2*pi/n}.\n\nspectrum(pol,vec,grp)-> pol\nspectrum(p,t,g) [p: decomposition, t: toral].   This is similar to spectrum (lambda,t,g),  but with the irreducible module V_lambda replaced by the module with decomposition polynomial p.",	
		"support" 		-> "support(pol)-> mat\nsupport(p).   Returns the matrix whose rows are the exponents of p.",	
		"sym_char" 		-> "sym_char(vec,vec)-> bin\nsym_char(lambda,mu) [lambda,mu: partition].   We should have |lambda|=|mu|; the function returns the (integral) value chi_lambda(mu) of the character of the symmetric group S_{|mu|} corresponding to  on the conjugacy class with cycle type lambda.\n\nsym_char(vec)-> pol\nsym_char(lambda) [lambda: partition,  result: character].   (Symmetric group character)  Let n=|lambda|; the function returns the character polynomial of the character chi_lambda of the symmetric group S_n corresponding to the partition lambda.",	
		"sym_orbit" 	-> "sym_orbit(vec)-> mat\nsym_orbit(v) [result: vectors].   (Symmetric group orbit)  Let n = size(v). The symmetric group on n letters acts on Z^n by permuting the coordinates; the function returns the orbit of v in this action. The rows of the result are ordered lexicographically. See also next_perm.",	
		"sym_tensor" 	-> "sym_tensor(int,vec,grp)-> pol\nsym_tensor(n,lambda,g) [lambda: weight, result: decomposition]. (symmetric tensor power)  Returns the decomposition polynomial of the n-th symmetric tensor power of V_lambda.  See also alt_tensor and plethysm.\n\nsym_tensor(int,pol,grp)-> pol\nsym_tensor(n,p,g) [p,result: decomposition].   This is similar to sym_tensor(n,,g), but with the irreducible module V_lambda replaced by the module with decomposition polynomial p.",	
		"tableaux" 		-> "tableaux(vec)-> mat\ntableaux(lambda)  [lambda: partition, result: tableaux].   Returns a matrix whose rows encode the set of all Young tableaux of shape lambda, in lexicographic order.",	
		"tensor" 		-> "tensor(vec,vec,grp)-> pol\ntensor(lambda,mu,g) [lambda,mu: weight, result: decomposition].   Returns the decomposition polynomial of the tensor product of V_lambda and V_mu. For groups of type A_n, see also LR_tensor.\n\ntensor(pol,pol,grp)-> pol\ntensor(p,q,g) [p,q,result: decomposition].   Returns the decomposition polynomial of the tensor product of the g-modules with respective decomposition polynomials p and q.\n\ntensor(vec,vec,vec,grp)-> bin\ntensor(lambda,mu,nu,g) [lambda,mu,nu: weight].   Returns the coefficient of the monomial `X nu' in tensor(lambda,mu,g).\n\ntensor(pol,pol,vec,grp)-> bin\ntensor(p,q,nu,g) [p,q,result: decomposition, nu: weight].   Returns the coefficient of the monomial `X nu' in tensor(p,q,g).",	
		"to_part" 		-> "to_part(vec)-> vec\nto_part(v) [v: weight, result: partition].   Let n = size(v), then v is interpreted as a weight for a group of type A_n (i.e., for SL_{n+1}); the expression of that weight in n+1 partition coordinates is returned. When v is dominant, this is a partition with n+1 parts. See also from_part.\n\nto_part(mat)-> mat\nto_part(m) [m: weights, result: partitions].   Replaces each row v of m by to_part(v).\n\nto_part(pol)-> pol\nto_part(p) [p: weights, result: partitions].   Replaces each exponent v occurring in p by to_part(v).",	
		"trans_part" 	-> "trans_part(vec)-> vec\ntrans_part(lambda) [lambda,result: partition].   Returns the transpose partition of lambda.",	
		"unique" 		-> "unique(mat)-> mat\nunique(m). Returns a canonical form for a matrix representing a set of vectors: it sorts the matrix and removes any multiple rows. Algorithm: heap sort.",	
		"v_decomp" 		-> "v_decomp(pol,grp)-> pol\nv_decomp(d,g) [d: dominant, result: decomposition].   (virtual decomposition) Returns the virtual decomposition polynomial of the virtual g-module with dominant character polynomial d.\n\nv_decomp(vec,grp)-> pol",	
		"W_action" 		-> "W_action(vec,vec,grp)-> vec\nW_action(lambda,w,g) [lambda,result: weight, w: Weyl word]. Returns the weight that is the image `lambda.w' of the weight lambda under the action of the Weyl group element w.\n\nW_action(vec,grp)-> mat\nW_action(w,g) [w: Weyl word, result: lin(weight,weight)]. Returns the matrix giving the action of the Weyl group element w in W on the weight lattice, expressed on the basis of fundamental weights. See also reflection, W_rt_action, and W_word.\n\nW_action(mat,vec,grp)-> mat\nW_action(m,w,g) [m,result: weights, w: Weyl word].   Returns the matrix obtained by replacing each row lambda of m by W_action(lambda,w,g); this matrix is equal to m*W_action(w,g), while conversely W_action(w,g) equals W_action(id(Lie_rank(g)),w,g).\n\nW_action(pol,vec,grp)-> pol\nW_action(p,w,g) [p,result: weights, w: Weyl word]. Returns the polynomial obtained by replacing each exponent lambda of p by W_action(lambda,w,g); this polynomial is equal to p*W_action(w,g).",	
		"W_orbit" 		-> "W_orbit(vec,grp)-> mat\nW_orbit(lambda,g) [lambda: weight, result: weights].   Returns the orbit  of the weight lambda under the Weyl group of g.\n\nW_orbit(pol,grp)-> pol\nW_orbit(pol p,g) [p,result: weights].   Returns the polynomial obtained by summing over all terms `n X lambda' of p the polynomial `n X W_orbit(lambda,g)'; the latter polynomial contains each weight in the W-orbit of lambda exactly once and with coefficient n. This operation can be used for instance to compute the full character polynomial of a module from its dominant character module.",	
		"W_orbit_graph" -> "W_orbit_graph(vec,grp)-> mat",	
		"W_orbit_size" 	-> "W_orbit_size(vec,grp)-> bin\nW_orbit_size(lambda,g) [lambda: weight, result: weights].   Returns the size of the orbit of the weight lamnda under the Weyl group of g. This size can also be computed as W_order(g)/W_order(I,g), where I is a vector whose entries indicate the positions at which the vector dominant(lambda) has zero entries.",	
		"W_order" 		-> "W_order(grp)-> bin\nW_order(g).   (Weyl group order)  Returns the order of the Weyl group of g.\n\nW_order(vec,grp)-> bin\nW_order(I,g)  [I: ints].   Returns the order of the subgroup W_I of the Weyl group of g generated by the set of fundamental reflections { r_i | i in I }. This subgroup is the stabiliser subgroup of any weight vector that has zero entries precisely at positions i for which i in I.",	
		"W_rt_action" 	-> "W_rt_action(vec,vec,grp)-> vec\nW_rt_action(vec alpha,w,g) [alpha: root, w: Weyl word].   Returns the root that is the image `alpha.w' of the root vector alpha under the Weyl group element w.\n\nW_rt_action(vec,grp)-> mat\nW_rt_action(w,g) [w: Weyl word, result: lin(root,root)]. (Weyl root action) Returns the matrix giving the action of the Weyl group element w on the root lattice, expressed on the basis of fundamental roots.\n\nW_rt_action(mat,vec,grp)-> mat\nW_rt_action(m,w,g) [m,result: roots, w: Weyl word].   Returns the matrix obtained by replacing each row alpha of m by W_rt_action(alpha,w,g); this matrix is equal to m*W_rt_action(w,g), while conversely W_rt_action(w,g) equals W_rt_action(id(Lie_rank(g)),w,g).",	
		"W_rt_orbit" 	-> "W_rt_orbit(vec,grp)-> mat\nW_rt_orbit(alpha,g) [alpha: root, result: roots].  (Weyl root orbit)  Returns the orbit of the root vector alpha under the Weyl group of g.",	
		"W_word" 		-> "W_word(vec,grp)-> vec\nW_word(lambda,g) [lambda: weight, result: Weyl word].   Returns a Weyl word for a Weyl group element w whose action sends lambda to a dominant weight. In fact, the canonical Weyl word for w is returned, while w is the distinguished representative of its right coset `w W_S' , where W_S is the stabiliser of dominant(lambda,g).\n\nW_word(mat,grp)-> vec\nW_word(m,g) [m: lin(weight,weight), result: Weyl word].   Returns the canonical Weyl word for the Weyl group element w, if it exists, whose action on the weight lattice is given by the square matrix m, i.e., such that W_action(w,g)==m."
	}

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

