(* Mathematica Test File *)

Print[System`$Version];

Test[
	MathLie`Private`CheckLieExecutable[]
	,
	True
	,
	TestID->"Test-20130103-U3S6O1"
]

Test[
	LieQuery["A2"]
	,
	"\"A2\""
	,
	TestID->"Test-20130910-J7A3E3"
]

Test[
	FileExistsQ @ $LieOutFile
	,
	True
	,
	TestID->"Test-20130910-S0B0X6"
]

Test[
	FileExistsQ @ $LieInFile
	,
	True
	,
	TestID->"Test-20130910-U4B0R8"
]

Test[
	LieQuery["2+2"]
	,
	"4"
	,
	TestID->"Test-20130910-K4R9Y1"
]

Test[
	LieQuery["[1,2,3]"]
	,
	"[1,2,3]"
	,
	TestID->"Test-20130910-I3M9P2"
]

Test[
	LieQuery["dim(E8)"]
	,
	"248"
	,
	TestID->"Test-20130910-G8I2T7"
]

oldexe 			= $LieExecutable;
$LieExecutable 	= "wrongfilename";

Test[
	MathLie`Private`CheckLieExecutable[]
	,
	False
	,
	{LieQuery::exe}
	,
	TestID->"Test-20130910-A6F2X2"
]

Test[
	CheckAbort[ LieQuery["dim(E8)"] , Null ]
	,
	Null
	,
	{LieQuery::exe}
	,	
	TestID->"Test-20130910-R5O4L3"
]

$LieExecutable 	= oldexe;

Test[
	CheckAbort[ LieQuery["asdfasdf"] , Null ]
	,
	Null
	,
	{LieQuery::invalid}
	,	
	TestID->"Test-20130910-I5Y2L1"
]

Test[
	( 4 LieTerm[-1, 4] - 3 LieTerm[1, 2] )^2 // Expand
	,
	16*LieTerm[-2, 8] - 24*LieTerm[0, 6] + 9*LieTerm[2, 4]
	,	
	TestID->"Test-20130910-R9U9Y9"
]


Test[
	FromLieOutput["2"]
	,
	2
	,
	TestID->"Test-20130910-P6S4V7"
]

Test[
	FromLieOutput["\"E8\""]
	,
	"E8"
	,
	TestID->"Test-20130910-D9D5F0"
]

Test[
	FromLieOutput["[1,2,3,4,5]"]
	,
	{1, 2, 3, 4, 5}
	,
	TestID->"Test-20130910-K9S8P4"
]

Test[
	FromLieOutput["[[1,2],[3,-4]]"]
	,
	{{1, 2}, {3, -4}}
	,
	TestID->"Test-20130910-M8W6X2"
]

Test[
	FromLieOutput["[1,[0,0],2,[1,1]]"]
	,
	LieTerm[0,0] + 2 LieTerm[1,1]
	,
	TestID->"Test-20130910-O2E1A7"
]

Test[
	ToLieInput[LieTerm[1, 0, 0]]
	,
	"[1,0,0]"
	,
	TestID->"Test-20130910-I8G2F8"
]

Test[
	ToLieInput[2 LieTerm[1, 0, 0]]
	,
	"2X[1,0,0]"
	,
	TestID->"Test-20130910-Z1S3I6"
]

Test[
	ToLieInput["E8"]
	,
	"E8"
	,
	TestID->"Test-20130910-H4C8U0"
]

Test[
	ToLieInput[4]
	,
	"4"
	,
	TestID->"Test-20130910-V5K6Z9"
]

Test[
	ToLieInput[{1, 2, 3, 4}]
	,
	"[1,2,3,4]"
	,
	TestID->"Test-20130910-E8W0Y7"
]

Test[
	SetDefaultAlgebra[]
	,
	None
	,
	TestID->"Test-20130910-H3R5B5"
]

Test[
	SetDefaultAlgebra["A2"]
	,
	"A2"
	,
	TestID->"Test-20130910-O8M7C0"
]

Test[
	SetDefaultAlgebra[]
	,
	"A2"
	,
	TestID->"Test-20130910-I6F5O2"
]

Test[
	LieFunction["dim", "E8"]
	,
	248
	,
	TestID->"Test-20130910-W7R3T9"
]

Test[
	Dim["A2"]
	,
	8
	,
	TestID->"Test-20130910-I6I2F4"
]

Test[
	Dim[{1, 1}]
	,
	8
	,
	TestID->"Test-20130910-H4D8L6"
]

Test[
	Dim[LieTerm[1,0]]
	,
	3
	,
	TestID->"Test-20130910-B0M5Q9"
]

Test[
	LookupLieFunction["dim"]
	,
	Dim
	,
	TestID->"Test-20130910-L1M2W1"
]

Test[
	LookupLieFunction["doesn't exist"]
	,
	$Failed
	,
	TestID->"Test-20130910-T4N5B5"
]
