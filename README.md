LieLink
=======

LieLink is a Mathematica interface for LiE. LiE is a computer algebra package for 
Lie group computations. LieLink makes it possible to call LiE functions directly 
from Mathematica. Here is an example:

    <<LieLink`
    
    SetDefaultAlgebra["A2"]
    
    LieTensor[{1, 0}, {1, 0}]
    (* => {0,1} + {2,0} *)

For a list of all supported LiE functions, see the last section.

Installation
------------

LieLink requires at least Mathematica 7.

### Obtaining the package ###

Download the latest release https://github.com/teake/LieLink/releases, unzip it, 
and move the LieLink directory to the Mathematica applications directory. The location 
of this directory can be found by typing the command

    FileNameJoin @ {$UserBaseDirectory, "Applications"}

into Mathematica. Alternatively, you can checkout this repository and move the LieLink 
subdirectory to the same Mathematica applications directory.

### Compiling LiE ###

LieLink relies on LiE for all the calculations, so it needs a compiled version of it. 
If you already have the compiled GAP version installed on your computer, it suffices 
to point LieLink to its location before loading the package:

    LieLink`$LieDirectory = "path/to/directoryofLiE";
    <<LieLink`

If not, you need to compile the GAP version of LiE. Download the compile-only version
of LiE from http://wwwmathlabo.univ-poitiers.fr/~maavl/LiE/, unzip it, and compile the 
GAP version by running

    make
    make Liegap.exe
    
from the command line. Note that OS X users will first need to replace the makefile with 
[this file](http://wwwmathlabo.univ-poitiers.fr/~maavl/LiE/Macfile).

After compilation, move the following files to the LiE subdirectory in the LieLink directory:

    Liegap.exe
    Lie.exe
    LEARN.ind
    INFO.ind
    INFO.a
    INFO.0
    INFO.1
    INFO.2
    INFO.3
    INFO.4
    
Running the package
-------------------

Once installed, LieLink can be loaded in Mathematica by typing

    <<LieLink`
    
All LiE functions have new Mathematica-like names (see the list in the last section). 
For example, the function `tensor` is now called `LieTensor`:

    LieTensor[{1, 1}, {1, 1}, "A2"]
    (* => {0, 0} + {0, 3} + 2 {1, 1} + {2, 2} + {3, 0} *)

The Mathematica names of LiE functions can be looked up with the `LieFunction`
command:

    LieFunction["sym_tensor"]
    (* => SymmetricTensorPower *)

Besides using the new Mathematica-like names of LiE functions, it is also possible
to directly call LiE with a textual query:

    LieQuery["dim(A2)"]
    (* => "8" *)

Unlinke the new short-hand functions like `LieTensor`, `LieQuery` doesn't parse its
output to Mathematica syntax but keeps it in plain-text.

LieLink also introduces a new Mathematica type called `LieTerm`, which is used for
terms in Laurent polynomials. `LieTerm[1,2,3]` prints as `{1,2,3}` with red brackets,
and is the equivalent of writing `1X[1,2,3]` in LiE. `LieTerm` can also be used
as input:

    SymmetricTensorPower[2, LieTerm[1,1], "A2"]
    (* => LieTerm[0, 0] + LieTerm[1, 1] + LieTerm[2, 2] *)


Mathematica names of Lie functions
----------------------------------

Below is a list of all new Mathematica-like names for LiE functions in LieLink.
This list is stored in the Mathematica variable `LieFunctionTable`.

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
