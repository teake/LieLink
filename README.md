LieLink
=======

LieLink is a Mathematica interface for LiE. With it you can for example compute tensor products
of representations:

    LieTensor[{1, 0}, {1, 0}, "A2"]
    (* => {0,1} + {2,0} *)

LieLink needs Mathematica version 7 or higher.

Installation
------------

### Obtaining the package ###

Download the latest release https://github.com/teake/LieLink/releases, unzip it, and move the LieLink
directory to the Mathematica applications directory. The location of this directory can be found by 
typing the command

    FileNameJoin @ {$UserBaseDirectory, "Applications"}

into Mathematica. Alternatively, you can checkout this repository and move the LieLink subdirectory to 
the same Mathematica applications directory.

### Compiling LiE ###

LieLink relies on LiE for all the calculations, so it needs a compiled version of it. 
Download the compile-only version of LiE from http://wwwmathlabo.univ-poitiers.fr/~maavl/LiE/, unzip it, 
and compile the GAP version by doing

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

Once installed, LieLink can be run in Mathematica by typing

    <<LieLink`
    
Lie commands can be executed with the `LieFunction` command:

    LieFunction["dim", "A2"]
    (* => 8 *)

    LieFunction["dim", "[1,0,1]", "A3"]
    (* => 15 *)

The default algebra can be set with the `SetDefaultAlgebra` command:

    SetDefaultAlgebra["A2"]
    (* => "A2" *)
    
    LieFunction["dim", "[1,0]"]
    (* => 3 *)

There are also short-hand functions for all LiE commands (a full list is given below):

    LieTensor[{1, 1}, {1, 1}, "A2"]
    (* => {0, 0} + LieTerm{0, 3} + 2 {1, 1} + {2, 2} + {3, 0} *)

The brackets in the above output print red, which indicate that they'r actually `LieTerm` objects.
These objects behave as terms in Laurent polynomials. They can also be used as input:

    SymTensor[2, LieTerm[1,1], "A2"]
    (* => LieTerm[0, 0] + LieTerm[1, 1] + LieTerm[2, 2] *)


Short-hand functions
--------------------

    "Adams" 		-> "Adams",
    "adjoint" 		-> "AdjointRepresentation",
    "alt_dom" 		-> "AlternatingDominant",
    "alt_tensor" 	-> "AlternatingTensor",
    "alt_W_sum" 	-> "AlternatingWeylSum",
    "block_mat" 	-> "BlockdiagonalMatrix",
    "branch" 		-> "Branch",
    "Bruhat_desc" 	-> "BruhatDesc",
    "Bruhat_leq" 	-> "BruhatLeq",
    "canonical" 	-> "Canonical",
    "Cartan" 		-> "Cartan",
    "Cartan_type" 	-> "CartanType",
    "center" 		-> "GroupCenter",
    "cent_roots" 	-> "CentralRoots",
    "centr_type" 	-> "CentralType",
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
    "KL_poly" 		-> "KazhdanLusztig",
    "Lie_code" 		-> "LieCode",
    "Lie_group" 	-> "LieGroup",
    "Lie_rank" 		-> "LieRank",
    "long_word" 	-> "LongestWord",
    "l_reduce" 		-> "LeftWeylReduce",
    "lr_reduce" 	-> "LeftRightWeylReduce",
    "LR_tensor" 	-> "LittlewoodRichardson",
    "max_sub" 		-> "MaximalSubgroup",
    "n_comp" 		-> "NumberOfSimpleComponents",
    "next_part" 	-> "NextPartition",
    "next_perm" 	-> "NextPermutation",
    "next_tabl" 	-> "NextTableau",
    "norm" 			-> "RootNorm",
    "n_tabl" 		-> "NumberOfTableaux",
    "n_vars" 		-> "NumberOfVariables",
    "orbit" 		-> "Orbit",
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
    "trans_part" 	-> "TransosePartition",
    "unique" 		-> "CanonicalMatrix",
    "v_decomp" 		-> "VirtualDecomposition",
    "W_action" 		-> "WeylAction",
    "W_orbit" 		-> "WeylOrbit",
    "W_orbit_grap" 	-> "WeylOrbitGrap",
    "W_orbit_size" 	-> "WeylOrbitSize",
    "W_order" 		-> "WeylOrder",
    "W_rt_action" 	-> "WeylRootAction",
    "W_rt_orbit" 	-> "WeylRootOrbit",
    "W_word" 		-> "WeylWord"