LieLink
=======

LieLink is a Mathematica interface for [LiE](http://wwwmathlabo.univ-poitiers.fr/~maavl/LiE/),
a computer package for Lie algebra computations. LieLink makes it possible to call
LiE functions directly from Mathematica, enabling Mathematica to compute e.g.

* Tensor products of representations,
* Symmetric or alternating tensor powers of representations,
* Branchings of representations to subalgebras,

and much more. See the [LiE manual](http://wwwmathlabo.univ-poitiers.fr/~maavl/LiEman/manual.pdf)
for all its capabilities. Here is an short LieLink example that computes the tensor
product of two fundamental representation of the Lie algebra *A*<sub>2</sub>:

    <<LieLink`
    
    SetDefaultAlgebra["A2"]
    
    LieTensor[{1, 0}, {1, 0}]
    (* => {0,1} + {2,0} *)

For a list of all supported LiE functions, see the [last section](#mathematica-names-of-lie-functions).
LieLink requires at least Mathematica 7.


Installation
------------


### Automatic installation ###

LieLink can be installed automatically by entering the following command in 
Mathematica:

    Import["https://raw.github.com/teake/LieLink/master/Install.m"]


### Manual installation ###

Download the latest release https://github.com/teake/LieLink/releases, unzip it, 
and move the LieLink directory to the Mathematica applications directory. The location 
of this directory can be found by typing the command

    FileNameJoin @ {$UserBaseDirectory, "Applications"}

into Mathematica. 


### Compiling LiE ###

LieLink needs a compiled version of LiE to run. It comes bundled with LiE versions for 
OS X, Windows, 32-bit and 64-bit Linux, all precompiled on x86 architecture. 
If none of these version work on your computer, you need to compile LiE yourself.

Download the compile-only version of LiE from http://wwwmathlabo.univ-poitiers.fr/~maavl/LiE/, 
unzip it, and compile the GAP version by running

    make noreadline
    make Liegap.exe
    
from the command line. Note that OS X users will first need to replace the makefile with 
[this file](http://wwwmathlabo.univ-poitiers.fr/~maavl/LiE/Macfile). Compilation
on Windows is best done with [MinGW](http://www.mingw.org/) in an 
[MSYS](http://www.mingw.org/wiki/MSYS) shell. 

After compilation, you need to tell LieLink where the compiled version of
LiE can be found. This can be done before loading the package as follows:

    LieLink`$LieDirectory = "path/to/directoryofcompiledLiE";
    <<LieLink`


Using the package
-----------------

Once installed, LieLink can be loaded in Mathematica by typing

    <<LieLink`
    
All LiE functions have new Mathematica-like names (see the list in the 
[last section](#mathematica-names-of-lie-functions)). 
For example, the LiE function `tensor` is called `LieTensor` in LieLink:

    LieTensor[{1, 1}, {1, 1}, "A2"]
    (* => {0, 0} + {0, 3} + 2 {1, 1} + {2, 2} + {3, 0} *)

The Mathematica names of LiE functions can be looked up with the `LieFunction`
command:

    LieFunction["tensor"]
    (* => LieTensor *)

Besides using the new Mathematica-like names of LiE functions, it is also possible
to directly call LiE with a textual query:

    LieQuery["dim(A2)"]
    (* => "8" *)

Unlike the new short-hand functions like `LieTensor`, `LieQuery` doesn't parse its
output to Mathematica syntax but keeps it in plain-text.

Groups are entered as strings in LieLink. For example, the equivalent of the LiE
command `dim(A2)` is now `Dim["A2"]`. 

LieLink also introduces a new Mathematica type called `LieTerm`, which is used for
terms in Laurent polynomials. `LieTerm[1,2,3]` prints as `{1,2,3}` with red brackets,
and is the equivalent of writing `1X[1,2,3]` in LiE. `LieTerm` can also be used
as input:

    SymmetricTensorPower[2, LieTerm[1,1], "A2"]
    (* => LieTerm[0, 0] + LieTerm[1, 1] + LieTerm[2, 2] *)

Currently LieLink supports two system parameters of LiE, namely `maxnodes` and `maxobjects`.
They can be set by changing the Mathematica variables `$MaxNodes` and `$MaxObjects` 
respectively, which should always be integers. Their default values are

    $MaxNodes   = 9999;
    $MaxObjects = 99999;


Mathematica names of Lie functions
----------------------------------

Below is a list of all new Mathematica-like names for LiE functions in LieLink.
This list is stored in the Mathematica variable `LieFunctionTable`.

    Adams       -> Adams,                     LR_tensor     -> LittlewoodRichardson,
    adjoint     -> AdjointRepresentation,     max_sub       -> MaximalSubgroups,
    alt_dom     -> AlternatingDominant,       n_comp        -> NumberOfSimpleComponents,
    alt_tensor  -> AlternatingTensorPower,    next_part     -> NextPartition,
    alt_W_sum   -> AlternatingWeylSum,        next_perm     -> NextPermutation,
    block_mat   -> BlockdiagonalMatrix,       next_tabl     -> NextTableau,
    branch      -> Branch,                    norm          -> RootNorm,
    Bruhat_desc -> BruhatDescendant,          n_tabl        -> NumberOfTableaux,
    Bruhat_leq  -> BruhatLeq,                 n_vars        -> NumberOfVariables,
    canonical   -> CanonicalWeylWord,         orbit         -> GroupOrbit,
    Cartan      -> CartanMatrix,              plethysm      -> Plethysm,
    Cartan      -> CartanProduct,             pos_roots     -> PositiveRoots,
    Cartan_type -> CartanType,                p_tensor      -> TensorPower,
    center      -> GroupCenter,               print_tab     -> PrintTableau,
    cent_roots  -> CentralizingRoots,         reduce        -> WeylReduce,
    centr_type  -> CentralizerType,           reflection    -> Reflection,
    class_ord   -> ConjugacyClassOrder,       res_mat       -> RestrictionMatrix,
    closure     -> Closure,                   row_index     -> RowIndex,
    collect     -> InverseBranch,             R_poly        -> RPolynomial,
    contragr    -> Contragradient,            r_reduce      -> RightWeylReduce,
    decomp      -> Decomposition,             RS            -> RobinsonSchensted,
    degree      -> PolynomialDegree,          shape         -> TableauShape,
    Demazure    -> Demazure,                  sign_part     -> PartitionSign,
    det_Cartan  -> DetCartan,                 spectrum      -> ToralSpectrum,
    diagram     -> DynkinDiagram,             support       -> Support,
    dim         -> Dim,                       sym_char      -> SymmetricCharacter,
    dom_char    -> DominantCharacter,         sym_orbit     -> SymmetricOrbit,
    dominant    -> Dominant,                  sym_tensor    -> SymmetricTensorPower,
    dom_weights -> DominantWeights,           tableaux      -> TableauxOfPartition,
    exponents   -> Exponents,                 tensor        -> LieTensor,
    filter_dom  -> FilterDominant,            to_part       -> ToPartition,
    from_part   -> FromPartition,             trans_part    -> TransposePartition,
    fundam      -> FundamentalRoots,          unique        -> CanonicalMatrix,
    high_root   -> HighestRoot,               v_decomp      -> VirtualDecomposition,
    i_Cartan    -> InverseCartan,             W_action      -> WeylAction,
    inprod      -> InnerProduct,              W_orbit       -> WeylOrbit,
    KL_poly     -> KazhdanLusztigPolynomial,  W_orbit_graph -> WeylOrbitGraph,
    Lie_code    -> LieCode,                   W_orbit_size  -> WeylOrbitSize,
    Lie_group   -> LieGroup,                  W_order       -> WeylOrder,
    Lie_rank    -> LieRank,                   W_rt_action   -> WeylRootAction,
    long_word   -> LongestWord,               W_rt_orbit    -> WeylRootOrbit,
    l_reduce    -> LeftWeylReduce,            W_word        -> WeylWord
    lr_reduce   -> LeftRightWeylReduce,
