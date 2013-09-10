MathLie
=======

A Mathematica interface for LiE. MathLie needs Mathematica version 7 or higher.

Installation
------------

### Obtaining the package ###

Download the latest release https://github.com/teake/MathLie/releases, unzip it, and move the MathLie
directory to the Mathematica applications directory. The location of this directory can be found by 
typing the command

    FileNameJoin @ {$UserBaseDirectory, "Applications"}

into Mathematica. Alternatively, you can checkout this repository and move the MathLie subdirectory to 
the same Mathematica applications directory.

### Compiling LiE ###

MathLie relies on LiE for all the calculations, so it needs a compiled version of it. 
Download the compile-only version of LiE from http://wwwmathlabo.univ-poitiers.fr/~maavl/LiE/, unzip it, 
and compile the GAP version by doing

    make
    make Liegap.exe
    
from the command line. Note that OS X users will first need to replace the makefile with 
[this file](http://wwwmathlabo.univ-poitiers.fr/~maavl/LiE/Macfile).

After compilation, move the following files to the LiE subdirectory in the MathLie directory:

    Liegap.exe
    Lie.exe
    LEARN.ind
    INFO.ind
    INFO.0
    INFO.1
    INFO.2
    INFO.3
    INFO.4
    
Running the package
-------------------

Once installed, MathLie can be run in Mathematica by typing

    <<MathLie`
    
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

There are also short-hand names for the following LiE commands:

    dim         -> Dim
    tensor      -> LieTensor
    sym_tensor  -> SymTensor

Available short-hands can be looked up with `LookupLieFunction`:

    LookupLieFunction["sym_tensor"]
    (* => SymTensor *)

These short-hand commands are entered as follows:

    LieTensor[{1, 1}, {1, 1}, "A2"]
    (* => LieTerm[0, 0] + LieTerm[0, 3] + 2*LieTerm[1, 1] + LieTerm[2, 2] + LieTerm[3, 0] *)

The `LieTerm` objects print as red parentheses (their color is controlled via `$LieTermColor`), and behave
as terms in Laurent polynomials. They can also be used as input:

    SymTensor[2, LieTerm[1,1], "A2"]
    (* => LieTerm[0, 0] + LieTerm[1, 1] + LieTerm[2, 2] *)


