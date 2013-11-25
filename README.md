MathSAT ML
==========

Bindings for [*MathSat solver*](http://mathsat.fbk.eu/) for the OCaml language.
Currently not much functionality is implemented, no point in using them.

Dependencies
------------
* [*OCaml*](http://ocaml.org/install.html) version 4.0.0 or newer 
* [*Ctypes*](https://github.com/ocamllabs/ocaml-ctypes) or via OPAM [here](http://opam.ocaml.org/pkg/ctypes/0.2.2/)  
* [*MathSAT*](http://mathsat.fbk.eu/download.html) version 5.2.10.


To compile and use the bindings:
----------------------------------

Due to some issues with static linking C libraries with OCaml programs using
Ctypes I had to create a dynamic library out of MathSAT's OSX binaries.

        $ cd mathsat/lib
        $ ar -x libmathsat.a
        $ g++ -dynamiclib -lc++ /opt/local/lib/libgmp.dylib *.o -o mathsat.dylib

You can then use the resulting mathsat.dylib file to link with the OCaml
bindings.

	$ ocamlfind ocamlopt -c -package ctypes.foreign -o mathsat.cmx mathsat.ml 
	$ ocamlfind ocamlopt -cclib 'mathsat.dylib' -linkpkg -package ctypes.foreign -o demo.native mathsat.cmx demo.cmx

Future Plans
-------------

* A makefile :D and support for ocamlfind
* The bindings are currently (very) incomplete but I will be slowly adding to
them. Ctypes makes FFI very simply, so if you are in a rush you can always do
it.
