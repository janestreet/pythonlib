LIBS="core/heap_block base_bigstring core base base/base_internalhash_types
ppx_expect/collector time_now bin_prot pyml stdcompat"

OPAM_LIBS="$OPAM_SWITCH_PREFIX/lib"


for l in $LIBS
do
	export LIBRARY_PATH="$OPAM_LIBS/$l:$LIBRARY_PATH"
done

dune build $1
