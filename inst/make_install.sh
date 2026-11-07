#! /bin/bash
# Install fslretho and its rethomics dependencies from the pinned git submodules.
# Run from the repository root. To move a dependency to a newer commit, update
# the submodule (and the matching Remotes entry in DESCRIPTION) and commit both.

set -e

# install order matters: later packages import earlier ones
SUBMODULES=("behavr" "zeitgebr" "ggetho" "damr" "scopr" "sleepr" "esquisse")

git submodule update --init

for SUBMOD in ${SUBMODULES[*]}
do
    CMD="R -e \"devtools::install('inst/dependencies/${SUBMOD}', dependencies = FALSE, upgrade = 'never')\""
    echo $CMD
    eval $CMD
done

R -e "devtools::install('.', upgrade = 'never')"
