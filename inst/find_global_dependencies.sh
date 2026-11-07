#! /bin/bash
# Collect the CRAN packages imported by the rethomics dependencies.
# Usage: find_global_dependencies.sh [DEPENDENCIES_DIR]   (default: /opt)
# Writes DEPENDENCIES_DIR/unique_dependencies.txt

set -e
cd "${1:-/opt}"
PKGS=("behavr" "damr" "scopr" "sleepr" "ggetho" "zeitgebr" "esquisse")
: > dependencies.txt
for PKG in ${PKGS[*]}
do
    awk '/^Imports:/ {p=1; next}; /^[A-Za-z@]+:/ {p=0}; p' $PKG/DESCRIPTION >> dependencies.txt
done

# drop version constraints and the local packages themselves (not on CRAN)
LOCAL=$(IFS='|'; echo "${PKGS[*]}")
cat dependencies.txt | tr -d ',' | tr -d " " | sed 's/(.*$//' | grep -v '^$' \
    | grep -vxE "$LOCAL" | sort | uniq > unique_dependencies.txt

cd - > /dev/null
