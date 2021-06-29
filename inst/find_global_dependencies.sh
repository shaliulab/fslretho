#! /bin/bash

cd /opt
touch dependencies.txt
PKGS=("behavr" "damr" "scopr" "sleepr" "ggetho" "zeitgebr" "esquisse")
for PKG in ${PKGS[*]}
do
    awk '/Imports:/ {p=1}; p; /Suggests/ {p=0}'  $PKG/DESCRIPTION | grep -v Imports | grep -v Suggests:  >> dependencies.txt
done


cat dependencies.txt | tr -d ',' | tr -d " " | sed 's/(.*$//' | sort | uniq > unique_dependencies.txt


cd - 
