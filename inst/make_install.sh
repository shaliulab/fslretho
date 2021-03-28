#! /bin/bash

SUBMODULES=("fslbehavr" "fsldamr" "fslscopr" "fslsleepr" "fslggetho" "esquisse")

git submodule > downloaded_submods.txt

for SUBMOD in ${SUBMODULES[*]}
do
    printf "Finding $SUBMOD ..."
    HIT=$(grep $SUBMOD downloaded_submods.txt | grep $SUBMOD | wc -l)
    #echo "$SUBMOD: $HIT"
    printf "$HIT\n"
    if [ $HIT -eq 0 ]
    then
        echo "Downloading $SUBMOD"
        git submodule add git@github.com:shaliulab/$SUBMOD inst/dependencies/$SUBMOD
    fi
done

git submodule update --init --recursive
git submodule foreach "(git checkout deployment; git pull)"


for SUBMOD in ${SUBMODULES[*]}
do
    if [ $HIT -eq 0 ]
    then
	    echo "One submodule is not installed!"
	    exit 1
    fi
done


for SUBMOD in ${SUBMODULES[*]}
do
    CMD="R -e \"devtools::install('inst/dependencies/${SUBMOD}')\""
    echo $CMD
    eval $CMD
done

