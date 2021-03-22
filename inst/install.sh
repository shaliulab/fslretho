#! /bin/bash


#git clone git@github.com:shaliulab/fslbehavr
#cd fslbehavr
#git checkout deployment
#git submodule update --init
#R -e "devtools::install()"
#cd ..
#
#git clone git@github.com:shaliulab/fsldamr
#cd fsldamr 
#git checkout deployment
#git submodule update --init
#R -e "devtools::install()"
#cd ..
#
#
#git clone git@github.com:shaliulab/fslscopr
#cd fslscopr 
#git checkout deployment
#git submodule update --init
#R -e "devtools::install()"
#cd ..
#
#
#git clone git@github.com:shaliulab/fslsleepr
#cd fslsleepr 
#git checkout deployment
#git submodule update --init
#R -e "devtools::install()"
#cd ..
#
#
#git clone git@github.com:shaliulab/fslggetho
#cd fslggetho
#git checkout deployment
#git submodule update --init
#R -e "devtools::install()"
#cd ..
#
#
#git clone git@github.com:shaliulab/esquisse
cd esquisse
git checkout master
R -e "devtools::install()"
cd ..
