#! /bin/bash

for PKG in fslbehavr fsldamr fslscopr fslsleepr fslggetho esquisse
do
  R -e "devtools::document('$PKG')"
  R -e "devtools::install('$PKG')"
done
