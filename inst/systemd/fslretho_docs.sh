#! /bin/bash

kill `netstat -tlpn | grep 3839 | awk '{print $7}' | cut -f 1 -d/`
kill `netstat -tlpn | grep 4041 | awk '{print $7}' | cut -f 1 -d/`
Rscript /opt/fslretho/inst/app/docs.R --port 4838 &&
Rscript /opt/fslretho/inst/app/docs.R --port 5040




