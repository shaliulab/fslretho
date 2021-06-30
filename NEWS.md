fslretho2 2.1.0
---------------------------------------------------------------------

* New automatically generated fields allow to filter time by hours.
* Behavioral traces use now hours as unit in the x axis.
* New rhythm analysis implemented in the period analysis tab. Periodograms and spectrograms can now be generated with the application.
* New snapshot viewer removes the need to download the dbfile to manually check frames. An "SD only" button makes it easy to look only at SD. Another button automates the generation of an .mp4 video for the selected frames.
* Updated unified download center where all datasets produced by the application can be downloaded in .csv format, including the dbfiles (as a .zip file).
* New annotation functions sd_inprogress_annotation and distance_annotation annotate on the datasets whether SD was active and how much fly the animal traverses in each time bin respectively. More annotation functions can be implemented. More than 1 annotation function can be used simultaneously.
* Aesthetical mappings now get automatic default values, that persist to loading new datasets.
* Downloaded plots now have a proper white background.
* A new raw section makes it possible to plot the raw positional trace (position along the tube) of one animal at a time.
* A new reproducibility section generates an R script that reproduces part of the pipeline implemented in the app
* A new quality control filter called "Pareto rule/principle" can be used to increase the quality of the sleep deprivation labeling. An SD is considered successful not just if the fly moves a few pixels frame to frame, but also the animal must traverse a minimal section of the tube in a minimal amount of time. If it does not, the SD is considered failed. More information under `help(apply_pareto_rule)`

fslretho2 1.0.0
------------------------------------------------------------------

* Fix bug in local esquisse build that prevented correct time filtering
* Fix bug in fslggetho that prevented correct LD annotations
