#
# Some generally useful but really simple things I forget about in R
# Which don't belong in any other real category
#
options(scipen = 999) # Mostly disable scientific notation in display and file writes
options(stringsAsFactors=F)   # Disable conversion of strings to factors on read.  Sometimes useful.

rm(list=ls());  # WARNING!  this is effectively rm -r /*  !  Destroy everything!  Use with care!

gc() ; Sys.time() ; start_time <- Sys.time()  # Run this at the beginning
gc() ; Sys.time() - start_time  # And run this at every useful checkpoint afterwards to do garbage collection and track time

