# $Id$
# rduciepc.R
# ==========
#
#     read raw data (electric power consumption imported from the UC Irvine 
#     Machine Learning Repository) into a corresponding data structure
#
#     input value:
#         basedir ----- base directory to read files from
#         fname ------- files to read
#
#     output value:
#         data table -- success; data table containing the read raw data
#         NULL -------- failure
#
#     note:
#         Both--baseDir and fname--have to be provided, and set to a 
#         non-NULL value.
#
# Copyright (c) Martin HEIN (m#)/October 2015
#
#     $Log$
#

rdUCIepc <- function(baseDir=file.path(inDir), fname=NULL) {
    
    # local variable declaration
    rc <- NULL
    
    # read raw data
    if (all((!is.null(baseDir)), (!is.null(fname)))) {
        rc <- try(fread(file.path(baseDir, fname), 
                        na.strings=c("?", "NA", "N/A")), silent=TRUE)
        
        # successfully read raw data, set variable/column classes
        if (class(rc)[1] != "try-error") {
            rc[, DateTime := dmy_hms(paste(Date, Time))]
            rc[, Date := dmy(Date)]
            
            setkeyv(rc, c("Date", "Time"))
            
        # failed to read raw data
        } else {
            rc <- NULL
        } # if
    } # if
    
    # return processing result
    rc
} # rdUCIepc

#
# end of file
#