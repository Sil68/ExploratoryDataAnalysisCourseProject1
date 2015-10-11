# $Id$
# estuciepc.R
# ===========
#
#     estimate the aount of memory required to load and store the read raw 
#     data (electric power consumption imported from the UC Irvine Machine 
#     Learning Repository) into a corresponding data structure
#
#     input value:
#         basedir ----- base directory to read files from
#         fname ------- file to read
#         unts -------- unit of size returned ("b", "k", "m", "g")
#
#     output value:
#         memory size - success; rough estimate of the memory required to store
#                       the raw data
#         NULL -------- failure
#
#     note:
#         :: Both--baseDir and fname--have to be provided, and set to a 
#            non-NULL value;
#         :: "b" = bytes; "k" = kilo bytes, "m" = mega bytes, "g" = giga bytes.
#
# Copyright (c) Martin HEIN (m#)/October 2015
#
#     $Log$
#

estUCIepc <- function(baseDir=file.path(inDir), fname=NULL, unts="b") {
    
    # local variable declaration
    rc <- NULL
    rcRows <- NULL
    tmpDT <- NULL
    tmpSize <- NULL
    tmpRows <- 100
    tmpUnits <- c("b", "k", "m", "g")
    tmpUnitFac <- c(1, 1024, 1024^2, 1024^3)
    
    # estimate size of raw data
    if (all((!is.null(baseDir)), (!is.null(fname)))) {
        tmpDT <- try(fread(file.path(baseDir, fname), nrows=tmpRows, 
                           na.strings=c("?", "NA", "N/A")), silent=TRUE)
        
        if (class(tmpDT)[1] != "try-error") {
            tmpSize <- object.size(tmpDT)
            tmpRows <- if (nrow(tmpDT) < tmpRows) nrow(tmpDT) else tmpRows
            
            rcRows <- as.numeric(gsub("[^0-9]", "", 
                                      system(paste('"', cmdWC, '"', " -l ", 
                                                   file.path(baseDir, fname), 
                                                   sep=""), 
                                             intern=T)))
            rc <- tmpSize * rcRows / tmpRows
            
            # convert to requested unit
            if (unts %in% tmpUnits) {
                rc <- ceiling(rc / tmpUnitFac[which(tmpUnits %in% unts)])
            } # if
        } # if
    } # if
    
    # return processing result
    as.numeric(rc)
} # estUCIepc

#
# end of file
#