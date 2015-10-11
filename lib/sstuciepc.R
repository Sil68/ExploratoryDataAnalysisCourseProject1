# $Id$
# sstuciepc.R
# ===========
#
#     extract a subset of the read raw data (electric power consumption 
#     imported from the UC Irvine Machine Learning Repository)
#
#     input value:
#         dtAll ------- data table containing the original data to be 
#                       subsetted
#         fromDate ---- subset dtAll with Date >= fromDate
#         toDate ------ subset dtAll with Date <= toDate
#
#     output value:
#         data table -- success; data table containing the requested subset of 
#                       the raw data
#         NULL -------- failure
#
#     note:
#         :: all of dtAll, fromDate, toDate have to be provided, and set to a 
#            non-NULL value;
#         :: fromDate and toDate are getting swapped in case fromDate is
#            greater/later than toDate;
#         :: fromDate and toDate have to be provided in the format
#            <year><month><day> (with or without separators in between)
#
# Copyright (c) Martin HEIN (m#)/October 2015
#
#     $Log$
#

subsetUCIepc <- function(dtAll=NULL, fromDate=NULL, toDate=NULL) {
    
    # local variable declaration
    rc <- NULL
    tmpDate <- NULL
    tmpFrom <- NULL
    tmpTo <- NULL
    
    # subset raw data
    if (all((!is.null(dtAll)), (!is.null(fromDate)), (!is.null(toDate)))) {
        tmpFrom <- ymd(fromDate)
        tmpTo <- ymd(toDate)
        
        # check subset date boundaries
        if (tmpFrom > tmpTo) {
            tmpDate <- toDate
            tmpTo <- tmpFrom
            tmpFrom <- tmpDate
        } # if
        
        # extract subset
        rc <- dtAll[(Date >= tmpFrom) & (Date <= tmpTo)]
    } # if
    
    # return processing result
    rc
} # subsetUCIepc

#
# end of file
#