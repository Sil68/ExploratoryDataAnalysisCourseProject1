# $Id$
# plot3.R
# =======
#
#     plot the data provided both to the screen as well as a file
#     (format png) by facilitating the R base plot system only
#
#     plot specification:
#         :: "time series";
#         :: date & time of day (x-axis);
#         :: sub metering 1, sub metering 2, sub metering 3 (y-axis).
#
#     input value:
#         dtPlt ------- data table containing the data to plot
#         fname ------- name of the file the plot has to sent to
#         imgSize ----- size/dimension (<pixels height>x<pixels width>x
#                       <resolution>) of the plot generated and written to file
#
#     output value:
#         TRUE -------- success; data have been plot both the screen as well
#                       as the file
#         FALSE ------- failure
#
#     note:
#         :: all of dtPlt, fname, and imgSize have to be provided, and set to 
#            a non-NULL value.
#
# Copyright (c) Martin HEIN (m#)/October 2015
#
#     $Log$
#

plot3 <- function(dtPlt=NULL, fname=NULL, imgSize=NULL) {
    
    # local variable declaration
    rc <- FALSE
    tmpfname <- file.path(fname)
    ofinf <- NULL
    nfinf <- NULL
    pltDims <- NULL
    pltWidth <- NULL
    pltHeight <- NULL
    pltRes <- NULL
    
    # plot data to screen and file
    if (all((!is.null(dtPlt)), (!is.null(fname)), (!is.null(imgSize)))) {
        
        # check if output file is already existing
        if (file.exists(tmpfname)) {
            ofinf <- file.info(tmpfname)
        } # if
        
        # split image size into individual components (height, width, 
        # resolution)
        pltDims <- strsplit(imgSize, "x", fixed=TRUE)[[1]]
        
        if (length(pltDims) == 3) {
            pltWidth <- as.numeric(pltDims[1])
            pltHeight <- as.numeric(pltDims[2])
            pltRes <- as.numeric(pltDims[3])
        } # if
        
        # proceed only in case of valid dimensions provided
        if (all(!is.na(pltWidth), !is.na(pltHeight), !is.na(pltRes))) {
            
            # plotting function
            pltData <- function(dtPlt, xfs=1, yfs=1, tfs=1, lfs=1, lwd=1) {
                opar <- par(no.readonly = TRUE)
                
                # adjust the graphical parameters
                par(mgp=c(2, 0.5, 0), mar=c(2, 3, 1, 1))
                
                # generate plot
                with(dtPlt, {
                    plot(DateTime, Sub_metering_1, 
                         col="black", type="l", lwd=lwd,
                         xlab="", ylab="", main="", 
                         ps=20, cex.axis=0.6, cex.lab=0.8, cex.main=1)
                    points(DateTime, Sub_metering_2, 
                           col="red", type="l", lwd=lwd)
                    points(DateTime, Sub_metering_3, 
                           col="blue", type="l", lwd=lwd)
                })
                mtext("Energy sub metering", side=2, line=2, cex=yfs)
                legend("topright", 
                       legend=c("Sub_metering_1", "Sub_metering_2", 
                                "Sub_metering_3"),
                       col=c("black", "red", "blue"),
                       lty=c(1, 1, 1), lwd=c(lwd, lwd, lwd),
                       cex=lfs)
                
                # restore original par settings
                par(opar)
            } # pltData
            
            # plot to screen
            pltData(dtPlt)
            
            # plot to file
            png(paste(tmpfname, sep=""), 
                width=pltWidth, height= pltHeight, res=pltRes, 
                bg="transparent")
            pltData(dtPlt, 0.5, 0.5, 0.6, 0.35, 0.2)
            dev.off()
            
            # check if output file has been written
            if (file.exists(tmpfname)) {
                nfinf <- file.info(tmpfname)
            } # if
            
            rc <- all(!identical(ofinf, nfinf), !is.null(nfinf))
        } # if
    } # if
    
    # return processing result
    rc
} # plot3

#
# end of file
#