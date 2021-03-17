###########################################
# Code by Brendon Mizener
# Saturday, Feb. 20, 2021
# my first function ever
###########################################
#
#  This function is intended used in conjunction with PTCA4CATA & one of ExPosistion - type packages
#  The goal is: if you have a crazy number of variables and you want to display only the ones that load or contribute significantly, 
#  you input that information into this function and you get a list of variables that you can then input into the PrettyBarPlot function
#  
# input - 
# one or two vectors or matrices of loadings or contributions, etc, such as output from epPCA or similar
# set of colors equal to the number of matrices to be plotted x 2
# desired % of contributions to be displayed; can be either a numeric value 1:100, indicating percent, or the word "mean"
# type of input: can be either "load" for loadings or "cont" for contributions
#

# output - Variables to input into the pretty bar plot function: 
# 1) set of 'significant' loadings or contributions, 
#     a) for the first extracted dimensions
#     b) for the second extracted dimensions
#     c) y limits to accompany those for plotting for each of the dimensions
# 2) set of colors
#     a) for all values in the matrix of loadings or contributions
#     b) for the specific values you want to plot
#     c) colors that are used
# 3) thresholds
#     a) for plotting the threshold line
#     b) to see what value is wanted for significance for each matrix/dimension
# 
#########################################################
#  still needs: threshold for pretty bar plot (for line), y limits (positive & negative)
#                 figure out how to handle the threshold for contributions.  
#

barplot.sigonly <- function(x, 
                            y, 
                            th, 
                            type = c("load", "cont"), 
                            dims = 2, 
                            colforthebars = NULL){
   
   if(is.null(dim(x)) && is.null(length(x))){return("Please enter either a vector, matrix, or dataframe")
   }else if(is.null(dim(x)) && length(x) > 1){
      dims = 1
      print("Vector as input, returning only one dimension.")
   }
   
   if(dims > 8){stop("Do you really need to plot that many dimensions? Select a number less than 8")}
   
   dimsvec <- c("Dim1", "Dim2", "Dim3", "Dim4", "Dim5", "Dim6", "Dim7", "Dim8") 
   
   if(is.null(y)){
      ########################################### BEGIN 1 MATRIX CONTRIBUTIONS
      if(type == "cont" && th == "mean"){
         if(sum(x[,1]) == 1){
            thr <- 1/nrow(x[,1])
         }
         else if(sum(x[,1]) == 100){
            thr <- 100/nrow(x[,1])
         } 
         xslist <- vector(mode = "list", length = 3)
         names(xslist)[1:dims] <- dimsvec[1:dims]
         names(xslist)[dims+1] <- "ylimits"
         
         ylimits <- vector(mode = list, length = dims)
         names(ylimits) <- dimsvec[1:dims]
         
         x.names <- rownames(x)
         colors4bars <- vector(mode = "list", length = 3)
         names(colors4bars) <- c("colors", "x.all","x.vec")
         
         colors4bars$colors <- wes_palettes$Darjeeling1[c(1,2)]
         
         for (i in 1:dims){
            xslist[[i]] <- x[which(abs(x[,i]) > thr),i]
            names(xlist[[i]]) <- x.names[abs(x[,i]) > thr]
         }
         
         colors4bars$x.all <- x
         colors4bars$x.all[colors4bars$x.all > 0] <- colors4bars$colors[1]
         colors4bars$x.all[colors4bars$x.all != colors4bars$colors[1]] <- colors4bars$colors[2]
         
         colors4bars$x.vec <- vector(mode = "list", length = 2)
         
         names(colors4bars$x.vec) <- names(xslist)
         
         for (i in 1:dims){
            colors4bars$x.vec[[i]] <- colors4bars$x.all[which(abs(x[,i]) > thr),i]
         }
         
            for (i in 1:dims){
               if(min(xslist[[i]]) < 0){
            
                  ylimits[[i]] <- c(1.2*min(xslist[[i]]), 1.2*max(xslist[[i]]))
            
               }else ylimits[[i]] <- NULL
            }
         
         xslist$ylimits <- ylimits
         
         bp.sig <- list("data" = xslist, 
                        "colors" = colors4bars, 
                        "t.hold" = thr)
         
         return(bp.sig)
         
         
         
         
      } else {thr <- th/100}
      ############################################ BEGIN 1 MATRIX LOADINGS
      if(type == "load"){
      
         xslist <- vector(mode = "list", length = dims+1)
         names(xslist)[1:dims] <- dimsvec[1:dims]
         names(xslist)[dims+1] <- "ylimits"
         
         ylimits <- vector(mode = list, length = dims)
         names(ylimits) <- dimsvec[1:dims]
      x.names <- rownames(x)
      colors4bars <- vector(mode = "list", length = 3)
      names(colors4bars) <- c("colors", "x.all","x.vec")
      
      colors4bars$colors <- wes_palettes$Darjeeling1[c(1,2)]
      
      for (i in 1:dims){
         xslist[[i]] <- x[which(abs(x[,i]) > thr*max(abs(x[,i]))),i]
         names(xlist[[i]]) <- x.names[abs(x[,i]) > thr*max(abs(x[,i]))]
      }
      
      colors4bars$x.all <- x
      colors4bars$x.all[colors4bars$x.all > 0] <- colors4bars$colors[1]
      colors4bars$x.all[colors4bars$x.all != colors4bars$colors[1]] <- colors4bars$colors[2]
      
      colors4bars$x.vec <- vector(mode = "list", length = 2)
      
      names(colors4bars$x.vec) <- names(xslist)
      
      for (i in 1:dims){
         colors4bars$x.vec[[i]] <- colors4bars$x.all[which(abs(x[,i]) > thr*max(abs(x[,i]))),i]
      }
      
      for (i in 1:dims){
         if(min(xslist[[i]]) < 0){
            
            ylimits[[i]] <- c(1.2*min(xslist[[i]]), 1.2*max(xslist[[i]]))
            
         }else ylimits[[i]] <- NULL
      }
      
      xslist$ylimits <- ylimits
      
      
      bp.sig <- list("data" = xslist, "colors" = colors4bars, "t.hold" = thr)
      
      return(bp.sig)
      }
      
      
      
      
      
   }
   ########################################## BEGIN 2 MATRIX CONTRIBUTIONS 
   if(type == "cont" && th == "mean"){
      thold.list <- vector(mode = "list", length = 3)
      names(thold.list) <- c("thr.x", "thr.y", "thr.tog")
      
      
      if(all.equal(sum(abs(x[,1])), 1)){
         thold.list$thr.x <- 1/nrow(x)
         thold.list$thr.y <- 1/nrow(y)
         thold.list$thr.tog <- min(thold.list$thr.x, thold.list$thr.y)
      }else if (all.equal(sum(abs(x[,1])), 100)){
         thold.list$thr.x <- 100/nrow(x)
         thold.list$thr.y <- 100/nrow(y)
         thold.list$thr.tog <- min(thold.list$thr.x, thold.list$thr.y)
      }   
      
      thr.x <- thold.list$thr.x
      thr.y <- thold.list$thr.y
      xslist <- vector(mode = "list", length = dims+1)
      yslist <- vector(mode = "list", length = dims+1)
      
      ylimits.x <- vector(mode = "list", length = dims)
      names(ylimits.x) <- dimsvec[1:dims]
      
      ylimits.y <- vector(mode = "list", length = dims)
      names(ylimits.y) <- dimsvec[1:dims]
      
      ylimits.tog <- vector(mode = "list", length = dims)
      names(ylimits.tog) <- dimsvec[1:dims]
      
      names(xslist)[1:dims] <- dimsvec[1:dims]
      names(xslist)[dims+1] <- "ylimits.x"
      names(yslist)[1:dims] <- dimsvec[1:dims]
      names(yslist)[dims+1] <- "ylimits.y"
      
      x.names <- rownames(x)
      y.names <- rownames(y)
      
      for (i in 1:dims){
         
         xslist[[i]] <- x[which(abs(x[,i]) > thr.x),i]
         yslist[[i]] <- y[which(abs(y[,i]) > thr.y),i]
         names(xslist[[i]]) <- x.names[abs(x[,i]) > thr.x]
         names(yslist[[i]]) <- y.names[abs(y[,i]) > thr.y]
      }
      
      colors4bars <- vector(mode = "list", length = 5)
      names(colors4bars) <- c("colors", "x.all","y.all","x.vec", "y.vec" )
      
      colors4bars$colors <- c(wes_palettes$Darjeeling1[c(1,2,4)],
                              wes_palettes$Darjeeling2[2])
      
      colors4bars$x.all <- x
      colors4bars$x.all[colors4bars$x.all > 0] <- colors4bars$colors[1]
      colors4bars$x.all[colors4bars$x.all != colors4bars$colors[1]] <- colors4bars$colors[2]
      
      colors4bars$y.all <- y
      colors4bars$y.all[colors4bars$y.all > 0] <- colors4bars$colors[3]
      colors4bars$y.all[colors4bars$y.all != colors4bars$colors[3]] <- colors4bars$colors[4]
      
      colors4bars$x.vec <- vector(mode = "list", length = dims)
      colors4bars$y.vec <- vector(mode = "list", length = dims)
      
      names(colors4bars$x.vec) <- dimsvec[1:dims]
      names(colors4bars$y.vec) <- dimsvec[1:dims]
      
      
      for (i in 1:2){
         colors4bars$x.vec[[i]] <- colors4bars$x.all[which(abs(x[,i]) > thr.x),i]
         colors4bars$y.vec[[i]] <- colors4bars$y.all[which(abs(y[,i]) > thr.y),i]
      }
      
      barsdata <- vector(mode = "list", length = dims+1)
      
      for (i in 1:dims){
         barsdata[[i]] <- c(xslist[[i]], yslist[[i]])
      }
      
      for (i in 1:dims){
         if(min(xslist[[i]]) < 0){
            
            ylimits.x[[i]] <- c(1.2*min(xslist[[i]]), 1.2*max(xslist[[i]]))
            ylimits.y[[i]] <- c(1.2*min(yslist[[i]]), 1.2*max(yslist[[i]]))
            ylimits.tog[[i]] <- c(1.2*min(c(xslist[[i]], yslist[[i]])), 1.2*max(c(xslist[[i]], yslist[[i]])))
            
         }else ylimits.x[[i]] <- ylimits.y[[i]] <- NULL
         
      }
      
      y.limits <- list("ylims.x" = ylimits.x, "ylims.y" = ylimits.y, "ylims.tog" = ylimits.tog)
      
      barsdata[[dims+1]] <- y.limits
      
      names(barsdata)[1:dims] <- dimsvec[1:dims]
      names(barsdata)[dims+1] <- "y.limits" 
      
      bp.sig <- list("data" = barsdata, "color" = colors4bars, "t.hold" = thold.list)
      
      return(bp.sig)
      
   } else {thr <- th/100}
   ##################################################### BEGIN 2 MATRIX LOADINGS
   if(type == "load"){
   xslist <- vector(mode = "list", length = dims+1)
   yslist <- vector(mode = "list", length = dims+1)
   
   ylimits.x <- vector(mode = "list", length = dims)
   names(ylimits.x) <- dimsvec[1:dims]
   
   ylimits.y <- vector(mode = "list", length = dims)
   names(ylimits.y) <- dimsvec[1:dims]
   
   ylimits.tog <- vector(mode = "list", length = dims)
   names(ylimits.tog) <- dimsvec[1:dims]
   
   names(xslist)[1:dims] <- dimsvec[1:dims]
   names(xslist)[dims+1] <- "ylimits.x"
   names(yslist)[1:dims] <- dimsvec[1:dims]
   names(yslist)[dims+1] <- "ylimits.y"
   
   x.names <- rownames(x)
   y.names <- rownames(y)
   
   for (i in 1:dims){
      
      xslist[[i]] <- x[which(abs(x[,i]) > thr*max(abs(x[,i]))),i]
      yslist[[i]] <- y[which(abs(y[,i]) > thr*max(abs(y[,i]))),i]
      names(xslist[[i]]) <- x.names[abs(x[,i]) > thr*max(abs(x[,i]))]
      names(yslist[[i]]) <- y.names[abs(y[,i]) > thr*max(abs(y[,i]))]
   }
   
   colors4bars <- vector(mode = "list", length = 5)
   names(colors4bars) <- c("colors", "x.all","y.all","x.vec", "y.vec" )
   
   colors4bars$colors <- c(wes_palettes$Darjeeling1[c(1,2,4)],
                           wes_palettes$Darjeeling2[2])
   
   colors4bars$x.all <- x
   colors4bars$x.all[colors4bars$x.all > 0] <- colors4bars$colors[1]
   colors4bars$x.all[colors4bars$x.all != colors4bars$colors[1]] <- colors4bars$colors[2]
   
   colors4bars$y.all <- y
   colors4bars$y.all[colors4bars$y.all > 0] <- colors4bars$colors[3]
   colors4bars$y.all[colors4bars$y.all != colors4bars$colors[3]] <- colors4bars$colors[4]
   
   colors4bars$x.vec <- vector(mode = "list", length = dims)
   colors4bars$y.vec <- vector(mode = "list", length = dims)
   
   names(colors4bars$x.vec) <- dimsvec[1:dims]
   names(colors4bars$y.vec) <- dimsvec[1:dims]
   
   
   for (i in 1:2){
      colors4bars$x.vec[[i]] <- colors4bars$x.all[which(abs(x[,i]) > thr*max(abs(x[,i]))),i]
      colors4bars$y.vec[[i]] <- colors4bars$y.all[which(abs(y[,i]) > thr*max(abs(y[,i]))),i]
   }
   
   barsdata <- vector(mode = "list", length = dims+1)
   
   for (i in 1:dims){
      barsdata[[i]] <- c(xslist[[i]], yslist[[i]])
   }
   
   thold.list <- vector(mode = "list", length = 3)
   names(thold.list) <- c("x.threshold", "y.threshold", "together")
   
   tholdimnames <- vector(mode = "list", length = 2)
   tholdimnames[[1]] <- dimsvec[1:dims]
   tholdimnames[[2]] <- "Threshold by dimension"
   
   thold.list$x.threshold <- as.data.frame(matrix(nrow = dims, dimnames = tholdimnames))
   thold.list$y.threshold <- as.data.frame(matrix(nrow = dims, dimnames = tholdimnames))
   thold.list$together <- as.data.frame(matrix(nrow = dims, dimnames = tholdimnames))
   
   for(i in 1:dims){
      thold.list$x.threshold[i,] <- thr*max(abs(x[,i]))
      thold.list$y.threshold[i,] <- thr*max(abs(y[,i]))
      thold.list$together[i,] <- min(c(thold.list$x.threshold[i,], thold.list$y.threshold[i,]))
      
   }

   for (i in 1:dims){
      if(min(xslist[[i]]) < 0){
         
         ylimits.x[[i]] <- c(1.2*min(xslist[[i]]), 1.2*max(xslist[[i]]))
         ylimits.y[[i]] <- c(1.2*min(yslist[[i]]), 1.2*max(yslist[[i]]))
         ylimits.tog[[i]] <- c(1.2*min(c(xslist[[i]], yslist[[i]])), 1.2*max(c(xslist[[i]], yslist[[i]])))
         
      }else c(ylimits.x[[i]],ylimits.y[[i]]) <- NULL
             
   }
   
   y.limits <- list("ylims.x" = ylimits.x, "ylims.y" = ylimits.y, "ylims.tog" = ylimits.tog)
   
   barsdata[[dims+1]] <- y.limits
   
   names(barsdata)[1:dims] <- dimsvec[1:dims]
   names(barsdata)[dims+1] <- "y.limits" 
   
   bp.sig <- list("data" = barsdata, "color" = colors4bars, "t.hold" = thold.list)
   }
}
