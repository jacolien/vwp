#' Defining timebins.
#' 
#' @description Function for calculating timebins.
#' @param x Numerical vector with timestamp information.
#' @param binsize Size of the timebin, measured in the same units (often ms) 
#' as \code{x}.
#' @param pos Numerical value that determines the label of the binsize 
#' as proportion of the binsize. A value of 0 will provide the minimum 
#' timestamp within the bin as label, a value of 1 will provide the maximum 
#' value within the bin as label. Defaults to 0.5, the center of the bin.
#' @return Anumerical vector of the same size as \code{x} with timebin 
#' information.
#' @author Jacolien van Rij
#' @family Functions for gaze data
#' @seealso \code{\link{getCountData}}

timeBins <- function(x, binsize, pos=.5){
  return( ( floor( x / binsize )+ pos ) * binsize ) 
}

#' Utility function.
#' 
#' @description Function uses \code{\link[base]{table}} with factors
#' to count the occurrances of each value in the predictor.
#' @param x A vector to be counted.
#' @param values A vector with all possible group names.
#' @param incl_na Logical: whether or not to return a count of missing values.
#' @return A table with count information.
#' @section Note:
#' Values that are not specified in \code{values} will be ignored.
#' @author Jacolien van Rij
#' @seealso \code{\link[base]{table}}
#' @family Functions for binomial count data.

countValues <- function(x, values, incl_na=FALSE){
    counts <- factor(x, levels=values)
    return( table(counts, 
        useNA=ifelse(incl_na==TRUE, "always", "no"), 
        dnn=list(deparse(substitute(x)))) )
    
}



#' Reducing fixations to count data.
#' 
#' @description Function uses \code{\link[base]{table}} with factors
#' to count the occurrances of each value in the predictor.
#' @param x Name of a column in \code{data} or vector with values 
#' to be counted.
#' @param data Optional: data frame.
#' @param split_by Vector with column names in \code{data} or named list with 
#' grouping predictors.
#' @param values Values of \code{x} that should be counted. 
#' If NULL (default) all values are counted.
#' @param incl_na Logical: whether or not to return a count of missing values.
#' @return A data frame with cbinded count information.
#' @section Note:
#' Values that are not specified in \code{values} will be ignored.
#' @seealso \code{\link[base]{table}}
#' @family Functions for gaze data
#' @author Jacolien van Rij
#' @examples
#' # simulate some gaze data:
#' dat <- data.frame(
#'  Subject = rep(1:3, 500),
#'  Timestamp = rep(1:500, 3),
#'  AOI = rep( rep( c('other','competitor', 'target'), 3), 
#'  c(184, 172, 144, 51, 264, 185, 127, 2, 371)) )
#' # add missing data:
#' dat[sample(nrow(dat), size=15),]$AOI <- NA
#'
#' # add timebins:
#' dat$Timebin <- timeBins(dat$Timestamp, 100)
#'
#' # calculate counts:
#' c1 <- getCountData('AOI', data=dat, split_by=c('Subject', 'Timebin'))
#' head(c1)
#' # calculating proportions:
#' c1$prop <- c1$AOI[,'target'] / ( c1$AOI[,'competitor']+c1$AOI[,'other'])
#'
#' # calculating counts for specific values, including missing data.
#' # Note that 'distractor' is not in the data:
#' c2 <- getCountData('AOI', data=dat, split_by=c('Subject', 'Timebin'),
#' values=c('target', 'distractor', 'competitor', 'other'), incl_na=TRUE)
#' head(c2)

getCountData <- function(x, split_by, data=NULL, values=NULL, 
    incl_na=FALSE){
    
    dat <- c()
    if(is.null(data)){
        dat <- data.frame(x=x)
        if(is.null( names(split_by) ) ){
            warning('Split_by is a not a named list. Names are given automatically.')
            names(split_by) <- paste('V', 1:length(split_by), sep='')
        }
        for(i in names(split_by)){
            dat[,i] <- split_by[[i]]
        }
    }else{
        split_by <- unlist(split_by)
        cn <- colnames(data)
        if(! x %in% cn){
            stop(sprintf('Column %s not found in data %s.', 
                deparse(substitute(x)),
                deparse(substitute(data))))
        }
        el <- which(!split_by %in% cn)
        if( length(el)> 0){
            stop(sprintf('Column(s) %s not found in data %s.', 
                paste(split_by[el], collapse=', '),
                deparse(substitute(data))))
        }
        dat <- data.frame( data[,c(x, split_by)], row.names=NULL)
        names(dat)[names(dat)==x] <- 'x'
    } 

    if(is.null(values)){
        values <- sort(unique(dat$x))
    }

    out <- values
    if(incl_na){
        out <- c(out, NA)
    }

    split <- list()
    if(length(split_by)>1){
        split <-  as.list(dat[,colnames(dat)!='x'])
    }else{
        split <-  list(dat[,colnames(dat)!='x'])
    }

    # To avoid dropping levels, treat NA in predictors as category:
    split <- lapply(split, function(x){factor(as.character(x), exclude="")})

    ## make more efficient with dplyr or data.table
    newd <- aggregate(dat$x, by=split, 
        function(x){unlist(countValues(x,values=values, incl_na=incl_na))})

    ## Convert numeric split predictors back to numeric:
    for(i in names(split)){
        if(is.numeric(dat[,i])){
            newd[,i] <- as.numeric(as.character(newd[,i]))
        }
    }
    
    colnames(newd)[colnames(newd)=='x'] <- deparse(substitute(x))
    colnames(newd) <- gsub('"','', colnames(newd))
    return(newd)
}
