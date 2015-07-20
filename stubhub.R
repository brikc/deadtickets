

stubhub <- function(eventid) {
    library(httr)
    library(jsonlite)
    library(dplyr)
    
    setwd("C:/Users/lilblue/Documents/stubhub")
    
    # format: "03-01-2015_14.38.12"
    timestamp<-format(Sys.time(), "%m-%d-%Y_%H.%M.%S")
    
    reqid <- paste("https://api.stubhub.com/search/inventory/v1?eventid=",eventid,"&rows=10000",sep="")
    
    ##  GET request with token added to it, asking for 10K rows to get them all
    req <- GET(reqid, add_headers(Authorization="Bearer qLQMB2RQ9WYtCflHBhIj805Pe1ka"))
    
    ## convert to data frame
    reqcontent = content(req)
    reqJSON = jsonlite::fromJSON(toJSON(reqcontent))
    summaryInfo <-data.frame(reqJSON[3],reqJSON[4],reqJSON[5],reqJSON[6],reqJSON[7])
    summaryInfo$eventid<-eventid
    summaryInfo$timestamp <- timestamp
    
    ## write summary info to the table
    #write.table(summaryInfo,file=paste("summary_",eventid,"_",timestamp,".txt",sep=""),row.names=FALSE)
    write.table(summaryInfo,file="summary.txt",append=TRUE,row.names=FALSE,col.names=FALSE)
        
    # get the listings object
    tix = reqJSON$listing
        
    tixdf <- as.data.frame(do.call(cbind, lapply(tix, tixFix)))
    write.table(tixdf,file=paste(eventid,"_",timestamp,".txt",sep=""),row.names=FALSE)

 }

# this function from stack overflow to format to dataframe
tixFix <- function(x) {
    if(is.list(x)) {
        unlist(lapply(x, tixFix))
    } else {
        x[which(is.null(x))] <- NA
        paste(x, collapse = ",")
    }
}

friday <- 9220595
saturday <- 9220596
sunday <- 9220597

stubhub(friday)
stubhub(saturday)
stubhub(sunday)

