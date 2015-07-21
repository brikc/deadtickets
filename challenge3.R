library(dplyr)
library(lubridate) 
library(ggplot2)

getData<-function() {
    
    path <- "/Users/bcalandra/Documents/coursera/dataincubator/stubhub/data"
    filenames <- dir(path, pattern =".txt")
    tickets <- ""
    for(i in 1:length(filenames)){
    #for(i in 1:50){
        file <- read.table(paste(path,"/",filenames[i],sep=""), header=TRUE, stringsAsFactors=FALSE)
        newdata <- select(file,listingId,currentPrice,sellerSectionName,sectionName)
        
        # get date/time from filename
        timestamp <- unlist(strsplit(filenames[i],"_"))
        day <- timestamp[2]
        #time <- strsplit(timestamp[3],".txt",fixed=TRUE)
        #timestamp <- paste(day," ",time,sep="")
        
        #print(timestamp)
        newdata$timestamp<-day
        
        newdata$currentPrice<-as.numeric(newdata$currentPrice)
        
        names(newdata) <- c("id","currentPrice","sellerSectionName","sectionName","date")
        tickets <- rbind(tickets, newdata)
        print(i)
    }
    tickets 
}

getSummary <- function() {
    
    filename <- "/Users/bcalandra/Documents/coursera/dataincubator/stubhub/summary.txt"
    file <- read.table(filename, header=TRUE, stringsAsFactors=FALSE)
}

stubhub<-function(tickets,summary) {

    library(stringr)
    library(lubridate)
    library(gridExtra)
    
    # make price a number
    tickets$currentPrice <- as.numeric(tickets$currentPrice)
    
    # make date a date
    #tickets$date <- mdy(tickets$date)
    
    # remove price NAs
    tickets<-filter(tickets,!is.na(currentPrice))
    
   
    # group by section id
    tixgroup <- group_by(tickets,sectionName, date)
    avgTixBySection <- summarise(tixgroup,avgPrice=mean(currentPrice))
    avgTixBySecNoOutliers <- filter(avgTixBySection,avgPrice<10000)
    
    medTixBySec <- summarise(tixgroup,medPrice=median(currentPrice))
    
    # create section groups
    medTixBySec$sectionGroup <- "Other"
    medTixBySec$sectionGroup[which(str_detect(medTixBySec$sectionName, ignore.case('upper')))] <- "Upper"
    medTixBySec$sectionGroup[which(str_detect(medTixBySec$sectionName, ignore.case('middle')))] <- "Middle"
    medTixBySec$sectionGroup[which(str_detect(medTixBySec$sectionName, ignore.case('lower')))] <- "Lower"
    medTixBySec$sectionGroup[which(str_detect(medTixBySec$sectionName, ignore.case('pit')))] <- "Pit"
    medTixBySec$sectionGroup[which(str_detect(medTixBySec$sectionName, ignore.case('ga|general')))] <- "GA"
    medTixBySec$sectionGroup[which(str_detect(medTixBySec$sectionName, ignore.case('flr|Floor')))] <- "Floor"
    medTixBySec$sectionGroup[which(str_detect(medTixBySec$sectionName, ignore.case('grandstand')))] <- "Grandstand"
    medTixBySec$sectionGroup[which(str_detect(medTixBySec$sectionName, ignore.case('club')))] <- "Club"
    medTixBySec$sectionGroup[which(str_detect(medTixBySec$sectionName, ignore.case('flr|Floor')))] <- "Floor"
    medTixBySec$sectionGroup[which(str_detect(medTixBySec$sectionName, ignore.case('grandstand')))] <- "Grandstand"
    medTixBySec$sectionGroup[which(str_detect(medTixBySec$sectionName, ignore.case('suite')))] <- "Suite"
    medTixBySecNoOutliers <- filter(medTixBySec,medPrice<4000)
    medTixUnder500 <- filter(medTixBySec,medPrice<500)
    
    medTixBySecNoOutliersDate <- medTixBySecNoOutliers
    medTixBySecNoOutliersDate$date <- mdy(medTixBySecNoOutliersDate$date)
    
    myplot <- ggplot(medTixBySecNoOutliersDate)+geom_point(aes(x=date,y=medPrice,colour=sectionGroup),size=3,alpha = .6)+geom_smooth(aes(x=date,y=medPrice),color="black",method="lm")+ggtitle("Grateful Dead Tickets\nMedian StubHub Price by Section Group")+xlab("date")+ylab("median price")+theme(axis.text.x=element_text(angle=90, vjust=0.5))
    #under500 <- ggplot(medTixUnder500)+geom_point(aes(x=date,y=medPrice,colour=sectionGroup),size=3,alpha=.6)+ggtitle("Grateful Dead Ticket Prices\nMedian StubHub Price by Section Group\nTickets Under $500")+xlab("date")+ylab("median price")
    under500 <- ggplot(medTixBySecNoOutliersDate)+geom_point(aes(x=date,y=medPrice,colour=sectionGroup),size=3,alpha = .6)+geom_smooth(aes(x=date,y=medPrice),color="black",method="lm")+ggtitle("Grateful Dead Ticket Prices\nMedian StubHub Price by Section Group\nTickets Under $500")+xlab("date")+ylab("median price")+theme(axis.text.x=element_text(angle=90, vjust=0.5))+ylim(c(0,500))
    
    
    
    # Open device
    png(filename="medianBySection.png",width=900,height=700,units="px")
    print(myplot)
    # Close device
    dev.off()
    
    png(filename="stubhub/medianUnder500.png",width=900,height=700,units="px")
    print(under500)
    # Close device
    dev.off()
    
    png(filename="stubhub/medianByDate.png",width=1300,height=600,units="px")
    grid.arrange(myplot, under500, ncol=2)   
    dev.off()
    
    # look at total ticket quantities posted
    summary$date<-mdy_hms(summary$timestamp)
    totalTix <- select(summary,totalTickets,eventid,date)
    totalTix$event[which(totalTix$eventid==9220595)] <- "Friday"
    totalTix$event[which(totalTix$eventid==9220596)] <- "Saturday"
    totalTix$event[which(totalTix$eventid==9220597)] <- "Sunday"
    
    summaryplot <- ggplot(totalTix)+geom_point(aes(x=date,y=totalTickets,colour=event),size=3,alpha = .6)+ggtitle("Grateful Dead Tickets\nTotal StubHub Listings by Event")+xlab("date")+ylab("total tickets")+theme(axis.text.x=element_text(angle=90, vjust=0.5))
    png(filename="stubhub/summary.png",width=900,height=700,units="px")
    print(summaryplot)
    # Close device
    dev.off()
    
}
