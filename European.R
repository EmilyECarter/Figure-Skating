ladies <- read.csv("~/R/ladies.csv")
European.Ladies <- read.csv("~/R/European Ladies.csv")
#set up, 
comp.date<-as.Date("2015-01-28")
European.Ladies["Date"]<-comp.date
ladies["diffdate"]<-NA
#create a new dataframe for each person
num.skaters<-nrow(European.Ladies)
for (i in 1:num.skaters){
  name.string<-European.Ladies$Name[i]
  assign(paste0("skater",i),ladies[which(ladies$SKATER==toString(name.string) ),])
}

#subtract dates for time series
for (i in num.skaters){
  dataset.i<-get(paste0("skater",i))
  jcount<-nrow(dataset.i)
  for (j in jcount){
    x<-difftime(comp.date,as.Date(dataset.i$Date[j], units="days"))
    paste0("skater",i)[j,7]<- x
      
  }
}




