#reads the outbreak data file
install.packages("xlsx")
library(xlsx)
clust_3<-read.xlsx("//Users/matt/Dropbox/EIS/ee/session 3/Session 3B/Download 1/Master S Typhimurium Line List_Both Clusters.xlsx",sheetIndex=1)



#creates the table with number of cases by state
statelist<-data.frame(table(clust_3$SourceState))
names(statelist)<-c("State","Freq")
statelist$Freq<-as.numeric(as.character(statelist$Freq))
write.csv(statelist, "//Users/matt/Dropbox/EIS/ee/statelist_EEday3b.csv", row.names=FALSE)

#load the built-in map files, create a file with state abbreviation in it
#install.packages("ggmap")
library(ggmap)
library(maps)
statesblank<-map_data("state" )
us_state<-map_data("state")
data(state)
state.info<-data.frame(state.name, state.abb, state.area, state.center, state.division, state.region, state.x77)
state.info$state.name<-tolower(state.info$state.name)
states<-merge(us_state, state.info, by.x="region", by.y="state.name", all.x=TRUE)

#merge outbreak data to state file
choro <- merge(states, statelist, sort = FALSE, by.x="state.abb", by.y = "State")
choro <- choro[order(choro$order), ] 

#plot the map
library(lubridate)
library(ggplot2)

ggplot(choro, aes(long, lat, group = group))+  
geom_polygon(data=statesblank, fill="darkgray")+
geom_polygon(aes( fill = Freq))+theme_nothing(legend=TRUE)+
coord_map("lagrange") +   
scale_fill_gradient2(low="darkgray", high="red", guide="colorbar")+
ggtitle("Num. of reported Salmonella cases by state,\n Upload Dates: Jan 8 2012 to May 16 2012\n(not shown: 3 cases in Hawaii)")



#EPI CURVES
ggplot(clust_2, aes(x=Upload.Date))+geom_histogram(binwidth=1, fill="darkred")+theme_minimal()

mult_format <- function() {
  function(x) format(length(clust_3[,1])*x,digits = 3) }

ggplot(clust_3, aes(x=Upload.Date, y=..density..))+
  geom_histogram(binwidth=1, fill="gray50")+theme_minimal()+geom_density(size=2, colour="red", adjust = 1/3, guide=TRUE)+
  scale_x_date(breaks = date_breaks("2 weeks"),labels = date_format("%d-%b"))+
  scale_y_continuous(labels=mult_format(), name="Reported Cases per day")
  