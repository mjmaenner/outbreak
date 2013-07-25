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
choro <- merge(states, statelist, sort = FALSE, by.x="state.abb", by.y = "State", all.x=TRUE)
choro <- choro[order(choro$order), ] 

#plot the map
library(lubridate)
library(ggplot2)

choro$Freq2<-as.character(cut(choro$Freq, breaks=c(0,10,20,30,40,50,60), labels=c("1-9","10-19","20-29","30-39","40-49","50-59")))
choro$Freq2<-as.factor(ifelse(is.na(choro$Freq), "None\nReported", as.character(choro$Freq2))) 
cols<-c("None\nReported"="#FFFFFF", "1-9"="#D0D1E6", "10-19"="#A6BDDB", "20-29"="#74A9CF","30-39"="#3690C0","40-49"="#0570B0","50-59"="#034E7B")

map_county<-map_data("county")


ggplot(choro, aes(long, lat, group = group))+  
  geom_polygon(data=statesblank, fill="white")+
  geom_polygon(aes( fill = Freq2))+theme_nothing(legend=TRUE)+
  geom_polygon(data=map_county, fill=NA, colour="white", size=0.05, alpha=0.9)+
  geom_polygon(data=statesblank,fill=NA, colour="darkgray", size=0.1)+
  coord_map("lagrange") +   
  scale_fill_manual(values=cols, breaks=c("None\nReported","1-9","10-19","20-29","30-39","40-49","50-59"),guide = guide_legend(reverse=TRUE), name="Number\nof Cases")+
  ggtitle("Number of reported Salmonella Typhimurium cases by state:\n PulseNet Upload Dates: Jan 8 to Apr 26 2012\n(1 case in Hawaii not shown)")


#EPI CURVES
ggplot(clust_2, aes(x=Upload.Date))+geom_histogram(binwidth=1, fill="darkred")+theme_minimal()

mult_format <- function() {
  function(x) format(length(clust_3[,1])*x,digits = 3) }

ggplot(clust_3, aes(x=Upload.Date, y=..density..))+
  geom_histogram(binwidth=1, fill="gray50")+theme_minimal()+geom_density(size=2, colour="red", adjust = 1/3, guide=TRUE)+
  scale_x_date(breaks = date_breaks("2 weeks"),labels = date_format("%d-%b"))+
  scale_y_continuous(labels=mult_format(), name="Reported Cases per day")
  