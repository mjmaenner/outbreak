install.packages("xlsx")
require(xlsx)
setwd("//Users/matt/Dropbox/EIS/ee/session 3/Session 3B/Download 2/") #note that you need to change the path
cc_2<-read.xlsx("Session 3B - Case Control 2 Database_For EISOs.xlsx",sheetIndex=1)
cc_2$agegrp<-cut(x=cc_2$AGEYRS, breaks=c(-1,5, 17,39,98), labels=c("0 to <6","6 to <18","18 to <40", "40 and older"))
install.packages(c("epitools","epicalc"))
lapply(c("epitools","epicalc"), require, character.only=T)

pb_output<-function(df, x){
  df.2<-cc_2[cc_2$CASE %in% c(0,1) & is.finite(cc_2$CASE) &
               eval(parse(text=paste("cc_2$",x,sep=""))) %in% c(0,1) & is.finite(eval(parse(text=paste("cc_2$",x, sep="")))) ,] 
  df.2$CASE<-factor(df.2$CASE, levels=c("1","0"), ordered=TRUE) #reorder so 1 is in upper left of tables
  df.2[, x]<-factor(df.2[,x], levels=c("1","0"), ordered=TRUE) #reorder so 1 is in upper left of tables
  print(eval(parse(text=paste0("with(df.2, table(CASE, ",x,"))"))))
  print(eval(parse(text=paste0("oddsratio.fisher(with(df.2, table(CASE, ",x,")))"))))
  print(eval(parse(text=paste0("mhor(mhtable=(with(df.2, table(CASE,",x,", agegrp))))"))))
}

list_of_foods<-c("FZNSTUF", "FZNCP", "FZNFING", "FZNSTRP", "FZNNUG", "ANYCHICKEN", "PBPETTX", "PBOTHER", "ICE",
                 "PBSAUCE", "PBNUTRIBAR", "CEREAL", "CANDY", "PBDESSERT","PBCHIPS", "PREDOUGH","CREME", "COOKIE", 
                 "PBPACKSAND","PEANUTS","PRETZEL","CRACK") 

for(i in 1:length(list_of_foods)){
  pb_output(cc_2, list_of_foods[i])
}