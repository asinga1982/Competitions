# Read new contracts data
newCont <- read.csv("Contracts_New.csv", header = T, sep = ",", 
                    na.strings = c("", "NA"), stringsAsFactors = T)

endCont <- read.csv("Contracts_End.csv", header = T, sep = ",", 
                    na.strings = c("", "NA"), stringsAsFactors = T)

preCont <- read.csv("Contacts_Pre_2017.csv", header = T, sep = ",", 
                    na.strings = c("", "NA"), stringsAsFactors = T)

# Standarize Month names and form a Date for New contracts, end contracts and Pre 2017 contacts
newCont$Month <- as.character(newCont$MONTH_CONTRACT)
newCont$Month[newCont$Month=="ago."] <- "aug."

newCont$Month <- gsub("[.]","",newCont$Month) 

newCont$Date <- as.Date(paste(newCont$YEAR_CONTRACT, newCont$Month, newCont$DAY_ALTA_CONTR, sep="-"), 
             format="%Y-%b-%d")


endCont$Month <- as.character(endCont$MONTH_END_CONTRACT)
endCont$Month[endCont$Month=="ago."] <- "aug."

endCont$Month <- gsub("[.]","",endCont$Month) 

endCont$Date <- as.Date(paste(endCont$YEAR_END_CONTRACT, endCont$Month, endCont$DAY_END_CONTRACT, sep="-"), 
                        format="%Y-%b-%d")

# Analyze new contracts
newCont1 <- newCont[newCont$MUNICIPALITY!="BARCELONA",]

a <- aggregate(newCont$NUMBER_OF_CONTRACTS, by=list(newCont$TYPE_OF_HOUSEHOLD), FUN=sum)
barplot(names.arg =  as.factor(a$Group.1), height = a$x, beside=T)

# Analyze contacts
preCont1 <- preCont[preCont$CONTACT.TYPE=="Call - Input" ,]
preCont1$Day <- weekdays(as.Date(preCont1$START.DATE, format = "%Y-%m-%d"))

a <- aggregate(preCont1$Contacts, by=list(substr(preCont1$START.DATE,6,7)), FUN=sum)

#a <- aggregate(preCont$Contacts, by=list(preCont$CONTACT.TYPE), FUN=sum)
barplot(names.arg =  as.factor(a$Group.1), height = a$x, beside=T)

#Remove unnecessary coulmns, Combine new and ended contracts

newCont1 <- newCont[ ,c(1,5,6,7,8,10)]
endCont1 <- endCont[ ,c(1,5,6,7,8,10)]

colnames(newCont1) <- c("MUNICIPALITY","CENSUS_SECTION", "WATER_USAGE", "TYPE_OF_HOUSEHOLD",
                        "NEW_CONTRACTS", "Date" )
colnames(endCont1) <- c("MUNICIPALITY","CENSUS_SECTION", "WATER_USAGE", "TYPE_OF_HOUSEHOLD", 
                        "ENDED_CONTRACTS", "Date" )

library(plyr)

Cont <- join(newCont1, endCont1, by =c("MUNICIPALITY", "CENSUS_SECTION", "Date", "WATER_USAGE", 
                                    "TYPE_OF_HOUSEHOLD"), type="full")

rm(newCont, endCont)

#Create aggregate at muncipality and Date level

a <- aggregate(newCont1$NEW_CONTRACTS, by=list(newCont1$MUNICIPALITY, newCont1$Date), FUN=sum)

b <- aggregate(endCont1$ENDED_CONTRACTS, by=list(endCont1$MUNICIPALITY, endCont1$Date), FUN=sum)

colnames(a) <- c("MUNICIPALITY","Date", "NEW_CONTRACTS")


colnames(b) <- c("MUNICIPALITY","Date", "ENDED_CONTRACTS")

new <- join(a,b, type="full")

new$NEW_CONTRACTS[is.na(new$NEW_CONTRACTS)] <- 0
new$ENDED_CONTRACTS[is.na(new$ENDED_CONTRACTS)] <- 0

rm(a,b)
new <- new[with(new, order(Date)),]

new$netAdd <- new$NEW_CONTRACTS - new$ENDED_CONTRACTS
new$totalCust <- 0

newTotal = 100
for(i in 1:nrow(new))
{
  newTotal <- newTotal + new$netAdd[i] 
  new$totalCust[i] <- newTotal
  
}

new$totalContracts <- new$NEW_CONTRACTS + new$ENDED_CONTRACTS

new1 <- aggregate(cbind(totalContracts,totalCust)~Date, data=new, sum)

new1$weekday <- weekdays(new1$Date)

ggplot(new1) + geom_point(aes(Date,totalContracts, group=weekday ,color=weekday) )

# Visualization of combined data

a <- aggregate(cbind(NEW_CONTRACTS,ENDED_CONTRACTS)~(substr(Date,1,4)), data=new, sum)

df = melt(a,id.vars = "Year")

ggplot(df,aes(x=variable,y=value,fill=factor(Year)))+
  geom_bar(stat="identity",position="dodge")+
  scale_fill_discrete(name="Year",
                      breaks=c(3, 3),
                      labels=c("New", "Ended"))+
  xlab("Contracts")+ylab("Count")


ggplot(df, aes(Year, value, fill=metric)) + 
  geom_bar(position="dodge")

# Combine and compare contracts and contacts

