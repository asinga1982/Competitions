#ANALYSIS FOR PREDICTING CONTACTS

# New,ended contracts and pre contacts consolidated by date

nc_byDate <- aggregate(newCont1$NEW_CONTRACTS, by=list(newCont1$Date), FUN=sum)
colnames(nc_byDate) <- c("Date", "NewContracts")
nc_byDate <- nc_byDate[with(nc_byDate, order(Date)),]

ec_byDate <- aggregate(endCont1$ENDED_CONTRACTS, by=list(endCont1$Date), FUN=sum)
colnames(ec_byDate) <- c("Date", "EndedContracts")
ec_byDate <- ec_byDate[with(ec_byDate, order(Date)),]

preCont$Date <- as.Date(preCont$START.DATE, format="%Y-%m-%d")
precont_byDay <- aggregate(preCont$Contacts, by=list(preCont$Date), FUN=sum)
colnames(precont_byDay) <- c("Date", "Contacts")
precont_byDay <- precont_byDay[with(precont_byDay, order(Date)),]

precont_byDayType <- aggregate(preCont$Contacts, by=list(preCont$Date, preCont$CONTACT.TYPE),
                               FUN=sum)
colnames(precont_byDayType) <- c("Date", "Type","Contacts")
precont_byDayType <- precont_byDayType[with(precont_byDayType, order(Date)),]

#Combine the 2 datasets - new and ended
comb_byDate <- join(nc_byDate, ec_byDate, type="full")
comb_byDate <- comb_byDate[-1,]

#Remove nulls 
comb_byDate$NewContracts[is.na(comb_byDate$NewContracts)] <- 0
comb_byDate$EndedContracts[is.na(comb_byDate$EndedContracts)] <- 0

#Add additonal features to combined data
comb_byDate$weekday <- weekdays(comb_byDate$Date)
comb_byDate$total <- comb_byDate$NewContracts + comb_byDate$EndedContracts

precont_byDayType$weekday <- weekdays(precont_byDayType$Date)
precont_byDayType$weekend <- "N"

precont_byDayType$weekend <- ifelse(precont_byDayType$weekday %in% 
            c('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday'), "N", "Y" )

# Visualization
ggplot(comb_byDate) + geom_point(aes(Date,total, group=weekday ,color=weekday) )
hist(log(comb_byDate$NewContracts+1), breaks=50)

#Combine this with contacts
allcomb <- join(comb_byDate, precont_byDay, type="inner")
allcomb$Mth <- as.numeric(substr(allcomb$Date,6,7))
allcomb$Qtr <- as.factor(quarters(allcomb$Date))
allcomb$year <- as.numeric(substr(allcomb$Date,1,4))

#Visuals
ggplot(allcomb) + geom_point(aes(total,Contacts, group=weekday ,color=weekday)) +
  facet_grid(year~Qtr) 

ggplot(precont_byDayType[precont_byDayType$Type!="Call - Input" & 
                           precont_byDayType$Type!="Visit" &
         precont_byDayType$Type!="Web - Input" &
           precont_byDayType$Type!= "Mail - Recieved" &
           precont_byDayType$Type!= "Internal Management",]) + 
  geom_point(aes(Date,Contacts, group=weekend ,color=weekend)) +
  facet_grid(Type~.) 


ggplot(precont_byDayType) + 
  geom_point(aes(Date,Contacts, group=weekend ,color=weekend))

# Consolidate by year and quarter
precont_byDayType$Qtr <- quarters(precont_byDayType$Date)
precont_byDayType$year <- as.numeric(substr(precont_byDayType$Date,1,4))

precont_byDay$Qtr <- quarters(precont_byDay$Date)
precont_byDay$year <- as.numeric(substr(precont_byDay$Date,1,4))

precont_byYearQtr <- aggregate(precont_byDay$Contacts, by=list(precont_byDay$Qtr,
                    precont_byDay$year),
                               FUN=sum)

colnames(precont_byYearQtr) <-  c("Qtr", "Year", "Contacts")
