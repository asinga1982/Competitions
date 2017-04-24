#ANALYSIS FOR RESOLUTION

pre.resol <- read.csv("Resolution_Pre_2017.csv", header = T, 
                      na.strings = c("", "NA"), stringsAsFactors = T)

#Aggregate by Date alone
pre.resol.byDay <- aggregate(pre.resol$Resolution, 
                               by=list(pre.resol$Date),
                               FUN=sum)

colnames(pre.resol.byDay) <- c("Date", "Resolutions")

pre.resol.byDay$Date <- as.Date(pre.resol.byDay$Date, format="%Y-%m-%d")

pre.resol.byDay <- pre.resol.byDay[with(pre.resol.byDay, order(Date)),]

#Aggregate by Date and Category
pre.resol.byDayCat <- aggregate(pre.resol$Resolution, 
                             by=list(pre.resol$Date, pre.resol$Category),
                             FUN=sum)

colnames(pre.resol.byDayCat) <- c("Date","category", "Resolutions")

pre.resol.byDayCat$Date <- as.Date(pre.resol.byDayCat$Date, format="%Y-%m-%d")

pre.resol.byDayCat <- pre.resol.byDayCat[with(pre.resol.byDayCat, order(Date)),]

#Aggregate by year
pre.resol.byYear <- aggregate(pre.resol$Resolution, 
                              by=list(substr(pre.resol$Date,1,4)),
                              FUN=sum)

#Aggregate by year and Category for Barcelona(since it has a large volume)

pre.resol.Barce <- pre.resol[pre.resol$City=="BARCELONA",]

pre.resol.Barce.byYear <- aggregate(pre.resol.Barce$Resolution, 
                              by=list(substr(pre.resol.Barce$Date,1,4)),
                              FUN=sum)

pre.resol.Barce.byDayCat <- aggregate(pre.resol.Barce$Resolution, 
                                by=list(pre.resol.Barce$Date, pre.resol.Barce$Category),
                                FUN=sum)

colnames(pre.resol.Barce.byDayCat) <- c("Date","category", "Resolutions")

pre.resol.Barce.byDayCat$Date <- as.Date(pre.resol.Barce.byDayCat$Date, format="%Y-%m-%d")

pre.resol.Barce.byDayCat <- pre.resol.Barce.byDayCat[with(pre.resol.Barce.byDayCat, order(Date)),]

#Aggregate by year and Category for Badalona

pre.resol.Bada <- pre.resol[pre.resol$City=="BADALONA",]

pre.resol.Bada.byYear <- aggregate(pre.resol.Bada$Resolution, 
                                    by=list(substr(pre.resol.Bada$Date,1,4)),
                                    FUN=sum)

pre.resol.Bada.byDayCat <- aggregate(pre.resol.Bada$Resolution, 
                                      by=list(pre.resol.Bada$Date, pre.resol.Bada$Category),
                                      FUN=sum)

colnames(pre.resol.Bada.byDayCat) <- c("Date","category", "Resolutions")

pre.resol.Bada.byDayCat$Date <- as.Date(pre.resol.Bada.byDayCat$Date, format="%Y-%m-%d")

pre.resol.Bada.byDayCat <- pre.resol.Bada.byDayCat[with(pre.resol.Bada.byDayCat, order(Date)),]

#Aggregate by year and Category for Request

pre.resol.Req <- pre.resol[pre.resol$Category=="Request",]

pre.resol.Req.byYear <- aggregate(pre.resol.Req$Resolution, 
                                   by=list(substr(pre.resol.Req$Date,1,4)),
                                   FUN=sum)

pre.resol.Req.byDaySubj <- aggregate(pre.resol.Req$Resolution, 
                                     by=list(pre.resol.Req$Date, pre.resol.Req$Subject),
                                     FUN=sum)

colnames(pre.resol.Req.byDaySubj) <- c("Date","Subjct", "Resolutions")

pre.resol.Req.byDaySubj$Date <- as.Date(pre.resol.Req.byDaySubj$Date, format="%Y-%m-%d")

pre.resol.Req.byDaySubj <- pre.resol.Req.byDaySubj[with(pre.resol.Req.byDaySubj, order(Date)),]

#Aggregate by year and Category for Request and Duplicate docs

pre.resol.Req.dd <- pre.resol[pre.resol$Category=="Request" & pre.resol$Subject=="Duplicate Documents",]

pre.resol.Req.dd.byDay <- aggregate(pre.resol.Req.dd$Resolution, 
                                  by=list(pre.resol.Req.dd$Date),
                                  FUN=sum)

colnames(pre.resol.Req.dd.byDay) <- c("Date", "Resolutions")

datediff <- (difftime(strptime(pre.resol.Req.dd$End_Date , format = "%Y-%m-%d"),
         strptime(pre.resol.Req.dd$Date , format = "%Y-%m-%d"),units="days") ) +1

# Add features - Day of week and weekends
pre.resol.byDay$weekday <- weekdays(pre.resol.byDay$Date)

pre.resol.byDay$weekend <- ifelse(pre.resol.byDay$weekday %in% 
                              c('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday'), "N", "Y" )

pre.resol.byDayCat$Qtr <- quarter(pre.resol.byDayCat$Date)

pre.resol.byDayCat$Mth <- as.factor(months(pre.resol.byDayCat$Date))

pre.resol.Barce.byDayCat$Mth <- as.factor(months(pre.resol.Barce.byDayCat$Date))
pre.resol.Barce.byDayCat$weekday <- weekdays(pre.resol.Barce.byDayCat$Date)

pre.resol.Barce.byDayCat$weekend <- ifelse(pre.resol.Barce.byDayCat$weekday %in% 
                                    c('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday'), "N", "Y" )

pre.resol.Bada.byDayCat$Mth <- as.factor(months(pre.resol.Bada.byDayCat$Date))
pre.resol.Bada.byDayCat$weekday <- weekdays(pre.resol.Bada.byDayCat$Date)

pre.resol.Bada.byDayCat$weekend <- ifelse(pre.resol.Bada.byDayCat$weekday %in% 
                                             c('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday'), "N", "Y" )

pre.resol.Req.byDaySubj$Mth <- as.factor(months(pre.resol.Req.byDaySubj$Date))
pre.resol.Req.byDaySubj$weekday <- weekdays(pre.resol.Req.byDaySubj$Date)

pre.resol.Req.byDaySubj$weekend <- ifelse(pre.resol.Req.byDaySubj$weekday %in% 
                                            c('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday'), "N", "Y" )
#Visualize
ggplot(pre.resol.byDay) + geom_point(aes(Date,Resolutions, group=weekday ,color=weekday)) 
  
ggplot(pre.resol.Req.byDaySubj) + geom_point(aes(Date,Resolutions, group=weekend ,color=weekend)) +  
  facet_grid(Subjct~.) 

