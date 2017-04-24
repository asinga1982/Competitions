#Modlling tries

#Create missing dates
yy1 <- seq(ymd('2010-01-01'),ymd('2016-12-31'), by = '1 day')
xx1 <- replicate(expr=0, n=length(yy1))

zz1 <- data.frame(as.Date(yy1, format="%Y-%m-%d"),xx1)
colnames(zz1) <- c("Date", "Zeros")

#Join with orifinal data and sort by Date
pre.resol.Req.dd.byDay.compl <- join(pre.resol.Req.dd.byDay, zz1, type="full")
pre.resol.Req.dd.byDay.compl <- pre.resol.Req.dd.byDay.compl[with(pre.resol.Req.dd.byDay.compl, order(Date)),]

#Replace null with 0
pre.resol.Req.dd.byDay.compl$Resolutions[is.na(pre.resol.Req.dd.byDay.compl$Resolutions)] <- 0

pre.resol.Req.dd.byDay.compl.ts <- msts(pre.resol.Req.dd.byDay.compl$Resolutions, seasonal.periods=c(6,7,60,90, 365.25))

plot(decompose(pre.resol.Req.dd.byDay.compl.ts))

#Using TBATS (this model used)
fit_tbats.res <- tbats(pre.resol.Req.dd.byDay.compl.ts,seasonal.periods=c(6,7,90, 365.25))
fc.res <- forecast(fit_tbats.res, 74)
