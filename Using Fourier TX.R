  #Initialize the matrix
  pred.mat <- matrix( nrow = length(idx), ncol = 34)
  models <- matrix()
  
  # Loop thru eac ATM and perform ARIMA modelling and prediction
  for (i in seq_along(idx)) {
    
    rm(df1)
    # Get the records for 1 ATM
    atmName <- idx[i] 
    atm_one <- comb[comb$ATM_ID == atmName,]
    
    # Extract start date
    startDate <- atm_one$Date[1]
    
    #Generate date sequence 
    time.seq <- as.Date(seq.POSIXt(as.POSIXct(startDate), as.POSIXct("2014-05-01"), by="day"))
    
    df1 <- data.frame(time.seq)
    colnames(df1) <- c("Date")
    
    #Join the sequence with actual date to find the missing ones  
    atm_one.allDate <- join(df1, atm_one, by="Date", type="left")
    
    # Fnd the missing ones
    idx1 <-  which(is.na(atm_one.allDate$Withdrawal))
    
    # Loop thru missing ones and assign prev day's value
    for (j in idx1) {
      atm_one.allDate$Withdrawal[j] <- atm_one.allDate$Withdrawal[j-1]
    }
  
  
  #train.ts <- ts(atm_one.allDate$Withdrawal[atm_one.allDate$Date < "2014-01-01" 
  #                                          & atm_one.allDate$Date > "2012-04-30"], 
  #               start=c(2012,5,1))
  
  # cv.data.full <- atm_one.allDate[atm_one.allDate$Date > "2013-12-31",]
  # cv.data <- cv.data.full$Withdrawal
  
  # train.msts <- msts(train.ts, seasonal.periods = c(7,30, 365.25))
  # 
  # z <- fourier(train.msts, K=c(3,15,50))
  # zf <- fourier(train.msts, K=c(3,15,50), h=120)
  # 
  # lam <- BoxCox.lambda(train.msts)
  #four.model <- auto.arima(train.msts, xreg=z, seasonal=FALSE, lambda = lam)
  
  #print(four.model)
  
  #four.model.fore <- forecast(four.model, xreg=zf, h=120)
  
  #pred.mat[i,1] <- atmName
  #pred.mat[i,2:121] <- four.model.fore$mean
  
  fulldata.ts <- ts(atm_one.allDate$Withdrawal[atm_one.allDate$Date > "2012-04-30"], 
                 start=c(2012,5,1))
  
  # Treat outliers
  medn <- median(fulldata.ts)
  
  fulldata.ts[fulldata.ts > 4*medn] <- 4*medn
  
  # Convert to time series of multple seasonality
  full.msts <- msts(fulldata.ts, seasonal.periods = c(7,30, 365.25))
  
  #Get Box cox Tx lambda 
  lam <- BoxCox.lambda(full.msts)
  
  z <- fourier(full.msts, K=c(3,15,50))
  
  # Create model
  full.model <- auto.arima(full.msts, xreg=z, seasonal=FALSE, lambda = lam)
  
  zf1 <- fourier(full.msts, K=c(3,15,50), h=31)
  
  # get predictions for May 2014
  actual_prd <- forecast(full.model, xreg=zf1, h=31, lambda = lam)
  
  # Handle -ve values
  actual_prd$mean[actual_prd$mean < 0] <- 0.0
  
  pred.mat[i,1] <- atmName
  pred.mat[i,2:32] <- actual_prd$mean
  
  # Find out the best refill option
  
  #Calculate Avg amt reqd on Thursdays and Mondays
  ThurDaily.Bal <- mean(pred99[1], pred99[8], pred99[15], pred99[22], pred99[29])
  MonDaily.Bal <- mean(pred99[5], pred99[12], pred99[19], pred99[26])
  
  #By default it would be on Thursday/Monday, which ever has the greatest amt
  
  if (ThurDaily.Bal > MonDaily.Bal){  
    LoadType <- 4 
    LoadAmt <- max(sum(actual_prd$mean[1:7]), 
                   sum(actual_prd$mean[8:14]),
                   sum(actual_prd$mean[15:21]),  
                   sum(actual_prd$mean[22:27]))
    }
  else{  
    LoadType <- 5 
    LoadAmt <- max(sum(actual_prd$mean[5:11]), 
                   sum(actual_prd$mean[12:18]),
                   sum(actual_prd$mean[19:25]),  
                   sum(actual_prd$mean[26:31]))
    
      }
  
  # Calculate amt reqd for 2 weeks
  
  week2.Bal <- max(sum(actual_prd$mean[1:13]), sum(actual_prd$mean[14:27]))
  
  if (week2.Bal < 150000) {
    LoadType <- 6
  
    LoadAmt <- max(sum(actual_prd$mean[1:13]), 
                   sum(actual_prd$mean[14:27]))
    
    }
  #Calcualate max and min amts, If difference is more than 1L, then load twice a week
  max.load <- max(actual_prd$mean)
  
  min.load <- min(actual_prd$mean)
  
  if ((max.load - min.load) > 250000) {
  # Load on alternate days
      LoadType <- 2
      LoadAmt <- max(sum(actual_prd$mean[1:2]), 
                     sum(actual_prd$mean[3:4]),
                     sum(actual_prd$mean[5:6]),
                     sum(actual_prd$mean[7:8]),
                     sum(actual_prd$mean[9:10]),
                     sum(actual_prd$mean[11:12]),
                     sum(actual_prd$mean[13:14]),
                     sum(actual_prd$mean[15:16]),
                     sum(actual_prd$mean[17:18]),
                     sum(actual_prd$mean[19:20]),    
                     sum(actual_prd$mean[21:22]),
                     sum(actual_prd$mean[23:24]),
                     sum(actual_prd$mean[25:26]),
                     sum(actual_prd$mean[27:28]),
                     sum(actual_prd$mean[29:30]))
        }
  
  else if ((max.load - min.load) > 100000) {
  # Load on Thurday and Monday
    LoadType <- 3
    LoadAmt <- max(sum(actual_prd$mean[1:4]), 
                   sum(actual_prd$mean[5:7]),
                   sum(actual_prd$mean[8:11]),
                   sum(actual_prd$mean[12:14]),
                   sum(actual_prd$mean[15:18]),
                   sum(actual_prd$mean[19:21]),
                   sum(actual_prd$mean[22:25]),
                   sum(actual_prd$mean[26:28]),
                   sum(actual_prd$mean[29:31])
                   )
    }
  
  pred.mat[i,33] <- LoadType
  pred.mat[i,34] <- LoadAmt
  
  }
