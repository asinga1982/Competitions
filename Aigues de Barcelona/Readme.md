# Winning Solution for Analytics Vidhya Machine Learning Competition-Xtreme ML Hack

This model scored 62.91 on Private Leaderboard and ranked #3.

## Prerequisites:

R version 3.3.3  
R Packages: plyr, forecast, hts, lubridate, reshape, reshape2, ggplot2

## Problem Statement:
The largest water supplier of Barcelona wants to leverage machine learning to effectively predict daywise-mediumwise-departmentwise breakdown of predictions of how many contacts (tickets/enquiries) would it receive and how many resolutions would it make so that they can size their team properly and improve customer satisfaction.

# Approach:

There was a close correlation between new contracts, ended contracts and number of contacts. I had 2 choices:
1.	To predict new contacts and ended contacts using time series and then predict contacts for them
2.	Directly predict contacts using time series forecasting
##### I used the second approach.
 
## Predicting Contacts:
I used top down approach as the there was only 1 level involved. I predicted the contacts at day level using Exponential smoothing state space model with Box-Cox transformation, ARMA errors, Trend and Seasonal components (tbats) using weekly, quarterly and yearly seasonality. To accommodate the trends at type level, I found the average contribution by type for 2016 and for all years (some types had different trends in recent years). Using separate contribution percentages by types and for weekdays and weekends, I split the overall forecast into detailed forecast.  

## Predicting Resolution:  
Resolutions had 2 levels (category and subject), hence top down approach was not suitable. Also, breaking the high-level forecast into 2 levels would be quite error prone so I selected hierarchical time series modeling for this. I used tbats() for each subject to account for multiple seasonality on the data. Most of the effort was in preparing data in the format accepted by hierarchical time series modeling function, hts(). I used only last 3 years of data (optimized after first submission) as that was having lesser missing values and avoided the spiked 2013 data. Combining of low level forecast with top level was done using LU decomposition algorithm.  

Prediction from this resulted in some negative values, esp. for the weekend. I replaced them with respective mean from 2016 weekend data.   

## Code Files:  
R_Notebook.R & Analysis_1.R -> Analysis files  
Modelling.R ->  For predicting contacts  
Resolution_Analysis.R -> For analyzing Resolutions  
Resolution_Modelling.R -> Modelling tries for Resolution (not used in final submission)  
resolutions hts.R -> For predicting Resolutions  

### To reproduce the output, perform the following steps:
1. Copy all the files in a directory and point R to Train folder inside Train_BG1IL20.
2. Rename files in test folder as Contacts2017_test.csv and Resolution2017_test.csv to Train folder.
3. Execute files Modelling.R and resolutions hts.R for predicting contacts and resolutions  

Output will be created as Contacts.csv and Resolution.csv in Train folder
