library(readr)
library(tidyverse)
library(data.table)
library(lubridate)
data <- read_csv("Desktop/Quantium/QVI_data1.csv")

theme_set(theme_bw())
theme_update(plot.title = element_text(hjust = 0.5))
theme(panel.grid.major = element_blank(), 
      panel.grid.minor = element_blank())

#We would want to match trial stores to control stores that are similar to the trial
#store prior to the trial period of Feb 2019 in terms of :
# Monthly overall sales revenue
# Monthly number of customers
# Monthly number of transactions per customer
data <- as.data.table(data)
data[, YEARMONTH := format(as.Date(data$DATE), "%Y%m")]
data$YEARMONTH <- as.numeric(as.character(data$YEARMONTH))

#### Next, we define the measure calculations to use during the analysis.
# For each store and month calculate total sales, number of customers,
# transactions per customer, chips per customer and the average price per unit.
measure_whole <- data[, .(SALES = sum(TOT_SALES),
                    CUSTOMERS = uniqueN(LYLTY_CARD_NBR),
                    TRAN_CUS= uniqueN(TXN_ID)/uniqueN(LYLTY_CARD_NBR),
                    PROD_QTY_CUS= sum(PROD_QTY)/uniqueN(LYLTY_CARD_NBR),
                    AVG_PRICE= sum(TOT_SALES)/sum(PROD_QTY)),
                by = c('STORE_NBR', 'YEARMONTH')][order(STORE_NBR,YEARMONTH)]



#### Filter to the pre-trial period and stores with full observation periods
storesWithFullObs <- unique(measure_whole[, .N, STORE_NBR][N == 12, STORE_NBR])

preTrialMeasures <- measure_whole %>% filter(YEARMONTH < '201902' & 
                                      STORE_NBR %in% storesWithFullObs,)

##how similar each potential control store is to the trial store. 
#We can calculate how correlated the performance of each store is to the trial store.
#### Create a function to calculate correlation for a measure, looping
# through each control store.
#### Let's define inputTable as a metric table with potential comparison stores,
# metricCol as the store metric used to calculate correlation on
# storeComparison as the store number of the trial store.

#### Create a function to calculate correlation for a measure, looping through each control store.
    
calCorr <- function(inputTable,metricCol,trialStoreN){
    calTable = data.table(Store1 = numeric(), 
                          Store2 = numeric(), 
                          corr_measure = numeric())
    stN <- unique(inputTable[,STORE_NBR])
    for(i in stN){
        calMeasure = data.table("Store1" = trialStoreN, 
                                "Store2" = i, 
                                "corr_measure" = cor(inputTable[STORE_NBR == trialStoreN, eval(metricCol)],
                                                   inputTable[STORE_NBR == i, eval(metricCol)]))
        calTable <- rbind(calTable, calMeasure) }
    return(calTable)
}


#Apart from correlation, we can also calculate a standardised metric based on the
#absolute difference between the trial store's performance and each control store's
#performance.

#### Create a function to calculate a standardized magnitude distance for a measure
# looping through each control store
calculateMagnitudeDistance <- function(inputTable,metricCol,trialStoreN){
    calTable = data.table(Store1 = numeric(), 
                          Store2 = numeric(), 
                          YEARMONTH = numeric(),
                          mag_measure = numeric())
    stN <- unique(inputTable[,STORE_NBR])
    for(i in stN){
        calMeasure = data.table("Store1" = trialStoreN, 
                                "Store2" = i, 
                                "YEARMONTH" = preTrialMeasures$YEARMONTH ,
                                "mag_measure" = abs(inputTable[STORE_NBR == trialStoreN, eval(metricCol)]-
                                                    inputTable[STORE_NBR == i, eval(metricCol)]))
        calTable <- rbind(calTable,calMeasure) 
        calTable <- unique(calTable)
    }
    return(calTable)
}


###Standardize sales
standMag<- function(magnitude) {
  minMaxDist <- magnitude[, .(minDist = min(magnitude$mag_measure), 
                                     maxDist = max(magnitude$mag_measure)), 
                                 by = c("Store1", "YEARMONTH")]
  distTable <- merge(magnitude, minMaxDist, by = c("Store1", "YEARMONTH"))
  distTable[, magnitudeMeasure := 1 - (mag_measure - minDist)/(maxDist - minDist)]
  finalDistTable <- distTable[, .(magN_measure = mean(magnitudeMeasure)), by = .(Store1, Store2)]
  return(finalDistTable)
}

#############################################################################
#For Sales #77
trialStoreN <- 77

correlation_sales <- calCorr(preTrialMeasures,quote(SALES), trialStoreN)
correlation_sales <- unique(correlation_sales)
correlation_sales

correlation_customers <- calCorr(preTrialMeasures,quote(CUSTOMERS),trialStoreN)
correlation_customers

mag_sales <- standMag(calculateMagnitudeDistance(preTrialMeasures, quote(SALES), trialStoreN))
mag_sales[order(-magN_measure)] ###233

mag_customers <- standMag(calculateMagnitudeDistance(preTrialMeasures, quote(CUSTOMERS), trialStoreN))
mag_customers[order(-magN_measure)] ###233
#average of the correlation and magnitude scores
#### Hint: A simple average on the scores would be 0.5 * corr_measure + 0.5 *mag_measure
corr_weight <- 0.5
score_nSales <- merge(correlation_sales,mag_sales, by = c('Store2','Store1')
                      )[,scoreNSales := (corr_weight*score_nSales$corr_measure)
                        + (1-corr_weight) * score_nSales$magN_measure]

score_nCustomers <- merge(correlation_customers,mag_sales, by = c('Store2','Store1')
                          )[, scoreNCust := corr_weight*corr_measure
                            + (1-corr_weight) * magN_measure]

##Now we have a score for each of total number of sales and number of customers.
####  Combine scores across the drivers by first merging our sales scores and customer scores into a single table
score_Control <- merge(score_nSales, score_nCustomers, by = c('Store1','Store2'))
score_Control[, finalControlScore := (scoreNSales * 0.5) + (scoreNCust * 0.5)]
score_Control <- score_Control[Store2 != trialStoreN]
Control_Store <- score_Control[finalControlScore == max(finalControlScore),]$Store2
Control_Store  ###4233

#check visually if the drivers are indeed similar in the period before the trial.
#### Visual checks on trends based on the drivers
measureOverTime <- as.data.table(measure_whole)
pastSales <- measureOverTime[, Store_type := ifelse(STORE_NBR == trialStoreN,"Trial",
                                                     ifelse(STORE_NBR == Control_Store,
                                                     "Control Store",
                                                     "Other Stores"))
                             ][, mean_sales := mean(SALES), by = c("YEARMONTH","Store_type")
                               ][, TransactionMonth := as.Date(paste(YEARMONTH %/% 100, 
                                                                      YEARMONTH %% 100, 
                                                                      1, 
                                                                      sep = "‐"), 
                                                                "%Y‐%m‐%d")
                                 ][YEARMONTH < 201902]

ggplot(data = pastSales, 
       aes(x = TransactionMonth, y = mean_sales, color = Store_type)) +
    geom_point() +
    geom_line()+
    labs(x = "Month of operation", 
         y = "Total sales ($)", 
         title = "Total sales by month of #77 Trial Store")+ 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank())

pastCustomers <- measureOverTime[, Store_type := ifelse(STORE_NBR == trialStoreN,"Trial",
                                                    ifelse(STORE_NBR == Control_Store,
                                                           "Control Store",
                                                           "Other Stores"))
                                 ][, mean_customers :=mean(CUSTOMERS), 
                                   by = c("YEARMONTH","Store_type")
                                 ][, TransactionMonth := as.Date(paste(YEARMONTH %/% 100, 
                                                                       YEARMONTH %% 100, 
                                                                       1, 
                                                                       sep = "‐"), 
                                                                 "%Y‐%m‐%d")
                                 ][YEARMONTH < 201902]
ggplot(data = pastCustomers, 
       aes(x = TransactionMonth, y = mean_customers, color = Store_type)) +
    geom_point() +
    geom_line()+
    labs(x = "Month of operation", 
         y = "Total Customers", 
         title = "Total customers by month of #77 Trial Store")+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank())

## We'll start with scaling the control store's sales to a level similar to control
# for any differences between the two stores outside of the trial period. 
#### Scale pre-trial control sales to match pre-trial trial store sales

scalingFactorForControlSales <- 
    preTrialMeasures[STORE_NBR == trialStoreN, sum(SALES)]/
    preTrialMeasures[STORE_NBR == Control_Store, sum(SALES)]
scalingFactorForControlSales
#### Apply the scaling factor
scaledControlSales <- 
    measureOverTime[STORE_NBR == Control_Store, 
                    ][ ,controlSales := SALES * scalingFactorForControlSales]
##### Calculate the percentage difference between scaled control sales and trial sales
percentageDiff <- merge(scaledControlSales[, c("YEARMONTH", "controlSales")],
                        measureOverTime[STORE_NBR == trialStoreN,c("mean_sales", "YEARMONTH")],
                        by = 'YEARMONTH'
                        )[, percentageDiff := abs(controlSales-mean_sales)/controlSales]
percentageDiff
stdDev <- sd(percentageDiff[YEARMONTH < 201902 , percentageDiff]) 
stdDev
#### Note that there are 8 months in the pre-trial period
#### hence 8 - 1 = 7 degrees of freedom
df <- 7 
#### We will test with a null hypothesis of there being 0 difference
#between trial and control stores.
####  Calculate the t-values for the trial months. After that, find the
# 95th percentile of the t distribution with the appropriate degrees of freedom
#### to check whether the hypothesis is statistically significant.
#### Hint: The test statistic here is (x - u)/standard deviation
percentageDiff[ , tvalue := (percentageDiff - 0)/stdDev
                ][ , TransactionMonth := as.Date(paste(YEARMONTH %/% 100, 
                                                       YEARMONTH %% 100, 
                                                       1, 
                                                       sep = "-"),
                                                 "%Y-%m-%d")
                   ][YEARMONTH < 201905 & YEARMONTH > 201901, 
                     .(TransactionMonth, tvalue)]

#### Also,find the 95th percentile of the t distribution with the appropriate degrees of freedom to check whether the hypothesis is statistically significant.
qt(0.95, df = df)

##We can observe that the t-value is much larger than the 95th percentile value of
#the t-distribution for March and April - i.e. the increase in sales in the trial
#store in March and April is statistically greater than in the control store.
###??????????????????

#Let's create a more visual version of this by plotting the sales of the control store
# the sales of the trial stores
# the 95th percentile value of sales of the control store.

#### Trial and control store total sales
#### Create new variables Store_type, totSales and TransactionMonth in the data table.
pastSales <- measureOverTime[, Store_type := ifelse(STORE_NBR == trialStoreN, 'Trial',
                                                         ifelse(STORE_NBR == Control_Store,
                                                                'Control', 'Others'))
                                      ][, mean_sales:= mean(SALES), 
                                        by = c('YEARMONTH','Store_type')
                                            ][, TransactionMonth := as.Date(paste(YEARMONTH %/% 100, 
                                                                                  YEARMONTH %% 100, 
                                                                                  1, 
                                                                                  sep = "‐"), 
                                                                            "%Y‐%m‐%d")
                                                  ][Store_type %in% c("Trial", "Control"), ]

#### Control store 95th percentile
pastSales_Controls95 <- pastSales[Store_type == "Control",
                                  ][, mean_sales := mean_sales * (1 + stdDev * 2)
                                    ][, Store_type := "Control 95th % confidence interval"]

#### Control store 5th percentile
pastSales_Controls5 <- pastSales[Store_type == "Control",
                                 ][, mean_sales := mean_sales * (1 - stdDev * 2)
                                   ][, Store_type := "Control 5th % confidence interval"]

trialAssessment <- rbind(pastSales, pastSales_Controls95, pastSales_Controls5)

#### Plotting these in one nice graph
ggplot(trialAssessment, aes(TransactionMonth, mean_sales, color = Store_type)) +
    geom_rect(data = trialAssessment[ YEARMONTH < 201905 & YEARMONTH > 201901 ,],
              aes(xmin = min(TransactionMonth), 
                  xmax = max(TransactionMonth), 
                  ymin = 0 , 
                  ymax =Inf, 
                  color = NULL), 
              show.legend = FALSE) +
    geom_line() +
    labs(x = "Month of operation", y = "Total sales", 
         title = "Total sales by month of Trial Store #77") +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank())

##The results show that the trial in store 77 is significantly different to its
#control store in the trial period as the trial store performance lies outside the
#5% to 95% confidence interval of the control store in two of the three trial months.

#### Let's have a look at assessing this for number of customers as well
#### Scale pre-trial control customers to match pre-trial trial store customers
#### Compute a scaling factor to align control store customer countsto our trial store.
#### Then, apply the scaling factor to control store customer counts.
#### Finally, calculate the percentage difference between scaled control store customers and trial customers. 

scalingFactorForControlCustomers<- 
    preTrialMeasures[STORE_NBR == trialStoreN, sum(CUSTOMERS)]/
    preTrialMeasures[STORE_NBR == Control_Store, sum(CUSTOMERS)]
scalingFactorForControlCustomers

scaledControlCustomers <- 
    measureOverTime[STORE_NBR == Control_Store, 
    ][ ,controlCustomers := CUSTOMERS * scalingFactorForControlSales]

percentageDiff <- merge(scaledControlCustomers[, c("YEARMONTH", "controlCustomers")],
                        measureOverTime[STORE_NBR == trialStoreN,c("CUSTOMERS", "YEARMONTH")],
                        by = 'YEARMONTH'
                        )[, percentageDiff := abs(CUSTOMERS-controlCustomers)/controlCustomers]
percentageDiff
stdDev <- sd(percentageDiff[YEARMONTH < 201902 , percentageDiff]) 
stdDev

df <- 7 

percentageDiff[ , tvalue := (percentageDiff - 0)/stdDev
                ][ , TransactionMonth := as.Date(paste(YEARMONTH %/% 100, 
                                       YEARMONTH %% 100, 
                                       1, 
                                       sep = "-"),
                                 "%Y-%m-%d")
                   ][YEARMONTH < 201905 & YEARMONTH > 201901, 
                     .(TransactionMonth, tvalue)]

qt(0.95, df = df)

pastCustomers <- measureOverTime[, Store_type := ifelse(STORE_NBR == trialStoreN, 'Trial',
                                                    ifelse(STORE_NBR == Control_Store,
                                                           'Control', 'Others'))
                                 ][, mean_customers := mean(CUSTOMERS), 
                                   by = c('YEARMONTH','Store_type')
                                   ][, TransactionMonth := as.Date(paste(YEARMONTH %/% 100, 
                                      YEARMONTH %% 100, 
                                      1, 
                                      sep = "‐"), 
                                "%Y‐%m‐%d")
                                ][Store_type %in% c("Trial", "Control"), ]

#### Control store 95th percentile
pastCustomers_Controls95 <- pastCustomers[Store_type == "Control",
                                          ][, mean_customers := mean_customers * (1 + stdDev * 2)
                                            ][, Store_type := "Control 95th % confidence interval"]

#### Control store 5th percentile
pastCustomers_Controls5 <- pastCustomers[Store_type == "Control",
                                         ][, mean_customers := mean_customers * (1 - stdDev * 2)
                                           ][, Store_type := "Control 5th % confidence interval"]

trialAssessment <- rbind(pastCustomers, pastCustomers_Controls95, pastCustomers_Controls5)

#### Plotting these in one nice graph
ggplot(trialAssessment, aes(TransactionMonth, mean_customers, color = Store_type)) +
    geom_rect(data = trialAssessment[ YEARMONTH < 201905 & YEARMONTH > 201901 ,],
              aes(xmin = min(TransactionMonth), 
                  xmax = max(TransactionMonth), 
                  ymin = 0 , 
                  ymax =Inf, 
                  color = NULL), 
              show.legend = FALSE) +
    geom_line() +
    labs(x = "Month of operation", y = "Number of Customers", 
         title = "Number of Customers by month of Trial Store #77")+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank())


#############################################################################
#For Sales #86
trialStoreN <- 86

correlation_sales <- calCorr(preTrialMeasures,quote(SALES), trialStoreN)
correlation_sales <- unique(correlation_sales)
correlation_sales

correlation_customers <- calCorr(preTrialMeasures,quote(CUSTOMERS),trialStoreN)
correlation_customers

mag_sales <- standMag(calculateMagnitudeDistance(preTrialMeasures, quote(SALES), trialStoreN))
mag_sales[order(-magN_measure)] ###109

mag_customers <- standMag(calculateMagnitudeDistance(preTrialMeasures, quote(CUSTOMERS), trialStoreN))
mag_customers[order(-magN_measure)] ###155

corr_weight <- 0.5
score_nSales <- merge(correlation_sales,mag_sales, by = c('Store2','Store1')
                      )[,scoreNSales := (corr_weight*score_nSales$corr_measure)
                        + (1-corr_weight) * score_nSales$magN_measure]

score_nCustomers <- merge(correlation_customers,mag_sales, by = c('Store2','Store1')
                          )[, scoreNCust := corr_weight*corr_measure
                            + (1-corr_weight) * magN_measure]

score_Control <- merge(score_nSales, score_nCustomers, by = c('Store1','Store2'))
score_Control[, finalControlScore := (scoreNSales * 0.5) + (scoreNCust * 0.5)]
score_Control <- score_Control[Store2 != trialStoreN]
Control_Store <- score_Control[finalControlScore == max(finalControlScore),]$Store2
Control_Store  ###155

measureOverTime <- as.data.table(measure_whole)
pastSales <- measureOverTime[, Store_type := ifelse(STORE_NBR == trialStoreN,"Trial",
                                                    ifelse(STORE_NBR == Control_Store,
                                                           "Control Store",
                                                           "Other Stores"))
                             ][, mean_sales := mean(SALES), by = c("YEARMONTH","Store_type")
                               ][, TransactionMonth := as.Date(paste(YEARMONTH %/% 100, 
                                      YEARMONTH %% 100, 
                                      1, 
                                      sep = "‐"), 
                                "%Y‐%m‐%d")
                                ][YEARMONTH < 201902]

ggplot(data = pastSales, 
       aes(x = TransactionMonth, y = mean_sales, color = Store_type)) +
  geom_point() +
  geom_line()+
  labs(x = "Month of operation", 
       y = "Total sales ($)", 
       title = "Total sales by month of #86 Trial Store")+ 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank())

pastCustomers <- measureOverTime[, Store_type := ifelse(STORE_NBR == trialStoreN,"Trial",
                                                        ifelse(STORE_NBR == Control_Store,
                                                               "Control Store",
                                                               "Other Stores"))
                                 ][, mean_customers :=mean(CUSTOMERS), 
                                   by = c("YEARMONTH","Store_type")
                                   ][, TransactionMonth := as.Date(paste(YEARMONTH %/% 100, 
                                      YEARMONTH %% 100, 
                                      1, 
                                      sep = "‐"), 
                                "%Y‐%m‐%d")
                                ][YEARMONTH < 201902]

ggplot(data = pastCustomers, 
       aes(x = TransactionMonth, y = mean_customers, color = Store_type)) +
  geom_point() +
  geom_line()+
  labs(x = "Month of operation", 
       y = "Total Customers", 
       title = "Total customers by month of #86 Trial Store")+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank())

scalingFactorForControlSales <- 
  preTrialMeasures[STORE_NBR == trialStoreN, sum(SALES)]/
  preTrialMeasures[STORE_NBR == Control_Store, sum(SALES)]
scalingFactorForControlSales

scaledControlSales <- 
  measureOverTime[STORE_NBR == Control_Store, 
  ][ ,controlSales := SALES * scalingFactorForControlSales]

percentageDiff <- merge(scaledControlSales[, c("YEARMONTH", "controlSales")],
                        measureOverTime[STORE_NBR == trialStoreN,c("mean_sales", "YEARMONTH")],
                        by = 'YEARMONTH'
                        )[, percentageDiff := abs(controlSales-mean_sales)/controlSales]
percentageDiff
stdDev <- sd(percentageDiff[YEARMONTH < 201902 , percentageDiff]) 
stdDev

percentageDiff[ , tvalue := (percentageDiff - 0)/stdDev
                ][ , TransactionMonth := as.Date(paste(YEARMONTH %/% 100, 
                                       YEARMONTH %% 100, 
                                       1, 
                                       sep = "-"),
                                 "%Y-%m-%d")
                   ][YEARMONTH < 201905 & YEARMONTH > 201901, 
                     .(TransactionMonth, tvalue)]

qt(0.95, df = df)

pastSales <- measureOverTime[, Store_type := ifelse(STORE_NBR == trialStoreN, 'Trial',
                                                    ifelse(STORE_NBR == Control_Store,
                                                           'Control', 'Others'))
                             ][, mean_sales:= mean(SALES), 
                               by = c('YEARMONTH','Store_type')
                               ][, TransactionMonth := as.Date(paste(YEARMONTH %/% 100, 
                                      YEARMONTH %% 100, 
                                      1, 
                                      sep = "‐"), 
                                "%Y‐%m‐%d")
                                ][Store_type %in% c("Trial", "Control"), ]

pastSales_Controls95 <- pastSales[Store_type == "Control",
                                  ][, mean_sales := mean_sales * (1 + stdDev * 2)
                                    ][, Store_type := "Control 95th % confidence interval"]

pastSales_Controls5 <- pastSales[Store_type == "Control",
                                 ][, mean_sales := mean_sales * (1 - stdDev * 2)
                                   ][, Store_type := "Control 5th % confidence interval"]

trialAssessment <- rbind(pastSales, pastSales_Controls95, pastSales_Controls5)

ggplot(trialAssessment, aes(TransactionMonth, mean_sales, color = Store_type)) +
  geom_rect(data = trialAssessment[ YEARMONTH < 201905 & YEARMONTH > 201901 ,],
            aes(xmin = min(TransactionMonth), 
                xmax = max(TransactionMonth), 
                ymin = 0 , 
                ymax =Inf, 
                color = NULL), 
            show.legend = FALSE) +
  geom_line() +
  labs(x = "Month of operation", y = "Total sales", 
       title = "Total sales by month of Trial Store #86") +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank())

scalingFactorForControlCustomers<- 
  preTrialMeasures[STORE_NBR == trialStoreN, sum(CUSTOMERS)]/
  preTrialMeasures[STORE_NBR == Control_Store, sum(CUSTOMERS)]
scalingFactorForControlCustomers

scaledControlCustomers <- 
  measureOverTime[STORE_NBR == Control_Store, 
  ][ ,controlCustomers := CUSTOMERS * scalingFactorForControlSales]

percentageDiff <- merge(scaledControlCustomers[, c("YEARMONTH", "controlCustomers")],
                        measureOverTime[STORE_NBR == trialStoreN,c("CUSTOMERS", "YEARMONTH")],
                        by = 'YEARMONTH'
)[, percentageDiff := abs(CUSTOMERS-controlCustomers)/controlCustomers]
percentageDiff
stdDev <- sd(percentageDiff[YEARMONTH < 201902 , percentageDiff]) 
stdDev

df <- 7 

percentageDiff[ , tvalue := (percentageDiff - 0)/stdDev
][ , TransactionMonth := as.Date(paste(YEARMONTH %/% 100, 
                                       YEARMONTH %% 100, 
                                       1, 
                                       sep = "-"),
                                 "%Y-%m-%d")
][YEARMONTH < 201905 & YEARMONTH > 201901, 
  .(TransactionMonth, tvalue)]

qt(0.95, df = df)

pastCustomers <- measureOverTime[, Store_type := ifelse(STORE_NBR == trialStoreN, 'Trial',
                                                        ifelse(STORE_NBR == Control_Store,
                                                               'Control', 'Others'))
][, mean_customers := mean(CUSTOMERS), 
  by = c('YEARMONTH','Store_type')
][, TransactionMonth := as.Date(paste(YEARMONTH %/% 100, 
                                      YEARMONTH %% 100, 
                                      1, 
                                      sep = "‐"), 
                                "%Y‐%m‐%d")
][Store_type %in% c("Trial", "Control"), ]

pastCustomers_Controls95 <- pastCustomers[Store_type == "Control",
][, mean_customers := mean_customers * (1 + stdDev * 2)
][, Store_type := "Control 95th % confidence interval"]

pastCustomers_Controls5 <- pastCustomers[Store_type == "Control",
][, mean_customers := mean_customers * (1 - stdDev * 2)
][, Store_type := "Control 5th % confidence interval"]

trialAssessment <- rbind(pastCustomers, pastCustomers_Controls95, pastCustomers_Controls5)

ggplot(trialAssessment, aes(TransactionMonth, mean_customers, color = Store_type)) +
  geom_rect(data = trialAssessment[ YEARMONTH < 201905 & YEARMONTH > 201901 ,],
            aes(xmin = min(TransactionMonth), 
                xmax = max(TransactionMonth), 
                ymin = 0 , 
                ymax =Inf, 
                color = NULL), 
            show.legend = FALSE) +
  geom_line() +
  labs(x = "Month of operation", y = "Number of Customers", 
       title = "Number of Customers by month of Trial Store #86")+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank())







#For Sales #88
trialStoreN <- 88

correlation_sales <- calCorr(preTrialMeasures,quote(SALES), trialStoreN)
correlation_sales <- unique(correlation_sales)
correlation_sales

correlation_customers <- calCorr(preTrialMeasures,quote(CUSTOMERS),trialStoreN)
correlation_customers

mag_sales <- standMag(calculateMagnitudeDistance(preTrialMeasures, quote(SALES), trialStoreN))
mag_sales[order(-magN_measure)] ###237

mag_customers <- standMag(calculateMagnitudeDistance(preTrialMeasures, quote(CUSTOMERS), trialStoreN))
mag_customers[order(-magN_measure)] ###237

corr_weight <- 0.5
score_nSales <- merge(correlation_sales,mag_sales, by = c('Store2','Store1')
                      )[,scoreNSales := (corr_weight*score_nSales$corr_measure)
                        + (1-corr_weight) * score_nSales$magN_measure]

score_nCustomers <- merge(correlation_customers,mag_sales, by = c('Store2','Store1')
                          )[, scoreNCust := corr_weight*corr_measure
                            + (1-corr_weight) * magN_measure]

score_Control <- merge(score_nSales, score_nCustomers, by = c('Store1','Store2'))
score_Control[, finalControlScore := (scoreNSales * 0.5) + (scoreNCust * 0.5)]
score_Control <- score_Control[Store2 != trialStoreN]
Control_Store <- score_Control[finalControlScore == max(finalControlScore),]$Store2
Control_Store  ###237

measureOverTime <- as.data.table(measure_whole)
pastSales <- measureOverTime[, Store_type := ifelse(STORE_NBR == trialStoreN,"Trial",
                                                    ifelse(STORE_NBR == Control_Store,
                                                           "Control Store",
                                                           "Other Stores"))
][, mean_sales := mean(SALES), by = c("YEARMONTH","Store_type")
][, TransactionMonth := as.Date(paste(YEARMONTH %/% 100, 
                                      YEARMONTH %% 100, 
                                      1, 
                                      sep = "‐"), 
                                "%Y‐%m‐%d")
][YEARMONTH < 201902]

ggplot(data = pastSales, 
       aes(x = TransactionMonth, y = mean_sales, color = Store_type)) +
  geom_point() +
  geom_line()+
  labs(x = "Month of operation", 
       y = "Total sales ($)", 
       title = "Total sales by month of #88 Trial Store")+ 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank())

pastCustomers <- measureOverTime[, Store_type := ifelse(STORE_NBR == trialStoreN,"Trial",
                                                        ifelse(STORE_NBR == Control_Store,
                                                               "Control Store",
                                                               "Other Stores"))
][, mean_customers :=mean(CUSTOMERS), 
  by = c("YEARMONTH","Store_type")
][, TransactionMonth := as.Date(paste(YEARMONTH %/% 100, 
                                      YEARMONTH %% 100, 
                                      1, 
                                      sep = "‐"), 
                                "%Y‐%m‐%d")
][YEARMONTH < 201902]

ggplot(data = pastCustomers, 
       aes(x = TransactionMonth, y = mean_customers, color = Store_type)) +
  geom_point() +
  geom_line()+
  labs(x = "Month of operation", 
       y = "Total Customers", 
       title = "Total customers by month of #88 Trial Store")+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank())

scalingFactorForControlSales <- 
  preTrialMeasures[STORE_NBR == trialStoreN, sum(SALES)]/
  preTrialMeasures[STORE_NBR == Control_Store, sum(SALES)]
scalingFactorForControlSales

scaledControlSales <- 
  measureOverTime[STORE_NBR == Control_Store, 
  ][ ,controlSales := SALES * scalingFactorForControlSales]

percentageDiff <- merge(scaledControlSales[, c("YEARMONTH", "controlSales")],
                        measureOverTime[STORE_NBR == trialStoreN,c("mean_sales", "YEARMONTH")],
                        by = 'YEARMONTH'
)[, percentageDiff := abs(controlSales-mean_sales)/controlSales]
percentageDiff
stdDev <- sd(percentageDiff[YEARMONTH < 201902 , percentageDiff]) 
stdDev

percentageDiff[ , tvalue := (percentageDiff - 0)/stdDev
][ , TransactionMonth := as.Date(paste(YEARMONTH %/% 100, 
                                       YEARMONTH %% 100, 
                                       1, 
                                       sep = "-"),
                                 "%Y-%m-%d")
][YEARMONTH < 201905 & YEARMONTH > 201901, 
  .(TransactionMonth, tvalue)]

qt(0.95, df = df)

pastSales <- measureOverTime[, Store_type := ifelse(STORE_NBR == trialStoreN, 'Trial',
                                                    ifelse(STORE_NBR == Control_Store,
                                                           'Control', 'Others'))
][, mean_sales:= mean(SALES), 
  by = c('YEARMONTH','Store_type')
][, TransactionMonth := as.Date(paste(YEARMONTH %/% 100, 
                                      YEARMONTH %% 100, 
                                      1, 
                                      sep = "‐"), 
                                "%Y‐%m‐%d")
][Store_type %in% c("Trial", "Control"), ]

pastSales_Controls95 <- pastSales[Store_type == "Control",
][, mean_sales := mean_sales * (1 + stdDev * 2)
][, Store_type := "Control 95th % confidence interval"]

pastSales_Controls5 <- pastSales[Store_type == "Control",
][, mean_sales := mean_sales * (1 - stdDev * 2)
][, Store_type := "Control 5th % confidence interval"]

trialAssessment <- rbind(pastSales, pastSales_Controls95, pastSales_Controls5)

ggplot(trialAssessment, aes(TransactionMonth, mean_sales, color = Store_type)) +
  geom_rect(data = trialAssessment[ YEARMONTH < 201905 & YEARMONTH > 201901 ,],
            aes(xmin = min(TransactionMonth), 
                xmax = max(TransactionMonth), 
                ymin = 0 , 
                ymax =Inf, 
                color = NULL), 
            show.legend = FALSE) +
  geom_line() +
  labs(x = "Month of operation", y = "Total sales", 
       title = "Total sales by month of Trial Store #88") +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank())

scalingFactorForControlCustomers<- 
  preTrialMeasures[STORE_NBR == trialStoreN, sum(CUSTOMERS)]/
  preTrialMeasures[STORE_NBR == Control_Store, sum(CUSTOMERS)]
scalingFactorForControlCustomers

scaledControlCustomers <- 
  measureOverTime[STORE_NBR == Control_Store, 
  ][ ,controlCustomers := CUSTOMERS * scalingFactorForControlSales]

percentageDiff <- merge(scaledControlCustomers[, c("YEARMONTH", "controlCustomers")],
                        measureOverTime[STORE_NBR == trialStoreN,c("CUSTOMERS", "YEARMONTH")],
                        by = 'YEARMONTH'
)[, percentageDiff := abs(CUSTOMERS-controlCustomers)/controlCustomers]
percentageDiff
stdDev <- sd(percentageDiff[YEARMONTH < 201902 , percentageDiff]) 
stdDev

df <- 7 

percentageDiff[ , tvalue := (percentageDiff - 0)/stdDev
][ , TransactionMonth := as.Date(paste(YEARMONTH %/% 100, 
                                       YEARMONTH %% 100, 
                                       1, 
                                       sep = "-"),
                                 "%Y-%m-%d")
][YEARMONTH < 201905 & YEARMONTH > 201901, 
  .(TransactionMonth, tvalue)]

qt(0.95, df = df)

pastCustomers <- measureOverTime[, Store_type := ifelse(STORE_NBR == trialStoreN, 'Trial',
                                                        ifelse(STORE_NBR == Control_Store,
                                                               'Control', 'Others'))
][, mean_customers := mean(CUSTOMERS), 
  by = c('YEARMONTH','Store_type')
][, TransactionMonth := as.Date(paste(YEARMONTH %/% 100, 
                                      YEARMONTH %% 100, 
                                      1, 
                                      sep = "‐"), 
                                "%Y‐%m‐%d")
][Store_type %in% c("Trial", "Control"), ]

pastCustomers_Controls95 <- pastCustomers[Store_type == "Control",
][, mean_customers := mean_customers * (1 + stdDev * 2)
][, Store_type := "Control 95th % confidence interval"]

pastCustomers_Controls5 <- pastCustomers[Store_type == "Control",
][, mean_customers := mean_customers * (1 - stdDev * 2)
][, Store_type := "Control 5th % confidence interval"]

trialAssessment <- rbind(pastCustomers, pastCustomers_Controls95, pastCustomers_Controls5)

ggplot(trialAssessment, aes(TransactionMonth, mean_customers, color = Store_type)) +
  geom_rect(data = trialAssessment[ YEARMONTH < 201905 & YEARMONTH > 201901 ,],
            aes(xmin = min(TransactionMonth), 
                xmax = max(TransactionMonth), 
                ymin = 0 , 
                ymax =Inf, 
                color = NULL), 
            show.legend = FALSE) +
  geom_line() +
  labs(x = "Month of operation", y = "Number of Customers", 
       title = "Number of Customers by month of Trial Store #88")+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank())
