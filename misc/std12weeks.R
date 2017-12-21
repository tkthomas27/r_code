# *------------------------------------------------------------------
# | PROGRAM NAME: 12 week standard deviation
# | DATE: 2017-12-07
# | CREATED BY: kyle thomas
# *----------------------------------------------------------------

#set working directory
setwd("/Volumes/tsandino_gomobile_project/Go Mobile/Paper_Daily Incentives_Select Stores/C. Stata files")

# load libraries
library(tidyverse) #data processing
library(xts) # time series
library(zoo) # time series
library(padr) # adding missing days if desired

# *----------------------------------------------------------------
# initialize data
# *----------------------------------------------------------------
# read in data
df <- read_csv("sales_variability_r.csv")
# format date
df$date <- as.Date(df$date, "%d %b %y")

#split data by store
dfs <- split(df,df$store)

# *----------------------------------------------------------------
# calculate rolling 84 day weekly standard deviation
# *----------------------------------------------------------------

# create empty dataframe to store the results
store_results <- data.frame(date=NA, store=NA, sales = NA, std_12=NA)

# loop through each store
for(z in 1:35){

    dfx <- dfs[[z]] #load select store

    #start with indices 1-84 and increment by 1 until none is left
    i<-1
    j<-84

    # create empty dataframe to store standard deviation results
    date = as.Date("2000-01-01")
    std_12 = 1111
    std_results = data.frame(date,std_12)

    # if we want, we can pad out missing days
    #dfx <- pad(dfx) %>% replace_na(list(sales = 0))

    # roll forward 84 day window by 1 and compute standard deviation on weekly basis
    while(j<nrow(dfx)){
        #subset for 84 days
        df1 <- dfx[i:j,]
        #make into a time series object
        df2 <- xts(df1[,3], order.by = df1$date)
        #down sample to weekly by summing sales amounts
        weeks <- period.apply(df2, INDEX = endpoints(df2, on="days", k=7), FUN = sum)
        #compute standard deviation with anti-Bessel's correction
        std_12_week <- sqrt(sd(weeks$sales)^2 * (11/12))
        #add to stored results
        std_results <- rbind(std_results, c(as.character(index(df2[84])), round(std_12_week,2)))
        #increment indices
        i<-i+1
        j<-j+1
    }

    #match store's std results to original data series; this may not be necessary
    df3 <- inner_join(dfx, std_results)
    #add to overall results file
    store_results <- rbind(store_results, df3)

}

# *----------------------------------------------------------------
# store results
# *----------------------------------------------------------------

# drop blank first row
store_results <- store_results[2:nrow(store_results),]
# convert dates
store_results$date <- as.Date(store_results$date)
# write to csv
write_csv(store_results,"store_results.csv")
