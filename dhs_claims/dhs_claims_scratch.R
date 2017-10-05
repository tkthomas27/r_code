library(tidyverse)
library(googlesheets)


x <- gs_read(gs_title("dhs_claims"), ws = "2015")

glimpse(x)
table(x$`Airport Code`)



# convert dates
# code item category
# convert close amount
# categorize disposition; claim type
# need airport statistics; drop if "small" airport <- non-military activity is greater than 25th percentile
# need airport location; match in data <- used for creating maps
# for medium post; will have make maps and then link to external shiny app

# filter to big airports; get coords
faa_airport <- gs_read(gs_title("faa_airports"), ws = "data") %>%
    filter(`ASSET GroupName` %in% c("National","Regional")) %>%
    select(`Loc ID`,Longitude,Latitude) %>%
    rename(code = `Loc ID`, lon=Longitude, lat=Latitude)

# create location data
airport_loc<-read_csv("~/github_data/airports.csv") %>%
    filter(country=="United States" & iata!="\\N") %>%
    select(iata,lat,lon) %>%
    rename(code = iata)

# get airport business data
airport_busy<-read_csv("~/github_data/faa_flights.csv") %>%
    filter(!grepl("Sub|Total",facility)) %>%
    mutate(pct_military = (military + `military local`)/total) %>%
    filter(pct_military<.5) %>%
    group_by(`calendar year`) %>% mutate(busy25 = quantile(total,.25)) %>%
    filter(total>busy25) %>%
    rename(code = facility, year = `calendar year`) %>%
    select(code, year, total)

# read in claims data from google sheets
claims_2015 <- gs_read(gs_title("dhs_claims"), ws = "2015") %>% rename(`Incident Date` = `Incident D`)
claims_2014 <- gs_read(gs_title("dhs_claims"), ws = "2014")
claims_2010_2013 <- gs_read(gs_title("dhs_claims"), ws = "2010-2013")
claims_2007_2009 <- gs_read(gs_title("dhs_claims"), ws = "2007-2009") %>% select(-Status,-`Claim Amount`) %>% rename(`Item Category` = Item)
claims_2002_2006 <- gs_read(gs_title("dhs_claims"), ws = "2002-2006") %>% select(-Status,-`Claim Amount`) %>% rename(`Item Category` = Item,`Claim Number` = eets)


claims <- rbind(claims_2015,claims_2014,claims_2010_2013,claims_2007_2009,claims_2002_2006) %>%
    rename(code = `Airport Code`) %>%
    filter(!is.na(`Incident Date`)) %>%
    mutate(gdate = gsub(" .*","",`Incident Date`),
           date = case_when(regexpr('/',gdate)>0 ~ as.Date(gdate,'%m/%d/%y'),
                            regexpr('-',gdate)>0 ~ as.Date(gdate,'%d-%b-%y')),
           year = as.integer(format(date,'%Y')),
           amount = case_when(`Close Amount`=="-" ~ 0,
                              is.na(`Close Amount`)==TRUE ~ 0,
                              regexpr('\\$',`Close Amount`)>0 ~ as.numeric(gsub("\\$","",`Close Amount`)))) %>%
    replace_na(list(amount = 0)) %>%
    inner_join(airport_loc, by="code") %>% inner_join(airport_busy,by=c("code","year")) %>%
    rename(name = `Airport Name`) %>%
    select(code, year, name, lat, lon, total, amount)





library(ggmap)

############################################################
x <- claims %>%
    group_by(code, name, lat, lon) %>%
    summarise(pass = sum(total), cash = sum(amount)) %>%
    mutate(pc = cash/pass) %>%
    arrange(desc(pc)) %>%
    head(10)

us <- c(left = -125, bottom = 25.75, right = -67, top = 49)
map <- get_stamenmap(us, zoom = 5, maptype = "toner-lite")
ggmap(map)

qmplot(lon, lat, data = x, maptype = "toner-lite", color = I("red"), size=pass)
############################################################














# items <- claims %>% select(`Item Category`) %>%
#     rename(items = `Item Category`) %>%
#     separate(items,range(a:z),sep=";")




