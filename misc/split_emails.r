
library(caTools)

# read in csvs
alumni<-read_csv('alumni.csv')
faculty<-read_csv('faculty.csv')
internal<-read_csv('internal.csv')
mary <- read_csv('mary.csv')
media<-read_csv('media.csv')
vip <- read_csv('vip.csv')
video_producers <- read_csv('video_producers.csv')
news<-read_csv('news_events.csv')

# function for splitting the data and saving it to a csv
email_split <-function(df){

    # create a vector of true/false to split data
    split_vector = sample.split(df$`Email Lists`, SplitRatio = 1/2)
    # create first split
    split1 = subset(df, split_vector == TRUE)
    split1['group'] <- "A"
    split1['dataset'] <- deparse(substitute(df))
    # create second split
    split2  = subset(df, split_vector == FALSE)
    split2['group'] <- "B"
    split2['dataset'] <- deparse(substitute(df))

    a <- rbind(split1,split2)

    return(a)
}

alumni_split <- email_split(alumni)
faculty_split <- email_split(faculty)
internal_split <- email_split(internal)
mary_split <- email_split(mary)
media_split <- email_split(media)
news_split <- email_split(news)
vip_split <- email_split(vip)
video_producers <- email_split(video_producers)


grouped_email <- alumni_split %>%
    bind_rows(faculty_split,internal_split,mary_split,media_split,news_split,vip_split) %>%
    mutate(email = case_when(
        !is.na(`Email address - other`) ~ `Email address - other`,
        !is.na(`Email address - work`) ~ `Email address - work`,
        !is.na(`Email address - home`) ~ `Email address - home`)) %>%
    distinct(email,.keep_all=TRUE) %>%
    select(email, group, dataset)

write_csv(grouped_email,"grouped_email.csv")



a <- alumni_split %>%
    bind_rows(faculty_split,internal_split,mary_split,media_split,news_split,vip_split) %>%
    mutate(email = case_when(
        !is.na(`Email address - other`) ~ `Email address - other`,
        !is.na(`Email address - work`) ~ `Email address - work`,
        !is.na(`Email address - home`) ~ `Email address - home`)) %>%
    select(email, group, dataset) %>%
    mutate(email = str_trim(email)) %>%
    count(email) %>%
    filter(n>1)

write_csv(a,"duplicate_emails.csv")
