
library(caTools)

# read in csvs
alumni<-read_csv('contact_export_111017_1107 (1).csv')
faculty<-read_csv('contact_export_111017_1107.csv')
internal<-read_csv('contact_export_111017_1108 (1).csv')
mary <- read_csv('contact_export_111017_1106.csv')
media<-read_csv('contact_export_111017_1107 (3).csv')
news<-read_csv('contact_export_111017_1108.csv')
vip <- read_csv('contact_export_111017_1107 (2).csv')

# function for splitting the data and saving it to a csv
email_split <-function(df){
    # create a vector of true/false to split data
    split_vector = sample.split(df, SplitRatio = .5)
    # create first split
    split1 = subset(df, split_vector == TRUE)
    # create second split
    split2  = subset(df, split_vector == FALSE)
    # save first split to csv
    write.csv(split1, paste("split1_", deparse(substitute(df)),".csv"), row.names=F)
    # save second split to csv
    write.csv(split2, paste0("split2_", deparse(substitute(df)),".csv"), row.names=F)
}

email_split(alumni)
email_split(faculty)
email_split(internal)
email_split(mary)
email_split(media)
email_split(news)
email_split(vip)






