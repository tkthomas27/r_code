# http://www.win-vector.com/blog/2016/02/using-postgresql-in-r/



iris = as.data.frame(iris)
skim(iris)

# make names db safe: no '.' or other illegal characters,
# all lower case and unique
dbSafeNames = function(names) {
    names = gsub('[^a-z0-9]+','_',tolower(names))
    names = make.names(names, unique=TRUE, allow_=TRUE)
    names = gsub('.','_',names, fixed=TRUE)
    names
}
colnames(iris) = dbSafeNames(colnames(iris))

# Create a connection to the database
library(RPostgreSQL)
library(DBI)

## Loading required package: DBI
pg = dbDriver("PostgreSQL")

# Local Postgres.app database; no password by default
# Of course, you fill in your own database information here.
con = dbConnect(pg, user="kthomas1", password="",
                host="localhost", port=5432, dbname="kthomas1")

# write the table into the database.
# use row.names=FALSE to prevent the query from adding the column 'row.names' to the table in the db
dbWriteTable(con,'iris',iris, row.names=FALSE)

# read back the full table: method 1
dtab = dbGetQuery(con, "select * from iris")

# read back the full table: method 2
rm(dtab)
dtab = dbReadTable(con, "iris")

# get part of the table
rm(dtab)
dtab = dbGetQuery(con, "select sepal_length, species from iris")
summary(dtab)





