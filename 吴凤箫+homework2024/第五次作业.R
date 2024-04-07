#---------------------------------------------------------------------------------
#script Name: r_database.R
#Purpose: This illustrats how to upload the data of Doubs, a built-in dataset of ade4 package into a schema of PostgreSQL or the SQLite.
#Author: Fengxiao Wu
#Email: wfx1876@163.com
#Date: 2024-04-07
#---------------------------------------------------------------------------------

# Load required libraries
library(reticulate)
library(rdataretriever)
library(ade4)
library(RSQLite)
# Connect to your PostgreSQL or SQLite database
# Replace 'username', 'password', 'host', 'port', 'dbname' with your actual database credentials
# For PostgreSQL:
db <- dbConnect(RPostgres::Postgres(),
                 dbname = "dbname",
                 host = "host",
                 port = "port",
                 user = "username",
                 password = "password")
# For SQLite:
 db <- dbConnect(SQLite(), dbname = "your_database.db")

# Load Doubs dataset from ade4 package
data(doubs, package = "ade4")

# Convert Doubs dataset to data frame
doubs_df <- data.frame(doubs)

# Upload Doubs data frame to your database
# For PostgreSQL:
 dbWriteTable(db, "doubs_table", doubs_df, schema = "your_schema_name")
# For SQLite:
 dbWriteTable(db, "doubs_table", doubs_df)

# Close database connection
library(DBI)
dbDisconnect(db)