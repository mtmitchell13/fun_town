## this script takes the inventory of MLS report IDs and exports them to csv

## package dependencies
library(reshape2)
library(plyr)

## source function
source("readMLS.R")

## read report IDs
rpt <- read.csv("reports.csv", stringsAsFactors = FALSE)

## build data frame
r1 <- data.frame()
for (i in 1:length(rpt$reportId)) {
        r1 <- rbind.fill(r1, readMLS(rpt$reportId[i]))
}

## add report date
r1 <- merge(r1, rpt, by = "reportId", all.x = TRUE)

## write to csv
write.csv(r1, "mls.csv", row.names = FALSE)

## remove objects except for mls df
rm(list = c("rpt", "i", "readMLS"))
