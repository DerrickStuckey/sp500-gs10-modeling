# from http://databank.worldbank.org/data/AgeGenderHistory/id/37786463
demodata.wide <- read.table("./raw_data/AgeGenderHistory/3c9c8018-e1c0-4fa5-82bc-5b1488c1d1d2_Data.txt",
                       sep="\t",header=TRUE)
head(demodata.wide)
names(demodata.wide)

## transform the demo data to a usable format
library(reshape2)

demodata.long <- melt(demodata.wide, id.vars=c("Country.Name","Country.Code","Series.Name","Series.Code"))
names(demodata.long)[names(demodata.long)=="variable"] <- "Year"
head(demodata.long)

demodata.long <- subset(demodata.long,select=c("Country.Name","Series.Name","Year","value"))
demodata.reshaped <- dcast(demodata.long, Country.Name + Year ~ Series.Name, value.var="value", fun.aggregate=mean)
# View(demodata.reshaped)

# convert the 'Year' values to integers
demodata.reshaped$Year <- as.numeric(substr(as.character(demodata.reshaped$Year),2,5))
table(demodata.reshaped$Year)

demodata.reshaped <- demodata.reshaped[demodata.reshaped$Country.Name!="",]
demodata.reshaped <- subset(demodata.reshaped,select=-c(Var.3))
# View(demodata.reshaped)

write.table(demodata.reshaped, file="./prepared_data/age_gender_history.tsv",
            sep="\t",row.names=FALSE)

