# Calculates the 10-year Treasury Bond vs 3-month T-Bill spread

# 10-year treasury bond yield
# from https://fred.stlouisfed.org/series/GS10
GS10 <- read.csv("./raw_data/GS10.csv", stringsAsFactors = FALSE)
head(GS10)
tail(GS10)
GS10$GS10 <- GS10$GS10 / 100

# 3-month T-bill rate
# from https://fred.stlouisfed.org/series/TB3MS
TB3MS <- read.csv("./raw_data/TB3MS.csv", stringsAsFactors = FALSE)
head(TB3MS)
tail(TB3MS)
names(TB3MS)[2] <- "Tbill.Rate"
TB3MS$Tbill.Rate <- TB3MS$Tbill.Rate / 100

combined.data <- merge(GS10, TB3MS, by="DATE")
# View(combined.data)
combined.data$DATE <- as.Date(combined.data$DATE, format="%Y-%m-%d")

# calculate the 10-Year T-Bill spread
combined.data$GS10.Tbill.Spread <- combined.data$GS10 - combined.data$Tbill.Rate
hist(combined.data$GS10.Tbill.Spread)
combined.data$Yield.Curve.Status <- "Positive"
combined.data$Yield.Curve.Status[combined.data$GS10.Tbill.Spread<0] <- "Inverted"
table(combined.data$Yield.Curve.Status)

write.table(combined.data, "./prepared_data/yield_curve_10y_3mo.tsv",sep="\t",
            row.names=FALSE)


