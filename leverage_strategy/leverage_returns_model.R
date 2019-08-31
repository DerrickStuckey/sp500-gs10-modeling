### model using yield curve, earnings yield, price momentum, fed funds rate as predictors
model.data <- read.csv("~/Downloads/M2 vs CPI scratch + Adjustable Leverage Model - model.data.tsv",sep = "\t", stringsAsFactors = FALSE)
head(model.data)
model.data$DATE <- as.Date(model.data$DATE,format="%m/%d/%Y")
model.data$FedFunds.Real <- model.data$FEDFUNDS - model.data$CPI.YoY
model.data$Risk.Premium <- model.data$Trailing.Earnings.Yield - model.data$FEDFUNDS
model.data$Risk.Premium.Real <- model.data$Trailing.Earnings.Yield - model.data$FedFunds.Real
model.data$SP.Forward.Return.1Y.to.3Y.Margin <- model.data$SP.Forward.Return.3Y.Margin - model.data$SP.Forward.Return.1Y.Margin
names(model.data)

# training/test split
training.data <- model.data[model.data$DATE<as.Date("2005-01-01"),]
tail(training.data)
test.data <- model.data[model.data$DATE>=as.Date("2006-01-01"),] # have to start 1 year after training data

# separate training data into positive and negative yield curve
pos.slope <- training.data[!is.na(training.data$GS10.FF.YC) & training.data$GS10.FF.YC>0,]
neg.slope <- training.data[!is.na(training.data$GS10.FF.YC) & training.data$GS10.FF.YC<=0,]

# test yield curve > 0 effect vs. various 1-year returns
boxplot(training.data$SP.Forward.Return.1Y.Compound.Untaxed ~ training.data$GS10.FF.YC>0)
boxplot(training.data$SP.Forward.Return.1Y.Compound.Taxable ~ training.data$GS10.FF.YC>0)
summary(pos.slope$SP.Forward.Return.1Y.Compound.Taxable)
summary(neg.slope$SP.Forward.Return.1Y.Compound.Taxable)
boxplot(training.data$SP.Forward.Return.1Y.Margin ~ training.data$GS10.FF.YC>0)
summary(pos.slope$SP.Forward.Return.1Y.Margin)
summary(neg.slope$SP.Forward.Return.1Y.Margin)
  # expected 1-year return still positive until you use margin, negative w/ margin

# test yield curve > 0 effect vs. various 3-year returns
boxplot(training.data$SP.Forward.Return.3Y.Compound.Untaxed ~ training.data$GS10.FF.YC>0)
boxplot(training.data$SP.Forward.Return.3Y.Compound.Taxable ~ training.data$GS10.FF.YC>0)
summary(pos.slope$SP.Forward.Return.3Y.Compound.Taxable)
summary(neg.slope$SP.Forward.Return.3Y.Compound.Taxable)
boxplot(training.data$SP.Forward.Return.3Y.Margin ~ training.data$GS10.FF.YC>0)
summary(pos.slope$SP.Forward.Return.3Y.Margin)
summary(neg.slope$SP.Forward.Return.3Y.Margin)
  # expected 3-year return still positive until you use margin, negative w/ margin

# does this mean we should not use margin for a full 3 years after an inversion, or is most of the damage done earlier?

boxplot(training.data$SP.Forward.Return.1Y.to.3Y.Margin ~ training.data$GS10.FF.YC > 0)
summary(pos.slope$SP.Forward.Return.1Y.to.3Y.Margin)
summary(neg.slope$SP.Forward.Return.1Y.to.3Y.Margin)
  # apparently even for months 12 to 36 after an inversion, expected returns are negative

# scratch
plot(training.data$DATE, training.data$GS10.FF.YC,type="l")
abline(h=0)

plot(pos.slope$GS10.FF.YC, pos.slope$SP.Forward.Return.1Y.Compound.Untaxed)
cor(pos.slope$GS10.FF.YC, pos.slope$SP.Forward.Return.1Y.Compound.Untaxed)
plot(pos.slope$GS10.FF.YC, pos.slope$SP.Forward.Return.3Y.Compound.Untaxed)
cor(pos.slope$GS10.FF.YC, pos.slope$SP.Forward.Return.3Y.Compound.Untaxed)

