### model using yield curve, earnings yield, price momentum, fed funds rate as predictors
model.data <- read.csv("~/Downloads/M2 vs CPI scratch - model.data.tsv",sep = "\t", stringsAsFactors = FALSE)
head(model.data)
model.data$DATE <- as.Date(model.data$DATE,format="%m/%d/%Y")
model.data$AF <- model.data$Trailing.Earnings.Yield + model.data$GS10.FF.YC
model.data$Excess.Return.1Y <- model.data$SP.Forward.Return.1Y - model.data$FEDFUNDS
model.data$Excess.Return.3Y <- model.data$SP.Forward.Price.Change.3Y + 3 * (model.data$Dividend.Yield - model.data$FEDFUNDS) # TODO update this to use actual Fed Funds rate going forward instead of just current
model.data$Excess.Return.1M <- model.data$SP.Forward.Price.Change.1M - model.data$FEDFUNDS/12 # ignores dividends TODO fix this
model.data$FedFunds.Real <- model.data$FEDFUNDS - model.data$CPI.YoY
model.data$Risk.Premium <- model.data$Trailing.Earnings.Yield - model.data$FEDFUNDS
model.data$Risk.Premium.Real <- model.data$Trailing.Earnings.Yield - model.data$FedFunds.Real
names(model.data)

# training/test split
training.data <- model.data[model.data$DATE<as.Date("2005-01-01"),]
tail(training.data)
test.data <- model.data[model.data$DATE>=as.Date("2006-01-01"),] # have to start 1 year after training data

# test various combinations of predictors
full.lm.tr1y <- lm(SP.Forward.Return.1Y ~ Trailing.Earnings.Yield + GS10.FF.YC + FEDFUNDS + 
                     SP.Momentum.1Y + SP.Momentum.3Y + SP.Momentum.6Mo + CPI.YoY,
                   data=training.data)
summary(full.lm.tr1y)

full.lm.er1y <- lm(Excess.Return.1Y ~ Trailing.Earnings.Yield + GS10.FF.YC + FEDFUNDS + SP.Momentum.1Y + SP.Momentum.6Mo,
                   data=training.data)
summary(full.lm.er1y)

full.lm.pc1m <- lm(SP.Forward.Price.Change.1M ~ Trailing.Earnings.Yield + GS10.FF.YC + FEDFUNDS + SP.Momentum.1Y + SP.Momentum.6Mo,
                   data=training.data)
summary(full.lm.pc1m)

lm.er.trim1 <- lm(Excess.Return.1Y ~ Trailing.Earnings.Yield + GS10.FF.YC + FedFunds.Real,
                   data=training.data)
summary(lm.er.trim1)

cor(training.data$SP.Forward.Price.Change.1M, training.data$SP.Momentum.1Y)

mo.lm.1Y <- lm(Excess.Return.1Y ~ SP.Momentum.6Mo + SP.Momentum.1Y, data=training.data)
summary(mo.lm.1Y)
plot(predict(mo.lm,newdata=training.data),training.data$Excess.Return.1Y)

mo.lm.1M <- lm(SP.Forward.Price.Change.1M ~ SP.Momentum.6Mo + SP.Momentum.1Y, data=training.data)
summary(mo.lm.1M)
plot(predict(mo.lm.1M,newdata=training.data),training.data$SP.Forward.Price.Change.1M)

mo.yc.lm <- lm(SP.Forward.Price.Change.1M ~ SP.Momentum.6Mo + GS10.FF.YC, data=training.data)
summary(mo.yc.lm)
plot(predict(mo.yc.lm,newdata=training.data),training.data$SP.Forward.Price.Change.1M)

mean(training.data$Excess.Return.1Y[training.data$pres.tf.lm>0.1])
mean(training.data$Excess.Return.1Y)
mean(training.data$FEDFUNDS)
mean(training.data$SP.Forward.Return.1Y)
mean(training.data$Excess.Return.1Y[training.data$GS10.FF.YC>0.02])

# earnings yield only
lm.ey <- lm(Excess.Return.1Y ~ Trailing.Earnings.Yield,data=training.data)
summary(lm.ey)
plot(predict(lm.ey,newdata=training.data), training.data$Excess.Return.1Y)

# yield curve only
lm.yc <- lm(Excess.Return.1Y ~ GS10.FF.YC,data=training.data)
summary(lm.yc)
plot(predict(lm.yc,newdata=training.data), training.data$Excess.Return.1Y)

# earnings yield and yield curve only
lm.ey.yc <- lm(Excess.Return.1Y ~ Trailing.Earnings.Yield + GS10.FF.YC,data=training.data)
summary(lm.ey.yc)
plot(predict(lm.ey.yc,newdata=training.data), training.data$Excess.Return.1Y)

# risk premium and yield curve only
lm.rp.yc <- lm(Excess.Return.1Y ~ Risk.Premium + GS10.FF.YC,data=training.data)
summary(lm.rp.yc)
plot(predict(lm.rp.yc,newdata=training.data), training.data$Excess.Return.1Y)

# actual used model
full.lm <- lm(Excess.Return.1Y ~ GS10.FF.YC, data=training.data)
summary(full.lm)
training.data$preds.full.lm <- predict(full.lm,newdata=training.data)
plot(training.data$preds.full.lm, training.data$Excess.Return.1Y)

# look at returns vs predictors in only the subset of the data w/ a positive yield curve
pos.slope <- training.data[!is.na(training.data$GS10.FF.YC) & training.data$GS10.FF.YC>0,]
neg.slope <- training.data[!is.na(training.data$GS10.FF.YC) & training.data$GS10.FF.YC<=0,]

cor(pos.slope$SP.Forward.Return.1Y,pos.slope$GS10.FF.YC)
plot(pos.slope$GS10.FF.YC,pos.slope$SP.Forward.Return.1Y)
plot(training.data$GS10.FF.YC,training.data$SP.Forward.Return.1Y)
# seems that the yield curve is only positively correlated with returns when YC > 0
cor(pos.slope$SP.Forward.Return.1Y,pos.slope$Trailing.Earnings.Yield)
plot(pos.slope$Trailing.Earnings.Yield,pos.slope$SP.Forward.Return.1Y)

# look at other relationships in the positive-slope period only 
cor(pos.slope$SP.Forward.Price.Change.1Y,pos.slope$SP.Momentum.1Y)
cor(pos.slope$SP.Forward.Price.Change.1M,pos.slope$SP.Momentum.6Mo)
plot(pos.slope$SP.Momentum.6Mo,pos.slope$SP.Forward.Price.Change.1M)
plot(neg.slope$SP.Momentum.6Mo,neg.slope$SP.Forward.Price.Change.1M)

boxplot(pos.slope$Excess.Return.1M ~ pos.slope$SP.Momentum.6Mo>0)
summary(pos.slope$Excess.Return.1M[pos.slope$SP.Momentum.6Mo>0])
summary(pos.slope$Excess.Return.1M[pos.slope$SP.Momentum.6Mo<0])

# risk premium vs 1-year excess returns
cor(pos.slope$Risk.Premium,pos.slope$Excess.Return.1Y)
plot(pos.slope$Risk.Premium,pos.slope$Excess.Return.1Y)

cor(pos.slope$Risk.Premium.Real,pos.slope$Excess.Return.1Y)
plot(pos.slope$Risk.Premium.Real,pos.slope$Excess.Return.1Y)

# risk premium vs 1-month excess returns
cor(pos.slope$Risk.Premium,pos.slope$Excess.Return.1M)
plot(pos.slope$Risk.Premium,pos.slope$Excess.Return.1M)

cor(pos.slope$Risk.Premium.Real,pos.slope$Excess.Return.1M)
plot(pos.slope$Risk.Premium.Real,pos.slope$Excess.Return.1M)

boxplot(pos.slope$Excess.Return.1M~pos.slope$Risk.Premium>0)
boxplot(pos.slope$Excess.Return.1M~pos.slope$Risk.Premium.Real>0.03)

# geometric mean of returns
geom.mean <- function(return.series) {
  return(exp(sum(log(return.series+1))/length(return.series))-1)
}
geom.mean(training.data$SP.Forward.Return.1Y)
geom.mean(training.data$Excess.Return.1Y)

geom.mean(training.data$Excess.Return.1Y[training.data$pres.tf.lm>0])

cutoffs <- seq(-0.15,0.12,0.01)
returns <- c()
for (i in 1:length(cutoffs)) {
  cutoff <- cutoffs[i]
  gr <- geom.mean(training.data$Excess.Return.1Y[training.data$pres.tf.lm>cutoff])
  returns <- c(returns,gr)
}
plot(cutoffs,returns)

# plot predictions and actual returns over time
plot(training.data$DATE,training.data$Excess.Return.1Y,type="l",col="blue")
plot(training.data$DATE,training.data$Excess.Return.1Y,type="l",col="blue")
lines(training.data$DATE,training.data$pres.tf.lm,col="red")
abline(h=0.03,col="black")



