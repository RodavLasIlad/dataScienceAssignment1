setwd("~/dataAnalysis/")
d <- read.csv("loansData.csv")
ir <- as.numeric(sub("%","",d$Interest.Rate))
fico <- as.numeric(lapply(strsplit(as.vector(d$FICO.Range),"-"),"[",1))
amountr <- d$Amount.Requested
funded <- d$Amount.Funded.By.Investors
length <- d$Loan.Length
purpose <- d$Loan.Purpose
dtir <- d$Debt.To.Income.Ratio
state <- d$State
home <- d$Home.Ownership
income <- d$Monthly.Income
credit <- d$Open.CREDIT.Lines
creditbal <- d$Revolving.Credit.Balance
demiyear <- d$Inquiries.in.the.Last.6.Months
employment <- d$Employment.Length
barplot(table(fico))
library(Hmisc)
ir1 <- cut2(ir, g=50)
plot(ir1)
model1 <- lm(ir ~ fico + amountr)
model2 <- lm(ir ~ fico + funded)
model3 <- lm(ir ~ fico + demiyear)
model4 <- lm(ir ~ fico + amountr + funded + demiyear)

library(maps)
library(ggplot2)
# a <- aggregate(ir,list(state),mean)
a <- aggregate(ir, list(state),mean)

a$fullnames <- c("alaska", "alabama", "arkansas", "arizona", "california", "colorado", "connecticut", "district of columbia", "delaware", "florida", "georgia", "hawaii", "iowa", "illinois", "indiana", "kansas", "kentucky", "louisiana", "massachusetts", "maryland", "michigan", "minnesota", "missouri", "mississippi", "montana", "north carolina", "new hampshire", "new jersey", "new mexico", "nevada", "new york", "ohio", "oklahoma", "oregon", "pennsylvania", "rhode island", "south carolina", "south dakota", "texas", "utah", "virginia", "vermont", "washington", "wisconsin", "west virginia", "wyoming")
library(plyr)
states_map <- map_data("state")
levels(a$Group.1) <- c(levels(a$Group.1), "ID", "ND", "NE", "ME", "TN")
a <- rbind(a, c("ID", NA, "idaho"), c("ND", NA, "north dakota"), c("NE", NA, "nebraska"), c("ME", NA, "maine"), c("TN", NA, "tennessee"))
heat_map <- merge(states_map, a, by.x="region", by.y="fullnames")

heatmapfixed=ddply(heat_map,.(region), function(x) x[order(x$order),])
ggplot(heatmapfixed, aes(x=long, y=lat, group=group, fill=as.numeric(x))) +
scale_fill_continuous(low="yellow1",high="red1") +
geom_polygon(colour="black") +
labs(fill="Average Interest Rate by %", title = "Figure 1: Heat map of Interest Rates in the US")





