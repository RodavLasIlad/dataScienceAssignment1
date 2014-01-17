
> names(data)
 [1] "Amount.Requested"               "Amount.Funded.By.Investors"    
 [3] "Interest.Rate"                  "Loan.Length"                   
 [5] "Loan.Purpose"                   "Debt.To.Income.Ratio"          
 [7] "State"                          "Home.Ownership"                
 [9] "Monthly.Income"                 "FICO.Range"                    
[11] "Open.CREDIT.Lines"              "Revolving.CREDIT.Balance"      
[13] "Inquiries.in.the.Last.6.Months" "Employment.Length" 

shortcuts:
amountr <- d$Amount.Requested
amountfunded <- d$Amount.Funded.By.Investors
length <- d$Loan.Length
purpose <- d$Loan.Purpose
dtir <- as.numeric(sub("%","",d$Debt.To.Income.Ratio))
state <- d$State
home <- d$Home.Ownership
income <- d$Monthly.Income
credit <- d$Open.CREDIT.Lines
creditbal <- d$Revolving.CREDIT.Balance
demiyear <- d$Inquiries.in.the.Last.6.Months
employment <- d$Employment.Length

barplot(table(data$FICO.Range))
barplot(table(sort(as.numeric(data$Interest.Rate))))
ir <- as.numeric(sub("%","",data$Interest.Rate))
plot(ir ~ purpose, cex.axis=0.6)
plot(ir ~ length)
plot
hist(ir)
fico <- as.numeric(lapply(strsplit(as.vector(data$FICO.Range),"-"),"[",1))   
[1] 660 810 740 690 760
> as.numeric(lapply(strsplit(data$FICO.Range[1:5],"-"),"[",2))
boxplot(ir ~ as.factor(fico), col = "blue")
plot(ir, fico, pch=19, col="blue")
cor.test(ir, fico)
p-value 2.2e-16
cor -0.709
model1 <- lm(ir ~ fico)
model2 <- lm(ir ~ fico + data$Amount.Requested)
plot(ir + fico, data$Amount.Funded.By.Investors, pch =19)

library(Hmisc)
ir1 <- cut2(ir, g=50)
plot(ir1)
cor.test(ir, 
plot(d$Amount.Funded.By.Investors)

library(maps)
map("state", boundary = FALSE, col="gray", add = TRUE)
map("usa")
> map("state", boundary = FALSE, col="gray", add = TRUE)
> all_states <- map_data("state")
Error: could not find function "map_data"
> help(map)
> library(ggplot2)
> all_states <- map_data("state")

plot(ir ~ purpose, cex.axis=0.6)
plot(ir ~ state, cex.axis=0.5)

tf= structure(list(state = structure(1:14, .Label = c("AK", "AL", 
"AR", "AZ", "CA", "CO", "CT", "DE", "FL", "GA", "IA", "IL", "IN", 
"KS"), class = "factor"), num = c(21L, 31L, 12L, 56L, 316L, 53L, 
31L, 7L, 335L, 63L, 42L, 73L, 40L, 2L), region = structure(c(2L, 
1L, 4L, 3L, 5L, 6L, 7L, 8L, 9L, 10L, 13L, 11L, 12L, 14L), .Label = c("alabama", 
"alaska", "arizona", "arkansas", "california", "colorado", "connecticut", 
"delaware", "florida", "georgia", "illinois", "indiana", "iowa", 
"kansas"), class = "factor")), .Names = c("state", "num", "region"
), class = "data.frame", row.names = c(NA, -14L))

require(maps);require(ggplot2)

states <- map_data("state")
tfmerged <- merge(states, tf, sort = FALSE, by = "region")
tfmerged <- tfmerged[order(tfmerged$order), ]
qplot(long, lat, data = tfmerged, group = group, fill = num,
geom="polygon")

levels(a2$Group.1) <- c(levels(a2$Group.1), "ID", "ND", "NE", "ME", "TN")
a2[47,] <- c("ID", 0, "idaho")

 levels(a2$Group.1) <- c(levels(a2$Group.1), "ID", 

ggplot(crime_map, aes(x=long, y=lat, group=group, fill=Assault)) +
geom_polygon(colour="black") +
coord_map("polyconic")

levels(iris$Species) <- c(levels(iris$Species), "new.species")

states_map <- map_data("state")
model1 <- lm(ir ~ fico + amountr)
model2 <- lm(ir ~ fico + funded)
model3 <- lm(ir ~ fico + demiyear)
