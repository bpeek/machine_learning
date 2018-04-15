library(readr)
library(ggplot2)
library(maps)
library(pscl)
library(knitr)

RejectStats_2017Q1 <- read_csv("C:/Users/Brendan/Desktop/Datasets/Lending Club/RejectStats_2017Q1.csv")
LoanStats_2017Q1 <- read_csv("C:/Users/Brendan/Desktop/Datasets/Lending Club/LoanStats_2017Q1.csv")

"""
Predictor Vars
  LoanAmt (Continuous)
    The listed amount of the loan applied for by the borrower. If at some point in time, the credit department   
    reduces the loan amount, then it will be reflected in this value.
  Title (Categ.)
    The loan title provided by the borrower
  DebtToInc (Continuous)
    A ratio calculated using the borrower's total monthly debt 
  Zip (Categ.)
    The first 3 numbers of the zip code provided by the borrower in the loan application.
  State (Categ.)
    The state provided by the borrower in the loan application
  EmpLength
    Employment length in years. Possible values are between 0 and 10 where 0 means less than one year and 10 means 
    ten or more years. 

Dependent Var
  Accepted (Categ.)
    dummy variable = 1 if loan application was accepted and = 0 if it was rejected
"""

accepted.df <- data.frame(LoanStats_2017Q1$loan_amnt,
                          LoanStats_2017Q1$title,
                          LoanStats_2017Q1$dti,
                          LoanStats_2017Q1$zip_code,
                          LoanStats_2017Q1$addr_state,
                          LoanStats_2017Q1$emp_length)
colnames(accepted.df) <- c("LoanAmt", "Title", "DebtToInc","Zip","State","EmpLength")

remove(LoanStats_2017Q1)


#create a dataframe of rejected loans with the columns in common with the accepted loan data
rejected.df <- data.frame(RejectStats_2017Q1$`Amount Requested`,
                          RejectStats_2017Q1$`Loan Title`,
                          RejectStats_2017Q1$`Debt-To-Income Ratio`,
                          RejectStats_2017Q1$`Zip Code`,
                          RejectStats_2017Q1$State,
                          RejectStats_2017Q1$`Employment Length`)
colnames(rejected.df) <- c("LoanAmt", "Title", "DebtToInc","Zip","State","EmpLength")

remove(RejectStats_2017Q1)

# add dummy column to each df
# add 1's for accepted loans
# add 0's for rejected loans
acc.dummy.vector <- rep(1,nrow(accepted.df))
rej.dummy.vector <- rep(0,nrow(rejected.df))

accepted.df["Accepted"] <- acc.dummy.vector
rejected.df["Accepted"] <- rej.dummy.vector

acc.dummy.vector <- rep(1,nrow(accepted.df))
rej.dummy.vector <- rep(0,nrow(rejected.df))

accepted.df["Accepted"] <- acc.dummy.vector
rejected.df["Accepted"] <- rej.dummy.vector

loans <- rbind(accepted.df,rejected.df)
#write.csv(loans, file = "C:/Users/Brendan/Desktop/Datasets/Lending Club/loans.csv")

remove(acc.dummy.vector, rej.dummy.vector, accepted.df, rejected.df)
###############################################################################################################
###############################################################################################################
# Exploration #################################################################################################
###############################################################################################################
###############################################################################################################


############################################################################################################
# Loan Amount Explore ######################################################################################
############################################################################################################
ggplot(data = loans, aes(loans$LoanAmt))+
  geom_histogram(bins = 50, col = "grey", fill = "black") +
  labs(title = "Histogram of Loan Amount", x = "Loan Amount", y = "Count")+
  theme(plot.title = element_text(hjust = 0.5))


summary(loans$LoanAmt)

ggplot(data = loans, aes(x = 0,y=loans$LoanAmt))+
  geom_boxplot()+
  stat_boxplot(geom ='errorbar') +
  labs(title = "Boxplot of Loan Amount", x = "", y = "Loan Amount")+
  theme(plot.title = element_text(hjust = 0.5))

###########################################################################################################
# Title Explore ###########################################################################################
###########################################################################################################
counts <- table(loans$Title)
c <- data.frame(counts)
colnames(c)<-c("Title","Freq")

ggplot(data = c, aes(x = reorder(Title, Freq), y= Freq))+
  geom_bar(stat = "identity")+
  labs(title = "Title Frequency Barchart", x = "Title", y = "Frequency")+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

title.df <- data.frame(table(loans$Title))
colnames(title.df) <- c("Title", "Freq")
title.df
#there are multiple categories that need to be combined such as "Home Improvement" and "home_improvement"
"""
Changes to be made:
  Business Loan, small_business -> Business
  major_purchase, Major purchase -> Major Purchase
  Car financing, car -> Car 
  vacation -> Vacation
  medical, Medical expense -> Medical
  Credit card refinancing, credit_card, Debt consolidation, debt_consolidation -> Debt Consolidation
  other -> Other
  Green loan, renewable_energy -> Green Loan
  Home buying, house, home_improvement -> Home Buying or Improvement
    this is being combined because the vague category 'house' could mean either and I didn't want to leave in its own category
  Moving and relocation, moving -> Moving
"""
loans$Title <-gsub(".*usiness.*","Business", loans$Title)
loans$Title <-gsub(".*purchase.*","Major Purchase", loans$Title)
# need to be careful here because "car" shows up in "credit card" so I can't use the "all character" expression I used above
loans$Title <-gsub("Car financing","Car", loans$Title)
loans$Title <-gsub("car","Car", loans$Title)
loans$Title <-gsub("vacation","Vacation", loans$Title)
loans$Title <-gsub(".*edical.*","Medical", loans$Title)
loans$Title <-gsub(".*redit.*","Debt Consolidation", loans$Title)
loans$Title <-gsub(".*consoli.*","Debt Consolidation", loans$Title)
loans$Title <-gsub("other","Other", loans$Title)
loans$Title <-gsub("Green loan","Green Loan", loans$Title)
loans$Title <-gsub("renew.*","Green Loan", loans$Title)
loans$Title <-gsub("Home.*","Home Buying or Improvement", loans$Title)
loans$Title <-gsub("home_.*","Home Buying or Improvement", loans$Title)
loans$Title <-gsub("house","Home Buying or Improvement", loans$Title)
loans$Title <-gsub(".*oving.*","Moving", loans$Title)

title.df <- data.frame(table(loans$Title))
colnames(title.df) <- c("Title", "Freq")
title.df


ggplot(data = title.df, aes(x = reorder(Title, Freq), y= Freq))+
  geom_bar(stat = "identity")+
  labs(title = "Title Frequency Barchart", x = "Title", y = "Frequency")+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


###########################################################################################################
# Debt To Income Explore ##################################################################################
###########################################################################################################
#trying to convert to numeric resulted in a lot of NA's
#found the first entry that was causing the problem to see what was up
v <- as.numeric(loans$DebtToInc)
na.index <- is.na(v)
first.na <- min(which(na.index == TRUE))
loans$DebtToInc[first.na]
#it turns out a lot of the entries were 0% and the % symbol was throwing off the numeric conversion
#to get around that I subbed all those entries with a plain 0 and replace the original vector
loans$DebtToInc <-gsub("%","", loans$DebtToInc)
v2 <- as.numeric(loans$DebtToInc)
sum(is.na(v2))
#it looks like I got them all so I'll replace the original character vector with the numeric
loans$DebtToInc <- as.numeric(loans$DebtToInc)
summary(data.frame(loans$DebtToInc))

ggplot(data = loans, aes(loans$DebtToInc))+
  geom_histogram(bins = 50, col = "grey", fill = "black") +
  labs(title = "Histogram of Debt to Income", x = "Debt to Income", y = "Count")+
  theme(plot.title = element_text(hjust = 0.5))

ggplot(data = loans, aes(x = 0,y=loans$DebtToInc))+
  geom_boxplot()+
  stat_boxplot(geom ='errorbar') +
  labs(title = "Boxplot of Debt to Income", x = "", y = "Debt to Income")+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

#the outliers are distorting the plots
#limit the axes to get a better view of the bulk of the data
ggplot(data = loans, aes(loans$DebtToInc))+
  geom_histogram(bins = 50, col = "grey", fill = "black") +
  labs(title = "Histogram of Debt to Income", x = "Debt to Income", y = "Count")+
  theme(plot.title = element_text(hjust = 0.5))+
  xlim(-1,100)

ggplot(data = loans, aes(x = 0,y=loans$DebtToInc))+
  geom_boxplot()+
  stat_boxplot(geom ='errorbar') +
  labs(title = "Boxplot of Debt to Income", x = "", y = "Debt to Income")+
  theme(plot.title = element_text(hjust = 0.5))+
  ylim(-1,100)+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())


#there are some very high DTI entries that need to be looked at one by one
high.dti <- which(loans$DebtToInc > 1000)
length(high.dti)
#there are over 10,000 DTI values above 1000% so they probably aren't mistakes, outliers


##########################################################################################################
# Zip Explore ############################################################################################
##########################################################################################################

# 986 levels might be too many for a logit model
zip.counts <- table(loans$Zip)
z <- data.frame(zip.counts)
colnames(z)<-c("Zip","Freq")
ggplot(data = z, aes(x = reorder(Zip, Freq), y= Freq))+
  geom_bar(stat = "identity")+
  labs(title = "Zip Code Frequency Barchart", x = "Zip Code", y = "Frequency")+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(axis.text.x=element_blank())
#the x axis text was removed because there were so many values that they blurred together and were unreadable.

#########################################################################################################
# State Exlpore #########################################################################################
#########################################################################################################

state.counts <- table(loans$State)
s <- data.frame(state.counts)
colnames(s)<-c("State","Freq")
ggplot(data = s, aes(x = reorder(State, Freq), y= Freq))+
  geom_bar(stat = "identity")+
  labs(title = "State Frequency Barchart", x = "State", y = "Frequency")+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 7))
# look at states with the most loan applications
tail(sort(state.counts))

# create a map of the US colored based on number of loan applications per state
state.df <- as.data.frame(state.counts)
colnames(state.df) <- c("State", "Freq")
state.names <- c("alaska","alabama", "arkansas","arizona","california","colorado","connecticut",
                 "district of columbia", "delaware","florida","georgia","hawaii","idaho","illinois",
                 "indiana","kansas","kentucky","louisiana","massachusetts", "maryland","maine","michigan", "minnesota",
                 "missouri","mississippi","montana","north carolina","north dakota","nebraska","new hampshire",
                 "new jersey", "new mexico","nevada","new york","ohio","oklahoma","oregon","pennsylvania",
                 "rhode island","south carolina","south dakota", "tennessee","texas","utah","virginia",
                 "vermont","washington","wisconsin","wyoming","west virginia")
state.df["region"] <- state.names

#Iowa is not in the dataset because there were no loan apps from that state
# to make it show up on the map we need to include it in the dataset as zero
ia.df <- data.frame(as.factor("IA"),0,"iowa")
names(ia.df) <- c("State","Freq","region")

#append it to the state.df dataframe
state.df <- rbind(state.df, ia.df)

states <- map_data("state")

ggplot(data = state.df, aes(map_id = region)) + 
  geom_map(aes(fill = Freq), map = states) +
  scale_fill_gradientn(colours=c("white","red","orange","yellow")) + 
  expand_limits(x = states$long, y = states$lat)+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(panel.background = element_rect(fill = 'grey', color = 'grey'))+
  theme(panel.border = element_rect(fill=NA, color = 'black'))+
  theme(axis.text = element_blank())+
  theme(axis.ticks = element_blank())+
  theme(panel.grid = element_blank())+
  labs(title = "State Frequency Map", x="",y="")

#########################################################################################################
# Employment Length Exlpore #############################################################################
#########################################################################################################

head(loans$EmpLength)
loans$EmpLength <- as.character(loans$EmpLength)
#the first step toward getting these as numeric values is to cut "years" of the strings in the column
loans$EmpLength <-gsub("years","", loans$EmpLength)
# now cut "year"
loans$EmpLength <-gsub("year","", loans$EmpLength)
# now cut "< 1" and replace it with 0
loans$EmpLength <-gsub("< 1","0", loans$EmpLength)
# make 10+ just 10
loans$EmpLength <-gsub("[+]","", loans$EmpLength)
# check to see if we have the values we want now
emp.length.table <- table(loans$EmpLength)
# find what row contains ".1" 
ind <- grepl(".1", loans$EmpLength)
which(ind == TRUE)
# ".1" is in row 109907
# I can't figure out why this is here but there are so many observations that I'm just going to delete this row
# and it won't effect the model in any significant way
loans <- loans[-c(109907), ]
# now I can convert the years of employment to numeric
# since the data is categorical I'll use a barplot and table to summarize
loans$EmpLength <- as.numeric(loans$EmpLength)
emp.length <- table(loans$EmpLength)
e <- data.frame(emp.length)
colnames(e)<-c("Years","Freq")

ggplot(data = e, aes(x = Years, y= Freq))+
  geom_bar(stat = "identity")+
  labs(title = "Employment Length Frequency Barchart", x = "Years", y = "Frequency")+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(axis.text.x = element_text(angle = 0, hjust = 1, size = 7))

# the great majority of applicants have been working for less than a year which leads me to believe that people
# come to lending club if they are not eligible for traditional credit

# the conversion warned of NAs but after checking the table of counts when it was char vs numeric
# i found that the only NAs were from the "n/a" char strings 


###############################################################################################################
# Clean NAs ###################################################################################################
###############################################################################################################
LoanAmt.na <- which(is.na(loans$LoanAmt))
length(LoanAmt.na)
# there are no NAs in LoanAmt
Title.na <- which(is.na(loans$Title))
length(Title.na)
# there are 47 NAs in Title. I'll print the rows to see if there is any pattern.
loans[Title.na, ]
# these entries all had a zero loan amount, no title, -1% DTI, employment length < 1, and were rejected
# these entries seem to either be incomplete applications or faulty entries. They will be removed from the
# dataset
loans <- loans[-c(Title.na), ]


dti.na <- which(is.na(loans$DebtToInc))
length(dti.na)
# there are no NAs but there are still odd values like -1% DTI to investigate
sum(loans$DebtToInc == -1)
# there are 112,046 applications with -1% DTI
dti.neg <- which(loans$DebtToInc == -1)
sum(loans$Accepted[dti.neg] == 1)
# only one of the 112,046 applications with -1% DTI was accepted 
summary(loans[dti.neg, ])
# the data for applicants with -1% DTI is quite varied except for the fact that they were almost all rejected
# this may be a valuable predictor even if I can't figure out what it means. I wonder if it means that the 
# applicant has no reported income and therefore has an undefined DTI ratio. I'll leave it in.

#I am not checking zip because it will not be included in the model (because I don't want to deal with 900+ dummies)

state.na <- which(is.na(loans$State))
length(state.na)
# no NAs in State

emp.na <- which(is.na(loans$EmpLength))
length(emp.na)
# there are 74623 NAs in the Employment Length column
summary(loans[emp.na, ])
summary(loans)
length(emp.na)/length(loans$EmpLength)
# the data with EmpLength equal to NA looks similarly distributed to the dataset as a whole and it makes up
# only 5% of the observations so I'm going to remove it from the dataset.
loans <- loans[-c(emp.na), ]
# check that the rows were removed
sum(which(is.na(loans$EmpLength)))

# Accepted shouldn't have any NAs because I created that column but I'm going to check anyway
sum(which(is.na(loans$Accepted)))
# no NAs found! w00t!

#now, just because I'm paranoid, I'm going to check the whole dataframe for NA one more time
colSums(is.na(loans))
# since zip wont't be included I don't care about the 1 NA value
# all the other vars are clean!

###############################################################################################################
###############################################################################################################
# Bivariate Exploration #######################################################################################
###############################################################################################################
###############################################################################################################

# here I'm going to look at the relationship between the chosen explanatory variables and the dependent variable.
"""
continuous explanatory vars:
  LoanAmt
  DebtToInc
  EmpLength

categorical explanatory vars:
  Title
  State

dependent var:
  Accepted
"""
by(data.frame(loans$LoanAmt), data.frame(loans$Accepted), summary)
options(scipen=10000)
ggplot(data = loans, aes(x = as.factor(Accepted), y=LoanAmt))+
  geom_boxplot()+
  stat_boxplot(geom ='errorbar') +
  labs(title = "Loan Amount Boxplot", x = "Accepted", y = "Loan Amount")+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(axis.text.x = element_text(angle = 0, hjust = 1))
# this seems to indicate that loan amounts > 50,000 are very unlikely (if not impossible) to get funded

by(loans$DebtToInc, loans$Accepted, summary)
ggplot(data = loans, aes(x = as.factor(Accepted), y=DebtToInc))+
  geom_boxplot()+
  stat_boxplot(geom ='errorbar') +
  labs(title = "Debt to Income Boxplot", x = "Accepted", y = "Debt to Income")+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(axis.text.x = element_text(angle = 0, hjust = 1))
# the outliers in the rejected loan requests are so extreme that the plot is effectively useless.
# the summary of DTI grouped by accepted show a wider spread and much higher extreme values in the rejected
# to get a better look I'll plot them again, ignoring the extreme values
ggplot(data = loans, aes(x = as.factor(Accepted), y=DebtToInc))+
  geom_boxplot()+
  stat_boxplot(geom ='errorbar') +
  labs(title = "Debt to Income Boxplot", x = "Accepted", y = "Debt to Income")+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(axis.text.x = element_text(angle = 0, hjust = 1))+
  ylim(0,100)
# this shows that the DTI range on accepted loan applications is much narrower and tends to be lower than the 
# rejects. This makes sense. Lower DTI indicates a greater ability to repay the loan because they aren't 
# spending all their money servicing other debt.

by(data.frame(loans$EmpLength), data.frame(loans$Accepted), summary)

ggplot(data = loans, aes(x = as.factor(Accepted), y=EmpLength))+
  geom_boxplot()+
  labs(title = "Employment Length Boxplot", x = "Accepted", y = "Years")+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(axis.text.x = element_text(angle = 0, hjust = 1))

# now for the categorical vs categorical viz! STACKED BAR PLOTS!!!!!!!
title.accept.counts <- table(loans$Accepted, loans$Title)
t <- data.frame(title.accept.counts)
colnames(t) <- c("Accepted", "Title", "Freq")
ggplot(data = t, aes(x = reorder(Title,Freq) , y= Freq, fill = Accepted))+
  geom_bar(stat = "identity")+
  labs(title = "Acceptance by Title", x = "Title", y = "Frequency")+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

title.sums <- by(loans$Accepted, loans$Title, sum)
title.len <- by(loans$Accepted, loans$Title, length)
title.rate <- title.sums/title.len

#perhaps a graphs of acceptance rates will be more informative
#get names and rates into a dataframe for ggplot
names <- c()
values <- c()
for (i in 1:length(title.rate)){
  df <- data.frame(title.rate[i])
  names[i] <- rownames(df)
  values[i] <- df[1,1]
}
title.df <- data.frame(names,values)
title.df

ggplot(data = title.df, aes(x = reorder(names, values), y= values))+
  geom_bar(stat = "identity")+
  labs(title = "Acceptance Rate by Title", x = "Title", y = "Rate")+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#after seeing the poor performance of stacked bar plots in the title category I am going to use ratios only in the States comparison
state.sums <- by(loans$Accepted, loans$State, sum)
state.len <- by(loans$Accepted, loans$State, length)
state.rate <- state.sums/state.len
#barplot(sort(state.rate))
#sort(state.rate, decreasing = TRUE)
#summary(state.rate)

#get names and rates into a dataframe for ggplot
names <- c()
values <- c()
for (i in 1:length(state.rate)){
  df <- data.frame(state.rate[i])
  names[i] <- rownames(df)
  values[i] <- df[1,1]
}
state.df <- data.frame(names,values)
state.df

ggplot(data = state.df, aes(x = reorder(names, values), y= values))+
  geom_bar(stat = "identity")+
  labs(title = "Acceptance Rate by State", x = "State", y = "Rate")+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 7))

#CA,FL,NY,TX have the highest ratio of accepted loans but they also have the most applications overall
# there does seem to be a difference in acceptance rates between states so it may be a good predictor var

"""
All of the explanatory vars chosen for the model appear to have relationships with the dependent var.
They will all be included in the model and then I'll test the model's predictive ability on a test set.
"""

###############################################################################################################
###############################################################################################################
# Model Building ##############################################################################################
###############################################################################################################
###############################################################################################################
#choose random set of indices to use as training set (about 10% of observations)
test.ind <- sample(1:length(loans$Accepted), 140000, replace = FALSE)
test.data <- loans[test.ind, ]
train.data <- loans[-test.ind, ]

accept.model <- glm(Accepted ~ Title+DebtToInc+State+EmpLength, data = train.data, family = binomial(link = 'logit'))
summary(accept.model)
round(pR2(accept.model), 2)

baseline = sum(test.data$Accepted == 0)/length(test.data$Accepted)
baseline
# this set the bar pretty high because the model will have to beat 93.5% accuracy
fitted.results <- predict(accept.model, newdata = test.data)
fitted.results <- ifelse(fitted.results > 0.5, 1, 0)

correct.classification.rate <- mean(fitted.results == test.data$Accepted)
correct.classification.rate


#see if a simpler model excluding the State variable will be as good
simp.accept.model <- glm(Accepted ~ Title+DebtToInc+EmpLength, data = train.data, family = binomial(link = 'logit'))
summary(simp.accept.model)
round(pR2(simp.accept.model), 2)

fitted.results2 <- predict(simp.accept.model, newdata = test.data)
fitted.results2 <- ifelse(fitted.results2 > 0.5, 1, 0)

correct.classification.rate2 <- mean(fitted.results2 == test.data$Accepted)
correct.classification.rate2
