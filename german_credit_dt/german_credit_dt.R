<<<<<<< HEAD
library(rpart)
library(rpart.plot)
library(plyr)

german_df = read.csv("C:/Users/Brendan/Desktop/ML/german_credit_dt/german_credit_data.csv", header=FALSE)

column_names = c("checking.account.status","duration.in.months","credit.history","purpose","credit.amount","savings.or.bonds","present.employment.since","installment.rate.as.percentage.of.disposable.income","personal.status.and.sex","cosigners","present.resident.since","collateral","age.in.years.","other.installment.plans","housing","number.of.existing.credits.at.this.bank","job","number.of.dependents","telephone","foreign.worker","good.loan")

colnames(german_df) = column_names

#Convert the int in the good.loan column to "Good" or "Bad" to make them more descriptive
german_df[german_df$good.loan == "1",]$good.loan = "Good"
german_df[german_df$good.loan == "2",]$good.loan = "Bad"

#Convert purpose codes to something more descriptive
german_df$purpose = revalue(german_df$purpose, c("A40" = "new car",
                             "A41" = "used car",
                             "A42" = "furniture/equipment",
                             "A43" = "radio/television",
                             "A44" = "domestic appliances",
                             "A45" = "repairs",
                             "A46" = "education",
                             "A48" = "retraining",
                             "A49" = "business",
                             "A410" = "others"))

#create more descriptive housing labels
german_df$housing = revalue(german_df$housing, c("A151" = "rent",
                             "A152" = "own",
                             "A153" = "for free"))


#create more descriptive labels for other installment plans
german_df$other.installment.plans = revalue(german_df$other.installment.plans, c("A141" = "bank",
                                             "A142" = "stores",
                                             "A143" = "none"))

View(german_df)

credit_dt = rpart(good.loan ~ duration.in.months + purpose + housing + installment.rate.as.percentage.of.disposable.income + credit.amount + other.installment.plans, data = german_df, control = rpart.control(maxdepth = 4), method = "class")
                  

rpart.plot(credit_dt)
=======
library(rpart)
library(rpart.plot)
library(plyr)

german_df = read.csv("C:/Users/Brendan/Desktop/ML/german_credit_dt/german_credit_data.csv", header=FALSE)

column_names = c("checking.account.status","duration.in.months","credit.history","purpose","credit.amount","savings.or.bonds","present.employment.since","installment.rate.as.percentage.of.disposable.income","personal.status.and.sex","cosigners","present.resident.since","collateral","age.in.years.","other.installment.plans","housing","number.of.existing.credits.at.this.bank","job","number.of.dependents","telephone","foreign.worker","good.loan")

colnames(german_df) = column_names

#Convert the int in the good.loan column to "Good" or "Bad" to make them more descriptive
german_df[german_df$good.loan == "1",]$good.loan = "Good"
german_df[german_df$good.loan == "2",]$good.loan = "Bad"

#Convert purpose codes to something more descriptive
german_df$purpose = revalue(german_df$purpose, c("A40" = "new car",
                             "A41" = "used car",
                             "A42" = "furniture/equipment",
                             "A43" = "radio/television",
                             "A44" = "domestic appliances",
                             "A45" = "repairs",
                             "A46" = "education",
                             "A48" = "retraining",
                             "A49" = "business",
                             "A410" = "others"))

#create more descriptive housing labels
german_df$housing = revalue(german_df$housing, c("A151" = "rent",
                             "A152" = "own",
                             "A153" = "for free"))


#create more descriptive labels for other installment plans
german_df$other.installment.plans = revalue(german_df$other.installment.plans, c("A141" = "bank",
                                             "A142" = "stores",
                                             "A143" = "none"))

View(german_df)

credit_dt = rpart(good.loan ~ duration.in.months + purpose + housing + installment.rate.as.percentage.of.disposable.income + credit.amount + other.installment.plans, data = german_df, control = rpart.control(maxdepth = 4), method = "class")
                  

rpart.plot(credit_dt)
>>>>>>> origin/master
