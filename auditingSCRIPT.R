##################################################################################################
# This script is intended for accompanying the creation of the "auditingInsurance.Rmd" document. #
##################################################################################################

# To do: add links to the end as a bibliography.

# Necessary libraries:
library(ggplot2)
library(lubridate)

# Setting ataset direction
setwd("C:\\Users\\riosa\\documents\\ucm\\cuarto\\segundo\\tfg")
insurance <- read.csv("insurance_claims.csv")

# Dimensions of the dataset
dim(insurance)
# 1000 observations and 40 variables, 39 in the kaggle version. Last column must be removed, since
# it contains nothing
insurance <- insurance[, -40]
# Let's see the names of all the columns in the dataset
names(insurance)

# Before exploring each variable let's see if there are missing values in some of them.
# With the code below, if zeros are obtained means that missing values aren't present in the
# corresponding variable.
colSums(apply(X = insurance, MARGIN = 2, FUN = is.na))
# Now let's explore the content of each variable one by one.

# 1) Variable: months_as_customer

head(insurance$months_as_customer)
# Looks that it only takes whole values, let's check it.
sum(vapply(insurance$months_as_customer, typeof, character(1)) == "integer")
# Vector elements must have the same type in R so the following code is also valid
typeof(insurance$months_as_customer)

summary(insurance$months_as_customer)

ggplot(insurance, aes(x = months_as_customer)) + geom_histogram(color = "white", fill = "seagreen") +
  theme_light() + labs(title = "Histogram of months_as_customer", x = "value") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

479/12 # maximum in years
276.2/12 # 3rd quartile in years
199.5/12 # median in years
115.8/12 # 1st quartile in years

# 2) Variable: age

head(insurance$age, 10)
summary(insurance$age)
# Histogram
ggplot(insurance, aes(x = age)) + geom_histogram(color = "white", fill = "seagreen", binwidth = 1) +
  theme_light() + labs(title = "Histogram of age", x = "value") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

# Boxplot
ggplot(insurance, aes(x = age)) + geom_boxplot(color = "seagreen") + coord_flip() +
  labs(title = "Boxplot of variable age", x = "value") + 
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

# Obtain more quantiles, 
quantile(insurance$age, seq(0, 1, 0.05))

# Low wishker length
32 - 1.5*(44 - 32)
# 14, no lower observation than 19, so no outliers this way
# Upper wishker length
44 + 1.5*(44 - 32)
# Upper whisker goes from 44 to 62, let's see which observations are bigger than 62
insurance$age[which(insurance$age > 62)]

# Intuitively, is expected to be a positive correlation between the age and the months_as_customer variable,
# since it's foreseeable that clients with more age have been more years with the company.
plot(insurance$age, insurance$months_as_customer)
cor(insurance$age, insurance$months_as_customer)
# Scatter plot
ggplot(insurance, aes(age, months_as_customer)) + geom_point(color = "seagreen") +
  theme_light() + labs(title = "age vs. months_as_customer", x = "age", y = "months_as_customer") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

# Are there observations with more months in the company than years alive?
length(which(insurance$age*12 < insurance$months_as_customer))
# And taking into account the 15 years that a person can't have a driving license?
length(which(insurance$age*12 - 15*12 < insurance$months_as_customer))

# 3) Variable: policy_number

head(insurance$policy_number, 10)
typeof(insurance$policy_number)
# We expect that there is only one single number for each row in this variable.
length(unique(insurance$policy_number))
# It's ok, so this variable isn't useful for modelling and will serve has an identifier
# for each observation.

# 4) Variable: policy_bind_date

summary(insurance$policy_bind_date)
typeof(insurance$policy_bind_date)
head(insurance$policy_bind_date)
# Changing its type to date
insurance$policy_bind_date <- date(insurance$policy_bind_date)
range(insurance$policy_bind_date)
length(table(insurance$policy_bind_date))
head(sort(table(insurance$policy_bind_date), decreasing = T), 10)
cor(as.numeric(insurance$policy_bind_date), insurance$age)
plot(insurance$policy_bind_date, insurance$age)
# scatter plot
ggplot(insurance, aes(as.numeric(policy_bind_date), age)) + geom_point(color = "seagreen") +
  theme_light() + labs(title = "age vs. months_as_customer", x = "age", y = "months_as_customer") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

ggplot(insurance, aes(as.numeric(policy_bind_date), age)) + geom_point(color = "seagreen") +
  theme_light() + labs(title = "policy_bind_date vs. age", x = "date", y = "age") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

# Let's check which dates take the younger observations.
# How many observations are younger than 28 years (less than the first quartile)?
length(insurance$age[which(insurance$age <= 28)])
head(insurance[which(insurance$age < 28), c("policy_bind_date", "age")])

# scatter plot against months_as_customer
ggplot(insurance, aes(as.numeric(policy_bind_date), months_as_customer)) + geom_point(color = "seagreen") +
  theme_light() + labs(title = "policy_bind_date vs. months_as_customer", x = "date", y = "months_as_customer") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

cor(as.numeric(insurance$policy_bind_date), insurance$months_as_customer)

# 5) Variable: policy_state

summary(insurance$policy_state)
length(unique(insurance$policy_state))

# Relative frequencies barplot
ggplot(as.data.frame(round(table(insurance$policy_state)/length(insurance$policy_state), 2)),
       aes(x = Var1, y = Freq)) + geom_bar(stat = "identity", color = "seagreen", fill = "seagreen") +
  labs(title = "Relative frequencies of variable policy_state", x = "state") + theme_classic() +
  geom_text(aes(label = Freq), vjust = -0.3) + 
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

# Previous type
typeof(insurance$policy_state)
insurance$policy_state <- factor(insurance$policy_state)

# 6) Variable: policy_csl

summary(insurance$policy_csl)
head(insurance$policy_csl)
length(unique(insurance$policy_csl))
unique(insurance$policy_csl)
insurance$policy_csl <- factor(insurance$policy_csl)

# Relative frequencies barplot
ggplot(as.data.frame(round(table(insurance$policy_csl)/length(insurance$policy_csl), 2)),
       aes(x = Var1, y = Freq)) + geom_bar(stat = "identity", color = "seagreen", fill = "seagreen") +
  labs(title = "Relative frequencies of variable policy_csl", x = "value") + theme_classic() +
  geom_text(aes(label = Freq), vjust = -0.3) + 
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

# 7) Variable: policy_deductable

summary(insurance$policy_deductable)
length(unique(insurance$policy_deductable))
unique(insurance$policy_deductable)
insurance$policy_deductable <- factor(insurance$policy_deductable)

# Relative frequencies barplot
ggplot(as.data.frame(round(table(insurance$policy_deductable)/length(insurance$policy_deductable), 2)),
       aes(x = Var1, y = Freq)) + geom_bar(stat = "identity", color = "seagreen", fill = "seagreen") +
  labs(title = "Relative frequencies of variable policy_deductable", x = "value") + theme_classic() +
  geom_text(aes(label = Freq), vjust = -0.3) + 
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

# 8) Variable: policy_annual_premium

summary(insurance$policy_annual_premium)
length(unique(insurance$policy_annual_premium))
head(insurance$policy_annual_premium)

# Histogram
ggplot(insurance, aes(x = policy_annual_premium)) + geom_histogram(color = "white", fill = "seagreen", binwidth = 60) +
  theme_light() + labs(title = "Histogram of policy_annual_premium", x = "value") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

# Boxplot
ggplot(insurance, aes(x = policy_annual_premium)) + geom_boxplot(color = "seagreen") + coord_flip() +
  labs(title = "Boxplot of variable policy_annual_premium", x = "value") + 
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

# Can be this variable related with policy_csl? (policies with expensive premiums have a higher 
# combined single limit.)

table(insurance[which(insurance$policy_annual_premium > 2000), "policy_csl"])
# Only one value its bigger than 2000

# 9) Variable: umbrella_limit

head(insurance$umbrella_limit)
length(unique(insurance$umbrella_limit))
summary(insurance$umbrella_limit)
# How many observations take negative values?
length(insurance$umbrella_limit[which(insurance$umbrella_limit < 0)])
table(insurance$umbrella_limit)
# Which observation is the negative one?
which(insurance$umbrella_limit < 0)
# Let's put it as a NA value.
insurance$umbrella_limit[which(insurance$umbrella_limit < 0)] <- NA

# Relative frequencies barplot
ggplot(as.data.frame(table(insurance$umbrella_limit)),
       aes(x = Var1, y = Freq)) + geom_bar(stat = "identity", color = "seagreen", fill = "seagreen") +
  labs(title = "Relative frequencies of variable umbrella_limit", x = "state") + theme_classic() +
  geom_text(aes(label = Freq), vjust = -0.3) + 
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))


ggplot(insurance, aes(x = umbrella_limit)) + geom_histogram(color = "white", fill = "seagreen") +
  theme_light() + labs(title = "Histogram of umbrella_limit", x = "value") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))


# Let's see if the observations with the bigger umbrella limits also have the highest premiums.
cor(insurance$umbrella_limit[-291], insurance$policy_annual_premium[-291])
# Check in the group of zeros and without the zeros.
# Let's analyze only the policies without zero umbrella limit
subset <- which(insurance$umbrella_limit > 0)
cor(insurance$policy_annual_premium[subset], insurance$umbrella_limit[subset])

# Scatter plot
ggplot(insurance[subset, ], aes(umbrella_limit, policy_annual_premium)) + geom_point(color = "seagreen") +
  theme_light() + labs(title = "umbrella_limit vs. policy_annual_premium", x = "umbrella_limit", y = "policy_annual_premium") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

# Without elimining the zero umbrella limit subset
# Scatter plot
ggplot(insurance, aes(umbrella_limit, policy_annual_premium)) + geom_point(color = "seagreen") +
  theme_light() + labs(title = "umbrella_limit vs. months_as_policy_anual_premium", x = "umbrella_limit", y = "policy_annual_premium") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

# Transforming the umbrella limit observations to its logarithms
ggplot(insurance[subset, ], aes(log(umbrella_limit), policy_annual_premium)) + geom_point(color = "seagreen") +
  theme_light() + labs(title = "umbrella_limit vs. policy_annual_premium", x = "umbrella_limit", y = "policy_annual_premium") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

# 10) Variable: insured_zip

head(insurance$insured_zip)
# How many distinct zip codes there are?
length(unique(insurance$insured_zip))
# Let's check if all the observations have a zip code of six digits.
intLength <- function(number) {
  # Inputs an integer number and returns the quantity of digits that it is formed on
  return(length(unlist(strsplit(as.character(number), ""))))
}

sum(vapply(X = insurance$insured_zip, FUN = intLength, FUN.VALUE = numeric(1)) == 6)

# Let's analyze the values of the variable when segregated by its state
# Complete observations range
range(insurance$insured_zip)
# Range of values of the Illinois state
range(insurance$insured_zip[which(insurance$policy_state == "IL")])
# Range of values of the Indiana state
range(insurance$insured_zip[which(insurance$policy_state == "IN")])
# Range of values of the Ohio state
range(insurance$insured_zip[which(insurance$policy_state == "OH")])

# Stract the first two digits of a zip code
firstTwo <- function(number) {
  # Recives a six digit number and returns the first two digits.
  # Any other number of 
  # Example: 
  # Input 478236
  # Output 47
  result <- numeric(1)
  if (typeof(number) == "integer" & intLength(number) == 6) {
    result <- (number - number%%10000)/10000
  } else {
    result <- -1
  }
  return(result)
}

test <- data.frame(observed = vapply(X = head(insurance$insured_zip),
                                     FUN = firstTwo, FUN.VALUE = numeric(1)),
                   state = head(insurance$policy_state))

checkZip <- function(code, state) {
  # Input:
  # code: a two digit integer
  # state: a two letters character 
  # Return: 1 if code and state are as expected and 0 otherwise
  # Note: expected 43-45 for state "OH", 60-62 for state "IL", and 46-47 for
  # state "IN".
  result <- 0 
  if (state == "OH" & (code >= 43 & code <= 45)) {
    result <- 1
  } else if (state == "IL" & (code >= 60 & code <= 62)) {
    result <- 1
  } else if (state == "IN" & (code >= 46 & code <= 47)) {
    result <- 1
  }
  return(result)
}

for (i in 1:dim(test)[1]) {
  test$check[i] <- checkZip(test$observed[i], test$state[i])
}

# Now let's do the same in the whole dataset
checkFrame <- data.frame(observed = vapply(X = insurance$insured_zip, FUN = firstTwo,
                                           FUN.VALUE = numeric(1)), state = insurance$policy_state)

for (i in 1:dim(checkFrame)[1]) {
  checkFrame$check[i] <- checkZip(checkFrame$observed[i], checkFrame$state[i])
}

# Results analysis
head(checkFrame)
table(checkFrame$check)
tail(checkFrame)

# 11) Variable: insured_sex

summary(insurance$insured_sex)
table(insurance$insured_sex)

# Let's create a function to help us recoding the variable 
recode <- function(value) {
  # Converts a caracter value ("FEMALE" or "MALE") to 0 or 1. Anything else it will
  # return -1.
  # Input: A chacaracter string, "FEMALE" or "MALE".
  # Output: 0 if "FEMALE", 1 if "MALE", -1 for the rest.
  result <- -1 # -1 as default value
  if (value == "FEMALE") {
    result <- 0
  } else if (value == "MALE") {
    result <- 1
  }
  return(result)
}

# Recode the categories
insurance$insured_sex <- unname(vapply(insurance$insured_sex, recode, numeric(1)))
# Transform its type to factor
insurance$insured_sex <- factor(insurance$insured_sex)

# 12) Variable: insured_education_level

summary(insurance$insured_education_level)
table(insurance$insured_education_level)

# Relative frequencies barplot
ggplot(as.data.frame(round(table(insurance$insured_education_level)/length(insurance$insured_education_level), 2)),
       aes(x = Var1, y = Freq)) + geom_bar(stat = "identity", color = "seagreen", fill = "seagreen") +
  labs(title = "Relative frequencies of variable insured_education_level", x = "state") + theme_classic() +
  geom_text(aes(label = Freq), vjust = -0.3) + 
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

insurance$insured_education_level <- factor(insurance$insured_education_level)

# Is related the education level and the premium that the insurer pays? 
# (i.e. higher education level -> higher income -> more expensive car -> higher premium)

plot(insurance$insured_education_level, insurance$policy_annual_premium)

# Annual premium boxplots for each eduation level
ggplot(insurance, aes(x = age)) + geom_boxplot(color = "seagreen") + coord_flip() +
  labs(title = "Boxplot of variable age", x = "value") + 
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))
  
  
# 13) Variable: insured_occupation

table(insurance$insured_occupation)
length(unique(insurance$insured_occupation))
insurance$insured_occupation <- factor(insurance$insured_occupation)

# Relative frequencies barplot
ggplot(as.data.frame(round(table(insurance$insured_occupation)/length(insurance$insured_occupation), 2)),
       aes(x = Var1, y = Freq)) + geom_bar(stat = "identity", color = "seagreen", fill = "seagreen") +
  labs(title = "Relative frequencies of variable insured_occupation", x = "occupation") + theme_classic() +
  geom_text(aes(label = Freq), hjust = 1.2) + 
  theme(plot.title = element_text(hjust = 0.5, face = "bold")) + coord_flip()

# It would be interesting to inspect the education level by occupation.

plot(insurance$insured_occupation, insurance$insured_education_level)


# Example of an stacked barchart

# create a dataset
specie <- c(rep("sorgho" , 3) , rep("poacee" , 3) , rep("banana" , 3) , rep("triticum" , 3) )
condition <- rep(c("normal" , "stress" , "Nitrogen") , 4)
value <- abs(rnorm(12 , 0 , 15))
data <- data.frame(specie,condition,value)

# Stacked
ggplot(data, aes(fill=condition, y=value, x=specie)) + 
  geom_bar(position="stack", stat="identity")

# We must create a dataframe as the above one, with all the combinations of the two variables
# and its frequencies

# First let's confirm that all the occupations contain observations of all the education levels
length(unique(insurance$insured_education_level))
# There were 7 different education levels
for (i in unique(insurance$insured_occupation)) {
  print(length(unique(insurance[which(insurance$insured_occupation == i), "insured_education_level"])))
}
# Ok, all the categories in insured_occupation take all the categories in insured_education_level,
# so let's continue with the data.frame creation.
length(unique(insurance$insured_education_level))*length(unique(insurance$insured_occupation))
# The dataset must have 7*14 = 98 rows
# Let's create an auxiliar list and an auxiliar vector to create a data.frame for plotting an
# stacked barplot.
auxList <- lapply(levels(insurance$insured_occupation), rep, 7)
occupation <- character(0)
for (i in 1:length(auxList)) {
  occupation <- c(occupation, unlist(auxList[i]))
}
# Data frame for use in the creation of the stacked barchart
stackFrame <- data.frame(occupation, education = rep(levels(insurance$insured_education_level), 14), freqs = numeric(98))
# Calculation of the frequencies
indexEnd <- 7 # Auxiliar variable for helping with rows selection
for (i in unique(stackFrame$occupation)) {
  stackFrame$freqs[(indexEnd - 6):indexEnd] <- table(insurance$insured_education_level[which(insurance$insured_occupation == i)])
  indexEnd <- indexEnd + 7
}

ggplot(stackFrame, aes(fill=education, y=freqs, x=occupation)) + 
  geom_bar(position="stack", stat="identity") + coord_flip()

# Now let's do it again with the relative frequencies

# Calculation of the frequencies
indexEnd <- 7 # Auxiliar variable for helping with rows selection
for (i in unique(stackFrame$occupation)) {
  absfreqs <- table(insurance$insured_education_level[which(insurance$insured_occupation == i)])
  stackFrame$freqs[(indexEnd - 6):indexEnd] <- round(absfreqs/sum(absfreqs), 2)
  indexEnd <- indexEnd + 7
}

# 14) Variable: insured_hobbies

length(unique(insurance$insured_hobbies))
# 20 different hobbies
table(insurance$insured_hobbies)
# Transform its type to factor
insurance$insured_hobbies <- factor(insurance$insured_hobbies)

# Absolute frequencies barplot
ggplot(as.data.frame(table(insurance$insured_hobbies)),
       aes(x = Var1, y = Freq)) + geom_bar(stat = "identity", color = "seagreen", fill = "seagreen") +
  labs(title = "Relative frequencies of variable insured_hobbies", x = "hobbies") + theme_classic() +
  geom_text(aes(label = Freq), hjust = -0.2) + 
  theme(plot.title = element_text(hjust = 0.5, face = "bold")) + coord_flip()

# Check if they seem related to insured sex with another stack barplot
length(levels(insurance$insured_hobbies))*length(levels(insurance$insured_sex))
# This time we need 20*2 = 40 rows
auxList <- lapply(levels(insurance$insured_hobbies), rep, 2)
hobbies <- character()
for (i in 1:length(auxList)) {
  hobbies <- c(hobbies, unlist(auxList[i]))
}
# Auxiliar dataframe for creating the stacked barplot
stackFrame <- data.frame(hobbies, sex = rep(c("FEMALE", "MALE"), 20), freqs = numeric(40))

# Calculating frequencies
indexEnd <- 2
for (i in unique(stackFrame$hobbies)) {
  stackFrame$freqs[(indexEnd - 1):indexEnd] <- table(insurance$insured_sex[which(insurance$insured_hobbies == i)])
  indexEnd <- indexEnd + 2
}

ggplot(stackFrame, aes(fill=sex, y=freqs, x=hobbies)) + 
  geom_bar(position="stack", stat="identity") + coord_flip() + 
  labs(title = "Sex frequency by hobby type", x = "hobby", y = "frequency") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

# Let's repeat it with relative ones
# Calculating relative frequencies of sex for each hobby
indexEnd <- 2
for (i in unique(stackFrame$hobbies)) {
  freqsh <- table(insurance$insured_sex[which(insurance$insured_hobbies == i)])
  stackFrame$freqs[(indexEnd - 1):indexEnd] <- round(freqsh/sum(freqsh), 2)
  indexEnd <- indexEnd + 2
}

# Result
head(stackFrame, 10)

ggplot(stackFrame, aes(fill=sex, y=freqs, x=hobbies)) + 
  geom_bar(position="stack", stat="identity") + coord_flip()


# 15) Variable: insured_relationship

length(unique(insurance$insured_relationship))
# 6 unique values
table(insurance$insured_relationship)
# Transform its type to factor
insurance$insured_relationship <- factor(insurance$insured_relationship)

# Relative frequencies barplot
ggplot(as.data.frame(round(table(insurance$insured_relationship)/length(insurance$insured_relationship), 2)),
       aes(x = Var1, y = Freq)) + geom_bar(stat = "identity", color = "seagreen", fill = "seagreen") +
  labs(title = "Relative frequencies of variable insured_relationship", x = "relationship") + theme_classic() +
  geom_text(aes(label = Freq), vjust = - 0.2) + 
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

# 16) Variable: capital.gains

head(insurance$capital.gains, 10)
length(unique(insurance$capital.gains))
# 338 unique values
summary(insurance$capital.gains)
# Seems that many observations take a value of zero for this variable
hist(insurance$capital.gains)

# Histogram
ggplot(insurance, aes(x = capital.gains)) + geom_histogram(color = "white", fill = "seagreen") +
  theme_light() + labs(title = "Histogram of capital.gains", x = "value") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

# Repeat without the zero ones, and also test for normality
# How many zeros there where?
table(insurance$capital.gains)[1]
length(unique(insurance$capital.gains[which(insurance$capital.gains > 0)]))
length(insurance$capital.gains[which(insurance$capital.gains > 0)])
head(sort(table(insurance$capital.gains[which(insurance$capital.gains > 0)]), decreasing = T), 10)
summary(insurance$capital.gains[which(insurance$capital.gains > 0)])
# Histogram
ggplot(insurance[which(insurance$capital.gains > 0), ], aes(x = capital.gains)) + 
  geom_histogram(color = "white", fill = "seagreen", binwidth = 5000) +
  theme_light() + labs(title = "Histogram of capital.gains", x = "value") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

head(insurance$capital.gains[which(insurance$capital.gains > 0 & insurance$capital.gains < 20000)])

ks.test(insurance$capital.gains[which(insurance$capital.gains > 0)], "pnorm",
        mean = mean(insurance$capital.gains[which(insurance$capital.gains > 0)]),
        sd = sd(insurance$capital.gains[which(insurance$capital.gains > 0)]))

print(c(mean(insurance$capital.gains[which(insurance$capital.gains > 0)]),
        sd(insurance$capital.gains[which(insurance$capital.gains > 0)])))

# Boxplot with zero observations
ggplot(insurance, aes(x = capital.gains)) + geom_boxplot(color = "seagreen") + coord_flip() +
  labs(title = "Boxplot of variable capital.gains", x = "value") + 
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))


# Boxplot without zero observations
ggplot(insurance[which(insurance$capital.gains > 0), ], aes(x = capital.gains)) + geom_boxplot(color = "seagreen") + coord_flip() +
  labs(title = "Boxplot of variable capital.gains", x = "value") + 
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))


# 17) Variable: capital.loss

length(unique(insurance$capital.loss))
# 354 unique values
summary(insurance$capital.loss)
# Seems that many observations take a value of zero for this variable
hist(insurance$capital.loss)
# Histogram
ggplot(insurance, aes(x = capital.loss)) + geom_histogram(color = "white", fill = "seagreen") +
  theme_light() + labs(title = "Histogram of capital.loss", x = "value") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

# How many zeros there there?
sort(table(insurance$capital.loss), decreasing = T)[1]

length(unique(insurance$capital.loss[which(insurance$capital.loss < 0)]))
length(insurance$capital.loss[which(insurance$capital.loss < 0)])
# Most frequent values
head(sort(table(insurance$capital.loss[which(insurance$capital.loss < 0)]), decreasing = T), 10)
summary(insurance$capital.loss[which(insurance$capital.loss < 0)])
# Histogram
ggplot(insurance[which(insurance$capital.loss < 0), ], aes(x = capital.loss)) + 
  geom_histogram(color = "white", fill = "seagreen", binwidth = 5000) +
  theme_light() + labs(title = "Histogram of capital.loss", x = "value") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))


ks.test(insurance$capital.loss[which(insurance$capital.loss < 0)], "pnorm",
        mean = mean(insurance$capital.loss[which(insurance$capital.loss < 0)]),
        sd = sd(insurance$capital.loss[which(insurance$capital.loss < 0)]))

print(c(mean(insurance$capital.loss[which(insurance$capital.loss < 0)]),
        sd(insurance$capital.loss[which(insurance$capital.loss < 0)])))

# Relation with capital.gains? Observations that take zero in one don't take it in the other?

length(which(insurance$capital.gains == 0))
length(which(insurance$capital.loss == 0))
# Both variables have a different quantity of zeros.
zeroposgain <- which(insurance$capital.gains == 0)
zeroposloss <- which(insurance$capital.loss == 0)
# Let's check how many elements of zeroposgain are also in zeroposloss
coincide <- logical(508)
names(coincide) <- zeroposgain
for (i in 1:508) {
  if (sum(zeroposgain[i] == zeroposloss)) {
    coincide[i] <- TRUE
  }
}

table(coincide)

# It would be interesting to see type of accident when woth variables are zero.
table(insurance$incident_type[as.numeric(names(which(coincide == T)))])
# Are both variables self cancelling?
sumcapital <- insurance$capital.gains + insurance$capital.loss
summary(sumcapital)
# Histogram of sumcapital
ggplot(as.data.frame(sumcapital), aes(x = sumcapital)) + 
  geom_histogram(color = "white", fill = "seagreen", binwidth = 5000) +
  theme_light() + labs(title = "Histogram of sumcapital", x = "value") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

# Histogram of sumcapital without observations that take zero
ggplot(data.frame("sumcapital" = sumcapital[which(sumcapital != 0)]), aes(x = sumcapital)) + 
  geom_histogram(color = "white", fill = "seagreen", binwidth = 5000) +
  theme_light() + labs(title = "Histogram of sumcapital", x = "value") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

# 18) Variable: incident_date

# This variable can be related to age and policy_bind_date

head(insurance$incident_date)
class(insurance$incident_date)
insurance$incident_date <- date(insurance$incident_date)
range(insurance$incident_date)
# How many different dates there are?
length(table(insurance$incident_date))
# All the difference between incident_date and policy_bind_date are positive?
checkDates <- data.frame(bind_date = insurance$policy_bind_date,
                         incident_date = insurance$incident_date)

# Now let's calculate each difference
for (i in 1:dim(checkDates)[1]) {
  checkDates$lapse[i] <- diff(c(checkDates$bind_date[i], checkDates$incident_date[i]))
}

# Result
head(checkDates, 10)
length(which(checkDates$lapse >= 0))
# Which observation has a negative time lapse?
checkDates[which(checkDates$lapse < 0), ]


# 19) Variable: incident_type

head(insurance$incident_type)
length(table(insurance$incident_type))
# Transform into factor
insurance$incident_type <- factor(insurance$incident_type)
table(insurance$incident_type)

# Relative frequencies barplot
ggplot(as.data.frame(round(table(insurance$incident_type)/length(insurance$incident_type), 2)),
       aes(x = Var1, y = Freq)) + geom_bar(stat = "identity", color = "seagreen", fill = "seagreen") +
  labs(title = "Relative frequencies of variable incident_type", x = "incident") + theme_classic() +
  geom_text(aes(label = Freq), vjust = -0.3) + 
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

# Are capital.loss realted with incident_type? (try including and excluding zero observations)

# Boxplot (zeros included)
ggplot(insurance, aes(x = incident_type, y = capital.loss)) + geom_boxplot(color = "seagreen") +
  labs(title = "Boxplot of capital loss by incident type", x = "incident type", y = "capital loss") + 
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

# Boxplot (zeros excluded)
ggplot(insurance[which(insurance$capital.loss < 0), ], aes(x = incident_type, y = capital.loss)) +
  geom_boxplot(color = "seagreen") +
  labs(title = "Boxplot of capital loss by incident type", x = "incident type", y = "capital loss") + 
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

# Let's repeat the boxplots, now with the variable capital.gains

# Boxplot (zeros included)
ggplot(insurance, aes(x = incident_type, y = capital.gains)) + geom_boxplot(color = "seagreen") +
  labs(title = "Boxplot of capital gains by incident type", x = "incident type", y = "capital gain") + 
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))


# Boxplot (zeros excluded)
ggplot(insurance[which(insurance$capital.gains > 0), ], aes(x = incident_type, y = capital.gains)) +
  geom_boxplot(color = "seagreen") +
  labs(title = "Boxplot of capital gains by incident type", x = "incident type", y = "capital gain") + 
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

# 20) Variable: collision_type

head(insurance$collision_type, 10)
length(table(insurance$collision_type))
table(insurance$collision_type)

?replace
# Replacing "?" by NA
# Provisionally let's replace the ? by "NA" instead of NA
insurance$collision_type <- replace(insurance$collision_type, which(insurance$collision_type == "?"), "NA")
sum(is.na(insurance$collision_type))
# Transforming the variable to factor
insurance$collision_type <- factor(insurance$collision_type)

# Relative frequencies barplot
ggplot(as.data.frame(round(table(insurance$collision_type)/length(na.omit(insurance$collision_type)), 2)),
       aes(x = Var1, y = Freq)) + geom_bar(stat = "identity", color = "seagreen", fill = "seagreen") +
  labs(title = "Relative frequencies of variable collision_type", x = "type") + theme_classic() +
  geom_text(aes(label = Freq), vjust = -0.3) + 
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

# Do a stacked barplot of collision type by type of incident

# How many rows we need? (na.omit since don't want to plot the naumber of NA's -> let's plot NA's
# also better).
length(unique(insurance$incident_type))*length(unique(insurance$collision_type))
# Auxiliar variable to create the incident column in the following dataframe
auxIncident <- lapply(levels(insurance$incident_type), rep, 4)
incident <- character()
for (i in 1:length(auxIncident)) {
  incident <- c(incident, unlist(auxIncident[i]))
}
# Auxiliar dataframe for the plot
stackFrame <- data.frame(incident, collision = rep(unique(insurance$collision_type), 4),
                         freqs = numeric(16))

# Example of how to calculate a batch of frequencies
table(insurance$collision_type[which(insurance$incident_type == "Multi-vehicle Collision")])
indexEnd <- 4
for (i in levels(insurance$incident_type)) {
  freqsc <- table(insurance$collision_type[which(insurance$incident_type == i)])
  stackFrame$freqs[(indexEnd - 3):indexEnd] <- freqsc/sum(freqsc)
  indexEnd <- indexEnd + 4
}

# Trying another type of loop
for (i in levels(insurance$incident_type)) {
  sumfreqsc <- sum(table(insurance$collision_type[which(insurance$incident_type == i)]))
  for (j in unique(insurance$collision_type)) {
    freqsc <- table(insurance$collision_type[which(insurance$incident_type == i & insurance$collision_type == j)])
    if (length(freqsc > 0)) {
      stackFrame[which(stackFrame$incident == i & stackFrame$collision == j), "freqs"] <- freqsc/sumfreqsc
    }
  }
}

# Stacked barplot
ggplot(stackFrame, aes(fill=collision, y=freqs, x=incident)) + 
  geom_bar(position="stack", stat="identity") + coord_flip() + 
  labs(title = "Collision type by incident type", x = "incident", y = "frequency") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))


# Rename "NA" as unknown and then transform it as a factor

insurance$collision_type <- replace(insurance$collision_type, which(insurance$collision_type == "NA"), "unknown")
insurance$collision_type <- factor(insurance$collision_type)
table(insurance$collision_type)
# 21) Variable: incident_severity

head(insurance$incident_severity)
table(insurance$incident_severity)
# Let's transform the observations into a factor
insurance$incident_severity <- factor(insurance$incident_severity)

# Relative frequencies barplot
ggplot(as.data.frame(round(table(insurance$incident_severity)/length(insurance$incident_severity), 2)),
       aes(x = Var1, y = Freq)) + geom_bar(stat = "identity", color = "seagreen", fill = "seagreen") +
  labs(title = "Relative frequencies of variable incident_severity", x = "severity") + theme_classic() +
  geom_text(aes(label = Freq), vjust = -0.3) + 
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

# Cross this variable with incident_type and capital.loss, the first with a stacked barplot and
# the second with a boxplot by category.

# Let's create again an auxiliar data.frame for creating the stacked barplot.
# How many observations we need?
length(levels(insurance$incident_severity))*length(levels(insurance$incident_severity))
# Auxiliar variable for creating the first column of the dataframe
auxList <- lapply(levels(insurance$incident_type), FUN = rep, 4)
incident <- character() # This will be the first column of the dataframe
for (i in 1:length(auxList)) {
  incident <- c(incident, unlist(auxList[i]))
}
# Create the dataframe
stackFrame <- data.frame(incident, severity = rep(levels(insurance$incident_severity), 4),
                         freqs = numeric(16))
# Calculate the frequencies
indexEnd <- 4
for (i in levels(insurance$incident_type)) {
  freqss <- table(insurance$incident_severity[which(insurance$incident_type == i)])
  stackFrame$freqs[(indexEnd - 3):indexEnd] <- freqss/sum(freqss)
  indexEnd <- indexEnd + 4
}

# Result
head(stackFrame)

# Stacked barplot
ggplot(stackFrame, aes(fill=severity, y=freqs, x=incident)) + 
  geom_bar(position="stack", stat="identity") + coord_flip() + 
  labs(title = "Incident severity by incident type", x = "incident", y = "frequency") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

# Boxplot (zeros included)
ggplot(insurance, aes(x = incident_severity, y = capital.loss)) +
  geom_boxplot(color = "seagreen") +
  labs(title = "Boxplot of capital loss by incident severity", x = "incident severity", y = "capital loss") + 
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

# Boxplot (zeros excluded)
ggplot(insurance[which(insurance$capital.loss < 0), ], aes(x = incident_severity, y = capital.loss)) +
  geom_boxplot(color = "seagreen") +
  labs(title = "Boxplot of capital loss by incident severity", x = "incident severity", y = "capital loss") + 
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))


# 22) Variable: authorities_contacted

head(insurance$authorities_contacted)
length(table(insurance$authorities_contacted))
table(insurance$authorities_contacted)
insurance$authorities_contacted <- factor(insurance$authorities_contacted)

# Relative frequencies barplot
ggplot(as.data.frame(round(table(insurance$authorities_contacted)/length(insurance$authorities_contacted), 2)),
       aes(x = Var1, y = Freq)) + geom_bar(stat = "identity", color = "seagreen", fill = "seagreen") +
  labs(title = "Relative frequencies of variable authorities_contacted", x = "authority") + theme_classic() +
  geom_text(aes(label = Freq), vjust = -0.3) + 
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

# Let's add an stacked barplot for incident severity by authority contacted.

# How many observations we need?
length(levels(insurance$authorities_contacted))*length(levels(insurance$incident_severity))
# Auxiliar list for creating the first column of the dataframe
auxList <- lapply(levels(insurance$authorities_contacted), rep, 4)
authority <- character() # First column of the data.frame
for (i in 1:length(auxList)) {
  authority <- c(authority, unlist(auxList[i]))
}

# Auxiliar dataframe for creating the plot
stackFrame <- data.frame(authority, severity = rep(levels(insurance$incident_severity), 5),
                         freqs = numeric(20))

# Calculating the frequencies
indexEnd <- 4
for (i in levels(insurance$authorities_contacted)) {
  freqsa <- table(insurance$incident_severity[which(insurance$authorities_contacted == i)])
  stackFrame$freqs[(indexEnd - 3):indexEnd] <- freqsa/sum(freqsa)
  indexEnd <- indexEnd + 4
}

# Result
head(stackFrame)

# Stacked barplot
ggplot(stackFrame, aes(fill=severity, y=freqs, x=authority)) + 
  geom_bar(position="stack", stat="identity") + coord_flip() + 
  labs(title = "Incident severity by authority contacted", x = "authority", y = "frequency") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))


# 23) Variable: incident_state

head(insurance$incident_state)
length(table(insurance$incident_state))
table(insurance$incident_state)
insurance$incident_state <- factor(insurance$incident_state)

# Relative frequencies barplot
ggplot(as.data.frame(round(table(insurance$incident_state)/length(insurance$incident_state), 2)),
       aes(x = Var1, y = Freq)) + geom_bar(stat = "identity", color = "seagreen", fill = "seagreen") +
  labs(title = "Relative frequencies of variable incident_state", x = "state") + theme_classic() +
  geom_text(aes(label = Freq), vjust = -0.3) + 
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))


# 24) Variable: incident_city

head(insurance$incident_city)
length(table(insurance$incident_city))
table(insurance$incident_city)
insurance$incident_city <- factor(insurance$incident_city)

# Relative frequencies barplot
ggplot(as.data.frame(round(table(insurance$incident_city)/length(insurance$incident_city), 2)),
       aes(x = Var1, y = Freq)) + geom_bar(stat = "identity", color = "seagreen", fill = "seagreen") +
  labs(title = "Relative frequencies of variable incident_city", x = "city") + theme_classic() +
  geom_text(aes(label = Freq), vjust = -0.3) + 
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

# All the cities appear in an unique state?
for (i in levels(insurance$incident_city)) {
  cat("city", i, "appears on:", names(table(insurance$incident_state[which(insurance$incident_city == i)])), "\n")
}

table(insurance$incident_state[which(insurance$incident_city == "Arlington")])
table(insurance$incident_state[which(insurance$incident_city == "Columbus")])
table(insurance$incident_state[which(insurance$incident_city == "Hillsdale")])
table(insurance$incident_state[which(insurance$incident_city == "Northbend")])
table(insurance$incident_state[which(insurance$incident_city == "Northbrook")])
table(insurance$incident_state[which(insurance$incident_city == "Riverwood")])
table(insurance$incident_state[which(insurance$incident_city == "Springfield")])

for (i in levels(insurance$incident_city)) {
  cat(i, "frequencies:\n")
  print(table(insurance$incident_state[which(insurance$incident_city == i)]))
  cat("--------------------\n")
}

# 25) Variable: incident_location

head(insurance$incident_location)
length(unique(insurance$incident_location))

# 26) Variable: incident_hour_of_the_day

head(insurance$incident_hour_of_the_day)
length(unique(insurance$incident_hour_of_the_day))
table(insurance$incident_hour_of_the_day)

# Relative frequencies barplot
ggplot(as.data.frame(round(table(insurance$incident_hour_of_the_day)/length(insurance$incident_hour_of_the_day), 2)),
       aes(x = Var1, y = Freq)) + geom_bar(stat = "identity", color = "seagreen", fill = "seagreen") +
  labs(title = "Relative frequencies of variable incident_hour_of_the_day", x = "hour") + theme_classic() +
  geom_text(aes(label = Freq), hjust = 1.3) + 
  theme(plot.title = element_text(hjust = 0.5, face = "bold")) + coord_flip()

# 27) Variable: number_of_vehicles_involved

# Do a stacked barplot with incident type

head(insurance$number_of_vehicles_involved)
length(table(insurance$number_of_vehicles_involved))
unique(insurance$number_of_vehicles_involved)
table(insurance$number_of_vehicles_involved)
insurance$number_of_vehicles_involved <- factor(insurance$number_of_vehicles_involved, ordered = T) # ORDINAL!!!

# Relative frequencies barplot
ggplot(as.data.frame(round(table(insurance$number_of_vehicles_involved)/length(insurance$number_of_vehicles_involved), 2)),
       aes(x = Var1, y = Freq)) + geom_bar(stat = "identity", color = "seagreen", fill = "seagreen") +
  labs(title = "Relative frequencies of variable number_of_vehicles_involved", x = "number of vehicles") + theme_classic() +
  geom_text(aes(label = Freq), vjust = -0.5) + 
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

# Stacked barplot for incident_type vs number_of_vehicles_involved
#
# How many rows we need?
length(levels(insurance$incident_type))*length(levels(insurance$number_of_vehicles_involved))

# Auxiliar variable  for creating the first column of the dataframe needed for the stacked barplot
auxList <- lapply(levels(insurance$number_of_vehicles_involved), rep, 4)
vehicles <- character() # First column of the dataframe
for (i in 1:length(auxList)) {
  vehicles <- c(vehicles, unlist(auxList[i]))
}

# Auxiliar dataframe for the plot
stackFrame <- data.frame(vehicles, incident = rep(levels(insurance$incident_type), 4),
                         freqs = numeric(16))

# Calculation of the frequencies
indexEnd <- 4
for (i in levels(insurance$number_of_vehicles_involved)) {
  freqsv <- table(insurance$incident_type[which(insurance$number_of_vehicles_involved == i)])
  stackFrame$freqs[(indexEnd - 3):indexEnd] <- freqsv/sum(freqsv)
  indexEnd <- indexEnd + 4
}

# Result
head(stackFrame)

# Stacked barplot
ggplot(stackFrame, aes(fill = incident, y = freqs, x = vehicles)) + 
  geom_bar(position="stack", stat="identity") + coord_flip() + 
  labs(title = "Incident type by number of vehicles involved", x = "number of vehicles", y = "frequency") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

# 28) Variable: property_damage

head(insurance$property_damage)
unique(insurance$property_damage)
table(insurance$property_damage)
# Since property damage only can be yes or no, although it was temporally, the ? should be
# transformed into NA's.

# Relative frequencies barplot
ggplot(as.data.frame(round(table(insurance$property_damage)/length(insurance$property_damage), 2)),
       aes(x = Var1, y = Freq)) + geom_bar(stat = "identity", color = "seagreen", fill = "seagreen") +
  labs(title = "Relative frequencies of variable property_damage", x = "property damage") + theme_classic() +
  geom_text(aes(label = Freq), vjust = -0.5) + 
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

for (i in levels(insurance$incident_type)) {
  cat(i, "frequencies:\n")
  print(table(insurance$property_damage[which(insurance$incident_type == i)]))
  cat("---------------------------\n")
}

for (i in levels(insurance$incident_severity)) {
  cat(i, "frequencies:\n")
  print(table(insurance$property_damage[which(insurance$incident_severity == i)]))
  cat("---------------------------\n")
}

for (i in levels(insurance$collision_type)) {
  cat(i, "frequencies:\n")
  print(table(insurance$property_damage[which(insurance$collision_type== i)]))
  cat("---------------------------\n")
}

insurance$property_damage <- replace(insurance$property_damage, which(insurance$property_damage == "?"), NA)
sum(is.na(insurance$property_damage))
insurance$property_damage <- factor(insurance$property_damage)

# 29) Variable: bodily_injuries

head(insurance$bodily_injuries)
length(unique(insurance$bodily_injuries))
table(insurance$bodily_injuries)

insurance$bodily_injuries <- factor(insurance$bodily_injuries, ordered = T)

# Relative frequencies barplot
ggplot(as.data.frame(round(table(insurance$bodily_injuries)/length(insurance$bodily_injuries), 2)),
       aes(x = Var1, y = Freq)) + geom_bar(stat = "identity", color = "seagreen", fill = "seagreen") +
  labs(title = "Relative frequencies of variable bodily_injuries", x = "bodily injuries") + theme_classic() +
  geom_text(aes(label = Freq), vjust = -0.5) + 
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))


# Stacked barplot of incident_severity by bodily_injuries

# How many rows we need?
length(levels(insurance$bodily_injuries))*length(levels(insurance$incident_severity))
# Auxiliar variable to obtain the first column of the dataframe for the stacked barplot
auxList <- lapply(levels(insurance$bodily_injuries), rep, 4)
b_injuries <- character() # First column for the dataframe
for (i in 1:length(auxList)) {
  b_injuries <- c(b_injuries, unlist(auxList[i]))
}

# Auxiliar dataframe for the stacked barplot
stackFrame <- data.frame(b_injuries, severity = rep(levels(insurance$incident_severity), 3),
                         freqs = numeric(12))

# Calculation of the frequencies
indexEnd <- 4
for (i in levels(insurance$bodily_injuries)) {
  freqss <- table(insurance$incident_severity[which(insurance$bodily_injuries == i)])
  stackFrame$freqs[(indexEnd - 3):indexEnd] <- freqss/sum(freqss)
  indexEnd <- indexEnd + 4
}

# Result 
head(stackFrame)

# Stacked barplot
ggplot(stackFrame, aes(fill = severity, y = freqs, x = b_injuries)) + 
  geom_bar(position="stack", stat="identity") + coord_flip() + 
  labs(title = "Incident severity by bodily injuries", x = "bodily injuries", y = "frequency") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

# 30) Variable: witnesses

head(insurance$witnesses)
unique(insurance$witnesses)
table(insurance$witnesses)
insurance$witnesses <- factor(insurance$witnesses, ordered = T)

# Relative frequencies barplot
ggplot(as.data.frame(round(table(insurance$witnesses)/length(insurance$witnesses), 2)),
       aes(x = Var1, y = Freq)) + geom_bar(stat = "identity", color = "seagreen", fill = "seagreen") +
  labs(title = "Relative frequencies of variable witnesses", x = "number of witnesses") + theme_classic() +
  geom_text(aes(label = Freq), vjust = -0.5) + 
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

# Checking if the collision type and the number of witnesses are related.
for (i in levels(insurance$witnesses)) {
  cat("Witnesses: ", i, "\n")
  print(table(insurance$collision_type[which(insurance$witnesses == i)]))
  cat("-----------------------\n")
}

# Stacked barplot of collision type by number of witnesses
# How many rows are needed?
length(levels(insurance$witnesses))*length(levels(insurance$collision_type))
# Auxiliar variable for creating the first row of the dataframe for stacked barplot
auxList <- lapply(levels(insurance$witnesses), rep, 4)
witnesses <- character()
for (i in 1:length(auxList)) {
  witnesses <- c(witnesses, unlist(auxList[i]))
}
# Auxiliar data.frame
stackFrame <- data.frame(witnesses, collision = rep(levels(insurance$collision_type), 4), 
                         freqs = numeric(16))

# Calculation of the frequencies
indexEnd <- 4
for (i in levels(insurance$witnesses)) {
  freqsw <- table(insurance$collision_type[which(insurance$witnesses == i)])
  stackFrame$freqs[(indexEnd - 3):indexEnd] <- freqsw/sum(freqsw)
  indexEnd <- indexEnd + 4
}

# Result 
head(stackFrame)

# Stacked barplot
ggplot(stackFrame, aes(fill = collision, y = freqs, x = witnesses)) + 
  geom_bar(position="stack", stat="identity") + coord_flip() + 
  labs(title = "Collision type by number of witnesses", x = "collision type", y = "frequency") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))


# 31) Variable: police_report_available

unique(insurance$police_report_available)
table(insurance$police_report_available)
insurance$police_report_available <- replace(insurance$police_report_available,
                                             which(insurance$police_report_available == "?"), NA)

insurance$police_report_available <- factor(insurance$police_report_available)

# Relative frequencies barplot
ggplot(as.data.frame(round(table(insurance$police_report_available)/length(insurance$police_report_available), 2)),
       aes(x = Var1, y = Freq)) + geom_bar(stat = "identity", color = "seagreen", fill = "seagreen") +
  labs(title = "Relative frequencies of variable police_report_available", x = "police report available") + theme_classic() +
  geom_text(aes(label = Freq), vjust = -0.5) + 
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

# Checking the frequencies of collision type for both categories
for (i in levels(insurance$police_report_available)) {
  freqsp <- table(insurance$collision_type[which(insurance$police_report_available == i)])
  cat("police report available: ", i, "\n")
  print(freqsp/sum(freqsp))
  cat("---------------------------------------------------------------\n")
}

# 32) Variable: total_claim_amount

head(insurance$total_claim_amount)
length(unique(insurance$total_claim_amount))
head(sort(table(insurance$total_claim_amount), decreasing = T))
summary(insurance$total_claim_amount)

# Histogram
ggplot(insurance, aes(x = total_claim_amount)) + 
  geom_histogram(color = "white", fill = "seagreen", binwidth = 3000) +
  theme_light() + labs(title = "Histogram of total claim amount", x = "value") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))



# Boxplot
ggplot(insurance, aes(x = total_claim_amount)) + geom_boxplot(color = "seagreen") + coord_flip() +
  labs(title = "Boxplot of variable total_claim_amount", x = "value") + 
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

# Histogram (only first quartile)
ggplot(insurance[which(insurance$total_claim_amount <= 15000), ], aes(x = total_claim_amount)) + 
  geom_histogram(color = "white", fill = "seagreen", binwidth = 500) +
  theme_light() + labs(title = "Histogram of total claim amount", x = "value") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

# Let's check that this variable is the sum of the other claims
checkTotal <- data.frame(injury = insurance$injury_claim, property = insurance$property_claim,
                         vehicle = insurance$vehicle_claim, total = insurance$total_claim_amount,
                         claimsums = integer(1000), check = logical(1000))

# Calculate the sums
for (i in 1:dim(checkTotal)[1]) {
  checkTotal$claimsums[i] <- sum(checkTotal[i, 1:3])
}
# Check if total and sum are equal
for (i in 1:dim(checkTotal)[1]) {
  if (checkTotal$total[i] == checkTotal$claimsums[i]) {
    checkTotal$check[i] <- T
  }
}

# Result
head(checkTotal)

sum(checkTotal$check)

# 33) Variable: injury_claim

head(insurance$injury_claim)
length(unique(insurance$injury_claim))
1000 - length(unique(insurance$injury_claim))
head(sort(table(insurance$injury_claim), decreasing = T), 10)

# Histogram
ggplot(insurance, aes(x = injury_claim)) + 
  geom_histogram(color = "white", fill = "seagreen", binwidth = 900) +
  theme_light() + labs(title = "Histogram of injury_claim", x = "value") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

# Boxplot
ggplot(insurance, aes(x = injury_claim)) + geom_boxplot(color = "seagreen") + coord_flip() +
  labs(title = "Boxplot of variable injury_claim", x = "value") + 
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

summary(insurance$injury_claim)

# Boxplot by incident_severity
ggplot(insurance, aes(x = injury_claim, y = incident_severity)) + geom_boxplot(color = "seagreen") + coord_flip() +
  labs(title = "Boxplot of variable injury_claim by incident_severity", x = "injury_claim") + 
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

# Let's change incident_severity to an ordered factorial

insurance$incident_severity <- ordered(insurance$incident_severity, 
                                   levels = c("Trivial Damage", "Minor Damage", "Major Damage", 
                                              "Total Loss"))
# 34) Variable: property_claim

head(insurance$property_claim)
length(unique(insurance$property_claim))
1000 - length(unique(insurance$property_claim))
head(sort(table(insurance$property_claim), decreasing = T), 10)

# Histogram
ggplot(insurance, aes(x = property_claim)) + 
  geom_histogram(color = "white", fill = "seagreen", binwidth = 900) +
  theme_light() + labs(title = "Histogram of property_claim", x = "value") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

# Boxplot
ggplot(insurance, aes(x = property_claim)) + geom_boxplot(color = "seagreen") + coord_flip() +
  labs(title = "Boxplot of variable property_claim", x = "value") + 
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

summary(insurance$property_claim)

# Boxplot by incident_severity
ggplot(insurance, aes(x = property_claim, y = incident_severity)) + geom_boxplot(color = "seagreen") + coord_flip() +
  labs(title = "Boxplot of variable property_claim by incident_severity", x = "property_claim") + 
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

# 35) Variable: vehicle_claim

head(insurance$vehicle_claim)
length(unique(insurance$vehicle_claim))
1000 - length(unique(insurance$vehicle_claim))
head(sort(table(insurance$vehicle_claim), decreasing = T), 10)

summary(insurance$vehicle_claim)

# Histogram
ggplot(insurance, aes(x = vehicle_claim)) + 
  geom_histogram(color = "white", fill = "seagreen", binwidth = 2000) +
  theme_light() + labs(title = "Histogram of vehicle_claim", x = "value") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

# Boxplot
ggplot(insurance, aes(x = vehicle_claim)) + geom_boxplot(color = "seagreen") + coord_flip() +
  labs(title = "Boxplot of variable vehicle_claim", x = "value") + 
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

# Boxplot by incident_severity
ggplot(insurance, aes(x = vehicle_claim, y = incident_severity)) + geom_boxplot(color = "seagreen") + coord_flip() +
  labs(title = "Boxplot of variable vehicle_claim by incident_severity", x = "vehicle_claim") + 
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

# 36) Variable: auto_make

head(insurance$auto_make)
length(unique(insurance$auto_make))
insurance$auto_make <- factor(insurance$auto_make)

table(insurance$auto_make)

# Relative frequencies barplot
ggplot(as.data.frame(round(table(insurance$auto_make)/length(insurance$auto_make), 2)),
       aes(x = Var1, y = Freq)) + geom_bar(stat = "identity", color = "seagreen", fill = "seagreen") +
  labs(title = "Relative frequencies of variable auto_make", x = "state") + theme_classic() +
  geom_text(aes(label = Freq), hjust = 1.3) + 
  theme(plot.title = element_text(hjust = 0.5, face = "bold")) + coord_flip()

# Replacing suburu by subaru

# First return its type to character
insurance$auto_make <- as.character(insurance$auto_make)
# Replace the misspelled value
insurance$auto_make <- replace(insurance$auto_make, which(insurance$auto_make == "Suburu"), 
                               "Subaru")
# Make it a factor again
insurance$auto_make <- factor(insurance$auto_make)

# Boxplot of vehicle_claim by auto_make

# Boxplot
ggplot(insurance, aes(x = auto_make, y = vehicle_claim)) + geom_boxplot(color = "seagreen") + coord_flip() +
  labs(title = "Boxplot of variable vehicle_claim by auto make", x = "auto make") + 
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

# 37) Variable: auto_model

head(insurance$auto_model)
length(unique(insurance$auto_model))
insurance$auto_model <- factor(insurance$auto_model)

# Relative frequencies barplot
ggplot(as.data.frame(round(table(insurance$auto_model)/length(insurance$auto_model), 2)),
       aes(x = Var1, y = Freq)) + geom_bar(stat = "identity", color = "seagreen", fill = "seagreen") +
  labs(title = "Relative frequencies of variable auto_model", x = "state") + theme_classic() +
  geom_text(aes(label = Freq), hjust = 1.3) + 
  theme(plot.title = element_text(hjust = 0.5, face = "bold")) + coord_flip()

# Let's check that all the models have only one brand

checkUniqueBrand <- numeric() # This variable will acount how many brans are registered for
                              # each model. If its sum is 39, are all ok.
names(checkUniqueBrand) <- levels(insurance$auto_model)
for (i in levels(insurance$auto_model)) {
  checkUniqueBrand <- c(checkUniqueBrand, length(unique(insurance$auto_make[which(insurance$auto_model == i)])))
}

sum(checkUniqueBrand)

# 38) Variable: auto_year

head(insurance$auto_year)
range(insurance$auto_year)

# Relative frequencies barplot
ggplot(as.data.frame(round(table(insurance$auto_year)/length(insurance$auto_year), 2)),
       aes(x = Var1, y = Freq)) + geom_bar(stat = "identity", color = "seagreen", fill = "seagreen") +
  labs(title = "Relative frequencies of variable auto_year", x = "state") + theme_classic() +
  geom_text(aes(label = Freq), hjust = 1.3) + 
  theme(plot.title = element_text(hjust = 0.5, face = "bold")) + coord_flip()

# Let's explore the years obtained for each model

for (i in levels(insurance$auto_model)) {
  cat("Model", i, "\n")
  print(table(insurance$auto_year[which(insurance$auto_model == i)]))
  cat("--------------------------------------------------------------\n")
}

# 39) Variable: fraud_reported

unique(insurance$fraud_reported)
insurance$fraud_reported <- replace(insurance$fraud_reported, which(insurance$fraud_reported == "Y"), "1")
insurance$fraud_reported <- replace(insurance$fraud_reported, which(insurance$fraud_reported == "N"), "0")
insurance$fraud_reported <- as.numeric(insurance$fraud_reported)
insurance$fraud_reported <- factor(insurance$fraud_reported)

# Relative frequencies barplot
ggplot(as.data.frame(round(table(insurance$fraud_reported)/length(insurance$fraud_reported), 2)),
       aes(x = Var1, y = Freq)) + geom_bar(stat = "identity", color = "seagreen", fill = "seagreen") +
  labs(title = "Relative frequencies of variable fraud_reported", x = "state") + theme_classic() +
  geom_text(aes(label = Freq), vjust = -0.3) + 
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))




