# This syntax shows some code for beginner users of R.

# Created by Sally Larsen using data files from the PSYC 422 tutorials
# Modified by Deborah Apthorp in 2023. 

# First set up a project as described in the slides - that way R knows where all your files are. 

# Next let's  read in the data 
# This dataset is for the Stats Excercise #2 in the 422 Intensive school 2022



# library(readr)
Data1 <- read.csv("Zhang et al. 2014 Study 3.csv")

# First we'll view the dataset
View(Data1)

# There are many, many packages built for R. 
# Here is how to get them to run (this is just one way)
# First install - use this command:

# install.packages("psych") 

# You should never include this in a normal R script though! 
# Next call the package to be used

library(psych)
# OR require(psych) does the same thing as library()

# Now let's generate descriptive statistics using the psych package
# describe() reports basic summary statistics (mean, sd, median, mad, 
# range, minimum, maximum, skew, kurtosis, standard error)

describe(Data1)

# You can see why it's a useful command!

# What if I'd like to export this information to excel so I can make tables?
# First let's make an data frame containing the descriptives. 
# The arrow means the same as "="
# We are assigning the data to an object called "Descriptives" 

Descriptives <- describe(Data1)

# You can see it in spreadsheet style using View() - or you can just click on it over to your right. 
View(Descriptives)

# Next let's  install a package that will export data to Excel!

# install.packages("xlsx")

library(xlsx)

# Now export data in excel
write.xlsx(Descriptives, file = "Descriptives.xlsx",
           sheetName = "Descriptives", append = FALSE)

# This command allows you to see the first few rows of every variable
# Useful if you have a bunch of weird missingness that you don't know about

head(Data1)

# If you want to see a particular number of rows you can use

head(Data1, n = 20)

# Now lets do some data visualisation using base R 
# (i.e. we're not using a package here)
# First let's look at histograms of some variables

hist(Data1$Age)

hist(Data1$Gender)

# What about boxplots?

boxplot(Data1$T2_Extraordinariness)

boxplot(Data1$T2_Memory)

# You can see how sometimes the formatting is nice, sometimes not.
# So people have made packages to get nice formatting
# One of my favourites for beginner data visualisation is flexplot
# flexplot creates 'objects' which are then editable using ggplot2

# Method 1 
# install.packages("devtools")
# install.packages("jmvcore")
# devtools::install_github("dustinfife/flexplot")

# Method 2 
# install.packages("flexplot")
# install.packages("ggplot2")
library(flexplot)
library(ggplot2)

# Dustin Fife -- Flexplot 

histogram1 <- flexplot(Age ~ 1, Data1)
plot(histogram1)

# In flexplot you can alter the look of the plots so that they are APA style
histogram2 <- flexplot(Age ~ 1, Data1) + theme_classic()
plot(histogram2)

# Try this:
histogram1 + theme_classic() 

# What about a scatterplot?
# For this we need to write a little bit of code describing a regression model
# Lets use a different dataset that contains more continuous variables
Data2 <- read.csv("Coldwell et al Data_2006.csv", header=T)

# require(psych)
describe(Data2)

scatter <- flexplot(mum_pos ~ mum_neg, Data2, 
                    method = "lm") 

# flexplot defaults to a loess line, so method=lm forces a regression line

plot(scatter)

# Let's add some nicer labels
scatter + theme_classic() + labs(x="Mother's Negativity", y="Mother's Positivity")

# This is how to do correlations using the psych package!
correlations <- corr.test(Data2)
print(correlations) 

# Here is another way to generate a correlation matrix, this time just 
# the lower diagonal

lowerCor(Data2, digits=2)


# Now we are attempting to export the correlation matrices to a table to save 
# faffing around with copy/paste

# install.packages("apaTables")
library(apaTables)
apa.cor.table(Data2, filename="Table1_APA.doc")

# What if I'm only using a subset of a larger number of items and I want 
# to make a correlation matrix of this subset?
# I need the tidyR package to make the pipe %>% work

library(tidyverse)
Data_Sub <- Data2 %>% select(child_age, mum_pos, mum_neg, chaos)
lowerCor(Data_Sub, digits=2)
apa.cor.table(Data_Sub, filename="Table2_APA.doc")

# NIFTY! 


# Here is another way to generate a correlation matrix in base R
# But its not as interpretable or visually appealing imo
cor(Data_Sub, use="complete.obs", method="pearson")

# So we see a slight negative correlation between mum_pos and mum_neg
# But how do we do the regression analysis in R?

# Does mother's level of negativity significantly predict their level of positivity?

Model1 <- lm(mum_pos ~ mum_neg, Data2)
summary(Model1)


# What if our research question required a multiple regression?

Model2 <- lm(child_warmth ~ mum_neg + child_anger + child_gender, Data2)
summary(Model2)

# What if we wanted to report standardized regression coefficients?
# Using the effectsize package use the standardize_parameters command and apply it to the 
# Model2 object
# install.packages("effectsize")

# This is another way, and the 'effectsize' package is quite useful for
# computing different effect sizes, e.g. Cohen's d, Hedges g etc.
library(effectsize)
standardize_parameters(Model2, type = "std", ci.lvl = 0.95)

# The 'effectsize' package is quite useful for
# computing different effect sizes, e.g. Cohen's d, Hedges g etc.


# What if our research question requires a comparison of means from two independent groups?

Model3 <- lm(child_anger ~ child_gender + mum_neg, Data2)
summary(Model3)
standardize_parameters(Model3, type = "std", ci.lvl = 0.95)

# Here is some more data to play around with: what code can you write to explore the data?

Data3 <-  read.csv("Atir Rosenzweig Dunning 2015 Study 1b.csv", header=T)
describe(Data3)

# Here's the results from your Intro Session survey if you want to play with them! 

library(readxl)
Zoom_PollReport <- read_excel("Zoom_PollReport.xlsx", sheet = "Poll1Results")

View(Zoom_PollReport)                                                             
 
# Separate the pets into separate columns
Zoom_PollReport <- Zoom_PollReport %>% separate(Pets, c("Pet1", "Pet2", "Pet3"), 
                                                extra = "drop", fill = "right")
