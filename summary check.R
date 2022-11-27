
## import data
ped <- read.csv("ped_crashes.csv")

## EDA
library(qacBase)
contents(ped)
df_plot(ped)
cor_plot(ped)

ped <- subset(ped, Weather.Conditions..2016..!="Uncoded & errors")
ped <- subset(ped, Person.Gender != "Uncoded & errors")
ped <- subset(ped, Person.Age != "DOB invalid")
