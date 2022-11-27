
# import data
ped <- read.csv("ped_crashes.csv")

# EDA
library(qacBase)
contents(ped)

# drop and rename variables, remove uncoded values, mutate variable types
library(dplyr)
ped <- ped %>% 
  select(-c("Crash.Day","City.or.Township","Party.Type")) %>% #party type only one value
  rename(year = Crash.Year,
         month = Crash.Month,
         time = Time.of.Day,
         weekday = Day.of.Week,
         intersection = Crash..Intersection,
         hit_run = Crash..Hit.and.Run,
         light = Lighting.Conditions,
         weather = Weather.Conditions..2016..,
         speedLimit = Speed.Limit.at.Crash.Site,
         worstInjury = Worst.Injury.in.Crash,
         age = Person.Age,
         gender = Person.Gender) %>% 
  filter(weather != "Uncoded & errors",
         gender != "Uncoded & errors",
         age != "DOB invalid",
         age != "Less than 1 year old",
         light != "Unknown",
         weather != "Unknown",
         speedLimit != "Uncoded & errors") %>% 
  mutate(speedLimit = as.numeric(speedLimit),
         age = as.numeric(age))
  
# recode time as categorical variable
morning <- c("6:00 AM - 7:00 AM" ,"7:00 AM - 8:00 AM","8:00 AM - 9:00 AM",
             "9:00 AM - 10:00 AM","10:00 AM - 11:00 AM","11:00 AM - 12:00 noon")
afternoon <- c("12:00 noon - 1:00 PM","1:00 PM - 2:00 PM","2:00 PM - 3:00 PM",
               "3:00 PM - 4:00 PM","4:00 PM - 5:00 PM","5:00 PM - 6:00 PM")
night <- c("6:00 PM - 7:00 PM","7:00 PM - 8:00 PM","8:00 PM - 9:00 PM",
           "9:00 PM - 10:00 PM","10:00 PM - 11:00 PM","11:00 PM - 12:00 midnight")
lateNight <- c("12:00 midnight - 1:00 AM","1:00 AM - 2:00 AM","2:00 AM - 3:00 AM",
               "3:00 AM - 4:00 AM","4:00 AM - 5:00 AM","5:00 AM - 6:00 AM")

ped$time <- ifelse(ped$time %in% morning, "morning",
                   ifelse(ped$time %in% afternoon, "afternoon",
                          ifelse(ped$time %in% night, "night", "midnight")))

df_plot(ped)
cor_plot(ped)

# recode worstinjury
ped$worstInjury <- recode(ped$worstInjury, 
                          "Suspected serious injury (A)" = "4", 
                          "Suspected minor injury (B)" = "3",
                          "Possible injury (C)" = "2",
                          "No injury (O)"  = "1", 
                          "Fatal injury (K)" = "5")

#recode intersection
ped$intersection <- recode(ped$intersection, 
                           "Not intersection crash" = "0", 
                           "Intersection crash" = "1")






