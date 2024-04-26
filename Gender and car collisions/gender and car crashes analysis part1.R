# source: https://data.cambridgema.gov/Public-Safety/Police-Department-Crash-Data-Updated/gb5w-yva3/about_data
# Question: Is gender implicit in car crashes?

library(tidyverse)
library(stringr)
library(lubridate)

df <- read.csv("Police_Department_Crash_Data_-_Updated_20240417.csv")

nrow(df) # entire data: 12815 rows

# exploring oldest and newest crashes
df %>% 
  mutate(year = year(mdy_hms(Date.Time))) %>% 
  filter(year==2024) %>% 
  arrange(desc(year)) %>% 
  head() %>% 
  pull(Date.Time)

df %>% 
  mutate(year = year(mdy_hms(Date.Time))) %>% 
  filter(year==2015) %>% 
  arrange(desc(year)) %>% 
  head() %>% 
  pull(Date.Time)

# data cleaning, subsetting
df_sub <- df %>% # when just filtering wrt objects, nrow is 2960; when filtering also wrt sex, nrow is 2598
  mutate_if(is.character, str_trim) %>% 
  filter(Object.1 == "PASSENGER CAR" & Object.2 == "PASSENGER CAR",
         P1.Sex %in% c("MALE", "FEMALE"), P2.Sex %in% c("MALE", "FEMALE")
         ) %>% 
  select(Date.Time, P1.Sex, P2.Sex, Date.Time:Object.2, May.involve.cyclist:Work.Zone, Street.or.Intersection, 
         Intersection.Direction.1, Intersection.Direction.2, Intersection.Direction.3,
         Street.Direction, v1.State.Code:V1.Configuration, V1.Hit.and.Run, V1.Action.Prior.to.Crash, #cont. here
         V1.Occupant.Count, V1.Most.Damaged.Area, V1.Travel.Direction,  
         V2.State.Code:V2.Configuration, V2.Hit.and.Run, V2.Action.Prior.to.Crash, 
         V2.Occupant.Count, V2.Most.Damaged.Area, V2.Travel.Direction,
         P1.Role, P1.Drivers.Lic.State, P1.Drivers.Lic.Class.1, P1.Age, P1.Veh.Owner,
         P2.Role, P2.Drivers.Lic.State, P2.Drivers.Lic.Class.1, P2.Age, P2.Veh.Owner
         ) %>% 
  select(-c(May.involve.cyclist, May.Involve.Pedestrian, School.Bus.Related, Work.Zone,
            Intersection.Direction.3, V1.Configuration, V2.Configuration, P1.Age,
            P2.Age, Object.1, Object.2)) %>% 
  # categorical variables imputation and collapsing (both based on frequencies using df_sub)
  mutate( 
    Manner.of.CollisionNA = as.numeric(Manner.of.Collision == ""),
    Manner.of.Collision = ifelse(Manner.of.Collision == "", "ANGLE", Manner.of.Collision),
    
    First.Harmful.Event.LocationNA = as.numeric(First.Harmful.Event.Location == ""),
    First.Harmful.Event.Location = ifelse(First.Harmful.Event.Location == "", "ROADWAY",
                                          First.Harmful.Event.Location),
    First.Harmful.Event.Location = ifelse(
      First.Harmful.Event.Location %in% c("MEDIAN", "SHOULDER - PAVED", "SHOULDER - TRAVEL LANE"),
      "OTHER", First.Harmful.Event.Location),
    
    First.Harmful.EventNA = as.numeric(First.Harmful.Event == ""),
    First.Harmful.Event = ifelse(First.Harmful.Event == "", "COLLISION WITH MOTOR VEHICLE IN TRAFFIC", 
                                 First.Harmful.Event),
    First.Harmful.Event = ifelse(First.Harmful.Event %in% 
                                   c("COLLISION WITH CURB", 
                                     "COLLISION WITH LIGHT POLE OR OTHER POST/SUPPORT",
                                     "COLLISION WITH MOPED",
                                     "COLLISION WITH OTHER MOVABLE OBJECT",
                                     "COLLISION WITH PEDALCYCLE",
                                     "COLLISION WITH PEDESTRIAN",
                                     "COLLISION WITH TREE",
                                     "OTHER",
                                     "UNKNOWN",
                                     "UNKNOWN NON-COLLISION"
                                     ),
                                 "OTHER",
                                 First.Harmful.Event
                                 ),
    
    Ambient.LightNA = as.numeric(Ambient.Light == ""),
    Ambient.Light = ifelse(Ambient.Light == "", "DAYLIGHT", Ambient.Light),
    Ambient.Light = ifelse(Ambient.Light %in% 
                             c("DARK - ROADWAY NOT LIGHTED",
                               "DARK - UNKNOWN ROADWAY LIGHTING",
                               "DAWN",
                               "OTHER"
                             ),
                           "OTHER",
                           Ambient.Light
                           ),
    # continue this endeavor with: Weather.Condition.1
    Weather.Condition.1NA = as.numeric(Weather.Condition.1 == ""),
    Weather.Condition.1 = ifelse(Weather.Condition.1 == "", "CLEAR", Weather.Condition.1),
    Weather.Condition.1 = ifelse(Weather.Condition.1 %in% 
                             c("BLOWING, SAND, SNOW",
                               "FOG, SMOG, SMOKE",
                               "OTHER",
                               "SLEET, HAIL, FREEZING RAIN OR DRIZZLE"
                             ),
                           "OTHER",
                           Weather.Condition.1
    ),
    Weather.Condition.2NA = as.numeric(Weather.Condition.2 == ""),
    Weather.Condition.2 = ifelse(Weather.Condition.2 == "", "CLEAR", Weather.Condition.2),
    Weather.Condition.2 = ifelse(Weather.Condition.2 %in% 
                                   c("BLOWING, SAND, SNOW",
                                     "FOG, SMOG, SMOKE",
                                     "OTHER",
                                     "SEVERE CROSSWINDS",
                                     "SLEET, HAIL, FREEZING RAIN OR DRIZZLE",
                                     "SNOW",
                                     "UNKNOWN"
                                   ),
                                 "OTHER",
                                 Weather.Condition.2
    ),
    Traffic.Control.Device.TypeNA = as.numeric(Traffic.Control.Device.Type == ""),
    Traffic.Control.Device.Type = ifelse(Traffic.Control.Device.Type == "", "NO CONTROLS", 
                                         Traffic.Control.Device.Type),
    Traffic.Control.Device.Type = ifelse(Traffic.Control.Device.Type %in% 
                                           c("FLASHING TRAFIC CONTROL SIGNAL",
                                             "UNKNOWN",
                                             "WARNING SIGNS",
                                             "YIELD SIGNS"
                                           ),
                                         "OTHER",
                                         Traffic.Control.Device.Type
    ),
    Traffic.Control.Device.FunctionalityNA = as.numeric(Traffic.Control.Device.Functionality == ""),
    Traffic.Control.Device.Functionality = ifelse(Traffic.Control.Device.Functionality =="",
                                                  "YES",
                                                  Traffic.Control.Device.Functionality),
    Road.Surface.ConditionNA =  as.numeric(Road.Surface.Condition == ""),
    Road.Surface.Condition = ifelse(Road.Surface.Condition == "", "DRY", 
                                    Road.Surface.Condition),
    Road.Surface.Condition = ifelse(Road.Surface.Condition %in% 
                                      c("ICE",
                                        "OTHER",
                                        "SAND, MUD, DIRT, OIL, GRAVEL",
                                        "SLUSH"
                                      ),
                                    "OTHER",
                                    Road.Surface.Condition
    ),
    
    Roadway.Junction.TypeNA = as.numeric( Roadway.Junction.Type == ""),
    Roadway.Junction.Type = ifelse(Roadway.Junction.Type == "", "NOT AT INTERSECTION", 
                                   Roadway.Junction.Type),
    Roadway.Junction.Type = ifelse(Roadway.Junction.Type %in% 
                                     c("DRIVEWAY",
                                       "FIVE-POINT OR MORE",
                                       "OFF RAMP",
                                       "TRAFFIC CIRCLE",
                                       "UNKNOWN"
                                     ),
                                   "OTHER",
                                   Roadway.Junction.Type
    ),
    Trafficway.DescriptionNA = as.numeric(Trafficway.Description == ""),
    Trafficway.Description = ifelse(Trafficway.Description == "", "TWO-WAY, NOT DIVIDED", 
                                    Trafficway.Description),
    Street.or.Intersection = ifelse(Street.or.Intersection == "", "NON INTERSECTION", 
                                    Street.or.Intersection),
    
    Intersection.Direction.1NA = as.numeric(Intersection.Direction.1 == ""),
    Intersection.Direction.1 = ifelse(Intersection.Direction.1 == "", "EASTBOUND", 
                                      Intersection.Direction.1),
    Intersection.Direction.2NA = as.numeric(Intersection.Direction.2 %in%  c("", "NOT REPORTED")),
    Intersection.Direction.2 = ifelse(Intersection.Direction.2 == "", "NORTHBOUND", 
                                      Intersection.Direction.2),
    Street.DirectionNA = as.numeric(Street.Direction %in%  c("", "NOT REPORTED", "UNKNOWN")),
    Street.Direction = ifelse(Street.Direction == "", "EASTBOUND", 
                              Street.Direction),
    v1.State.CodeNA = as.numeric(v1.State.Code == ""),
    v1.State.Code = ifelse(v1.State.Code == "", "MA", v1.State.Code),
    v1.State.Code = as.numeric(v1.State.Code == "MA"),
    V1.Hit.and.Run = as.numeric(V1.Hit.and.Run == "Y"),
    V1.Action.Prior.to.CrashNA = as.numeric(V1.Action.Prior.to.Crash == ""),
    V1.Action.Prior.to.Crash = ifelse( V1.Action.Prior.to.Crash == "", "TRAVELING STRAIGHT AHEAD", 
                                       V1.Action.Prior.to.Crash),
    V1.Action.Prior.to.Crash = ifelse(V1.Action.Prior.to.Crash %in% 
                                     c("CHANGING LANES",
                                       "LEAVING TRAFFIC LANE",
                                       "MAKING U-TURN ",
                                       "OTHER",
                                       "OVERTAKING / PASSING",
                                       "UNKNOWN"
                                       
                                     ),
                                   "OTHER",
                                   V1.Action.Prior.to.Crash
    ),
    V1.Occupant.Count = ifelse(V1.Occupant.Count == 0, "0",
                               ifelse(
                                 V1.Occupant.Count == 1, "1",
                                 ifelse(
                                   V1.Occupant.Count == 2, "2",
                                   "more_than2"
                                 )
                               )
                               ),
    V1.Most.Damaged.AreaNA = as.numeric(V1.Most.Damaged.Area == ""),
    V1.Most.Damaged.Area = ifelse(V1.Most.Damaged.Area == "", "RIGHT FRONT", V1.Most.Damaged.Area),
    V1.Most.Damaged.Area = ifelse(
      V1.Most.Damaged.Area %in% c("NONE", "OTHER", "TOP AND WINDOWS", "TOTAL / ALL AREAS",
                                  "UNDERCARRIAGE", "UNKNOWN"),
      "OTHER", V1.Most.Damaged.Area
    ),
    V1.Travel.Direction = as.numeric(V1.Travel.Direction %in% c("", "NOT REPORTED", "UNKNOWN")),
    V1.Travel.Direction = ifelse(
      V1.Travel.Direction %in% c("", "NOT REPORTED", "UNKNOWN"), "OTHER", V1.Travel.Direction 
    ),
    
    V2.State.CodeNA = as.numeric(V2.State.Code == ""),
    V2.State.Code = ifelse(V2.State.Code == "", "MA", V2.State.Code),
    V2.State.Code = as.numeric(V2.State.Code == "MA"),
    V2.Hit.and.Run = as.numeric(V2.Hit.and.Run == "Y"),
    V2.Action.Prior.to.CrashNA = as.numeric(V2.Action.Prior.to.Crash == ""),
    V2.Action.Prior.to.Crash = ifelse( V2.Action.Prior.to.Crash == "", "TRAVELING STRAIGHT AHEAD", 
                                       V2.Action.Prior.to.Crash),
    V2.Action.Prior.to.Crash = ifelse(V2.Action.Prior.to.Crash %in% 
                                        c("CHANGING LANES",
                                          "LEAVING TRAFFIC LANE",
                                          "MAKING U-TURN ",
                                          "OTHER",
                                          "OVERTAKING / PASSING"
                                        ),
                                      "OTHER",
                                      V2.Action.Prior.to.Crash
    ),
    # V2.Occupant.Count - transform from numeric to categorical
    V2.Occupant.Count = ifelse(V2.Occupant.Count %in% c(1, 99, 2015), "1",
                               ifelse(
                                 V2.Occupant.Count == 0, "0",
                                 ifelse(
                                   V2.Occupant.Count == 2, "2",
                                   "more_than2"
                                 )
                               )
    ),
    V2.Most.Damaged.AreaNA = as.numeric(V2.Most.Damaged.Area == ""),
    V2.Most.Damaged.Area = ifelse(V2.Most.Damaged.Area == "", "RIGHT FRONT", V2.Most.Damaged.Area),
    V2.Most.Damaged.Area = ifelse(
      V2.Most.Damaged.Area %in% c("NONE", "OTHER", "TOP AND WINDOWS", "TOTAL / ALL AREAS",
                                  "UNDERCARRIAGE", "UNKNOWN"),
      "OTHER", V2.Most.Damaged.Area
    ),
    V2.Travel.Direction = as.numeric(V2.Travel.Direction %in% c("", "NOT REPORTED", "UNKNOWN")),
    V2.Travel.Direction = ifelse(
      V2.Travel.Direction %in% c("", "NOT REPORTED", "UNKNOWN"), "OTHER", V2.Travel.Direction 
    ),
    
    P1.Role = ifelse(
      P1.Role %in% c("NON-MOTORIST", "OCCUPANT/PASSENGER", "WITNESS"), "OTHER",
      ifelse(
        P1.Role %in% c("OPERATOR/DRIVER", ""), "OPERATOR/DRIVER",
        P1.Role
      )
    ),
    P1.Drivers.Lic.StateNA = as.numeric(P1.Drivers.Lic.State == ""),
    P1.Drivers.Lic.State = ifelse(P1.Drivers.Lic.State == "", "MASSACHUSETTS",
                                  P1.Drivers.Lic.State),
    P1.Drivers.Lic.State = as.numeric(P1.Drivers.Lic.State == "MASSACHUSETTS"),
    
    P1.Drivers.Lic.Class.1NA = as.numeric(P1.Drivers.Lic.Class.1 == ""),
    P1.Drivers.Lic.Class.1 = ifelse(
      P1.Drivers.Lic.Class.1 %in% c("", "D, CLASS D VEHICLES"), "D, CLASS D VEHICLES",
      ifelse(
        P1.Drivers.Lic.Class.1 == "UNKNOWN", "UNKNOWN", "OTHER"
      )
    ),
    P1.Veh.Owner = as.numeric(P1.Veh.Owner == "Y"),
    P2.RoleNA = as.numeric(P2.Role == ""),
    P2.Role = ifelse(
      P2.Role %in% c("NON-MOTORIST", "WITNESS"), "OTHER",
      ifelse(
        P2.Role %in% c("", "OPERATOR/DRIVER"), "OPERATOR/DRIVER",
        P2.Role
      )
    ),
    P2.Drivers.Lic.StateNA = as.numeric(P2.Drivers.Lic.State == ""),
    P2.Drivers.Lic.State = ifelse(P2.Drivers.Lic.State == "", "MASSACHUSETTS", 
                                  P2.Drivers.Lic.State),
    P2.Drivers.Lic.State = as.numeric(P2.Drivers.Lic.State == "MASSACHUSETTS"),
    
    P2.Drivers.Lic.Class.1NA = as.numeric(P2.Drivers.Lic.Class.1 == ""),
    P2.Drivers.Lic.Class.1 = ifelse(
      P2.Drivers.Lic.Class.1 %in% c("", "D, CLASS D VEHICLES"), "D, CLASS D VEHICLES",
      ifelse(
        P2.Drivers.Lic.Class.1 == "UNKNOWN", "UNKNOWN", "OTHER"
      )
    ),
    P2.Veh.Owner = as.numeric(P2.Veh.Owner == "Y")
  ) %>% 
  # add variables based on Date.Time
  mutate(
    month = month(mdy_hms(Date.Time)),
    day = day(mdy_hms(Date.Time)),
    year = year(mdy_hms(Date.Time)),
    hour = hour(mdy_hms(Date.Time))
  ) %>% 
  select(!Date.Time) %>% 
  select(year, month, day, hour, everything()) 
  
# next step: delete any NA dummies for which there are too few rows switched on 
NAcols_to_remove <- df_sub %>% 
  select(names(df_sub)[43:67] ) %>% 
  summarize(across(everything(), mean)) %>% 
  pivot_longer(everything(), values_to = "mean", names_to = "variable") %>% 
  filter(mean <  60/nrow(df_sub)) %>% 
  pull(variable)

df_sub <- df_sub %>% select(!(all_of(NAcols_to_remove)))

# for predicting person 1's gender: further restrict df_sub's attention to P1.Role==driver and thats it
# for predicting person 1's gender: further restrict df_sub's attention to P2.Role==driver and thats it

df_sub1 <- df_sub %>% 
  filter(P1.Role == "OPERATOR/DRIVER") %>% 
  select(!P1.Role)

train1 <- df_sub1 %>% filter(year %in% 2015:2022) 
test1 <- df_sub1 %>% filter(year %in% 2023:2024) 


df_sub2 <- df_sub %>% 
  filter(P2.Role == "OPERATOR/DRIVER") %>% 
  select(!P2.Role)

train2 <- df_sub2 %>% filter(year %in% 2015:2022) 
test2 <- df_sub2 %>% filter(year %in% 2023:2024) 

# self-note: outcomes are not terribly imbalanced
table(df_sub1$P1.Sex)/nrow(df_sub1)
table(df_sub2$P2.Sex)/nrow(df_sub2)

table(train1$P1.Sex)/nrow(train1)
table(test1$P1.Sex)/nrow(test1)

table(train2$P2.Sex)/nrow(train2)
table(test2$P2.Sex)/nrow(test2)

# export train and test sets to csv 
write.csv(train1, "gender_car_crashes_train1.csv")
write.csv(test1, "gender_car_crashes_test1.csv")

write.csv(train2, "gender_car_crashes_train2.csv")
write.csv(test2, "gender_car_crashes_test2.csv")
