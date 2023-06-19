
#this creates a new data frame for manipulating
riders_tableau <- riders

#this calculates ride length as minutes
riders_tableau$ride_length <- difftime(riders$ended_at, riders$started_at, units = "mins")

#this changes member_casual column name
riders_tableau <- riders %>% 
  rename("membership" = "member_casual")


#this creates a column for hour of day
riders_tableau$hour <- hour(riders_tableau$started_at)


#this creates column for quarters of the year
riders_tableau <- riders_tableau %>% mutate(quarter = 
                                                    case_when(month == "01" ~ "1Q",
                                                              month == "02" ~ "1Q",
                                                              month == "03" ~ "1Q",
                                                              month == "04" ~ "2Q",
                                                              month == "05" ~ "2Q",
                                                              month == "06" ~ "2Q",
                                                              month == "07" ~ "3Q",
                                                              month == "08" ~ "3Q",
                                                              month == "09" ~ "3Q",
                                                              month == "10" ~ "4Q",
                                                              month == "11" ~ "4Q",
                                                              month == "12" ~ "4Q"))

#this creates column for time of day
riders_tableau <- riders_tableau %>% mutate(time_of_day = 
                                                    case_when(hour == "0" ~ "Night",
                                                              hour == "1" ~ "Night",
                                                              hour == "2" ~ "Night",
                                                              hour == "3" ~ "Night",
                                                              hour == "4" ~ "Night",
                                                              hour == "5" ~ "Night",
                                                              hour == "6" ~ "Morning",
                                                              hour == "7" ~ "Morning",
                                                              hour == "8" ~ "Morning",
                                                              hour == "9" ~ "Morning",
                                                              hour == "10" ~ "Morning",
                                                              hour == "11" ~ "Morning",
                                                              hour == "12" ~ "Afternoon",
                                                              hour == "13" ~ "Afternoon",
                                                              hour == "14" ~ "Afternoon",
                                                              hour == "15" ~ "Afternoon",
                                                              hour == "16" ~ "Afternoon",
                                                              hour == "17" ~ "Afternoon",
                                                              hour == "18" ~ "Evening",
                                                              hour == "19" ~ "Evening",
                                                              hour == "20" ~ "Evening",
                                                              hour == "21" ~ "Evening",
                                                              hour == "22" ~ "Evening",
                                                              hour == "23" ~ "Evening"))

#this changes month column content from number to full month name
riders_tableau <- riders_tableau %>% mutate(month = 
                                                    case_when(month == "01" ~ "January",
                                                              month == "02" ~ "February",
                                                              month == "03" ~ "March",
                                                              month == "04" ~ "April",
                                                              month == "05" ~ "May",
                                                              month == "06" ~ "June",
                                                              month == "07" ~ "July",
                                                              month == "08" ~ "August",
                                                              month == "09" ~ "September",
                                                              month == "10" ~ "October",
                                                              month == "11" ~ "November",
                                                              month == "12" ~ "December"))

#this views the final data
View(riders_tableau)

#this exports data frame as a .csv file
write.csv(riders_tableau, "riders_tableau.csv")
