# https://data-flair.training/blogs/chi-square-test-in-r/

# load libraries
library(dplyr)
library(lubridate)
library(readr)
library(data.table)
library(ggplot2)
library(writexl)

options(scipen = 999)

# list all files with English Premier League Results
all_files = list.files("./Input Data/",full.names = T)

# Read and bind all files in one data frame
EPL_Full_Stats = do.call(bind_rows,lapply(all_files, read.csv))

# Wrangle the data
EPL_Full_Stats_Wrangled = EPL_Full_Stats %>%
  filter(Div != "") %>%
  select(Date, FTR, HomeTeam, AwayTeam, FTHG, FTAG, B365H, B365D, B365A) %>%
  mutate(DateFormatted = dmy(Date)
         , DateFormatted2 = as.Date(ifelse(is.na(DateFormatted), mdy(Date),DateFormatted), origin = "1970-01-01")
         , DateFinal = coalesce(DateFormatted,DateFormatted2)
         , Year = format(DateFinal, "%Y")
         , DateFinal = as.Date(DateFinal)
         , Covid = ifelse(DateFinal >= as.Date("2020-06-17") 
                          #| DateFinal < as.Date("2021-05-17")
                          ,"After_Covid","Before_Covid")
         , Covid = ifelse(DateFinal >= as.Date("2021-05-17") 
                          #| DateFinal < as.Date("2021-05-17")
                          ,"After_Covid_Audience",Covid)
         , FTR_Agg = ifelse(FTR == "H" | FTR == "D","Home Team Win or Draw","Away Team Win")
         , FTR_All = case_when(FTR == "H" ~ "Home"
                               , FTR == "A" ~ "Away"
                               , FTR == "D" ~ "Draw"
                               )
         , Audience = ifelse(DateFinal >= as.Date("2020-06-17") 
                             #| DateFinal < as.Date("2021-05-17")
                             ,"After COVID - Without Audience","Before COVID - With Audience")
         , Audience = ifelse(DateFinal >= as.Date("2021-05-17") 
                             #| DateFinal < as.Date("2021-05-17")
                             ,"After COVID - With Audience",Audience)
         , Month = as.numeric(format(DateFinal, "%m"))
         , Season = case_when(Month >= 8 ~ paste0(as.numeric(Year),"/",as.numeric(Year)+1)
                              , Month >= 1 ~ paste0(as.numeric(Year)-1,"/",as.numeric(Year))
                              )
         , Match = paste0(HomeTeam,"-",AwayTeam)
         ) %>%
  select(Date = DateFinal,Covid,Audience, FTR = FTR_Agg, FTR_All, HomeTeam, AwayTeam, FTHG, FTAG, Year
         , Month, Season, B365H, B365D, B365A, Match
         )


# Share of home losses before covid 
Total_Matches_Before_Covid = EPL_Full_Stats_Wrangled %>%
  filter(Covid == "Before_Covid") %>%
  nrow()

Total_Home_Losses_Before_Covid = EPL_Full_Stats_Wrangled %>%
  filter(Covid == "Before_Covid"
         & FTR == "Away Team Win"
         ) %>%
  nrow()

Share_Home_Losses_Before_Covid = Total_Home_Losses_Before_Covid/Total_Matches_Before_Covid

# Share of home losses after covid 
Total_Matches_After_Covid = EPL_Full_Stats_Wrangled %>%
  filter(Covid == "After_Covid") %>%
  nrow()

Total_Home_Losses_After_Covid = EPL_Full_Stats_Wrangled %>%
  filter(Covid == "After_Covid"
         & FTR == "Away Team Win"
  ) %>%
  nrow()

Share_Home_wins_After_Covid = Total_Home_Losses_After_Covid/Total_Matches_After_Covid

# Contingency Table
Covid_Impact_Home_Losses = data.frame(HomeLosses = c(Total_Home_Losses_Before_Covid, Total_Home_Losses_After_Covid)
                                    , TotalMatches = c(Total_Matches_Before_Covid,Total_Matches_After_Covid)
                                    , Covid = c("Before","After")
                                    ) %>%
  mutate(Share_Home_Losses = HomeLosses/TotalMatches)

EPL_Full_Stats_Wrangled_After_2010 = EPL_Full_Stats_Wrangled %>%
  filter(Year >= 2010)

# chisq.test
chisq_test = chisq.test(EPL_Full_Stats_Wrangled$Covid
           , EPL_Full_Stats_Wrangled$FTR
           , correct=FALSE 
           )

# chisq.test
chisq_test_after_2010 = chisq.test(EPL_Full_Stats_Wrangled_After_2010$Covid
                        , EPL_Full_Stats_Wrangled_After_2010$FTR
                        , correct=FALSE 
)

chisq_test_p_Value = chisq_test$p.value

chisq_test_df = data.frame(chisq_statistic = chisq_test$statistic
                           , chisq_pvalue = chisq_test$p.value
                           , method = chisq_test$method
                           )

# The null hypothesis (The two variables are independent) can't be rejected
# At a confidence level of 99% we can say that the two variables are dependent

# Export to excel 
write_xlsx(EPL_Full_Stats_Wrangled,"./Output Data/010_EPL_Full_Stats_Wrangled.xlsx")
write_xlsx(chisq_test_df,"./Output Data/010_chisq_test_df.xlsx")

### Only 2020 ###
# Share of home losses after covid 
All_Matches_After_Covid =  EPL_Full_Stats %>%
  filter(Div != "") %>%
  select(Date, FTR, HomeTeam, AwayTeam, FTHG, FTAG , B365H, B365D, B365A) %>%
  mutate(DateFormatted = dmy(Date)
         , DateFormatted2 = as.Date(ifelse(is.na(DateFormatted), mdy(Date),DateFormatted), origin = "1970-01-01")
         , DateFinal = coalesce(DateFormatted,DateFormatted2)
         , Year = format(DateFinal, "%Y")
         , Covid = ifelse(DateFinal >= as.Date("2020-06-17"),"After_Covid","Before_Covid")
         , Covid = ifelse(DateFinal >= as.Date("2021-05-17"),"After_Covid_Audience",Covid)
         , FTR_Agg = ifelse(FTR == "H" | FTR == "D","Home Team Win or Draw","Away Team Win")
         , FTR_All = case_when(FTR == "H" ~ "Home"
                               , FTR == "A" ~ "Away"
                               , FTR == "D" ~ "Draw"
         )
         , Audience = ifelse(DateFinal >= as.Date("2020-06-17"),"After COVID - Without Audience","Before COVID - With Audience")
         , Audience = ifelse(DateFinal >= as.Date("2021-05-17"),"After COVID - With Audience",Audience)
         ) %>%
  select(Date = DateFinal,Covid,Audience, FTR, FTR_All, HomeTeam, AwayTeam, FTHG, FTAG, Year
         , B365H, B365D, B365A
         ) %>%
  filter(Covid == "After_Covid") %>%
  filter(B365A > 1.5) 

counter = rep(seq(1:10),41)
counter[411] = 1
counter[412] = 2
counter[413] = 3
counter[414] = 4
counter[415] = 5

All_Matches_After_Covid$Counter = counter

All_Matches_After_Covid_Wrangled = All_Matches_After_Covid %>%
  mutate(Away_Win = ifelse(FTR == "A", B365A,0)) 


write_xlsx(All_Matches_After_Covid_Wrangled,"./Output Data/010_All_Matches_After_Covid_Wrangled.xlsx")

All_Matches_After_Covid_No_Audience =  EPL_Full_Stats %>%
  filter(Div != "") %>%
  select(Date, FTR, HomeTeam, AwayTeam, FTHG, FTAG , B365H, B365D, B365A) %>%
  mutate(DateFormatted = dmy(Date)
         , DateFormatted2 = as.Date(ifelse(is.na(DateFormatted), mdy(Date),DateFormatted), origin = "1970-01-01")
         , DateFinal = coalesce(DateFormatted,DateFormatted2)
         , Year = format(DateFinal, "%Y")
         , Covid = ifelse(DateFinal >= as.Date("2020-06-17"),"After_Covid","Before_Covid")
         , Covid = ifelse(DateFinal >= as.Date("2021-05-17"),"After_Covid_Audience",Covid)
         , FTR_Agg = ifelse(FTR == "H" | FTR == "D","Home Team Win or Draw","Away Team Win")
         , FTR_All = case_when(FTR == "H" ~ "Home"
                               , FTR == "A" ~ "Away"
                               , FTR == "D" ~ "Draw"
         )
         , Audience = ifelse(DateFinal >= as.Date("2020-06-17"),"After COVID - Without Audience","Before COVID - With Audience")
         , Audience = ifelse(DateFinal >= as.Date("2021-05-17"),"After COVID - With Audience",Audience)
  ) %>%
  select(Date = DateFinal,Covid,Audience, FTR, FTR_All, HomeTeam, AwayTeam, FTHG, FTAG, Year
         , B365H, B365D, B365A
  ) %>%
  filter(Covid == "After_Covid")

counter = rep(seq(1:10),45)
counter[451] = 1
counter[452] = 2

All_Matches_After_Covid_No_Audience$Counter = counter

All_Matches_After_Covid_No_Audience_Wrangled = All_Matches_After_Covid_No_Audience %>%
  mutate(Away_Win = ifelse(FTR == "A", B365A,0)) 

write_xlsx(All_Matches_After_Covid_No_Audience_Wrangled,"./Output Data/010_All_Matches_After_Covid_No_Audience_Wrangled.xlsx")












