# https://data-flair.training/blogs/chi-square-test-in-r/

# load libraries
library(dplyr)
library(lubridate)
library(readr)
library(data.table)
library(ggplot2)
library(writexl)

# list all files with English Premier League Results
all_files = list.files("./Input Data/",full.names = T)

# Read and bind all files in one data frame
EPL_Full_Stats = do.call(bind_rows,lapply(all_files, read.csv))

# Wrangle the data
EPL_Full_Stats_Wrangled = EPL_Full_Stats %>%
  filter(Div != "") %>%
  select(Date, FTR, HomeTeam, AwayTeam, FTHG, FTAG) %>%
  mutate(DateFormatted = dmy(Date)
         , DateFormatted2 = as.Date(ifelse(is.na(DateFormatted), mdy(Date),DateFormatted), origin = "1970-01-01")
         , DateFinal = coalesce(DateFormatted,DateFormatted2)
         , Year = format(DateFinal, "%Y")
         , Covid = ifelse(DateFinal >= as.Date("2020-06-17"),"After_Covid","Before_Covid")
         , FTR_Agg = ifelse(FTR == "H" | FTR == "D","Home Team Win or Draw","Away Team Win")
         , FTR_All = case_when(FTR == "H" ~ "Home"
                               , FTR == "A" ~ "Away"
                               , FTR == "D" ~ "Draw"
                               )
         , Audience = ifelse(DateFinal >= as.Date("2020-06-17"),"After COVID - Without Audience","Before COVID - With Audience")
         ) %>%
  select(Date = DateFinal,Covid,Audience, FTR = FTR_Agg, FTR_All, HomeTeam, AwayTeam, FTHG, FTAG, Year)


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








