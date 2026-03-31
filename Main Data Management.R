# 数据处理相关
library(tidyverse)
library(data.table)
# scale()
library(dplyr)
library(zoo)
library(FedData)
library(sf)
library(dplyr)
library(mapview)
library(terra)
library(lubridate)


coarsedata <- readRDS("dat_iSSA_coarse.RDS")
coarsedata <- coarsedata %>%
  mutate(
    fire_60perc5000_start=lag(fire_60perc5000_end),
    fire_2perc5000_start=lag(fire_2perc5000_end),
  )

#originaldata <- subset(data, case_==TRUE)
rawdata <- subset(coarsedata, case_==TRUE)
#rm(data)
rm(coarsedata)

## subset the yukon data
ytdata <- rawdata[rawdata$prov=="yt",]
locations <- ytdata[,c("id", "area", "x1_","y1_","t1_")]
ytdata$month <- month(ytdata$t1_)
ytdata$year <- year(ytdata$t1_)
ytdata <- ytdata %>%
  mutate(
    day_of_year = yday(t1_),
    # Use case_when for clear, sequential logic to assign seasons
    Seasons = case_when(
      # Winter: Nov 1 (day 305) to Apr 30 (day 120 of the next year)
      day_of_year >= 305 | day_of_year <= 120 ~ "1_Winter",
      # Shoulder 1 (Spring movement): May 1 (day 121) to June 30 (day 181)
      day_of_year >= 121 & day_of_year <= 181 ~ "2_Spring",
      # Summer: July 1 (day 182) to Sept 30 (day 273)
      day_of_year >= 182 & day_of_year <= 273 ~ "3_Summer",
      # Shoulder 2 (Fall movement): Oct 1 (day 274) to Oct 31 (day 304)
      day_of_year >= 274 & day_of_year <= 304 ~ "4_Fall",
      # A fallback for safety, though it shouldn't be needed
      TRUE ~ "Unknown" 
    ),
    BioYear = ifelse(month < 5, year - 1, year)
  )


ytdata <- ytdata %>%
  group_by(id) %>%
  mutate(pa_mining_5000=case_when(
    pa_placer_5000_start == 1 | pa_quartz_5000_start == 1 | pa_othermines_5000_start == 1 ~ 1,
    TRUE ~ 0
  )) %>%
  ungroup()
ytdata$id <- as.character(ytdata$id)
ytdata$Date <- as.Date(ytdata$t1_)

# see details in download weather.R, for points_df_clean
# locations$Date <- as.Date(locations$t1_)
# unique_coords <- locations[,c("id", "x1_", "y1_","Date")]
# unique_coords <- subset(unique_coords, Date <= "2024-12-31")
# points_sf <- st_as_sf(unique_coords, coords = c("x1_", "y1_"), crs = 3154)
# hull_sf <- st_convex_hull(st_union(points_sf))
# study_area_buffered <- st_buffer(hull_sf, 5000)
# study_area84 <- st_transform(study_area_buffered, 4326)

final_dataset <- left_join(ytdata, 
                           points_df_clean %>% select(id, Date,x_sf,y_sf, daily_swe), 
                           by = c("id" = "id", 
                                  "Date" = "Date",
                                  "x1_" = "x_sf",  
                                  "y1_" = "y_sf"))

## combine and calculate the variables
MergedData <- final_dataset %>%
  group_by(id) %>%
  mutate(
    last_known_date=max(t1_),
    first_known_date=min(t1_)
  ) %>%
  group_by(id,BioYear,month) %>%
  summarise(year=first(year),
            area=first(area),
            Seasons=first(Seasons),
            
            pa_mining=as.factor(if_else(any(pa_mining_5000==1, na.rm = TRUE), 1, 0)),
            proportion_mining_5000=sum(pa_mining_5000),
            pa_harv_5000=as.factor(if_else(any(harv_pa_5000_start==1, na.rm = TRUE), 1, 0)),
            proportion_harv_5000=sum(harv_pa_5000_start),
            pa_anthro_5000=as.factor(if_else(any(pa_anthrodisturbance_5000_start==1, na.rm = TRUE), 1, 0)),
            proportion_anthro_5000=sum(pa_anthrodisturbance_5000_start),
            fire_60_5000=mean(fire_60perc5000_end),
            dens_road_5000=mean(dens_paved_5000_start+dens_unpaved_5000_start),
            dens_lfother_5000=mean(dens_lfother_5000_start),
            prop_veg_5000=mean(prop_veg_5000_start),
            prop_needleleaf_5000=mean(prop_needleleaf_5000_start),
            last_known_date=first(last_known_date),
            first_known_date=first(first_known_date),
            day_swe=mean(daily_swe),
            #p90_swe = quantile(daily_swe, probs = 0.9, na.rm = TRUE),
            max_swe = max(daily_swe),
            total_fixes=n(),
            .groups = "drop")


## attach the death information to ytdata and calculate the days since the start of the season
# Define the season start months
season_starts <- tibble(
  Seasons = c("2_Spring", "3_Summer", "4_Fall", "1_Winter"),
  start_month = c(5, 7, 10, 11)
)
# Create a comprehensive table of all season start dates
season_dates <- expand.grid(year = (min(ytdata$year)-1):max(ytdata$year),
                            Seasons = season_starts$Seasons) %>%
  left_join(season_starts, by = "Seasons") %>%
  mutate(season_start_date = make_date(year, start_month, 1)
  ) %>%
  arrange(year, start_month) %>%
  mutate(
    next_season_start = lead(season_start_date),
    # For the very last season, the end date is the end of the year
    # use coalesce() to fill in the NA from lead() with Dec 31st
    season_end_date = coalesce(
      next_season_start, 
      make_date(year, 12, 31) + days(1)
    ),
    season_end_date = season_end_date - days(1),
    # Calculate the duration in days
    season_length_days = as.numeric(
      difftime(season_end_date, season_start_date, units = "days")
    )
  ) %>%
  select(year, Seasons, season_start_date, season_end_date, season_length_days)
names(season_dates)[1] <- "BioYear"
# combine all the information to the df
# for deathfate, see bottom of this script
survival_data <- MergedData %>%
  left_join(season_dates, by=c("BioYear","Seasons")) %>%
  left_join(deathfate, by="id") %>%
  arrange(id, year, month) %>%
  group_by(id) %>%
  mutate(
    last_year=last(year),
    last_season=last(Seasons),
    first_year=first(year),
    first_season=first(Seasons),
    is_lastseason=if_else(Seasons == last_season & BioYear == last_year, 1, 0),
    is_firstseason=if_else(Seasons == first_season & year == first_year, 1, 0),
    last_month=last(month),
    first_month=first(month),
    is_lastmonth=if_else(month == last_month &year == last_year, 1, 0),
    is_firstmonth=if_else(month == first_month & year == first_year, 1, 0),
    Event=case_when(
      is_lastseason==FALSE ~ 0,
      is_lastseason==TRUE&ending==1 ~ 1,
      is.na(ending) ~ 0,
      TRUE ~ 0
    ),
    Death=case_when(
      is_lastseason==FALSE ~ 0,
      is_lastseason==TRUE&ending==1&row_number() == n() ~ 1,
      is.na(ending) ~ 0,
      TRUE ~ 0
    ),
    EventTime=case_when(
      #ceiling(as.numeric(difftime(last_known_date, first_known_date, units = "days")))
      #is_lastseason==FALSE ~ season_length_days,
      is_firstmonth==TRUE ~ ceiling(as.numeric(difftime(ceiling_date(make_date(year, month, 1), unit = "month") - days(1), first_known_date, units = "days"))),
      is_lastmonth==FALSE ~ days_in_month(make_date(year,month,1)),
      #ceiling是干嘛的
      is_lastmonth==TRUE & Event==1 ~
        #ceiling(as.numeric(difftime(last_known_date, season_start_date, units = "days"))),
        ceiling(as.numeric(difftime(last_known_date, make_date(year,month,1), units = "days"))),
      is_lastmonth==TRUE&Event==0 ~ 99999,
      TRUE ~ 9999
    ),
    DayStart=case_when(
      is_firstmonth==TRUE & is_lastmonth==FALSE ~ 0,
      is_firstmonth==FALSE & is_lastmonth==FALSE ~ ceiling(as.numeric(difftime(make_date(year, month, 1), first_known_date, units = "days"))) - 1,
      is_firstmonth==TRUE & is_lastmonth==TRUE ~ 0,
      is_firstmonth==FALSE & is_lastmonth==TRUE ~ ceiling(as.numeric(difftime(make_date(year, month, 1), first_known_date, units = "days"))) - 1,
      TRUE ~ 9999
    ),
    DayStop=case_when(
      is_firstmonth==TRUE & is_lastmonth==FALSE ~ ceiling(as.numeric(difftime(ceiling_date(make_date(year, month, 1), unit = "month") - days(1), first_known_date, units = "days"))),
      is_firstmonth==FALSE & is_lastmonth==FALSE ~ ceiling(as.numeric(difftime(ceiling_date(make_date(year, month, 1), unit = "month") - days(1), first_known_date, units = "days"))),
      is_firstmonth==TRUE & is_lastmonth==TRUE ~ ceiling(as.numeric(difftime(ceiling_date(make_date(year, month, 1), unit = "month") - days(1), first_known_date, units = "days"))),
      is_firstmonth==FALSE & is_lastmonth==TRUE ~ ceiling(as.numeric(difftime(last_known_date, first_known_date, units = "days"))),
      TRUE ~ 9999
    )
  ) %>%
  ungroup() %>%
  select(-last_year,-last_season, -last_month,-first_year,-first_season, -first_month)

## fix the gaps in dataset
get_season <- function(m){
  case_when(
    m %in% c(11, 12, 1, 2, 3, 4) ~ "1_Winter",
    m %in% c(5,6) ~ "2_Spring", # Adjust based on your actual definition
    m %in% c(7, 8, 9) ~ "3_Summer",
    m %in% c(10) ~ "4_Fall", # Adjust based on your actual definition
    TRUE ~ NA_character_
  )
}


survival_data <- survival_data %>%
  # 1. Create a proper Date column to generate the sequence
  mutate(date_temp = make_date(year, month, 1)) %>%
  
  # 2. Group by ID and fill missing months between min and max date
  group_by(id) %>%
  complete(date_temp = seq.Date(min(date_temp), max(date_temp), by = "month")) %>%
  ungroup() %>%
  
  # 3. Fill in the columns for the newly created rows
  mutate(
    year = year(date_temp),
    month = month(date_temp),
    
    # If Event is NA (new row), assume 0 (Alive)
    Event = replace_na(Event, 0),
    Death = replace_na(Death, 0),
    is_lastseason = replace_na(is_lastseason, 0),
    
    # Recalculate Seasons/BioYear for new rows if they are NA
    Seasons = if_else(is.na(Seasons), get_season(month), Seasons),
    
    # BioYear Logic: If month >= 6, BioYear is current year, else previous year
    # (Adjust this logic to match your specific project rules)
    BioYear = if_else(month >= 5, year, year - 1)
  ) %>%
  select(-date_temp) # Remove temp column


season_order <- as.factor(c("2_Spring", "3_Summer","4_Fall", "1_Winter"))
survival_data$Seasons <- factor(survival_data$Seasons, levels=season_order)
survival_data <- survival_data %>%
  arrange(id, year, month) %>%
  group_by(id) %>%
  mutate(
    proportion_mining_5000=proportion_mining_5000/total_fixes,
    #percent_harv_5000=percent_harv_5000,
    proportion_harv_5000=proportion_harv_5000/total_fixes, 
    proportion_anthro_5000=proportion_anthro_5000/total_fixes,
    next_Seasons = case_when(
      is_lastmonth == 1 ~ "No_next",
      TRUE ~ lead(Seasons)
    ),
    AreaBioYear = interaction(area,BioYear,drop=T)
  ) %>%
  ungroup() %>%
  select(-ending)

season_summary <- survival_data %>%
  group_by(id, BioYear, Seasons) %>%
  summarize(
    event_in_this_season = max(Event, na.rm = TRUE),
    .groups = "drop"
  )%>%
  # Step D: Look ahead to find the event status of the NEXT season
  group_by(id) %>%
  mutate(
    # This takes the event status of the NEXT block and puts it in the CURRENT block row
    next_season_event_status = lead(event_in_this_season, default = NA)
  ) %>%
  ungroup()
survival_data <- survival_data %>%
  left_join(season_summary, by = c("id", "BioYear","Seasons")) %>%
  rename(next_Event = next_season_event_status) %>%
  group_by(id, BioYear, Seasons) %>%
  mutate(
    next_Fate = case_when(
      row_number() == n() ~ next_Event,
      TRUE ~ 0
    )
  ) %>%
  select(-event_in_this_season)
survival_data <- survival_data %>%
  mutate(
    log_swe = log(day_swe+1)
  )
# report the mean and variance of different seasons
winter <- subset(survival_data, Seasons=="1_Winter")
spring <- subset(survival_data, Seasons=="2_Spring")
summer <- subset(survival_data, Seasons=="3_Summer")
fall <- subset(survival_data, Seasons=="4_Fall")

#mean & SD
appendix_scaling_swe <- data.frame(array(dim = c(4, 3)))
names(appendix_scaling_swe) <- c("Covariate", "mean", "sd")
appendix_scaling_swe[,"Covariate"] <- c("SWE(Spring)", "SWE(Summer)", "SWE(Fall)", "SWE(Winter)")
appendix_scaling_swe[,"mean"] <- c(mean(spring$log_swe, na.rm=T),mean(summer$log_swe, na.rm=T),
                                   mean(fall$log_swe, na.rm=T),mean(winter$log_swe, na.rm=T))
appendix_scaling_swe[,"sd"] <- c(sd(spring$log_swe, na.rm=T),sd(summer$log_swe, na.rm=T),
                                 sd(fall$log_swe, na.rm=T),sd(winter$log_swe, na.rm=T))
appendix_scaling_swe <- appendix_scaling_swe %>%
  mutate(
    mean = round(mean, 3), 
    sd = round(sd, 3)      
  )

winter$log_swe <- (winter$log_swe - mean(winter$log_swe, na.rm=T)) / sd(winter$log_swe, na.rm = TRUE)
spring$log_swe <- (spring$log_swe - mean(spring$log_swe, na.rm=T)) / sd(spring$log_swe, na.rm = TRUE)
summer$log_swe <- (summer$log_swe - mean(summer$log_swe, na.rm=T)) / sd(summer$log_swe, na.rm = TRUE)
fall$log_swe <- (fall$log_swe - mean(fall$log_swe, na.rm=T)) / sd(fall$log_swe, na.rm = TRUE)
#hist(na.omit(scale(winter$log_swe)))

survival_data <- rbind(winter,summer,spring,fall)


survival_data <- survival_data %>%
  arrange(id, year, month) %>%
  group_by(id) %>%
  mutate(
    Mining_2Month = rollmean(proportion_mining_5000, k = 2, fill = NA, align = "right"),
    Harv_2Month = rollmean(proportion_harv_5000, k = 2, fill = NA, align = "right"),
    Anthro_2Month = rollmean(proportion_anthro_5000, k = 2, fill = NA, align = "right"),
    Fire_2Month = rollmean(fire_60_5000, k = 2, fill = NA, align = "right"),
    Road_2Month = rollmean(dens_road_5000, k = 2, fill = NA, align = "right"),
    Lfother_2Month = rollmean(dens_lfother_5000, k = 2, fill = NA, align = "right"),
    Needleleaf_2Month = rollmean(prop_needleleaf_5000, k = 2, fill = NA, align = "right"),
    Veg_2Month = rollmean(prop_veg_5000, k = 2, fill = NA, align = "right"),
    SWE_2Month = rollmean(day_swe, k = 2, fill = NA, align = "right"),
    SWEmax_2Month = rollmax(max_swe, k = 2, fill = NA, align = "right"),
    SWElog = rollmean(log_swe, k = 2, fill = NA, align = "right"),

    Mining_6Month = rollmean(proportion_mining_5000, k = 6, fill = NA, align = "right"),
    Harv_6Month = rollmean(proportion_harv_5000, k = 6, fill = NA, align = "right"),
    Anthro_6Month = rollmean(proportion_anthro_5000, k = 6, fill = NA, align = "right"),
    Fire_6Month = rollmean(fire_60_5000, k = 6, fill = NA, align = "right"),
    Road_6Month = rollmean(dens_road_5000, k = 6, fill = NA, align = "right"),
    Lfother_6Month = rollmean(dens_lfother_5000, k = 6, fill = NA, align = "right"),
    Needleleaf_6Month = rollmean(prop_needleleaf_5000, k = 6, fill = NA, align = "right"),
    Veg_6Month = rollmean(prop_veg_5000, k = 6, fill = NA, align = "right"),
    SWE_6Month = rollmean(day_swe, k = 6, fill = NA, align = "right"),
    SWEmax_6Month = rollmax(max_swe, k = 6, fill = NA, align = "right")
  ) %>%
  ungroup()


survival_data <- subset(survival_data, EventTime!=99999)
# survival_data$AreaYear <- interaction(survival_data$area, survival_data$year,drop=T)


start_year <- min(survival_data$year)
survival_data <- survival_data %>%
  group_by(id, BioYear) %>%
  mutate(
    month_index = as.numeric(month),
    month_number = case_when(
      month_index < 5 ~ month_index-5+13,
      TRUE ~ month_index-5+1
    )
  ) %>%
  ungroup()
survival_data <- survival_data %>%
  # Sort by individual and time
  arrange(id, year) %>%
  mutate(
    tstart = month_number - 1,
    tstop = month_number
  ) %>%
  ungroup()

survival_data$next_Seasons <- na_if(survival_data$next_Seasons,"No_next")

nonScaled <- survival_data

vars_to_scale <- c(
  "proportion_mining_5000", "proportion_harv_5000", "proportion_anthro_5000",
  "fire_60_5000", "dens_road_5000", "dens_lfother_5000", "prop_veg_5000",
  "prop_needleleaf_5000", "Mining_2Month", "Harv_2Month", "Anthro_2Month",
  "Fire_2Month", "Road_2Month", "Lfother_2Month", "Needleleaf_2Month",
  "Veg_2Month", "SWEmax_2Month", "Mining_6Month", "Harv_6Month",
  "Anthro_6Month", "Fire_6Month", "Road_6Month", "Lfother_6Month",
  "Needleleaf_6Month", "Veg_6Month", "SWEmax_6Month"
)

appendix_scaling_table <- survival_data %>%
  summarise(across(all_of(vars_to_scale), 
                   list(mean = ~mean(.x, na.rm = TRUE), 
                        sd   = ~sd(.x, na.rm = TRUE)))) %>%
  
  pivot_longer(everything(), 
               names_to = c("Covariate", ".value"), 
               names_pattern = "(.*)_(mean|sd)") %>%
  mutate(
    mean = round(mean, 3), 
    sd = round(sd, 3)      
  )
appendix_scaling_table <- rbind(appendix_scaling_table,appendix_scaling_swe)
#write.csv(appendix_scaling_table, "Appendix_Table_Scaling_Parameters.csv", row.names = FALSE)
survival_data <- survival_data %>%
  mutate(across(all_of(vars_to_scale), 
                ~ (.x - mean(.x, na.rm = TRUE)) / sd(.x, na.rm = TRUE))) %>%
  ungroup()


survival_data <- survival_data %>%
  rename(
    CaribouID = id,
    SeasonSWElog =log_swe
  )

survival_data$next_Seasons <- as.factor(survival_data$next_Seasons)
survival_data$next_Seasons <- relevel(survival_data$next_Seasons, ref = "2_Spring")
survival_data$pa_mining <- as.factor(survival_data$pa_mining)
survival_data$pa_harv_5000 <- as.factor(survival_data$pa_harv_5000)
survival_data$BioYear <- as.factor(survival_data$BioYear)
survival_data_no2025 <- subset(survival_data, year != 2025)




## create a dataset of the fate of all indivduals from different dataset
## DO NOT CHANGE THIS PART
originaldata <- readRDS("originaldata.rds")
deathfate <- originaldata[originaldata$prov=="yt",] %>%
  group_by(id) %>%
  summarise(
    final_status=last(deployment_end_type)) %>%
  ungroup()
rm(originaldata)
# dead <- readxl::read_xlsx("./Mortality Tracker NEW_20240429.xlsx", sheet=2)
dead <- readxl::read_xlsx("./Mortality Tracker NEW_20240920.xlsx", sheet=2)
dead <- dead[,c("ANIM_ID","ANIMAL FATE/ COLLAR STATUS")]
names(dead) <- c("id","fate")
deathfate <- full_join(deathfate,dead, by="id")
deathfate <- deathfate %>%
  group_by(id) %>% # IMPORTANT: Perform the operation for each caribou ID
  mutate(
    ending = as.numeric(case_when(
      final_status == "dead" | fate == "dead" ~ 1,  
      TRUE ~ 0                                     
    ))
  ) %>%
  ungroup()
deathfate <- deathfate[,c(1,4)]
