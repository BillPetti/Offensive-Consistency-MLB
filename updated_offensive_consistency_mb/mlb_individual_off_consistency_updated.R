################################
# Individual Offensive Consistency in Baseball
# Updated code and method (incorporates negative values)
# Bill Petti
# May 2016
###############################

# load required packages
# dplyr for data manipulation
# baseballr for scraping some necessary data
# RMySQL for connecting to and querying databases
# reldist for calculating Gini coefficients
# relaimpo for calculating the relative importance of variables
# ggplot2 for graphs and plotting

require(dplyr)
require(baseballr)
require(RMySQL)
require(reldist)
require(relaimpo) # note that calling relaimpo will mask dplyr::select
require(ggplot2)

# load custom theme and color palette

source("https://raw.githubusercontent.com/BillPetti/R-Plotting-Resources/master/theme_bp_grey")

# We are going to be calculating daily weighted runs created for individual hitters and then calculating gini coefficients based on the daily distribution to classify a player's degree of offensive consistency. The formula for wRC is (((wOBA-League wOBA)/wOBA Scale)+(League R/PA))*PA. So, we need to create lookup tables for Leaguw wOBA, wOBA scale, and League R/PA for each year back to 1974. We can scrape this from FanGraphs

# Scrape FanGraphs Guts! table

fg_guts <- fg_guts()

# Now we need to pull individual player logs going back to 1974. Here I am pulling from the FanGraphs' writers database. 
# load connection values

fg_database_values <- c("/Users/williampetti/bpAuthsCons/fg_dbname.rda", "/Users/williampetti/bpAuthsCons/fg_username.rda", "/Users/williampetti/bpAuthsCons/fg_password.rda", "/Users/williampetti/bpAuthsCons/fg_host.rda")

lapply(fg_database_values, load, .GlobalEnv)

# connect to database

con <- dbConnect(RMySQL::MySQL(), dbname = fg_dbname, username = fg_username, password = fg_password, host = fg_host, port = 3306)

# query for daily records, excluding players that were primarily pitchers

daily <- dbGetQuery(con, "select w.*, s.ShortName as 'Team', s.PF, p.FirstName, p.LastName from wpa_daily_batting w left join season_team s on w.TeamId = s.TeamId left join player_info p on w.PlayerId = p.PlayerId where substr(w.GameDate, 1,4) = s.Season")

dbDisconnect(con)

# We have a lot of data manipulation to do, so lets grab a sample of the data first to make sure everything will work like hope since the actual data frame has over XX million records

Sample <- daily %>% sample_n(1000)

# Let's clean up the variables, as we don't need them all

Sample <- Sample[,-c(1, 44:52)]

# Create a column that indicates what season each record is from

Sample$season <- as.numeric(substr(Sample$GameDate, 1, 4))

# Create a column for the number of unintentional walks

Sample$uBB <- Sample$BB - Sample$IBB

# We need to match in the guts data so that we can accurate calculate daily wOBA and then wRC for each player

Sample_join <- left_join(Sample, fg_guts, by = "season")

# Calculate daily wOBA for each record

Sample_join$daily_woba <- round((((Sample_join$wBB * Sample_join$uBB) + (Sample_join$wHBP * Sample_join$HBP) + (Sample_join$w1B * Sample_join$`1B`) + (Sample_join$w2B * Sample_join$`2B`) + (Sample_join$w3B * Sample_join$`3B`) + (Sample_join$wHR * Sample_join$HR))/Sample_join$PA),3)

# now we have to calculate wRC for each daily record. Again, here is the formala for wRC: (((wOBA-League wOBA)/wOBA Scale)+(League R/PA))*PA

Sample_join$wRC <- round(((((Sample_join$daily_woba - Sample_join$lg_woba) / Sample_join$woba_scale) + (Sample_join$lg_r_pa)) * Sample_join$PA),3)

# generate total wRC per player, per year on the sample data

wrc_player_yr <- Sample_join %>% group_by(PlayerId, season) %>% summarise(count = n(), wRC = sum(wRC))

## if everything checks out, apply the prior code to the daily data frame
rm(Sample, Sample_join, wrc_player_yr)

# Let's clean up the variables, as we don't need them all

daily2 <- daily[,-c(1, 44:52)]

# Create a column that indicates what season each record is from

daily2$season <- as.numeric(substr(daily2$GameDate, 1, 4))

# Create a column for the number of unintentional walks

daily2$uBB <- daily2$BB - daily2$IBB

# We need to match in the guts data so that we can accurate calculate daily wOBA and then wRC for each player

daily2_join <- left_join(daily2, fg_guts, by = "season")

# Calculate daily wOBA for each record

daily2_join$daily_woba <- round((((daily2_join$wBB * daily2_join$uBB) + (daily2_join$wHBP * daily2_join$HBP) + (daily2_join$w1B * daily2_join$`1B`) + (daily2_join$w2B * daily2_join$`2B`) + (daily2_join$w3B * daily2_join$`3B`) + (daily2_join$wHR * daily2_join$HR))/daily2_join$PA),3)

# now we have to calculate wRC for each daily record. Again, here is the formala for wRC: (((wOBA-League wOBA)/wOBA Scale)+(League R/PA))*PA

daily2_join$wRC <- round(((((daily2_join$daily_woba - daily2_join$lg_woba) / daily2_join$woba_scale) + (daily2_join$lg_r_pa)) * daily2_join$PA),3)

# Concatenate the player name, and move it to the first column in the data set using dplyr::select

daily2_join$Name <- paste(daily2_join$FirstName, daily2_join$LastName, sep = " ")
daily2_join <- daily2_join %>% dplyr::select(Name, everything())

# generate total wRC per player, per year on the sample data

wrc_player_yr <- daily2_join %>% dplyr::select(PlayerId, Name, season, G, PA, wRC) %>% na.omit() %>% group_by(PlayerId, Name, season) %>% summarise(Games = sum(G), PA = sum(PA), wRC = sum(wRC))

# generate total wRC per team, per year

wrc_team_yr <- daily2_join %>% dplyr::select(TeamId, Team, season, wRC) %>% na.omit() %>% group_by(TeamId, Team, season) %>% summarise(count = n(), wRC = sum(wRC))

### Calculate Gini coefficients by player, by year to serve as consistency scores (wRC_CON), where lower scores indicate more even distribution of wRC on a game by game basis. Merge player names into consistency scores from our daily2_join data set

# create a function that better handles negative values for Gini coefficients

Gini_neg <- function(Y) {
  
  Y <- sort(Y)
  
  N <- length(Y)
  
  u_Y <- mean(Y)
  
  top <- 2/N^2 * sum(seq(1,N)*Y) - (1/N)*sum(Y) - (1/N^2) * sum(Y)
  
  min_T <- function(x) {
    
    return(min(0,x))
    
  }
  
  max_T <- function(x) {
    
    return(max(0,x))
    
  }
  
  T_all <- sum(Y)
  
  T_min <- abs(sum(sapply(Y, FUN = min_T)))
  
  T_max <- sum(sapply(Y, FUN = max_T))
  
  u_P <- (N-1)/N^2*(T_max + T_min)
  
  return(top/u_P)
  
}

wrc_player_yr <- daily2_join %>% dplyr::select(PlayerId, season, wRC) %>% 
  na.omit() %>% 
  aggregate(wRC ~ PlayerId + season, data = ., FUN = "Gini_neg") %>%
  left_join(wrc_player_yr, ., by = c("PlayerId" = "PlayerId", "season" = "season"))

names(wrc_player_yr)[7] <- "wRC_VOL"
names(wrc_player_yr)[6] <- "wRC"

wrc_player_yr$wRC_VOL <- round(wrc_player_yr$wRC_VOL, 3)

# group each player season by PAs per game to compare how consistency varies by playing time

wrc_player_yr$PA_G <- round((wrc_player_yr$PA / wrc_player_yr$G),1) 

wrc_player_yr$PA_grp <- ifelse(wrc_player_yr$PA_G <=1,1,ifelse(wrc_player_yr$PA_G >1 & wrc_player_yr$PA_G <= 1.5, 1.5, ifelse(wrc_player_yr$PA_G >1.5 & wrc_player_yr$PA_G <=2,2, ifelse(wrc_player_yr$PA_G > 2 & wrc_player_yr$PA_G <= 2.5, 2.5, ifelse(wrc_player_yr$PA_G > 2.5 & wrc_player_yr$PA_G <= 3, 3, ifelse(wrc_player_yr$PA_G > 3 & wrc_player_yr$PA_G <= 3.5, 3.5, ifelse(wrc_player_yr$PA_G > 3.5 & wrc_player_yr$PA_G <= 4, 4, ifelse(wrc_player_yr$PA_G > 4 & wrc_player_yr$PA_G <= 4.5, 4.5, 5))))))))

# now group by PA_grp and run correlations between PA_G and wRC_CON

wrc_player_yr %>% filter(PA_grp > 1) %>% group_by(PA_grp) %>% summarise(N = n(), Correlation = cor(PA_G, wRC_VOL, use = "pairwise.complete.obs"))

# correlate for players with a min number of 100 games played

wrc_player_yr %>% filter(PA_grp > 1, Games > 99) %>% group_by(PA_grp) %>% summarise(N = n(), Correlation = cor(PA_G, wRC_VOL, use = "pairwise.complete.obs"))

# correlate w/ wRC

wrc_player_yr %>% filter(PA_grp > 2, Games > 99) %>% group_by(PA_grp) %>% summarise(N = n(), Correlation = cor(wRC, wRC_VOL, use = "pairwise.complete.obs"))

# create a data set with just players with >= 100 games and >=3 PA per Game

wrc_player_yr_m100 <- filter(wrc_player_yr, PA_grp > 2, Games > 99)

# check to see whether we need to adjust scores for general league-wide shifts in wRC_VOL

VOL_season <- wrc_player_yr %>% 
  group_by(season) %>%
  summarise(N = n(), mean = round(mean(wRC_VOL, na.rm = TRUE),3),  sd = round(sd(wRC_VOL, na.rm = TRUE),3))

ggplot(wrc_player_yr, aes(season, wRC_VOL)) + geom_boxplot(aes(group = season)) + geom_smooth()

VOL_season_min100 <- wrc_player_yr_m100 %>% 
  group_by(season) %>%
  summarise(N = n(), mean = round(mean(wRC_VOL, na.rm = TRUE),3), sd = round(sd(wRC_VOL, na.rm = TRUE),3))

ggplot(wrc_player_yr_m100, aes(season, wRC_VOL)) + geom_boxplot(aes(group = season)) + geom_smooth()

# since the average consistency of a hitter shifts slightly each year, we should create a normalized metric to make it easier to compare hitters in different environments over time. For this, we will use z-scores. Z-scores are calculated by taking (value - pop mean) / pop std

z_scores_year <- wrc_player_yr %>% group_by(season) %>% summarise(dat_sd = sd(wRC_VOL, na.rm = TRUE), dat_mean = mean(wRC_VOL, na.rm = TRUE))

wrc_player_yr <- left_join(wrc_player_yr, z_scores_year, by = "season")

wrc_player_yr$zscore <- with(wrc_player_yr, (wRC_VOL - dat_mean) / dat_sd)

# we can also restrict to everyday players, using 100 games played as a cut off

z_scores_year_everyday <- wrc_player_yr_m100 %>% group_by(season) %>% summarise(dat_sd = sd(wRC_VOL, na.rm = TRUE), dat_mean = mean(wRC_VOL, na.rm = TRUE))

wrc_player_yr_m100 <- left_join(wrc_player_yr_m100, z_scores_year_everyday, by = "season")

wrc_player_yr_m100$zscore_everyday <- with(wrc_player_yr_m100, (wRC_VOL - dat_mean) / dat_sd)

# join the two separate z_scores

z_scores_year_all <- left_join(wrc_player_yr, wrc_player_yr_m100[,c(1,3,10:12)], by = c("PlayerId" = "PlayerId", "season" = "season"))

# plot average consistency by year 

plot_CON_yr <- ggplot(z_scores_year, aes(x=season,y=dat_mean)) + geom_point(size = 4, colour="firebrick") + theme_bp_grey() + xlab("\nYear") + ylab("Less Volatile                                                                                               More Volatile\n\n") + ggtitle ("Average League-wide Individual Volatility\n") + stat_smooth() 

plot_CON_yr

ggsave("lg_con_year.png", plot_CON_yr, scale = 1.2, width = 11, height = 8.5, units = "in")

# plot average consistency by year for players with >= 100 games played

plot_CON_yr_min100 <- ggplot(z_scores_year_everyday, aes(x=season,y=dat_mean)) + geom_point(size = 4, colour="firebrick") + theme_bp_grey() + xlab("\nYear") + ylab("Less Volatile                                                                                               More Volatile\n\n") + ggtitle ("Average League-wide Individual Volatility: Min 100 Games\n") + stat_smooth() 

plot_CON_yr_min100

ggsave("lg_con_year_min100.png", plot_CON_yr_min100, scale = 1.2, width = 11, height = 8.5, units = "in")

# join and co-plot the two different sets of players

z_join <- left_join(z_scores_year[,c(1,3)], z_scores_year_everyday[,c(1,3)], by = "season") %>% filter(season != 2016)
names(z_join) <- c("season", "All", "Min 100 G")
z_join_melt <- melt(z_join , id.vars="season")

plot_CON_yr_compare <- ggplot(z_join_melt, aes(x=season, y=value, colour=factor(variable))) + geom_point(size=6) + theme_bp_grey() + xlab("\nYear") + ylab("Less Volatile                                                                                                         More Volatile\n\n") + ggtitle ("League-wide Individual Consistency Over Time\n") + stat_smooth() + labs(colour="# Games Played")

plot_CON_yr_compare

ggsave("lg_con_year_comp.png", plot_CON_yr_compare, width = 10, height = 5.76, units = "in")

### Now we need to pull in seasonal data for each player year so we can explore relationships between consistency and hitter attributes. Start by restablishing a connection with your database

con <- dbConnect(RMySQL::MySQL(), dbname = fg_dbname, username = fg_username, password = fg_password, host = fg_host, port = 3306)

season_batting <- dbGetQuery(con, "select * from stats_batting_master_pfx where Season > 1973 and type = 0")

dbDisconnect(con)

# Let's trim down the data set

source("https://gist.githubusercontent.com/BillPetti/cf1e082b5e580b3b7209/raw/28b62dddfd1e494b2cf9d10c4ef79d68e6033a42/variable_list.R")

vars <- variable_list(season_batting)

season_batting_red <- dplyr::select(season_batting, playerid:PA, `BB%`:`BUH%`, wOBA:wRC, Spd:Age, `SwStr%`, `pfxFA%`:`pfxUN%`, `pfxO-Swing%`:`pfxZone%`, `Pull%`:`Hard%`)

# join annual z_score_year_all data with seasonal data

season_con_join <- left_join(z_scores_year_all, season_batting_red, by = c("PlayerId" = "playerid", "season" = "Season"))
season_con_join <- dplyr::select(season_con_join, -PA.x, -wRC.x)
names(season_con_join)[c(18,38)] <- c("PA", "wRC")

# now, pull in all data for each player season after the current observation

# first, let's pull a sample of data to make sure it pulls correctly

Sample <- ungroup(season_con_join) %>% filter(PlayerId == "1" | PlayerId == "2" |  PlayerId == "97")

Sample$YR2 <- Sample$season + 1

Sample_join <- left_join(Sample[,c(1,3,69)], Sample[,-c(2,69)], by = c("PlayerId" = "PlayerId", "YR2" = "season"))

# now, renname the new variables

colnames(Sample_join)[-c(1:3)] <- paste("yr2", colnames(Sample_join)[-c(1:3)], sep = "_")

# join the two sets of data so that a players t and t+1 seasons are in the same case

Sample_yoy <- left_join(Sample[,-69], Sample_join, by = c("PlayerId" = "PlayerId", "season" = "season"))

# calculate the year over year change for each relevant variable

change <- dplyr::select(Sample_yoy, yr2_Games:yr2_zscore_everyday, yr2_G:`yr2_IFH%`) - dplyr::select(Sample_yoy, Games:zscore_everyday, G:`IFH%`)

colnames(change) <- gsub("yr2_", "", colnames(change), fixed=TRUE)

# clear up the names of the columns and then merge back in to the main data set

colnames(change) <- paste("change", colnames(change), sep = "_")

Sample_yoy <- cbind(Sample_yoy, change)

# once the sample code works out, apply to the larger data sets

season_con_join$YR2 <- season_con_join$season + 1

season_join <- left_join(season_con_join[,c(1,3,73)], season_con_join[,-c(2,73)], by = c("PlayerId" = "PlayerId", "YR2" = "season"))

# now, renname the new variables

colnames(season_join)[-c(1:3)] <- paste("yr2", colnames(season_join)[-c(1:3)], sep = "_")

# join the two sets of data so that a players t and t+1 seasons are in the same case

season_yoy <- left_join(season_con_join[,-73], season_join, by = c("PlayerId" = "PlayerId", "season" = "season"))

# calculate the year over year change for each relevant variable

season_change <- season_yoy[,c(74:83, 86:142)] - season_yoy[,c(4:13, 16:72)]
colnames(season_change) <- gsub("yr2_", "change_", colnames(season_change), fixed=TRUE)

# clear up the names of the columns and then merge back in to the main data set

season_yoy <- cbind(season_yoy, season_change)
yoy_vars <- variable_list(season_yoy)

### now that we have our data set up properly, we can start the analysis
### we want to do two things: 1) calculate aging curves for consistency to see how much consistency and changes in consistency is partly a function of player age, and  2) understand what other player characteristics relate to consistency
### first, let's look at aging curves

# we are going to use the delta method of the harmonic mean to calculate aging curves

# first, restrict the data to players with at least 100 games played and more than 2 plate appearances per game

aging <- filter(season_yoy, Games > 99, PA_G > 2)
vars_aging <- variable_list(aging)
# test calculating the harmonic mean with only three sets of age groups

aging_test <- filter(aging, Age > 22, Age < 26)
aging_test <- aging_test[,c(5,10, 13, 18,41, 80, 83, 88,75,111,144,149,152)]
aging_test <- na.omit(aging_test)
aging_test$PA_ave <- (aging_test$PA + aging_test$yr2_PA)/2
aging_test$age_cohort <- paste(aging_test$Age, aging_test$yr2_Age, sep = "-")

# in R, the formula for calculating the harmonic mean is 1 / (mean(1/x)), where x is a vector of data. If you want a weighted harmonic mean, you just need to substitute the weighted mean function, i.e. 1 / (weighted.mean(1/x, w)) where w = the variable you are weighting by

x <- aging_test %>% group_by(age_cohort) %>% summarise(N = n(), YR1_PA = sum(PA), YR2_PA = sum(yr2_PA), YR1_wRC_CON = sum(wRC_CON), YR2_wRC_CON = sum(yr2_wRC_CON))

x$YR1w <- x$YR1_wRC_CON * (x$YR1_PA / (x$N/(1/x$YR1_PA + 1/x$YR2_PA)))
x$YR2w <- x$YR2_wRC_CON * (x$YR2_PA / (x$N/(1/x$YR1_PA + 1/x$YR2_PA)))
x$w_wRC_CON <- round((x$YR2w - x$YR1w),4)
x$cum_change <- cumsum(x$w_wRC_CON)

# now apply to the entire aging data set

aging <- aging[,c(5,10, 13, 18, 38, 41, 80, 83, 88,75, 108, 111,144,149,152, 175)]
aging <- na.omit(aging) %>% filter(yr2_Age - Age == 1)
aging$PA_ave <- (aging$PA + aging$yr2_PA)/2
aging$age_cohort <- paste(aging$Age, aging$yr2_Age, sep = "-")

# aging curve for wRC_CON

aging_group <- aging %>% group_by(Age, age_cohort) %>% summarise(N = n(), YR1_PA = sum(PA), YR2_PA = sum(yr2_PA), YR1_wRC_CON = sum(wRC_CON), YR2_wRC_CON = sum(yr2_wRC_CON))

aging_group$YR1w <- aging_group$YR1_wRC_CON * (aging_group$YR1_PA / (aging_group$N/(1/aging_group$YR1_PA + 1/aging_group$YR2_PA)))
aging_group$YR2w <- aging_group$YR2_wRC_CON * (aging_group$YR2_PA / (aging_group$N/(1/aging_group$YR1_PA + 1/aging_group$YR2_PA)))
aging_group$w_wRC_CON <- round((aging_group$YR2w - aging_group$YR1w),4)
aging_group$cum_change <- cumsum(aging_group$w_wRC_CON)

# remove smaller sample sized cohorts and re-run cummulative change

aging_group_red <- filter(aging_group, N >= 100)
aging_group_red$cum_change <- cumsum(aging_group_red$w_wRC_CON)

aging_plot <- ggplot(aging_group_red, aes(x=age_cohort,y=cum_change, group=1)) + geom_point(size = 10, colour="grey50") + geom_point(size = 8, colour="firebrick") + theme(plot.title = element_text(size=20, face="bold", vjust=2)) + theme(axis.title.x = element_text(face="bold")) + theme(axis.title.y = element_text(face="bold")) + theme(axis.text.y = element_text(face="bold",colour="black")) + theme(axis.text.x = element_text(face="bold",colour="black")) + xlab("Age") + ylab("Cummulative Change in Consistency") + ggtitle ("Aging Curve for Offensive Consistency") + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank()) + stat_smooth()

aging_plot

# rescale so that the max cummulative score is set to 0
aging_group_red$rescale <- aging_group_red$cum_change - max(aging_group_red$cum_change)

aging_plot_zeroscale <- ggplot(aging_group_red, aes(x=age_cohort,y=rescale, group=1)) + geom_point(size = 10, colour = "firebrick1") + theme(plot.title = element_text(size=20, face="bold", vjust=2)) + theme(axis.text = element_text(size = rel(1.05),angle = 0)) + theme(axis.title = element_text(size = rel(1.2))) + theme(axis.title.x = element_text(face="bold", vjust = -.25)) + theme(axis.title.y = element_text(face="bold", vjust = 1.75)) + theme(axis.text.y = element_text(face="bold",colour="black")) + theme(axis.text.x = element_text(face="bold",colour="black")) + xlab("Age") + ylab("Cummulative Change in Consistency") + ggtitle ("Aging Curve for Offensive Consistency") + theme(panel.grid.major = element_line(colour = "grey50", linetype = "dotted"), panel.grid.minor = element_blank()) + stat_smooth(colour = "firebrick1")

aging_plot_zeroscale

ggsave("aging_m100.png", aging_plot_zeroscale, width = 10, height = 5.76, units = "in")

# aging for zscore

aging_group <- aging %>% group_by(Age, age_cohort) %>% summarise(N = n(), YR1_PA = sum(PA), YR2_PA = sum(yr2_PA), YR1_zscore = sum(zscore), YR2_zscore = sum(yr2_zscore))

aging_group$YR1w <- aging_group$YR1_zscore * (aging_group$YR1_PA / (aging_group$N/(1/aging_group$YR1_PA + 1/aging_group$YR2_PA)))
aging_group$YR2w <- aging_group$YR2_zscore * (aging_group$YR2_PA / (aging_group$N/(1/aging_group$YR1_PA + 1/aging_group$YR2_PA)))
aging_group$w_zscore <- round((aging_group$YR2w - aging_group$YR1w),4)
aging_group$cum_change <- cumsum(aging_group$w_zscore)

# remove smaller sample sized cohorts and re-run cummulative change

aging_group_red <- filter(aging_group, N >= 200)
aging_group_red$cum_change <- cumsum(aging_group_red$w_zscore)

aging_plot <- ggplot(aging_group_red, aes(x=age_cohort,y=cum_change, group=1)) + geom_point(size = 10, colour="grey50") + geom_point(size = 8, colour="firebrick") + theme(plot.title = element_text(size=20, face="bold", vjust=2)) + theme(axis.title.x = element_text(face="bold")) + theme(axis.title.y = element_text(face="bold")) + theme(axis.text.y = element_text(face="bold",colour="black")) + theme(axis.text.x = element_text(face="bold",colour="black")) + xlab("Age") + ylab("Cummulative Change in Consistency") + ggtitle ("Aging Curve for Offensive Consistency") + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank()) + stat_smooth()

aging_plot

# rescale so that the max cummulative score is set to 0
aging_group_red$rescale <- aging_group_red$cum_change - max(aging_group_red$cum_change)

aging_plot_zeroscale <- ggplot(aging_group_red, aes(x=age_cohort,y=rescale, group=1)) + geom_point(size = 10, colour = "firebrick1") + theme(plot.title = element_text(size=20, face="bold", vjust=2)) + theme(axis.text = element_text(size = rel(1.05),angle = 0)) + theme(axis.title = element_text(size = rel(1.2))) + theme(axis.title.x = element_text(face="bold", vjust = -.25)) + theme(axis.title.y = element_text(face="bold", vjust = 1.75)) + theme(axis.text.y = element_text(face="bold",colour="black")) + theme(axis.text.x = element_text(face="bold",colour="black")) + xlab("Age") + ylab("Cummulative Change in Consistency") + ggtitle ("Aging Curve for Offensive Consistency") + theme(panel.grid.major = element_line(colour = "grey50", linetype = "dotted"), panel.grid.minor = element_blank()) + stat_smooth(colour = "firebrick1")

aging_plot_zeroscale

ggsave("aging_m100.png", aging_plot_zeroscale, width = 10, height = 5.76, units = "in")

# aging for zscore_everyday

aging_group <- aging %>% group_by(Age, age_cohort) %>% summarise(N = n(), YR1_PA = sum(PA), YR2_PA = sum(yr2_PA), YR1_zscore_everyday_everyday = sum(zscore_everyday), YR2_zscore_everyday = sum(yr2_zscore_everyday))

aging_group$YR1w <- aging_group$YR1_zscore_everyday * (aging_group$YR1_PA / (aging_group$N/(1/aging_group$YR1_PA + 1/aging_group$YR2_PA)))
aging_group$YR2w <- aging_group$YR2_zscore_everyday * (aging_group$YR2_PA / (aging_group$N/(1/aging_group$YR1_PA + 1/aging_group$YR2_PA)))
aging_group$w_zscore_everyday <- round((aging_group$YR2w - aging_group$YR1w),4)
aging_group$cum_change <- cumsum(aging_group$w_zscore_everyday)

# remove smaller sample sized cohorts and re-run cummulative change

aging_group_red <- filter(aging_group, N >= 100)
aging_group_red$cum_change <- cumsum(aging_group_red$w_zscore_everyday)

aging_plot <- ggplot(aging_group_red, aes(x=age_cohort,y=cum_change, group=1)) + geom_point(size = 10, colour="grey50") + geom_point(size = 8, colour="firebrick") + theme(plot.title = element_text(size=20, face="bold", vjust=2)) + theme(axis.title.x = element_text(face="bold")) + theme(axis.title.y = element_text(face="bold")) + theme(axis.text.y = element_text(face="bold",colour="black")) + theme(axis.text.x = element_text(face="bold",colour="black")) + xlab("Age") + ylab("Cummulative Change in Consistency") + ggtitle ("Aging Curve for Offensive Consistency") + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank()) + stat_smooth()

aging_plot

# rescale so that the max cummulative score is set to 0
aging_group_red$rescale <- aging_group_red$cum_change - max(aging_group_red$cum_change)

aging_plot_zeroscale <- ggplot(aging_group_red, aes(x=age_cohort,y=rescale, group=1)) + geom_point(size = 10, colour = "firebrick1") + theme(plot.title = element_text(size=20, face="bold", vjust=2)) + theme(axis.text = element_text(size = rel(1.05),angle = 0)) + theme(axis.title = element_text(size = rel(1.2))) + theme(axis.title.x = element_text(face="bold", vjust = -.25)) + theme(axis.title.y = element_text(face="bold", vjust = 1.75)) + theme(axis.text.y = element_text(face="bold",colour="black")) + theme(axis.text.x = element_text(face="bold",colour="black")) + xlab("Age") + ylab("Cummulative Change in Consistency") + ggtitle ("Aging Curve for Offensive Consistency") + theme(panel.grid.major = element_line(colour = "grey50", linetype = "dotted"), panel.grid.minor = element_blank()) + stat_smooth(colour = "firebrick1")

aging_plot_zeroscale

ggsave("aging_m100.png", aging_plot_zeroscale, width = 10, height = 5.76, units = "in")


### second, let's look at co-variates of consistency. We will once again restrict to our Games and PA_G minimums

correlates <- filter(season_yoy, Games > 99, PA_G > 2)
correlates <- correlates[,-c(1:3,8:9, 11:12, 14:15, 46:209)]

# creat a correlation matrix and select for wRC_CON, yr2_wRC_CON, and change_wRC_CON

cor.matrix <- as.data.frame(round(cor(correlates, use = "pairwise.complete.obs"), 2))
cor.matrix.vars <- as.data.frame(t(cor.matrix[1,]))
cor.matrix.vars$var_number <- c(1:206)
cor.matrix.vars$var_name <- row.names(cor.matrix.vars)
cor.matrix.reduce <- cor.matrix[,c(4,73,141)]

# top correlations for wRC_CON: GB/FB, pfxContact%, BB/K, wRC+, K%, wOBA, FB%, OPS, SLG, ISO
# others of interest BABIP, OBP, BB%

correlates <- correlates[,c(5:6, 10, 13, 17:72)]
cor.matrix.reduce <- round(cor(correlates[,c(1,3,4,7,8,10:60)], use = "pairwise.complete.obs", method= "spearman"), 2)
cor.matrix.reduce <- cor.matrix.reduce[c(1:3),]

# generate a linear model for wRC_CON

RC_lm_reduced <- lm(wRC_CON ~ wRC + G + PA_G + `GB/FB` + ISO + OBP, data = correlates)
summary(RC_lm_reduced)
relaimpo.RC <- calc.relimp(RC_lm_reduced, type = "lmg", rela = TRUE)
relaimpo.RC
relaimpo.RC_lmg <- as.data.frame(relaimpo.RC$lmg)
relaimpo.RC_lmg$vars <- row.names(relaimpo.RC_lmg)
names(relaimpo.RC_lmg) <- c("Relative Importance", "Metric")
relaimpo.RC_lmg <- arrange(relaimpo.RC_lmg, desc(`Relative Importance`))
relaimpo.RC_lmg

# plot the relative importance of each variabe

RC_lm_reduced_plot <- ggplot(relaimpo.RC_lmg, aes(x=factor(Metric), y=`Relative Importance`)) + geom_bar(stat="identity")
RC_lm_reduced_plot

### generate some leaderboards 
current_m100 <- season_yoy[,c(2:6,10,13,15:42)] %>% filter(season == 2015, Games > 80, PA_G > 2) %>% write.csv("current_leaders_2015.csv")

past3years_m100 <- season_yoy[,c(2:6,10,13,15:42)] %>% filter(season > 2011, season < 2015, Games > 100, PA_G > 2) %>% write.csv("2012_2014_v2_leaders.csv")







