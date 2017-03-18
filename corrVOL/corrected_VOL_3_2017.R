# Bill Petti

require(tidyverse)
require(reshape2)
require(baseballr)
require(gam) 
require(DAAG)
require(RMySQL)
require(reldist)
require(scales)
require(DT)

# load custom theme and color palettes for The Hardball Times

tht_palette <- c('#3E0002', '#8e001c', '#D87F83', '#969696', '#636363', '#252525')

tht_theme <- function(base_size = 12, base_family = "Arial") {
  theme_minimal(base_size = base_size, base_family = base_family) %+replace%
    theme(
      panel.border = element_blank(),
      panel.background = element_blank(),
      panel.grid.major = element_line(color='#BFBFBF', size=.25),
      axis.title = element_text(face='bold', hjust=.5, vjust=0),
      axis.text = element_text(color='black')
    )
}

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

daily <- dbGetQuery(con, "select w.*, s.ShortName as 'Team', s.PF, p.FirstName, p.LastName from wpa_daily_batting w left join season_team s on w.TeamId = s.TeamId left join player_info p on w.PlayerId = p.PlayerId where substr(w.GameDate, 1,4) = s.Season and p.Position <> 'P' and w.GameDate between '1974-03-01' and '2016-11-30'")

yearly <- dbGetQuery(con, "select s.*, p.FirstName, p.LastName from stats_batting_master s left join player_info p on s.PlayerId = p.PlayerId where p.Position <> 'P' and s.Season >= 1974 and type = 0")

dbDisconnect(con)

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

wrc_player_yr <- daily2_join %>% select(PlayerId, Name, season, G, PA, wRC) %>% group_by(PlayerId, Name, season) %>% summarise(Games = sum(G), PA = sum(PA), wRC = sum(wRC, na.rm = TRUE))

# generate total wRC per team, per year

wrc_team_yr <- daily2_join %>% dplyr::select(TeamId, Team, season, wRC) %>% na.omit() %>% group_by(TeamId, Team, season) %>% summarise(count = n(), wRC = sum(wRC, na.rm = TRUE))

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

wrc_player_yr <- daily2_join %>% select(PlayerId, season, wRC) %>% 
  na.omit() %>% 
  aggregate(wRC ~ PlayerId + season, data = ., FUN = "Gini_neg") %>%
  left_join(wrc_player_yr, ., by = c("PlayerId" = "PlayerId", "season" = "season"))

wrc_player_yr <- ungroup(wrc_player_yr)

names(wrc_player_yr)[7] <- "VOL"
names(wrc_player_yr)[6] <- "wRC"

wrc_player_yr$VOL <- round(wrc_player_yr$VOL, 3)

# group each player season by PAs per game to compare how consistency varies by playing time

wrc_player_yr$PA_G <- round((wrc_player_yr$PA / wrc_player_yr$Games),1) 
wrc_player_yr$PA_grp <- ifelse(wrc_player_yr$PA_G <=1,1,ifelse(wrc_player_yr$PA_G >1 & wrc_player_yr$PA_G <= 1.5, 1.5, ifelse(wrc_player_yr$PA_G >1.5 & wrc_player_yr$PA_G <=2,2, ifelse(wrc_player_yr$PA_G > 2 & wrc_player_yr$PA_G <= 2.5, 2.5, ifelse(wrc_player_yr$PA_G > 2.5 & wrc_player_yr$PA_G <= 3, 3, ifelse(wrc_player_yr$PA_G > 3 & wrc_player_yr$PA_G <= 3.5, 3.5, ifelse(wrc_player_yr$PA_G > 3.5 & wrc_player_yr$PA_G <= 4, 4, ifelse(wrc_player_yr$PA_G > 4 & wrc_player_yr$PA_G <= 4.5, 4.5, 5))))))))

# now group by PA_grp and run correlations between PA_G and wRC_CON

wrc_player_yr %>% filter(PA_grp > 1) %>% group_by(PA_grp) %>% summarise(N = n(), Correlation = cor(PA_G, VOL, use = "pairwise.complete.obs"))

# correlate for players with a min number of 100 games played

wrc_player_yr %>% filter(PA_grp > 1, Games > 99) %>% group_by(PA_grp) %>% summarise(N = n(), Correlation = cor(PA_G, VOL, use = "pairwise.complete.obs"))

# correlate w/ wRC

wrc_player_yr %>% filter(PA_grp > 1) %>% group_by(PA_grp) %>% summarise(N = n(), Correlation = cor(wRC, VOL, use = "pairwise.complete.obs"))

wrc_player_yr %>% 
  filter(PA_grp > 2, Games > 99) %>% 
  group_by(PA_grp) %>% 
  summarise(N = n(), Correlation = cor(wRC, VOL, use = "pairwise.complete.obs")) %>%
  as.data.frame() %>%
  ggplot(aes(PA_grp, Correlation)) +
  geom_point(size = 7, color = "#8e001c") + 
  geom_text(aes(label = round(Correlation,2)), fontface = "bold", vjust = -1.25) + 
  scale_x_continuous(breaks = seq(from = 2, to = 5, by = .5)) +
  xlab("\nPlate Appearances per Game") +
  ylab("\nCorrelation Between VOL and wRC\n") +
  ggtitle("\nCorrelation Between VOL and wRC, by Plate Appearances per Game") +
  labs(subtitle = "Minimum of 100 Games Played\n") + 
  tht_theme() + 
  theme(title = element_text(face = "bold", size = 16), 
        axis.text = element_text(size = 12, face = "bold"))

ggsave("corr_pa_g_VOL_wRC.png", scale = 1, height = 8.5, width = 11, units = "in")

# create a data set with just players with >= 100 games

wrc_player_yr_m100 <- filter(wrc_player_yr, Games > 99)

# check to see whether we need to adjust scores for general league-wide shifts in VOL

VOL_season <- wrc_player_yr %>% 
  group_by(season) %>%
  summarise(N = n(), mean = round(mean(VOL, na.rm = TRUE),3),  sd = round(sd(VOL, na.rm = TRUE),3))

ggplot(VOL_season, aes(season, mean)) + 
  geom_point(color = "#8e001c") + 
  geom_smooth() + 
  tht_theme()

VOL_season_min100 <- wrc_player_yr_m100 %>% 
  group_by(season) %>%
  summarise(N = n(), mean = round(mean(VOL, na.rm = TRUE),3), sd = round(sd(VOL, na.rm = TRUE),3))

ggplot(VOL_season_min100, aes(season, mean)) + 
  geom_point(color = "#8e001c") + 
  geom_smooth() + 
  tht_theme()

# since the average consistency of a hitter shifts slightly each year, we should create a normalized metric to make it easier to compare hitters in different environments over time. For this, we will use z-scores. Z-scores are calculated by taking (value - pop mean) / pop std

z_scores_year <- wrc_player_yr %>% group_by(season) %>% summarise(dat_sd = sd(VOL, na.rm = TRUE), dat_mean = mean(VOL, na.rm = TRUE))

wrc_player_yr <- left_join(wrc_player_yr, z_scores_year, by = "season")

wrc_player_yr$zscore <- with(wrc_player_yr, (VOL - dat_mean) / dat_sd)

# we can also restrict to everyday players, using 100 games played as a cut off

z_scores_year_everyday <- wrc_player_yr_m100 %>% group_by(season) %>% summarise(dat_sd = sd(VOL, na.rm = TRUE), dat_mean = mean(VOL, na.rm = TRUE))

wrc_player_yr_m100 <- left_join(wrc_player_yr_m100, z_scores_year_everyday, by = "season")

wrc_player_yr_m100$zscore_everyday <- with(wrc_player_yr_m100, (VOL - dat_mean) / dat_sd)

# join the two separate z_scores

z_scores_year_all <- left_join(wrc_player_yr, wrc_player_yr_m100[,c(1,3,10:12)], by = c("PlayerId" = "PlayerId", "season" = "season"))

### correlation between player attributes

# create a data set to make it easier to merge in wRC+ data for players

yearly_red <- dplyr::select(yearly, playerid, Season, G, `wRC+`)
yearly_red_100G <- yearly_red %>%
  filter(G > 99)

z_scores_year_all_rwcplus <- z_scores_year_all %>%
  left_join(yearly_red, by = c("PlayerId" = "playerid", "season" = "Season"))

corr_data <- z_scores_year_all_rwcplus %>%
  select(Games, PA, wRC, `wRC+`, PA_G, VOL, zscore) %>%
  filter(VOL != "NaN") %>%
  filter(!is.na(VOL)) %>%
  cor(.)

corr_data_100G <- z_scores_year_all_rwcplus %>%
  select(Games, PA, wRC, `wRC+`, PA_G, VOL, zscore) %>%
  filter(VOL != "NaN") %>%
  filter(!is.na(VOL)) %>%
  filter(Games > 99) %>%
  cor(.)

z_scores_year_all_rwcplus %>%
  select(Games, PA, wRC, `wRC+`, PA_G, PA_grp, VOL, zscore) %>%
  filter(VOL != "NaN") %>%
  filter(!is.na(VOL)) %>%
  filter(Games >= 100) %>%
  ggplot(aes(VOL, wRC)) + 
  geom_point(aes(color = PA_grp), size = 2, alpha = .6) + 
  scale_color_gradient2(low = "#006BA4", high = "#C85200", midpoint = 3.5, "PA/G") +
  xlab("\nVOL") +
  ylab("\nwRC") +
  ggtitle("\nRelationship Between VOL and wRC") +
  labs(subtitle = "Minimum 100 Games Played\n") +
  tht_theme() + 
  theme(title = element_text(face = "bold", size = 16), 
        axis.text = element_text(size = 12, face = "bold"))

ggsave("scatter_pa_g_VOL_wRC.png", scale = 1, height = 8.5, width = 11, units = "in")

model_1_data <- z_scores_year_all_rwcplus %>%
  select(PlayerId, Name, season, Games, PA, wRC, PA_G, VOL, zscore) %>%
  filter(VOL != "NaN") %>%
  filter(!is.na(VOL)) %>%
  filter(Games > 99) %>%
  left_join(yearly_red, by = c("PlayerId" = "playerid", "season" = "Season"))

model_1_data_melted <- model_1_data %>%
  gather(key = feature, value = value, -VOL) %>%
  filter(feature %in% c("PA_G", "Games", "wRC")) %>%
  mutate(value = as.numeric(value), feature = ifelse(feature == "PA_G", "PA per Game", feature), feature = factor(feature))

model_1_data_melted %>%
  ggplot(aes(value, VOL)) + 
  geom_point(aes(color = feature), size = 2, alpha = .5) + 
  facet_wrap(~feature, scales = "free_x") +
  xlab("\nFeature Value") +
  ylab("\nVOL\n") +
  scale_color_manual(values = tht_palette, "Features") +
  ggtitle("\nRelationship Between VOL, Games Played, PA/G, and wRC") +
  labs(subtitle = "Minimum 100 Games Played\n") +
  tht_theme() + 
  theme(title = element_text(face = "bold", size = 16), 
        axis.text = element_text(size = 12, face = "bold"),
        strip.text.x = element_text(face = "bold", size = 12))

ggsave("scatter_facet_model_featured.png", scale = 1, height = 8.5, width = 14, units = "in")

set.seed(42)
train <- sample_frac(model_1_data, .7, replace = FALSE)
test <- dplyr::setdiff(x = model_1_data, y = train)

# check if the data set was split correctly 

length(model_1_data$Name) == length(train$Name) + length(test$Name)

model_1 <- lm(VOL ~ wRC + PA_G + Games, data = train)
summary(model_1)
fit_1 <- predict(model_1, test)
model_1_res_act <- data.frame(actuals = test$VOL, predicted = fit_1) %>%
  mutate(errors = predicted - actuals)
rmse <- function(df) {
  rmse <- sqrt(mean((df$actuals-df$predicted)^2))
  rmse
}

rmse_model_1 <- rmse(model_1_res_act)
round(rmse_model_1, 4)

#### check variance inflation for model_1

vif_model_1 <- vif(model_1)
vif_model_1

#### despite a pretty high correlation between the two, the VIF values are tolerable
# wRC   PA_G  Games 
# 2.7296 2.0661 1.9197 

# apply model_1 to all cases

fit_all <- data_frame(predicted = predict(model_1, model_1_data))
fit_values <- cbind(model_1_data, fit_all) %>%
  mutate(error = predicted - VOL, corrVOL = round(VOL/predicted, 3)*100)

# check distribution of errors

ggplot(fit_values, aes(error, ..density..)) + 
  geom_histogram(color = "white") + 
  geom_freqpoly() + 
  tht_theme()

ggplot(fit_values, aes(sample = error)) + 
  stat_qq(color = "#8e001c", alpha = .2) + 
  tht_theme()

# compare correlations between VOL and corrVOL

cor_model_1 <- fit_values %>%
  select(PA, Games, wRC, `wRC+`, VOL, corrVOL) %>%
  cor()

cor_model_1[lower.tri(cor_model_1)] <- NA
cor_model_1 <- melt(cor_model_1)
cor_model_1$value <- abs(cor_model_1$value)
cor_model_1 <- na.omit(cor_model_1)

# correlation matrix without corrVOL

cor_model_1 %>%
  filter(value != 1) %>%
  filter(Var1 != "corrVOL") %>%
  filter(Var2 != "corrVOL") %>%
  ggplot(aes(Var1, Var2)) + 
  geom_tile(aes(fill = value),  color = "white") + 
  geom_text(aes(label = round(value, 3)), size = 4, fontface = "bold") +
  scale_fill_gradient2(low = "#67a9cf", high = "#ef8a62", midpoint = .5, "Absolute Correlation") +
  ggtitle("Absolute Correlation of Volatility and Other Variables") +
  tht_theme() +
  theme(title = element_text(face = "bold", size = 16), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        axis.title = element_blank(), 
        axis.text = element_text(size = 12, face = "bold"))

ggsave("key_correlations_VOL.png", scale = 1, height = 8.5, width = 14, units = "in")

# correlation matrix with corrVOL

cor_model_1 %>%
  filter(value != 1) %>%
  ggplot(aes(Var1, Var2)) + 
  geom_tile(aes(fill = value),  color = "white") + 
  geom_text(aes(label = round(value, 3)), size = 4, fontface = "bold") +
  scale_fill_gradient2(low = "#67a9cf", high = "#ef8a62", midpoint = .5, "Absolute Correlation") +
  ggtitle("Absolute Correlation of Volatility and Other Variables") +
  tht_theme() +
  theme(title = element_text(face = "bold", size = 16), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        axis.title = element_blank(), 
        axis.text = element_text(size = 12, face = "bold"))

ggsave("key_correlations_VOL_corrVOL.png", scale = 1, height = 8.5, width = 14, units = "in")

ggplot(averages_player, aes(wRC_plus, average_corrected)) + geom_point() + geom_smooth(method = "loess") + tht_theme()

fit_values %>%
  filter(!season %in% c(1981, 1994)) %>%
  ggplot(aes(corrVOL)) +
  geom_histogram(fill = "#8e001c") +
  facet_wrap(~season) + 
  xlab("\ncorrVOL") +
  ylab("\n") +
  ggtitle("\nDistribution of corrVOL by Season") +
  tht_theme() +
  theme(title = element_text(face = "bold", size = 16), 
        strip.text.x = element_text(face = "bold", size = 12),
        axis.title = element_blank(), 
        axis.text = element_text(size = 12, face = "bold"))

ggsave("hist_corrVOL.png", scale = 1, height = 8.5, width = 14, units = "in")

fit_values %>%
  ggplot(aes(VOL, corrVOL)) +
  geom_point(color = "#8e001c") +
  geom_smooth(method = "lm") +
  xlab("\nVOL") +
  ylab("\ncorrVOL\n") +
  ggtitle("\nDistribution of corrVOL by Season") +
  tht_theme() +
  theme(title = element_text(face = "bold", size = 16), 
        strip.text.x = element_text(face = "bold", size = 12),
        axis.title = element_blank(), 
        axis.text = element_text(size = 12, face = "bold"))

#### leaders

top_20 <- fit_values %>%
  arrange(corrVOL) %>%
  slice(1:20) %>%
  select(Name, season, Games, PA_G, wRC, `wRC+`, VOL, round(predicted,3), corrVOL)

names(top_20) <- c("Names", "Season", "Games", "PA per Game", "wRC", "wRC+", "VOL", "expectedVOL", "corrVOL")

write.csv(top_20, "top_20.csv", row.names = FALSE)

fit_values %>%
  filter(predicted >= .607, predicted <= .611) %>%
  ggplot(aes(`wRC+`, corrVOL)) +
  geom_point(color = "#636363", alpha = .4, size = 3) + 
  geom_point(data = filter(fit_values, Name == "Rickey Henderson"), aes(`wRC+`, corrVOL), color = "#8e001c", size = 3) +
  geom_text(data = filter(fit_values, Name == "Rickey Henderson"), aes(`wRC+`, corrVOL, label = season, vjust = 1.5, fontface = "bold")) + 
  xlab("\nwRC+") +
  ylab("\ncorrVOL\n") +
  ggtitle("\nCorrected Volatilty and Weighted Runs Created Plus") +
  labs(subtitle = "Players with an Expected Volatility betweeen .607 and .611 (Rickey Henderson in red)\n") +
  tht_theme() +
  theme(title = element_text(face = "bold", size = 16), 
        strip.text.x = element_text(face = "bold", size = 12),
        axis.text = element_text(size = 12, face = "bold"))

ggsave("hist_corrVOL_Henderson.png", scale = 1, height = 8.5, width = 14, units = "in")

bottom_20 <- fit_values %>%
  arrange(desc(corrVOL)) %>%
  slice(1:20) %>%
  select(Name, season, Games, PA_G, wRC, `wRC+`, VOL, round(predicted,3), corrVOL)

names(bottom_20) <- c("Names", "Season", "Games", "PA per Game", "wRC", "wRC+", "VOL", "expectedVOL", "corrVOL")

write.csv(bottom_20, "bottom_20.csv", row.names = FALSE)

ggplot() + 
  geom_freqpoly(data = fit_values, aes(corrVOL, ..density..), color = "red") + 
  geom_freqpoly(data = fit_values, aes(`wRC+`, ..density..), color = "blue") + 
  tht_theme()

averages_player <- fit_values %>%
  group_by(PlayerId, Name) %>%
  summarize(seasons = n(), average_corrected = round(weighted.mean(corrVOL, w = PA),1), wRC_plus = round(weighted.mean(`wRC+`, w = PA),0)) %>%
  ungroup() %>%
  select(-PlayerId)

names(averages_player) <- c("Name", "Seasons", "Average corrVOL", "Average wRC+") 

top_5_ave <- averages_player %>%
  filter(Seasons > 4) %>%
  arrange(`Average corrVOL`) %>%
  slice(1:5)

bottom_5_ave <- averages_player %>%
  filter(Seasons > 4) %>%
  arrange(desc(`Average corrVOL`)) %>%
  slice(1:5)

top_and_bottom_ave <- rbind(top_5_ave, bottom_5_ave)

write.csv(top_and_bottom_ave, "top_bottom_ave.csv", row.names = FALSE)

#### make Lorenz curves for individual player seasons

lorenz_data <- daily2_join %>%
  ungroup() %>%
  mutate(Season = substr(GameDate, 1, 4)) %>%
  select(PlayerId, Name, Season, wRC) %>%
  group_by(PlayerId, Season) %>%
  arrange(PlayerId, Season, wRC) %>%
  filter(!is.na(wRC)) %>%
  mutate(number = 1, cumulative_games = cumsum(number), cumulative_runs = cumsum(wRC), perc_game = cumulative_games/max(cumulative_games), perc_runs = cumulative_runs/max(cumulative_runs, na.rm = TRUE))

lorenz_players_seasons <- fit_values %>%
  select(PlayerId, season, `wRC+`) %>%
  mutate(keep = 1, season = as.character(season)) %>%
  filter(`wRC+` >= 70)

lorenz_data_reduced <- lorenz_data %>%
  left_join(lorenz_players_seasons, by = c("PlayerId" = "PlayerId", "Season" = "season")) %>%
  mutate(player_year = paste0(PlayerId, "-", Season)) %>%
  filter(keep == 1)

line_coord <- data.frame(x = 0, y = 0, xend = 1, yend = 1)

rhenderson <- filter(lorenz_data_reduced, Name == "Rickey Henderson", Season == 1992)
mramirez <- filter(lorenz_data_reduced, Name == "Manny Ramirez", Season == 1998)

ggplot() + 
  geom_segment(data = line_coord, 
               aes(x = x, y = y, xend = xend, yend = yend), linetype = "dashed") +
  geom_line(data = mramirez, 
            aes(perc_game, perc_runs, 
                group = player_year),
            color = "#E31937", size = 1.25) + 
  geom_line(data = rhenderson, 
            aes(perc_game, perc_runs, 
                group = player_year),
            color = "#003831", size = 1.25) + 
  xlab("\nPercent of Cumulative Games Played\n") +
  ylab("\nPercent of Cumulative wRC\n") +
  ggtitle("\nThe Most and Least Volatile Individual Seasons Since 1974") +
  labs(subtitle = "Rickey Henderson, 1992 (dark green) versus Manny Ramirez, 1998 (red)\n") +
  scale_y_continuous(labels = percent) +
  scale_x_continuous(labels = percent) +
  tht_theme() +
  theme(title = element_text(face = "bold", size = 16), 
        strip.text.x = element_text(face = "bold", size = 12),
        axis.text = element_text(size = 12, face = "bold"))

ggsave("Henderson_v_Ramirez.png", scale = 1, height = 8.5, width = 14, units = "in")

ggplot() + 
  geom_segment(data = line_coord, 
               aes(x = x, y = y, xend = xend, yend = yend), linetype = "dashed") +
  geom_line(data = lorenz_data_reduced, 
            aes(perc_game, perc_runs, 
                group = player_year),
            color = "#969696",
            size = .70, 
            alpha = .1) +
  geom_line(data = rhenderson, 
            aes(perc_game, perc_runs), 
            color = "#003831", 
            size = 1.25) +
  xlab("\nPercent of Cumulative Games Played\n") +
  ylab("\nPercent of Cumulative wRC\n") +
  ggtitle("\nwRC Distribution for Individual Seasons Since 1974") +
  labs(subtitle = "Rickey Henderson's 1992 Season (dark green)\n") +
  scale_y_continuous(labels = percent) +
  scale_x_continuous(labels = percent) +
  tht_theme() +
  theme(title = element_text(face = "bold", size = 16), 
        strip.text.x = element_text(face = "bold", size = 12),
        axis.text = element_text(size = 12, face = "bold"))

ggsave("Henderson_v_All.png", scale = 1, height = 8.5, width = 14, units = "in")

slice(arrange(fit_values, corrVOL), 1:25) %>%
  DT::datatable(data = .)

filter(arrange(fit_values, corrVOL), season == 2016) %>%
  DT::datatable(data = .)

jramirez2016 <- filter(lorenz_data_reduced, Name == "Jose Ramirez", Season == 2016)
narendo2016 <- filter(lorenz_data_reduced, Name == "Nolan Arenado", Season == 2016)

ggplot() + 
  geom_segment(data = line_coord, 
               aes(x = x, y = y, xend = xend, yend = yend), linetype = "dashed") +
  geom_line(data = narendo2016, 
            aes(perc_game, perc_runs, 
                group = player_year),
            color = "purple") + 
  geom_line(data = jramirez2016, 
            aes(perc_game, perc_runs),
            color = "red") +
  xlab("\nPercent of Cumulative Games Played\n") +
  ylab("\nPercent of Cumulative wRC\n") +
  coord_flip() +
  scale_y_continuous(labels = percent) +
  scale_x_continuous(labels = percent) +
  tht_theme()

# Example to illustrate how Gini coefficients work

country_A <- c(30,35,40,45,50,55,60,65,70,75,80,85,90,95,100)
country_B <- c(1,1,1,3,3,3,4,5,5,16,55,78,200,200,400)

gini_example <- data_frame(Egalistan = country_A, Concentratistan = country_B)

write.csv(gini_example, "gini.example.csv", row.names = FALSE)

round(Gini_neg(gini_example$Egalistan),2)
# 0.205128

round(Gini_neg(gini_example$Concentratistan),2)
# 0.8007326

lorenz_curves_example <- gini_example %>%
  gather(key = Country, value = value) %>%
  group_by(Country) %>%
  arrange(Country, value) %>%
  mutate(number = 1, 
         cumulative_citizens = cumsum(number), 
         cumulative_wealth = cumsum(value), 
         perc_citizens = cumulative_citizens/max(cumulative_citizens),
         perc_wealth = cumulative_wealth/max(cumulative_wealth, na.rm = TRUE)) %>%
  select(Country, perc_citizens, perc_wealth) %>%
  as.data.frame()

zero_rows <- data_frame(Country = c("Egalistan", "Concentratistan"), perc_citizens = c(0, 0),  perc_wealth = c(0, 0))

lorenz_curves_example <- rbind(lorenz_curves_example, zero_rows)

ggplot(lorenz_curves_example, aes(perc_citizens, perc_wealth)) + 
  geom_segment(data = line_coord, 
               aes(x = x, y = y, xend = xend, yend = yend), linetype = "dashed") +
  geom_line(aes(color = Country), size = 1.25) +
  ggtitle("\nLorenz Curve Example: Distribution of Wealth Comparison\n") + 
  ylab("\nCumulative Share of Citizens (highes to lowest incomes)\n") +
  xlab("\nCumulative Share of Wealth\n") +
  scale_y_continuous(breaks = seq(from=0, to=1, by=.1), labels = percent) +
  scale_x_continuous(breaks = seq(from=0, to=1, by=.1), labels = percent) +
  scale_color_manual(values = c("Egalistan" = '#8e001c', "Concentratistan" = '#D87F83')) + 
  tht_theme() +
  theme(title = element_text(face = "bold", size = 16))

ggsave("lorenz_ex.png", scale = 1, height = 8.5, width = 11, units = "in")

#### clean up and export individual seasons

individual_seasons_VOL_corrVOL <- fit_values %>%
  select(-PlayerId, -zscore, -error) %>%
  mutate(predicted = round(predicted, 3), wRC = round(wRC,1), `wRC+` = round(`wRC+`,0))

names(individual_seasons_VOL_corrVOL) <- c("Name", "Season", "Games", "PA", "wRC", "PA_G", "VOL", "wRC+", "expectedVOL", "corrVOL")

individual_seasons_VOL_corrVOL <- individual_seasons_VOL_corrVOL %>%
  select(Name, Season, Games, PA, PA_G, wRC, `wRC+`, VOL, expectedVOL, corrVOL)

write.csv(individual_seasons_VOL_corrVOL, "individual_seasons_VOL_corrVOL_1974_2016.csv", row.names = FALSE)
