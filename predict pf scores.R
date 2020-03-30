setwd("D:/ChrisT/Projects/Pitchfork")
rm(list=ls())


library("caret")
library("ggplot2")
library("dplyr")
library("tidyr")
library("MASS")
library("xtable")

set.seed(123)

pf <- read.csv("pitchfork_ohe.csv", header = TRUE)
lfm <- read.csv("lastfm.csv", header = TRUE)

# Remove rownames and user-generated tags that already exist as record labels
rownames(pf) <- NULL
rownames(lfm) <- NULL
lfm$stones.throw <- NULL
lfm$X4ad <- NULL
lfm$kranky <- NULL
lfm$kompakt <- NULL
lfm$domino <- NULL
lfm$warp <- NULL
lfm$anticon <- NULL
lfm$ninja.tune <- NULL
lfm$thrill.jockey <- NULL

# Merge pitchfork and last.fm datasets
mrg_df <- merge(x = pf, y = lfm, by = c("title", "artist"), all.x = FALSE, all.y = TRUE)

# Create prefixes for labels (l_) and tags (t_)
colnames(mrg_df)[26:104] <- paste("l", colnames(mrg_df[,c(26:104)]), sep = "_")
colnames(mrg_df)[111:270] <- paste("t", colnames(mrg_df[,c(111:270)]), sep = "_")

# Fix inconsistent names across the two datasets
mrg_df[is.na(mrg_df$score),][1:3]
mrg_df[mrg_df$title == "rockabyebaby" & mrg_df$artist == "cassie",][3:104] <- pf[pf$title == "#rockabyebaby" & pf$artist == "cassie",][3:104]
mrg_df[mrg_df$title == "duet for guitars 2" & mrg_df$artist == "m. ward",][3:104] <- pf[pf$title == "duet for guitars #2" & pf$artist == "m. ward",][3:104]
mrg_df[mrg_df$title == "suite 420" & mrg_df$artist == "devin the dude",][3:104] <- pf[pf$title == "suite #420" & pf$artist == "devin the dude",][3:104]

# Only retain albums that were released after 1998
mrg_df <- mrg_df[mrg_df$year>=1999,]


# Retain the features used for analysis in new dataframe
df <- mrg_df[,c(1:2,5,14,17:104,107:270)] 

# Compute average track length in seconds for each album
df$avg_track_length <- df$duration/df$tracks


# Examine the features with the highest collinearity
cor(df[,-c(1,2)]) %>%
  as.data.frame() %>%
  mutate(var1 = rownames(.)) %>%
  gather(var2, value, -var1) %>%
  arrange(desc(value)) %>%
  group_by(value) %>%
  filter(row_number()==1 & value != 1) %>%
  top_n(10)


  
# Rescale the numeric features so all features are on the same scale
df$year <- (df$year -min(df$year))/(max(df$year)-min(df$year))
df$duration <- (df$duration -min(df$duration))/(max(df$duration)-min(df$duration))
df$tracks <- (df$tracks -min(df$tracks))/(max(df$tracks)-min(df$tracks))
df$listeners <- (df$listeners -min(df$listeners))/(max(df$listeners)-min(df$listeners))
df$avg_track_length <- (df$avg_track_length -min(df$avg_track_length))/(max(df$avg_track_length)-min(df$avg_track_length))


ggplot(data = df, aes(x=score)) +
  geom_histogram(binwidth = 0.5, fill = "#A3B9AA", col = "#616F66", alpha = 0.7) +
  theme(plot.title = element_text(hjust = 0.5),
        panel.background = element_rect(fill = "grey85"),
        plot.background = element_rect(fill = "transparent", color = NA)) +
  ggtitle("Album Score Histogram") +                     
  xlab("Album Score") +                                  
  ylab("Count") +    
  ggsave(filename = "Album_Score_Histogram.png",                      
         bg = "transparent", dpi = 600, width = 7, height = 7)

ggplot(data = df, aes(x=playcount)) +
  geom_histogram(fill = "#E68A99", col = "#b45867", alpha = 0.7) +
  theme(plot.title = element_text(hjust = 0.5),
        panel.background = element_rect(fill = "grey85"),
        plot.background = element_rect(fill = "transparent", color = NA)) +
  ggtitle("Album Playcount Histogram") +                     
  xlab("Album Playcount") +                                  
  ylab("Count") +    
  ggsave(filename = "Album_Playcount_Histogram.png",                      
         bg = "transparent", dpi = 600, width = 7, height = 7)

# Around 8000 records have less than a million playcount
# Some albums like XX by The XX or In Rainbows by Radiohead have almost 80 million plays
df[df$playcount < 1000000,c(1:3)]

#print(xtable(df[1:5,]), type="html", html.table.attributes="")


# LINEAR REGRESSION ANALYSIS

hist(df$score)

# Full model including all features in the dataset except for title, artist, listeners and playcount
lm_full <- lm(
  score ~ . - title - artist - listeners - playcount,
  data = df
)
summary(lm_full)
# Adj R^2: 0.0971


# Incrimentally take out insignificant variables until the adjusted R^2 stops increasing
lm_final <- lm(
  score ~ . - title - artist - listeners - playcount - t_X00s - t_electronica - t_female.vocalist - t_abstract
  - t_chillwave - t_minimal.techno - t_rap - t_country - t_jazz - t_gangsta.rap - l_kompakt - t_punk - t_female - t_blues.rock
  - t_X70s - t_thrash.metal - l_anticon - l_anti. - t_japanese - t_acoustic - t_shoegaze - l_legacy - l_carpark - t_hardcore
  - tracks - t_folk.rock - g_experimental - l_def.jam - t_experimental.hip.hop - t_seattle - t_doom - t_new.wave
  - t_britpop - l_arts...crafts - l_parlophone - t_twee - t_dark.ambient - l_hydra.head - t_contemporary.classical - year
  - t_freak.folk - t_heavy.metal - t_glitch - t_funk - t_metalcore - t_synth.pop - t_neo.psychedelia - t_dub - t_progressive.metal
  - t_electropop - l_xsmall.label - l_ninja.tune - t_french - t_stoner.metal - t_disco - t_atmospheric.black.metal - t_indie.electronic
  - t_X2010s - t_chill - t_twee.pop - t_psychedelic.folk - t_guitar - t_alternative.hip.hop - t_neo.soul - t_dubstep - t_emusic
  - l_barsuk - t_dance - t_instrumental - t_live - t_math.rock - t_female.vocalists - t_darkwave - t_world - t_ethereal - l_X.k7
  - t_X2000s - l_fat.possum - t_indietronica - t_mathcore - t_brooklyn - t_idm - t_german - t_grindcore - l_yep.roc - l_emi,
  data = df
)
summary(lm_final)
# Adj R2 increased from 0.0971 to 0.1027
#sort(summary(lm_final)$coefficients[,4],decreasing = FALSE)
#plot(lm_final)



lm_final_coeff <- as.data.frame(round(summary(lm_final)$coefficients[,c(1,4)],3))
lm_final_coeff <- tibble::rownames_to_column(lm_final_coeff, "Variable")
lm_final_coeff <- lm_final_coeff[order(lm_final_coeff$Estimate, decreasing = TRUE),]

for (i in 1:nrow(lm_final_coeff)){  
  if (lm_final_coeff$`Pr(>|t|)`[i] < 0.001){
    lm_final_coeff$Sig[i] <- 0.001
    lm_final_coeff$Coefficient[i] <- paste(lm_final_coeff$Estimate[i],"***", sep = " ")
  } else if(lm_final_coeff$`Pr(>|t|)`[i] < 0.01) {
    lm_final_coeff$Sig[i] <- 0.01
    lm_final_coeff$Coefficient[i] <- paste(lm_final_coeff$Estimate[i],"**", sep = " ")
  } else if(lm_final_coeff$`Pr(>|t|)`[i] < 0.05) {
    lm_final_coeff$Sig[i] <- 0.05
    lm_final_coeff$Coefficient[i] <- paste(lm_final_coeff$Estimate[i],"*", sep = " ")
  } else if(lm_final_coeff$`Pr(>|t|)`[i] < 0.1) {
    lm_final_coeff$Sig[i] <- 0.1
    lm_final_coeff$Coefficient[i] <- paste(lm_final_coeff$Estimate[i],".", sep = " ")
  } else {
    lm_final_coeff$Sig[i] <- 1
    lm_final_coeff$Coefficient[i] <- lm_final_coeff$Estimate[i]
  }
}
rownames(lm_final_coeff) <- NULL
print(xtable(lm_final_coeff[,c(1,5)]), type="html", html.table.attributes="")




# ===================================================================================================================

# PREDICTIONS


train_control <- trainControl(method="cv", number=10, savePredictions = TRUE)


# LINEAR REGRESSION MODEL

# Full model
lm_full_rating_fit <- train(
  score ~ . - title - artist - listeners - playcount,
  data = df,
  method = "lm",
  metric = "RMSE",
  trControl = train_control
  
)
lm_full_rating_fit
# 1.2536 RMSE


# Final model after deleting redundant variables
lm_final_rating_fit <- train(
  score ~ . - title - artist - listeners - playcount - t_X00s - t_electronica - t_female.vocalist - t_abstract
  - t_chillwave - t_minimal.techno - t_rap - t_country - t_jazz - t_gangsta.rap - l_kompakt - t_punk - t_female - t_blues.rock
  - t_X70s - t_thrash.metal - l_anticon - l_anti. - t_japanese - t_acoustic - t_shoegaze - l_legacy - l_carpark - t_hardcore
  - tracks - t_folk.rock - g_experimental - l_def.jam - t_experimental.hip.hop - t_seattle - t_doom - t_new.wave
  - t_britpop - l_arts...crafts - l_parlophone - t_twee - t_dark.ambient - l_hydra.head - t_contemporary.classical - year
  - t_freak.folk - t_heavy.metal - t_glitch - t_funk - t_metalcore - t_synth.pop - t_neo.psychedelia - t_dub - t_progressive.metal
  - t_electropop - l_xsmall.label - l_ninja.tune - t_french - t_stoner.metal - t_disco - t_atmospheric.black.metal - t_indie.electronic
  - t_X2010s - t_chill - t_twee.pop - t_psychedelic.folk - t_guitar - t_alternative.hip.hop - t_neo.soul - t_dubstep - t_emusic
  - l_barsuk - t_dance - t_instrumental - t_live - t_math.rock - t_female.vocalists - t_darkwave - t_world - t_ethereal - l_X.k7
  - t_X2000s - l_fat.possum - t_indietronica - t_mathcore - t_brooklyn - t_idm - t_german - t_grindcore - l_yep.roc - l_emi,
  data = df,
  method = "lm",
  metric = "RMSE",
  trControl = train_control
)
lm_final_rating_fit
# 1.2431 RMSE



# DECISION TREE PREDICTIONS

grid = expand.grid(cp = c(0.001, 0.0025, 0.005, 0.0075, 0.01))

# Full model
dt_rating_fit <- train(
  score ~ . - title - artist - listeners - playcount,
  data = df,
  method = "rpart",
  metric = "RMSE",
  trControl = train_control,
  tuneGrid = grid
)
dt_rating_fit
# 1.2827 RMSE with cp = 0.0025

par(bg=NA)
plot(dt_rating_fit$finalModel)
text(dt_rating_fit$finalModel)
dev.copy(png,'Rating_Full_Tree.png')
dev.off()




# Final model after deleting redundant variables
dt_final_rating_fit <- train(
  score ~ . - title - artist - listeners - playcount - t_X00s - t_electronica - t_female.vocalist - t_abstract
  - t_chillwave - t_minimal.techno - t_rap - t_country - t_jazz - t_gangsta.rap - l_kompakt - t_punk - t_female - t_blues.rock
  - t_X70s - t_thrash.metal - l_anticon - l_anti. - t_japanese - t_acoustic - t_shoegaze - l_legacy - l_carpark - t_hardcore
  - tracks - t_folk.rock - g_experimental - l_def.jam - t_experimental.hip.hop - t_seattle - t_doom - t_new.wave
  - t_britpop - l_arts...crafts - l_parlophone - t_twee - t_dark.ambient - l_hydra.head - t_contemporary.classical - year
  - t_freak.folk - t_heavy.metal - t_glitch - t_funk - t_metalcore - t_synth.pop - t_neo.psychedelia - t_dub - t_progressive.metal
  - t_electropop - l_xsmall.label - l_ninja.tune - t_french - t_stoner.metal - t_disco - t_atmospheric.black.metal - t_indie.electronic
  - t_X2010s - t_chill - t_twee.pop - t_psychedelic.folk - t_guitar - t_alternative.hip.hop - t_neo.soul - t_dubstep - t_emusic
  - l_barsuk - t_dance - t_instrumental - t_live - t_math.rock - t_female.vocalists - t_darkwave - t_world - t_ethereal - l_X.k7
  - t_X2000s - l_fat.possum - t_indietronica - t_mathcore - t_brooklyn - t_idm - t_german - t_grindcore - l_yep.roc - l_emi,
  data = df,
  method = "rpart",
  metric = "RMSE",
  trControl = train_control,
  tuneGrid = grid
)
dt_final_rating_fit
# 1.2831 RMSE with cp = 0.0025

par(bg=NA)
plot(dt_final_rating_fit$finalModel)
text(dt_final_rating_fit$finalModel)
dev.copy(png,'Rating_Final_Tree.png')
dev.off()

# KNN MODEL

grid = expand.grid(k = c(10, 20, 30, 40, 50))

# Full KNN model
knn_full_rating_fit <- train(
  score ~ . - title - artist - listeners - playcount,
  data = df,
  method = "knn",
  metric = "RMSE",
  trControl = train_control,
  tuneGrid = grid
  
)
knn_full_rating_fit
# 1.2667 RMSE for k = 50


# Final KNN model
knn_final_rating_fit <- train(
  score ~ . - title - artist - listeners - playcount - t_X00s - t_electronica - t_female.vocalist - t_abstract
  - t_chillwave - t_minimal.techno - t_rap - t_country - t_jazz - t_gangsta.rap - l_kompakt - t_punk - t_female - t_blues.rock
  - t_X70s - t_thrash.metal - l_anticon - l_anti. - t_japanese - t_acoustic - t_shoegaze - l_legacy - l_carpark - t_hardcore
  - tracks - t_folk.rock - g_experimental - l_def.jam - t_experimental.hip.hop - t_seattle - t_doom - t_new.wave
  - t_britpop - l_arts...crafts - l_parlophone - t_twee - t_dark.ambient - l_hydra.head - t_contemporary.classical - year
  - t_freak.folk - t_heavy.metal - t_glitch - t_funk - t_metalcore - t_synth.pop - t_neo.psychedelia - t_dub - t_progressive.metal
  - t_electropop - l_xsmall.label - l_ninja.tune - t_french - t_stoner.metal - t_disco - t_atmospheric.black.metal - t_indie.electronic
  - t_X2010s - t_chill - t_twee.pop - t_psychedelic.folk - t_guitar - t_alternative.hip.hop - t_neo.soul - t_dubstep - t_emusic
  - l_barsuk - t_dance - t_instrumental - t_live - t_math.rock - t_female.vocalists - t_darkwave - t_world - t_ethereal - l_X.k7
  - t_X2000s - l_fat.possum - t_indietronica - t_mathcore - t_brooklyn - t_idm - t_german - t_grindcore - l_yep.roc - l_emi,
  data = df,
  method = "knn",
  metric = "RMSE",
  trControl = train_control,
  tuneGrid = grid
  
)
knn_final_rating_fit
#1.2731 RMSE with k = 40





































# ===================================================================================================================
# ===================================================================================================================
# ===================================================================================================================

# LINEAR REGRESSION ANALYSIS PLAYS

hist(df$playcount)
df$score <- (df$score -min(df$score))/(max(df$score)-min(df$score))


# Full linear model including all features in the dataset except for title, artist, listeners and playcount
lm_plays_full <- lm(
  playcount ~ . - title - artist - listeners,
  data = df
)
summary(lm_plays_full)
# Adjusted R^2: 0.1495


# Incrimentally take out insignificant variables until the adjusted R^2 decreases
lm_plays_final <- lm(
  playcount ~ . - title - artist - listeners - t_reggae - t_ambient - t_underground.hip.hop - l_constellation
  - t_dark.ambient - l_profound.lore - l_kompakt - t_math.rock - l_kill.rock.stars - t_swedish - t_noise.pop
  - t_stoner.metal - t_glitch - t_art.pop - t_psychedelic.folk - t_cover - l_carpark - l_polyvinyl - t_alt.country - t_electronica
  - l_secretly.canadian - tracks - t_free.jazz - t_contemporary.classical - l_thrill.jockey - t_sludge.metal
  - l_relapse - l_rhino - l_paper.bag - l_southern.lord - t_experimental - l_jagjaguwar - l_sacred.bones - l_kranky - t_french
  - t_drone - t_funk - l_light.in.the.attic - t_post.hardcore - t_hardcore - t_british - l_ipecac - t_jazz - t_pop.punk - l_numero.group
  - t_freak.folk - l_xsmall.label - t_gammarec - t_heavy.metal - t_darkwave - l_ninja.tune - t_r.b - t_world - l_mexican.summer - t_grunge
  - g_pop.r.b - t_sludge - t_stoner.rock - t_shoegaze - l_rough.trade - t_post.metal - l_dead.oceans - l_anticon - g_jazz
  - t_metalcore - t_female.vocalist - l_ghostly - t_trap - t_neo.soul - t_chillwave - l_ato - t_country - t_alternative.hip.hop
  - l_touch.and.go - l_hydra.head - t_psychedelic - t_grindcore - duration - t_doom - t_hardcore.punk - t_X2010s - t_deep.house
  - t_avant.garde - t_pop - l_small.label - t_soundtrack - l_saddle.creek - t_noise.rock - t_dubstep - l_vagrant - t_ethereal
  - l_X.k7 - t_grime - t_chillout - t_guitar - t_atmospheric.black.metal - t_minimal.techno - t_thrash.metal - l_dfa
  - l_temporary.residence - l_fat.cat - t_punk - t_emd - l_yep.roc - t_post.rock - t_japanese - t_twee.pop - t_synth.pop - t_black.metal
  - t_instrumental - t_female.vocalists - t_electroclash - t_singer.songwriter - l_smalltown.supersound - t_X60s - t_house
  - t_brooklyn - l_anti. - t_X90s - l_drag.city - t_blues - t_X80s - t_pop.rock - t_techno - t_experimental.hip.hop - g_rock - t_idm
  - t_female - t_krautrock - t_piano - l_k - t_emusic - t_dub - t_folk - t_slowcore - t_folk.rock,
  data = df
)
summary(lm_plays_final)
# Adj R2 increased from 0.1495 to 0.1577
#sort(summary(lm_plays_final)$coefficients[,4],decreasing = FALSE)
plot(lm_plays_final)












lm_plays_coeff <- as.data.frame(summary(lm_plays_final)$coefficients[,c(1,4)])
lm_plays_coeff <- tibble::rownames_to_column(lm_plays_coeff, "Variable")
lm_plays_coeff$Estimate <- round(lm_plays_coeff$Estimate,0)
lm_plays_coeff <- lm_plays_coeff[order(lm_plays_coeff$Estimate, decreasing = TRUE),]

for (i in 1:nrow(lm_plays_coeff)){  
  if (lm_plays_coeff$`Pr(>|t|)`[i] < 0.001){
    lm_plays_coeff$Sig[i] <- 0.001
    lm_plays_coeff$Coefficient[i] <- paste(lm_plays_coeff$Estimate[i],"***", sep = " ")
  } else if(lm_plays_coeff$`Pr(>|t|)`[i] < 0.01) {
    lm_plays_coeff$Sig[i] <- 0.01
    lm_plays_coeff$Coefficient[i] <- paste(lm_plays_coeff$Estimate[i],"**", sep = " ")
  } else if(lm_plays_coeff$`Pr(>|t|)`[i] < 0.05) {
    lm_plays_coeff$Sig[i] <- 0.05
    lm_plays_coeff$Coefficient[i] <- paste(lm_plays_coeff$Estimate[i],"*", sep = " ")
  } else if(lm_plays_coeff$`Pr(>|t|)`[i] < 0.1) {
    lm_plays_coeff$Sig[i] <- 0.1
    lm_plays_coeff$Coefficient[i] <- paste(lm_plays_coeff$Estimate[i],".", sep = " ")
  } else {
    lm_plays_coeff$Sig[i] <- 1
    lm_plays_coeff$Coefficient[i] <- lm_plays_coeff$Estimate[i]
  }
}
rownames(lm_plays_coeff) <- NULL
print(xtable(lm_plays_coeff[,c(1,5)]), type="html", html.table.attributes="")








# ===================================================================================================================
# PREDICTION PLAYS

# LINEAR REGRESSION MODEL

# Full model
lm_full_plays_fit <- train(
  playcount ~ . - title - artist - listeners,
  data = df,
  method = "lm",
  metric = "RMSE",
  trControl = train_control
  
)
lm_full_plays_fit
#RMSE: 4.066.639



# Final model after deleting redundant variables
lm_final_plays_fit <- train(
  playcount ~ . - title - artist - listeners - t_reggae - t_ambient - t_underground.hip.hop - l_constellation
  - t_dark.ambient - l_profound.lore - l_kompakt - t_math.rock - l_kill.rock.stars - t_swedish - t_noise.pop
  - t_stoner.metal - t_glitch - t_art.pop - t_psychedelic.folk - t_cover - l_carpark - l_polyvinyl - t_alt.country - t_electronica
  - l_secretly.canadian - tracks - t_free.jazz - t_contemporary.classical - l_thrill.jockey - t_sludge.metal
  - l_relapse - l_rhino - l_paper.bag - l_southern.lord - t_experimental - l_jagjaguwar - l_sacred.bones - l_kranky - t_french
  - t_drone - t_funk - l_light.in.the.attic - t_post.hardcore - t_hardcore - t_british - l_ipecac - t_jazz - t_pop.punk - l_numero.group
  - t_freak.folk - l_xsmall.label - t_gammarec - t_heavy.metal - t_darkwave - l_ninja.tune - t_r.b - t_world - l_mexican.summer - t_grunge
  - g_pop.r.b - t_sludge - t_stoner.rock - t_shoegaze - l_rough.trade - t_post.metal - l_dead.oceans - l_anticon - g_jazz
  - t_metalcore - t_female.vocalist - l_ghostly - t_trap - t_neo.soul - t_chillwave - l_ato - t_country - t_alternative.hip.hop
  - l_touch.and.go - l_hydra.head - t_psychedelic - t_grindcore - duration - t_doom - t_hardcore.punk - t_X2010s - t_deep.house
  - t_avant.garde - t_pop - l_small.label - t_soundtrack - l_saddle.creek - t_noise.rock - t_dubstep - l_vagrant - t_ethereal
  - l_X.k7 - t_grime - t_chillout - t_guitar - t_atmospheric.black.metal - t_minimal.techno - t_thrash.metal - l_dfa
  - l_temporary.residence - l_fat.cat - t_punk - t_emd - l_yep.roc - t_post.rock - t_japanese - t_twee.pop - t_synth.pop - t_black.metal
  - t_instrumental - t_female.vocalists - t_electroclash - t_singer.songwriter - l_smalltown.supersound - t_X60s - t_house
  - t_brooklyn - l_anti. - t_X90s - l_drag.city - t_blues - t_X80s - t_pop.rock - t_techno - t_experimental.hip.hop - g_rock - t_idm
  - t_female - t_krautrock - t_piano - l_k - t_emusic - t_dub - t_folk - t_slowcore - t_folk.rock,
  data = df,
  method = "lm",
  metric = "RMSE",
  trControl = train_control
)
lm_final_plays_fit
# RMSE: 4.053.860



# DECISION TREE PREDICTIONS

grid = expand.grid(cp = c(0.001, 0.0025, 0.005, 0.0075, 0.01))

# Full model
dt_plays_fit <- train(
  playcount ~ . - title - artist - listeners,
  data = df,
  method = "rpart",
  metric = "RMSE",
  trControl = train_control,
  tuneGrid = grid
)
dt_plays_fit
# RMSE: 4.231.604 for 0.01 cp
par(bg=NA)
plot(dt_plays_fit$finalModel)
text(dt_plays_fit$finalModel, cex = 0.6, digits = 1)
dev.copy(png,'Plays_Full_Tree.png')
dev.off()




# Final model after deleting redundant variables
dt_final_plays_fit <- train(
  playcount ~ . - title - artist - listeners - t_reggae - t_ambient - t_underground.hip.hop - l_constellation
  - t_dark.ambient - l_profound.lore - l_kompakt - t_math.rock - l_kill.rock.stars - t_swedish - t_noise.pop
  - t_stoner.metal - t_glitch - t_art.pop - t_psychedelic.folk - t_cover - l_carpark - l_polyvinyl - t_alt.country - t_electronica
  - l_secretly.canadian - tracks - t_free.jazz - t_contemporary.classical - l_thrill.jockey - t_sludge.metal
  - l_relapse - l_rhino - l_paper.bag - l_southern.lord - t_experimental - l_jagjaguwar - l_sacred.bones - l_kranky - t_french
  - t_drone - t_funk - l_light.in.the.attic - t_post.hardcore - t_hardcore - t_british - l_ipecac - t_jazz - t_pop.punk - l_numero.group
  - t_freak.folk - l_xsmall.label - t_gammarec - t_heavy.metal - t_darkwave - l_ninja.tune - t_r.b - t_world - l_mexican.summer - t_grunge
  - g_pop.r.b - t_sludge - t_stoner.rock - t_shoegaze - l_rough.trade - t_post.metal - l_dead.oceans - l_anticon - g_jazz
  - t_metalcore - t_female.vocalist - l_ghostly - t_trap - t_neo.soul - t_chillwave - l_ato - t_country - t_alternative.hip.hop
  - l_touch.and.go - l_hydra.head - t_psychedelic - t_grindcore - duration - t_doom - t_hardcore.punk - t_X2010s - t_deep.house
  - t_avant.garde - t_pop - l_small.label - t_soundtrack - l_saddle.creek - t_noise.rock - t_dubstep - l_vagrant - t_ethereal
  - l_X.k7 - t_grime - t_chillout - t_guitar - t_atmospheric.black.metal - t_minimal.techno - t_thrash.metal - l_dfa
  - l_temporary.residence - l_fat.cat - t_punk - t_emd - l_yep.roc - t_post.rock - t_japanese - t_twee.pop - t_synth.pop - t_black.metal
  - t_instrumental - t_female.vocalists - t_electroclash - t_singer.songwriter - l_smalltown.supersound - t_X60s - t_house
  - t_brooklyn - l_anti. - t_X90s - l_drag.city - t_blues - t_X80s - t_pop.rock - t_techno - t_experimental.hip.hop - g_rock - t_idm
  - t_female - t_krautrock - t_piano - l_k - t_emusic - t_dub - t_folk - t_slowcore - t_folk.rock,
  data = df,
  method = "rpart",
  metric = "RMSE",
  trControl = train_control,
  tuneGrid = grid
)
dt_final_plays_fit
# RMSE: 4.213.601 for cp = 0.0025
par(bg=NA)
plot(dt_final_plays_fit$finalModel)
text(dt_final_plays_fit$finalModel, cex = 0.6, digits = 1)
dev.copy(png,'Plays_Final_Tree.png')
dev.off()



# KNN MODEL PREDICTIONS

grid = expand.grid(k = c(10, 20, 30, 40, 50))

# Full KNN model
knn_full_plays_fit <- train(
  playcount ~ . - title - artist - listeners,
  data = df,
  method = "knn",
  metric = "RMSE",
  trControl = train_control,
  tuneGrid = grid
  
)
knn_full_plays_fit
# RMSE: 4.054.056 for k = 20


# Final KNN model
knn_final_plays_fit <- train(
  playcount ~ . - title - artist - listeners - t_reggae - t_ambient - t_underground.hip.hop - l_constellation
  - t_dark.ambient - l_profound.lore - l_kompakt - t_math.rock - l_kill.rock.stars - t_swedish - t_noise.pop
  - t_stoner.metal - t_glitch - t_art.pop - t_psychedelic.folk - t_cover - l_carpark - l_polyvinyl - t_alt.country - t_electronica
  - l_secretly.canadian - tracks - t_free.jazz - t_contemporary.classical - l_thrill.jockey - t_sludge.metal
  - l_relapse - l_rhino - l_paper.bag - l_southern.lord - t_experimental - l_jagjaguwar - l_sacred.bones - l_kranky - t_french
  - t_drone - t_funk - l_light.in.the.attic - t_post.hardcore - t_hardcore - t_british - l_ipecac - t_jazz - t_pop.punk - l_numero.group
  - t_freak.folk - l_xsmall.label - t_gammarec - t_heavy.metal - t_darkwave - l_ninja.tune - t_r.b - t_world - l_mexican.summer - t_grunge
  - g_pop.r.b - t_sludge - t_stoner.rock - t_shoegaze - l_rough.trade - t_post.metal - l_dead.oceans - l_anticon - g_jazz
  - t_metalcore - t_female.vocalist - l_ghostly - t_trap - t_neo.soul - t_chillwave - l_ato - t_country - t_alternative.hip.hop
  - l_touch.and.go - l_hydra.head - t_psychedelic - t_grindcore - duration - t_doom - t_hardcore.punk - t_X2010s - t_deep.house
  - t_avant.garde - t_pop - l_small.label - t_soundtrack - l_saddle.creek - t_noise.rock - t_dubstep - l_vagrant - t_ethereal
  - l_X.k7 - t_grime - t_chillout - t_guitar - t_atmospheric.black.metal - t_minimal.techno - t_thrash.metal - l_dfa
  - l_temporary.residence - l_fat.cat - t_punk - t_emd - l_yep.roc - t_post.rock - t_japanese - t_twee.pop - t_synth.pop - t_black.metal
  - t_instrumental - t_female.vocalists - t_electroclash - t_singer.songwriter - l_smalltown.supersound - t_X60s - t_house
  - t_brooklyn - l_anti. - t_X90s - l_drag.city - t_blues - t_X80s - t_pop.rock - t_techno - t_experimental.hip.hop - g_rock - t_idm
  - t_female - t_krautrock - t_piano - l_k - t_emusic - t_dub - t_folk - t_slowcore - t_folk.rock,
  data = df,
  method = "knn",
  metric = "RMSE",
  trControl = train_control,
  tuneGrid = grid
  
)
knn_final_plays_fit
# RMSE: 4.032.367 for k = 10































