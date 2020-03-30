rm(list=ls())

# Load packages & data
library("reshape2")
library("ggplot2")
library("dplyr")
library("tidyr")


pitchfork <- read.csv("pitchfork.csv", header = TRUE,  encoding = "UTF-8")


# Convert to appropriate data types
pitchfork$reviewid <- as.character(pitchfork$reviewid)
pitchfork$title <- as.character(pitchfork$title)
pitchfork$artist <- as.character(pitchfork$artist)
pitchfork$url <- as.character(pitchfork$url)
pitchfork$pub_date <- as.Date(pitchfork$pub_date, format = "%Y-%m-%d")
pitchfork$content <- as.character(pitchfork$content)
pitchfork$label <- as.character(pitchfork$label)

pitchfork$genre <- as.character(pitchfork$genre)
pitchfork$genre[pitchfork$genre == ""] <- "NA"                        # If there is no specified genre, impute NA
pitchfork$genre <- as.factor(pitchfork$genre)

# Remove duplicate entries
pitchfork <- pitchfork %>% distinct()


# Flatten dateframe by one-hot encoding genres (albums can have multiple genres)
df_ohe <- dcast(pitchfork, reviewid + title + artist + url + score + best_new_music + author + author_type +
              pub_date + pub_weekday + pub_day + pub_month + pub_year + year + content + label ~ genre, value.var = "genre", fun = length)
# Name the one-hot encoded genres
names(df_ohe)[17:26] <- c("g_na", "g_electronic", "g_experimental", "g_folk.country", "g_global", "g_jazz", "g_metal", "g_pop.r.b",
                      "g_rap", "g_rock")

#We want to do the same thing wrt record labels but since there are more than 3000 unique values, group the smaller labels together

# If the record label occurs less than 3 times transform the label name into "xsmall label"
df_ohe$label[df_ohe$label %in% names(which(table(df_ohe$label) < 3))] <- "xsmall label"
# If the record label occurs between 3 and 9 times transform the label name into "small label"
df_ohe$label[df_ohe$label %in% names(which(table(df_ohe$label) > 2 & table(df_ohe$label) < 10))] <- "small label"
# If the record label occurs between 10 and 24 times transform the label name into "medium label"
df_ohe$label[df_ohe$label %in% names(which(table(df_ohe$label) > 9 & table(df_ohe$label) < 25))] <- "medium label"
# If the record label occurs between 25 and 49 times transform the label name into "large label"
df_ohe$label[df_ohe$label %in% names(which(table(df_ohe$label) > 24 & table(df_ohe$label) < 50))] <- "large label"

# Flatten dateframe by one-hot encoding record labels (albums can have multiple record labels)
df_ohe <- dcast(df_ohe, reviewid + title + artist + url + score + best_new_music + author + author_type +
                  pub_date + pub_weekday + pub_day + pub_month + pub_year + year + content  + 
                  g_na + g_electronic + g_experimental + g_folk.country + g_global + g_jazz + g_metal +
                  g_pop.r.b + g_rap + g_rock ~ label, value.var = "label", fun = length)

#colSums(df_ohe[26:104])

# Some albums have multiple reviews because they have been re-released for example. 
# We only keep the first release of the record or in case the release year is the same, the first review published.
# This also removes albums without a specified year.
df_ohe <- df_ohe %>%
  group_by(artist, title) %>%
  filter(year == min(year),
         pub_date == min(pub_date))


# Remove last duplicate manually because it is the same review with different review id's
df_ohe <- df_ohe[df_ohe$reviewid != "4175",]


ggplot(df_ohe, aes(x = year, y = score, color = pub_year)) +
  geom_point(size = 2) +
  scale_colour_gradient(low = "#E68A99", high = "#A3B9AA") +
  theme(plot.title = element_text(hjust = 0.5),
        panel.background = element_rect(fill = "grey85"),
        plot.background = element_rect(fill = "transparent", color = NA),
        strip.background = element_rect(fill = "grey75", colour = NA),
        legend.background = element_rect(fill = "transparent", color = NA)) +
  ggtitle("Album Score per Year") +                     
  xlab("Album Release Year") +                                  
  ylab("Album Score") +    
  labs(fill = "Year Published") +
  ggsave(filename = "Album_Score_per_Year.png",                      
         bg = "transparent", dpi = 600)





df_ohe_long <- gather(df_ohe, key="genre", value="value", c("g_rock", "g_electronic", "g_experimental", "g_folk.country", "g_global",
                                                            "g_jazz", "g_metal", "g_pop.r.b", "g_rap", "g_na"))
df_ohe_long <- df_ohe_long[df_ohe_long$value == 1,]

ggplot(df_ohe_long, aes(x = year, fill = genre)) +
  geom_bar(stat = "count") +
  facet_wrap(~genre) +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5),
        panel.background = element_rect(fill = "grey85"),
        plot.background = element_rect(fill = "transparent", color = NA),
        strip.background = element_rect(fill = "grey75", colour = NA),
        legend.background = element_rect(fill = "transparent", color = NA),
        panel.spacing = unit(1.0, "lines")) +
  ggtitle("Albums Reviewed per Year by Genre") +                     
  xlab("Album Release Year") +                                  
  ylab("Album Count") +    
  labs(fill = "Genre") +
  ggsave(filename = "Albums_Released_per_Genre.png",                      
         bg = "transparent", dpi = 600)
  


df_ohe_long_label <- gather(df_ohe, key="label", value="value", names(df_ohe[,26:104]))
df_ohe_long_label <- df_ohe_long_label[df_ohe_long_label$value == 1,]

df_ohe_long_label %>%
  select(reviewid,score, label) %>%
  group_by(label) %>%
  summarise(
    album_count = n(),
    avg_score = mean(score, na.rm = FALSE)) %>%
  top_n(20, wt = album_count) %>%
  arrange_(~ desc(album_count)) %>%
  ggplot(., aes(x = reorder(label,-album_count), y=album_count)) +
  geom_bar(stat = "identity", fill = "#E68A99", col = "#b45867", alpha = 0.7) +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5),
        panel.background = element_rect(fill = "grey85"),
        plot.background = element_rect(fill = "transparent", color = NA),
        strip.background = element_rect(fill = "grey75", colour = NA),
        legend.background = element_rect(fill = "transparent", color = NA)) +
  ggtitle("Record Label Distribution") +                     
  xlab("Record Label") +                                  
  ylab("Album Count") + 
  geom_text(stat='identity', aes(label=album_count), vjust=-0.5) +
  ggsave(filename = "Record_Label_Distribution.png",                      
         bg = "transparent", dpi = 600)



write.csv(df_ohe, "pitchfork_ohe.csv", row.names = FALSE)

albums <- df_ohe[,c(1,2)]
write.csv(albums, "albums.csv", row.names = FALSE)

