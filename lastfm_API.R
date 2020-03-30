rm(list=ls())

# Import jsonlite package to connect to the API
# Import stringr for text manipulation after collecting the data
library("jsonlite")
library("stringr")

# Read the .csv files containing titles and artists of nearly 18000 music albums from our Pitchfork dataset
albums <- read.csv("albums.csv", header = TRUE,  encoding = "UTF-8")

# Define API key obtained from Last FM
api_key = "INSERT API KEY HERE"


# Change the type of the title and artist features to be character strings
albums$title <- as.character(albums$title)
albums$artist <- as.character(albums$artist)

# Initialize new dataframe by copying the original albums dataframe and defining new features of data we will request from the Last FM API
album_info$LFM_title <- 0     # Album title according to Last FM
album_info$LFM_artist <- 0    # Artist according to Last FM
album_info$listeners <- 0     # Number of album listeners on Last FM
album_info$playcount <- 0     # Playcount of album on Last FM
album_info$duration <- 0      # Duration of all the tracks on the album combined
album_info$tracks <- 0        # Number of tracks on the album
album_info$tags <- 0          # Most popular tags for each album 


# Find index of album titles that include hashtags
hashtags_titles <- grep("#", album_info[,1])
# Remove hashtags from album titles
for(i in hashtags_titles){
  album_info[i,1] <- gsub("#","",album_info[i,1],fixed = TRUE)
}

# Find index of artist names that include hashtags
hashtags_artists <- grep("#", album_info[,2])
# Remove hashtags from artist names
album_info[hashtags_artists,2] <- gsub("#","",album_info[hashtags_artists,2],fixed = TRUE)



# Initialize url variable which will contain the API request url
url = as.character()

# Iteratively request additional album data for each album from the Last FM API
for(i in 1:nrow(album_info)){
  
  url[i] <- paste("http://ws.audioscrobbler.com/2.0/?method=album.getinfo&api_key=",api_key,"&artist=",album_info[i,2],"&album=",album_info[i,1],"&format=json&autocorrect[1]", sep = "")
  info <- fromJSON(url[i]) # Store album information retrieved from API call
  
  
  if("error" %in% names(info)){                     # In case of an error: 
    album_info[i,3:7] <- "NA"                       # Input "NA" for all of the features and skip to the next album
    
  } else {                                          # In case of no error:
    
    album_info$LFM_title[i] <- info$album$name      # Input album name
    album_info$LFM_artist[i] <- info$album$artist   # Input artist name(s)
    album_info$listeners[i] <- info$album$listeners # Input album listener count
    album_info$playcount[i] <- info$album$playcount # Input album play count
    
    if(length(info$album$tracks$track) == 0){       # In case there is no data on the tracks:
      album_info$tracks[i] <- "NA"                  # Input "NA" for number of tracks
      album_info$duration[i] <- "NA"                # Input "NA" for duration of album
    } else{                                         # In case data on tracks is available:
      album_info$duration[i] <- sum(as.numeric(info$album$tracks$track[,3]))  # Input duration of album
      album_info$tracks[i] <- nrow(info$album$tracks$track)                   # Input number of tracks on album
    }
    
    if(length(info$album$tags$tag) == 0){          # In case there is no data on tags:
      album_info$tags[i] <- "NA"                   # Input "NA" for tags
    } else{                                        # In case data on tags is available:
      album_info$tags[i] <- toString(info$album$tags$tag[,1], sep=",")        # Input the top tags
    }
  }
  Sys.sleep(sample(c(5:10),1))                     # Add 5-10 second delay before the next call
  print(paste("iteration ",i,"/",nrow(album_info), sep = "")); flush.console() # Print progress
}

# Export all the retrieved data to a csv file
write.csv(album_info, "album_info.csv", row.names = FALSE)

# Load the created csv file to clean the data
df <- read.csv("album_info.csv")

# Drop rows with NAs for title, duration or tags
df <- df[!is.na(df$LFM_title) & !is.na(df$duration) & !is.na(df$tags),]

df$tags <- as.character(df$tags)
df$tags <- sapply(df[,9], function(x) gsub( "[^,a-zA-Z0-9\\s]" , " " , x , perl = TRUE )) # Remove special signs from tags
df$tags <- sapply(df[,9], function(x) strsplit(x, split = ", ")) # Split the tags by comma
df$tags <- sapply(df[,9], function(x) tolower(x)) # Make all letters of the tags lower case



'%!in%' <- Negate('%in%') # 'is not in' operator
df$tags <- sapply(df[,9], function(x) x[x %!in% c(1800:2100)])
df$tags <- sapply(df[,9], function(x) x[x %!in% c("vinyl", "wishlist", "seen live", "rather good stuff", "freepurp1e", "my gang 10", "playlist", "wfmu heavily played records",
                                                    "beautiful", "registret", "love at first listen", "to check out", "awesome", "merkliste", "my gang 11")])


# Remove tags that include the words "best of", "releases", "album", "favorites" or "favourites"
df$tags <- sapply(df[,9], function(x) x[!grepl("best of", x)])
df$tags <- sapply(df[,9], function(x) x[!grepl("releases", x)])
df$tags <- sapply(df[,9], function(x) x[!grepl("album", x)])
df$tags <- sapply(df[,9], function(x) x[!grepl("favorites", x)])
df$tags <- sapply(df[,9], function(x) x[!grepl("favourites", x)])



tags <- unlist(df$tags) # Get all tags
tagfreq <- sort(table(tags),decreasing=TRUE)[1:500] # Create table with 500 most used tags
tagdf <- as.data.frame(tagfreq[tagfreq >= 20]) # Create dataframe of tags used more than 20 times
df[,as.character(tagdf[,1])] <- NA # Add tags that occur more than 20 times as empty feature columns to the 'df' dataframe



# One-hot encode empty feature columns
for(i in 1:nrow(df)){
  
  for(j in 1:ncol(df[,-c(1:9)])){
    
    if (colnames(df[j+9]) %in% df[[i,9]]){
      df[i,j+9] <- 1
    } else {
        df[i,j+9] <- 0
      }
  }
}

df$tags <- NULL
write.csv(df[,-9], "lastfm.csv", row.names = FALSE)


