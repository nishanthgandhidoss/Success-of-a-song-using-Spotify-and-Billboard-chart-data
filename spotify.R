# loading the dataset
songsList <- read.csv("song-list.csv")

# function to Install packages
installNewPackage <- function(packageName) {
        if(packageName  %in% rownames(installed.packages()) == FALSE) 
        {
                install.packages(packageName, repos = "http://cran.us.r-project.org", dependencies=TRUE)
        }
}

# installing packages
installNewPackage("httr")
installNewPackage("jsonlite")
installNewPackage("httpuv")
# if("spotifyr"  %in% rownames(installed.packages()) == FALSE)
#         devtools::install_github('rweyant/spotifyr')

# loading the packages
library("httr")
library("jsonlite")
library("httpuv")
# library("spotifyr")


# ============================================================================


# Intialization of variables
clientKey <- '7065b7a80f43444796c73ef454903cdd'
clientSecret <- '5d65bc39a92349659e3a16634ef1680f'
userID <- '22uhyoyypkqsrhpmvjfchltva'
playlistID <- '6bkAwdiDi9lQG0IHISpwh0'
redirectURL <- 'http://localhost:1410/'


# ============================================================================


# Token
response <- POST('https://accounts.spotify.com/api/token', 
                 accept_json(), authenticate(clientKey, clientSecret), 
                 body = list(grant_type = 'client_credentials'), 
                 encode = 'form', verbose())

mytoken <- content(response)$access_token
HeaderValue <- paste0('Bearer ', mytoken)

# oauth
spotifyEndpoint <- oauth_endpoint("spotify",
                                  "https://accounts.spotify.com/authorize",
                                  "https://accounts.spotify.com/api/token")
spotifyApp <- oauth_app("spotify", clientKey, clientSecret)
options(httr_oob_default=FALSE) 
spotifyToken <- oauth2.0_token(spotifyEndpoint, spotifyApp, scope = "playlist-modify-public")
authToken <- spotifyToken$credentials$access_token


# ============================================================================


# Get the song SpotifyID based on title and Artist - Start
# Got 305 songs songs
songListIDs <- list()
songListName <- list()
failedSongListName <- list()
failedSongListNo <- list()
fullSongListIDs <- list()
pass <- 0
fail <- 0
for(i in 1:nrow(songsList)) {
        
        trackSearchURL <- URLencode(paste("https://api.spotify.com/v1/search/?q=",
                                          songsList$Title[i],
                                          "&type=track",
                                          sep = ""), reserved = FALSE, repeated = FALSE)
        getSong <- GET(url=trackSearchURL, add_headers(Authorization = HeaderValue))
        songInfo <- fromJSON(toJSON(content(getSong)))
        for(j in 1:length(songInfo$tracks$items$artists))  {
                if(songsList$Artist[i] %in% as.vector(unlist(songInfo$tracks$items$artists[j]))) {
                        songListName[length(songListName) + 1] <- songInfo$tracks$items$name[j]
                        songListIDs[length(songListIDs) + 1] <- songInfo$tracks$items$id[j]
                        fullSongListIDs[length(fullSongListIDs) + 1] <- songInfo$tracks$items$id[j]
                        pass<-pass+1
                        break;
                }
                else {
                        failedSongListName[length(failedSongListName) + 1] <- songInfo$tracks$items$name[j]
                        fullSongListIDs[length(fullSongListIDs) + 1] <- NA
                        if(!is.null(unlist(songInfo$tracks$items$name[j]))){
                                failedSongListNo[length(failedSongListNo) + 1] <- i
                                print(paste(i, songInfo$tracks$items$name[j], sep = "---"))
                        }
                        fail<-fail+1
                        break;
                }
        }
}
songsFound <- data.frame(unlist(songListName), unlist(songListIDs))
songsNotFound <- data.frame(unlist(failedSongListNo), unlist(failedSongListName))
colnames(songsFound) <- c("Name", "ID")
colnames(songsNotFound) <- c("No", "Name")

# Preparing the csv file for songs found and not found
write.csv(songsFound, file = "songs-found.csv")
write.csv(songsNotFound, file = "songs-not-found.csv")

# Get the song SPotifyID based on title and Artist - End


# ============================================================================


# Manupulating id column in songList Dataset - Start

assign("songsListDup", songsList)
songsListDup["id"] <- unlist(songListIDs)

songsNotFoundList <- read.csv("songs-not-found.csv")
songsNotFoundList <-  na.omit(songsNotFoundList)
songsNotFoundList$Track.ID
songsNotFoundList$No
for(i in 1:nrow(songsNotFoundList)) {
        songsListDup$id[songsNotFoundList$No[i]] <- as.character(paste(songsNotFoundList$Track.ID[i]))
}

spotifyTrackConstant <- 'spotify:track:'
songuri[1] <- paste(spotifyTrackConstant, songsNotFoundList$Track.ID[1], sep="")
for(i in 2:nrow(songsNotFoundList)) {
        songuri <- paste(songuri, ',', spotifyTrackConstant, songsNotFoundList$Track.ID[i], sep = "")
}

# Manupulating id column in songList Dataset - End


# ============================================================================


# forming spotify URL - Start

# songuri has for 305 songs 
# split it before passing to api

spotifyTrackConstant <- 'spotify:track:'
songuri[1] <- paste(spotifyTrackConstant, songsFound$ID[1], sep="")
for(i in 2:nrow(songsFound)) {
       songuri <- paste(songuri, ',', spotifyTrackConstant, songsFound$ID[i], sep = "")
}

# forming spotify URL - End


# ============================================================================


# Adding Song to playlist - Start

songuri <- 'spotify:track:4iV5W9uYEdYUVa79Axb7Rh,spotify:track:1301WleyT98MSxVHPZCA6M'
addTrackURL <- URLencode(paste('https://api.spotify.com/v1/users/',
                     userID,'/playlists/',playlistID,'/tracks?uris=',
                     songuri,
                     sep=''))
responseAddTrack <- POST(addTrackURL, add_headers(Authorization = paste('Bearer', spotifyToken$credentials$access_token, sep = " "),
                                     Accept = 'application/json'))
info <- fromJSON(toJSON(content(responseAddTrack)))

# Adding Song to playlist - Start


# ============================================================================


# Getting tracks from playlist - Start


flag <- TRUE
loop <- 0
i <- 0
fullTrackIDs <- list()
track_str <- list()
audioFeatures <- data.frame()
while(flag) {
        i <- i + 1
        playlistTracksURL <- paste("https://api.spotify.com/v1/users/",
                                   userID,
                                   "/playlists/",
                                   playlistID,
                                   "/tracks/?offset=",
                                   loop,
                                   sep="")
        getTracks <- GET(url=playlistTracksURL, add_headers(Authorization = HeaderValue))
        info <- fromJSON(toJSON(content(getTracks)))
        fullTrackIDs[[i]] <- unlist(info$items$track$id)
        track_str[[i]] <- fullTrackIDs[[i]][1]
        for (j in 2:length(fullTrackIDs[[i]])) {
                track_str[[i]] <- paste(track_str[[i]], fullTrackIDs[[i]][j], sep=",")
        }
        trackFeaturesURL <- paste("https://api.spotify.com/v1/audio-features/?ids=",
                                  track_str[[i]],
                                  sep="")
        getSongFeautures <- GET(url=trackFeaturesURL, add_headers(Authorization = HeaderValue))
        songFeauturesInfo <- fromJSON(toJSON(content(getSongFeautures)))
        audioFeatures <- rbind.data.frame(audioFeatures, songFeauturesInfo$audio_features)
        if(length(unlist(info$items$track$id)) != 100) {
                break;
        }
        loop <- loop + 100
}

# Getting tracks from playlist - End

# ============================================================================

# Merge two Songlist and audio features by id column - Start

df <- merge(songsListDup, audioFeatures, "id", all = TRUE)
df <- na.omit(df) # Missing data
df <- data.frame(lapply(df, as.character), stringsAsFactors=FALSE)
write.csv(df, file = "df.csv")
# Merge two Songlist and audio features by id column - 