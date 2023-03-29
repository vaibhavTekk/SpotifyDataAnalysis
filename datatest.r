install.packages('spotifyr')
library(spotifyr)
Sys.setenv(SPOTIFY_CLIENT_ID = 'fecaca040b82440c8cc8f31516e47050')
Sys.setenv(SPOTIFY_CLIENT_SECRET = 'e8db744eb9e94395952577c56195245f')
access_token <- get_spotify_access_token()

categories <- get_categories()
View(categories)
id_tamil = categories$id[4]
View(id_tamil)

tamil_playlists <- get_category_playlists(id_tamil)
View(tamil_playlists)

trackdetails <- get_playlist_tracks(tamil_playlists$id[6])
View(trackdetails$track.id)
audio_features <- get_track_audio_features(trackdetails$track.id)
View(audio_features)

#audio_features <- audio_features[!(audio_features$energy>0.5),]
danceability <- audio_features$danceability
energy <- audio_features$energy


plot(x = energy, y = danceability,
     xlab = "Energy",
     ylab = "Danceability",
     ylim = c(min(danceability) - 0.2, max(danceability) + 0.2),
     xlim = c(min(energy) - 0.2, max(energy) +0.2),       
     main = "Danceability vs Energy"
)

de_lm <- lm(formula = danceability ~ energy)
abline(de_lm)
