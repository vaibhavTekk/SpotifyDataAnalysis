category <- get_categories()
categoryid <- category$id[9]
playlists <- get_category_playlists(categoryid)
trackids <- get_playlist_tracks(playlists$id[3])$track.id
audio_features <- get_track_audio_features(trackids)

audio_features <- audio_features[!(audio_features$loudness < -10),]
danceability <- audio_features$danceability
energy <- audio_features$energy
loudness <- audio_features$loudness
tempo <- audio_features$tempo
tempo

plot(energy,danceability,ylim=c(min(danceability),max(danceability)),xlim=c(min(energy),max(energy)))
DEmodel = lm(danceability ~ energy)
abline(DEmodel)
cor.test(energy,danceability)

plot(loudness,danceability,ylim=c(min(danceability),max(danceability)),xlim=c(min(loudness),max(loudness)))
DLmodel = lm(danceability ~ loudness)
abline(DLmodel)

plot(tempo,danceability,ylim=c(min(danceability),max(danceability)),xlim=c(min(tempo),max(tempo)))
DTmodel = lm(danceability ~ tempo)
abline(DTmodel)


library(car)
DELmodel = lm(danceability ~ energy + loudness + tempo)
avPlots(DELmodel)



