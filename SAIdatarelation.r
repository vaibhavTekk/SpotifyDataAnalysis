#DATA EXTRACTION
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
#ANALYSIS DATA
speechiness <- audio_features$speechiness
acousticness <- audio_features$acousticness
instrumentalness <- audio_features$instrumentalness
#Scatter Plot LINEAR REGRESSION AMONG EACH OTHER X1 X2
plot(x = speechiness, y = acousticness,
     xlab = "speechiness",
     ylab = "acousticness",
     ylim = c(0,1) ,
     xlim = c(0,0.5),       
     main = "speechiness vs acousticness"
)
de_lm <- lm(formula = speechiness ~ acousticness)
abline(de_lm)

cor.test(speechiness,acousticness,method="pearson")
#Scatter Plot and regression Line X3 X1
plot(x = instrumentalness, y = speechiness,
     xlab = "instrumentalness",
     ylab = "speechiness",
     ylim = c(0,0.5) ,
     xlim = c(0,1),
     main = "instrumentalness vs speechiness"
)
de_lm <- lm(formula = instrumentalness ~ energy)
abline(de_lm)
summary(de_lm)
cor.test(instrumentalness,speechiness,method="pearson")
#Scatter Plot and regression Line X2 X3
plot(x = acousticness, y = instrumentalness,
     xlab = "acousticness",
     ylab = "instrumentalness",
     ylim = c(0,1),
     xlim = c(0,1),       
     main = "acousticness vs instrumentalness"
)
de_lm <- lm(formula = acousticness ~ instrumentalness)
lm(formula = acousticness ~ instrumentalness)
abline(de_lm)
summary(de_lm)
cor.test(acousticness,instrumentalness,method="pearson")
#MULTIPLE CORELATION x REGRESSION
SAImodel=lm(speechiness~acousticness+instrumentalness)
summary(SAImodel)
scatterplot3d(speechiness,acousticness,instrumentalness)
xx=scatterplot3d(speechiness,acousticness,instrumentalness)
xx$plane3d(SAImodel)

AISmodel=lm(acousticness~+instrumentalness+speechiness)
summary(AISmodel)
scatterplot3d(acousticness,instrumentalness,speechiness)
yy=scatterplot3d(acousticness,instrumentalness,speechiness)
yy$plane3d(AISmodel)

ISAmodel=lm(instrumentalness~speechiness+acousticness)
summary(ISAmodel)
scatterplot3d(instrumentalness,speechiness,acousticness)
zz=scatterplot3d(instrumentalness,speechiness,acousticness)
zz$plane3d(ISAmodel)

frame=data.frame(speechiness,acousticness,instrumentalness)
cor(frame)
#Write Equation Also
