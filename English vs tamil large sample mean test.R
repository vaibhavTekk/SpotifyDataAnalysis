# Load required packages
library(spotifyr)

# Set Spotify API credentials and get access token
Sys.setenv(SPOTIFY_CLIENT_ID = 'fecaca040b82440c8cc8f31516e47050')
Sys.setenv(SPOTIFY_CLIENT_SECRET = 'e8db744eb9e94395952577c56195245f')
access_token <- get_spotify_access_token()

# Get English playlists
categories <- get_categories()
id_english <- categories$id[4]
english_playlists <- get_category_playlists(id_english)

# Get Tamil playlists
id_tamil <- categories$id[1]
tamil_playlists <- get_category_playlists(id_tamil)

# Extract popularity data for both sets of playlists
english_popularity <- numeric()
for (i in 1:length(english_playlists$id)) {
  playlist_data <- get_playlist(english_playlists$id[i])
  english_popularity[i] <- playlist_data$tracks$total
}
tamil_popularity <- numeric()
for (i in 1:length(tamil_playlists$id)) {
  playlist_data <- get_playlist(tamil_playlists$id[i])
  tamil_popularity[i] <- playlist_data$tracks$total
}

# Compute sample means and standard errors
english_mean <- mean(english_popularity)
tamil_mean <- mean(tamil_popularity)
english_se <- sd(english_popularity) / sqrt(length(english_popularity))
tamil_se <- sd(tamil_popularity) / sqrt(length(tamil_popularity))

# Compute test statistic and p-value
test_statistic <- (english_mean - tamil_mean) / sqrt(english_se^2 + tamil_se^2)
p_value <- 2 * (1 - pnorm(abs(test_statistic)))

# Output results
cat("English playlists mean popularity:", english_mean, "\n")
cat("Tamil playlists mean popularity:", tamil_mean, "\n")
cat("Test statistic:", test_statistic, "\n")
cat("P-value:", p_value, "\n")
if (p_value < 0.05) {
  cat("Reject the null hypothesis - people like English playlists more than Tamil playlists.\n")
} else {
  cat("Fail to reject the null hypothesis - there is no significant difference in popularity between English and Tamil playlists.\n")
}

