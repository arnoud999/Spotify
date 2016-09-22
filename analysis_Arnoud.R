# Analyze top 200 charts in Spotify

require(dplyr, warn.conflicts=F, quietly=T)
require(cowplot, warn.conflicts=F, quietly=T)
require(ggthemes, warn.conflicts=F, quietly=T)
require(coefplot, warn.conflicts=F, quietly=T)
require(corrgram, warn.conflicts=F, quietly=T)
require(gridExtra, warn.conflicts=F, quietly=T)
require(tidyr, warn.conflicts=F, quietly=T)


# Prepare data ----------------------------------------------------------------------

# Read data
dat <- read.csv("spotify.csv", sep=";")
dat$date <- as.Date(dat$date)

# Store acoustic features in features
features = dat
vars <- c('energy', 'liveness', 'tempo', 'speechiness', 'acousticness', 
          'instrumentalness', 'danceability', 'duration', 'loudness', 'valence')
features <- select(features, id, one_of(vars))
features = unique(features)

# Create a dataframe of songs
songs = dat 
songs = songs %>%
  group_by(id) %>%
  mutate(minrank = min(rank)) %>%
  mutate(totalstreams = sum(streams)) %>%
  mutate(enterdate = min(date)) %>%
  mutate(timesintop10 = sum(rank<=10)) %>%
  mutate(timesintop200 = n()) %>%
  filter(date == enterdate) %>%
  mutate(enterrank = rank) %>%
  select(artist:valence, minrank, totalstreams, enterdate, enterrank, duration,
         timesintop10, timesintop200) %>%
  filter(row_number() == 1)

# Descriptives ------------------------------------------------------------

# How many observed dates?
dat %>% count(date)

# How many observed songs?
dat %>% count(id)

# How many observed artists?
dat %>% count(artist)

# Compute max and min streams, add as column
dat <- dat %>% group_by(id) %>% 
  mutate(max_streams = max(dat$streams)) %>%
  mutate(min_streams = min(dat$streams))

# Plot streams by chart position
ggplot(dat, aes(x=rank, y=streams)) + geom_smooth()

# Who has max and min streams?
dat %>% filter(streams==max_streams) %>% select(id, title, artist)
dat %>% filter(streams==min_streams) %>% select(id, title, artist)

# Find least and most danceable tracks among the top 10
tmp <- filter(dat, rank<11) %>% group_by(id) 
tmp <- filter(songs, minrank<11)
filter(tmp, danceability==max(tmp$danceability)) %>% select(artist, title)
filter(tmp, danceability==min(tmp$danceability)) %>% select(artist, title)
  
# Correlation log streams and danceability
cor.test(songs$danceability, log(songs$totalstreams))
ggplot(songs, aes(danceability, log(totalstreams))) + geom_point() + geom_smooth()
# Curvilinear effect?
ggplot(songs, aes(danceability, log(totalstreams))) + geom_smooth()

# Features ----------------------------------------------------------------

# Correlation between the features
corrgram(select(features, -id), upper.panel = panel.pie,
         lower.panel=panel.conf, cex.labels = 1.1)

# Plot all features over time
datlong <- gather(dat, "feature", "value", energy:danceability)
plotfeature <- function(dat, group) ggplot(dat, aes(x=date, y=value)) +
  geom_smooth() + ggtitle(group)
allplots = datlong %>% group_by(feature) %>% do(plots = plotfeature(., unique(.$feature)))
do.call("grid.arrange", c(allplots$plots, ncol=3))

# Features and totalstreams 
songslong <- gather(songs, "feature", "value", energy:danceability)
pltfeature_streams <- function(dat, group) ggplot(dat, aes(value, totalstreams)) +
	geom_smooth() + ggtitle(group)
feature_streams = songslong %>% group_by(feature) %>% 
  do(plots = pltfeature_streams(., unique(.$feature)))
do.call("grid.arrange", c(feature_streams$plots, ncol=3))

# 3rd polynomial effect of tempo?
  # Local maximum at 106.94 BPM, although the mean is 119.50 BPM
lm(totalstreams ~ value, filter(songslong, feature=="tempo")) %>% summary()
ggplot(filter(songslong, feature=="tempo"), aes(x=value, y=totalstreams)) +
  geom_smooth(formula = y ~ poly(x, 3))  

# Features and times in top 200
pltfeature_top200 <- function(dat, group) ggplot(dat, aes(value, timesintop200)) +
  geom_smooth() + ggtitle(group)
feature_top200 = songslong %>% group_by(feature) %>% 
  do(plots = pltfeature_top200(., unique(.$feature)))
do.call("grid.arrange", c(feature_top200$plots, ncol=3))

# Duration ----------------------------------------------------------------

# Plot duration and log(totalstreams)
ggplot(songs, aes(duration, log(totalstreams))) + 
  geom_smooth() + geom_point(alpha=.3)

# Optimal duration?
# songs %>% filter(duration<750) %>% 
#   ggplot(aes(duration, log(totalstreams))) + 
#   geom_smooth(method="lm", formula = y ~ poly(x,3)) + geom_point(alpha=.3)
# lmdur <- songs %>% filter(duration<750) %>% 
#   lm(log(totalstreams) ~ poly(duration, 3), .)
# max(predict(lmdur, data.frame(duration=seq(180, 230))))
# data.frame(duration=seq(180, 230)) %>%
#   mutate(streams=predict(lmdur, data.frame(duration)))

# Shortest and longest song?
songs %>% filter(duration == max(songs$duration) | 
                   duration == min(songs$duration)) %>% 
  select(artist, title, duration)

# Analysis --------------------------------------------------------------------------

# Prepare data
  
  # Identify "slow starters"
  # ggplot(songs, aes(x=enterrank)) + geom_histogram(fill="darkblue")

  # Restrict sample to songs wither enterrank >100
  # starters = songs %>% filter(enterrank > 100)

  # Create dummy whether song will make it to top 10
  # starters$top10 = starters$minrank <= 10 # Only 36 cases!
  songs$slowstarter = songs$enterrank > 100
  songs$top10 = songs$minrank <= 10
  
# Estimate logit model

  lm1 <- glm(top10 ~ energy + tempo + speechiness + acousticness + instrumentalness +
             danceability + duration + loudness + valence, family=binomial(logit),
             data=songs) #, subset = slowstarter)
  summary(lm1)
  coefplot(lm1, intercept = F, title="Ever in the top 10")
  
# Estimate times in top 10
  
  # Most often in top 10?
  songs %>% filter(timesintop10==max(songs$timesintop10)) %>% 
    select(artist, title, timesintop10)
  
  # Histogram of timesintop10
  ggplot(songs, aes(x=log(timesintop10))) + geom_histogram()
  
  # Linear model with timesintop10
  lm2 <- lm(log(timesintop10+1) ~ energy + tempo + speechiness + acousticness +
              instrumentalness + danceability + duration + loudness + valence, data=songs)
  summary(lm2)
  coefplot(lm2, intercept = F, title = "Times in the top 10")
  
  # Histogram of timesintop200
  ggplot(songs, aes(x=log(timesintop200))) + geom_histogram()
  
  # Linear model with timesintop200
  lm3 <- lm(log(timesintop200) ~ energy + tempo + speechiness + acousticness +
              instrumentalness + danceability + duration + loudness + valence, data=songs)
  summary(lm3)
  coefplot(lm3, intercept = F, title="Times in the top 200")
  
  # Histogram of totalstreams
  ggplot(songs, aes(x=log(totalstreams))) + geom_histogram()
  
  # Linear model with total number of streams
  lm4 <- lm(log(totalstreams) ~ energy + tempo + speechiness + acousticness +
              instrumentalness + danceability + duration + loudness + valence, data=songs)
  summary(lm4)
  coefplot(lm4, intercept = F, title="Total number of streams")













