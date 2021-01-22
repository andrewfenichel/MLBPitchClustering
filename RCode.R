######################### LOAD PACKAGES #########################
library(tidyverse)
library(janitor)
library(cluster)
library(factoextra)
library(gridExtra)

######################### LOAD IN DATA #########################
pitch_dat <- read_csv("Data/Uncleaned/candex_pitch_data.csv") %>%
  clean_names()

set.seed(2020)

######################### QUICK EDA #########################

pitch_dat %>%
  skimr::skim_without_charts()

pitch_dat %>%
  sample_n(10000) %>%
  view()

pitch_dat %>%
  sample_n(10000) %>%
  ggplot(aes(horz_break, vert_break)) +
  geom_point() +
  theme_minimal()

pitch_dat %>%
  sample_n(10000) %>%
  ggplot(aes(release_speed, vert_break)) +
  geom_point() +
  theme_minimal()

pitch_dat %>%
  sample_n(10000) %>%
  ggplot(aes(release_speed)) +
  geom_bar() +
  theme_minimal()

######################### PITCH CLUSTERING #########################

###Data Cleaning
clustering_pitch_dat <- pitch_dat %>%
  filter(
    horz_break > -100, ## Remove extreme outliers/ incorrect data
    horz_break < 100, ## Remove extreme outliers/ incorrect data
    release_speed <= 105.1,  ## Remove extreme outliers/ incorrect data (based on MLB Record 105.1mph Fastball)
    !is.na(release_speed), ## Removing 45 release_speed NAs so K-Means can run
  ) %>%
  mutate(horz_break = case_when(
    pitcher_throwing == 'R' ~ horz_break, ### Adjusting for LHP and RHP directional differences
    pitcher_throwing == 'L' ~ -horz_break
  )) %>%
  select(release_speed, horz_break, vert_break)

### K Means
k2 <- kmeans(clustering_pitch_dat, centers = 2, nstart = 25)

fviz_cluster(k2, data = clustering_pitch_dat)

k3 <- kmeans(clustering_pitch_dat, centers = 3, nstart = 25)
k4 <- kmeans(clustering_pitch_dat, centers = 4, nstart = 25)
k5 <- kmeans(clustering_pitch_dat, centers = 5, nstart = 25)
k6 <- kmeans(clustering_pitch_dat, centers = 6, nstart = 25)

# plots to compare
p1 <- fviz_cluster(k2, geom = "point", data = clustering_pitch_dat) + ggtitle("k = 2")
p2 <- fviz_cluster(k3, geom = "point",  data = clustering_pitch_dat) + ggtitle("k = 3")
p3 <- fviz_cluster(k4, geom = "point",  data = clustering_pitch_dat) + ggtitle("k = 4")
p4 <- fviz_cluster(k5, geom = "point",  data = clustering_pitch_dat) + ggtitle("k = 5")
p5 <- fviz_cluster(k6, geom = "point",  data = clustering_pitch_dat) + ggtitle("k = 6")

grid.arrange(p1, p2, p3, p4, p5, nrow = 3)

## Assesing Optimal # Of Clusters
fviz_nbclust(clustering_pitch_dat, kmeans, method = "wss")
fviz_nbclust(clustering_pitch_dat, kmeans, method = "silhouette")
gap_stat <- clusGap(clustering_pitch_dat, FUN = kmeans, nstart = 25,
                    K.max = 10, B = 50)
fviz_gap_stat(gap_stat)


fviz_cluster(k5, data = clustering_pitch_dat)


##Calculating CLusters Using K=5
final <- kmeans(clustering_pitch_dat, 5, nstart = 25)

clustered_pitches <- clustering_pitch_dat %>%
  mutate(Cluster = final$cluster)


## Writing CSV
write_csv(x = clustered_pitches, path = "Data/Cleaned/clustered_pitches.csv")

clustered_pitch_dat <- read_csv("Data/Cleaned/clustered_pitches.csv")

final_cluster_dat <- clustered_pitch_dat %>%
  select(game_event_id, Cluster)

write_csv(final_cluster_dat, "Data/Cleaned/4191706_categories.csv")

########## HARDEST CLUSTERS TO HIT ##########

overall_dat <- merge(final_cluster_dat, pitch_dat, by = "game_event_id")

overall_dat %>%
  mutate(
    exit_speed = as.numeric(exit_speed),
    launch_angle = as.numeric(launch_angle)
  ) %>%
  filter(
    !is.na(exit_speed),
    !is.na(launch_angle)
  ) %>%
  group_by(Cluster) %>%
  summarise(exit_speed = mean(exit_speed), launch_angle = mean(launch_angle))


numb_clust_pitches <- overall_dat %>%
  group_by(Cluster) %>%
  tally() %>%
  rename(total_clustered_pitches = n)

clustered_events <- overall_dat %>%
  select(Cluster, result) %>%
  group_by(Cluster, result) %>%
  add_tally()


clustered_pitches_events_dat <- merge(clustered_events, numb_clust_pitches, by = "Cluster")

### Calculating wOBA (Weighted On-Base Average)

overall_dat %>%
  group_by(Cluster) %>%
  mutate(wOBA = (
    (0.69 * bb) + (0.72 * hbp) + (0.89 * x1b) + (1.27 * x2b) + (1.62 * x3b) + (2.1 * hr) 
    / (ab + bb = sf + sf + hbp)
    ))

