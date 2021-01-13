library(Rtsne)
library(ggplot2)
library(dplyr)
library(cluster)
library(readr)

df_profiles <- read_csv("df_profiles.csv")
View(df_profiles)
df_profiles_clean = df_profiles %>% select(`SAS Expertise`, Age, `Exp A -G1C1`, `Exp B - G1C2`, `Exp C - G1C3`)


gower_df <- daisy(df_profiles_clean,
                  metric = "gower",
                  type = list())
summary(gower_df)


silhouette <- c()
silhouette = c(silhouette, NA)
for(i in 2:10){
  pam_clusters = pam(as.matrix(gower_df),
                     diss = TRUE,
                     k = i)
  silhouette = c(silhouette ,pam_clusters$silinfo$avg.width)
}
plot(1:10, silhouette,
     xlab = "Clusters",
     ylab = "Silhouette Width")
lines(1:10, silhouette)

pam_profile = pam(gower_df, diss = TRUE, k = 2)
df_profiles[pam_profile$medoids, ]

pam_summary <- df_profiles_clean %>%
  mutate(cluster = pam_profile$clustering) %>%
  group_by(cluster) %>%
  do(cluster_summary = summary(.))
pam_summary$cluster_summary[[1]]

tsne_object <- Rtsne(gower_df, is_distance = TRUE)
tsne_df <- tsne_object$Y %>%
  data.frame() %>%
  setNames(c("X", "Y")) %>%
  mutate(cluster = factor(pam_profile$clustering))
ggplot(aes(x = X, y = Y), data = tsne_df) +
  geom_point(aes(color = cluster))

