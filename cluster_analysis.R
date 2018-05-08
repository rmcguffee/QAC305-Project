library(haven)
GSS2016 <- read_sav("GSS2016.sav")

library(dplyr)
g_df <- select(GSS2016, PARTYID, PRES12, POLVIEWS, SIZE,
               SIZE, AGE, CHILDS, RACE, NATSPAC, NATENVIR,
               NATHEAL, NATCITY, NATDRUG, NATEDUC, NATRACE,
               NATAID, NATARMS, NATFARE, NATROAD, NATSOC, NATMASS,
               NATPARK, NATCHLD, NATSCI, NATENRGY, TAX,
               SPKATH, COLATH, LIBATH, SPKCOM, COLCOM, LIBCOM,
               SPKMIL, COLMIL, LIBMIL, SPKHOMO, COLHOMO, LIBHOMO,
               SPKRAC, COLRAC, LIBRAC, SPKMSLM, COLMSLM, LIBMSLM,
               REVSPEAK, REVPUB, TWITTER, INSTAGRM, LINKEDIN,
               FACEBOOK, SNAPCHAT, TUMBLR, WHATSAPP, GOOGLESN, 
               PINTERST, FLICKR, VINE, CLSSMTES)

g_df_selected <- select(g_df, NATHEAL, NATCITY, NATDRUG, NATEDUC, NATRACE,
                        NATAID, NATARMS, NATFARE, NATROAD, NATSOC, NATMASS,
                        NATPARK, NATCHLD, NATSCI, NATENRGY, TAX,
                        SPKATH, COLATH, LIBATH, SPKCOM, COLCOM, LIBCOM,
                        SPKMIL, COLMIL, LIBMIL, SPKHOMO, COLHOMO, LIBHOMO,
                        SPKRAC, COLRAC, LIBRAC, SPKMSLM, COLMSLM, LIBMSLM,
                        REVSPEAK, REVPUB)

select_df_scaled <- data.frame(scale(g_df_selected))

library(VIM)
df_imput <- kNN(select_df_scaled)
df_imput_only <- df_imput[,1:36]


library(factoextra)
plot(fviz_nbclust(df_imput_only, FUN=kmeans, method="wss"))
kmeans.fit <- kmeans(df_imput_only, 3)

gss_centroids <- data.frame(kmeans.fit$centers)
gss_centroids$Cluster <- row.names(gss_centroids)

library(ggplot2)
library(tidyr)

df_long <- gather(gss_centroids, key="Variable",
                  value="Value", NATHEAL:REVPUB)

ggplot(df_long, aes(x=Variable, y=Value, group=Cluster, 
                    color=Cluster, shape=Cluster)) + 
  geom_point(size=5) + 
  geom_line(size=1) + labs(title="Profiles for Crime Clusters",
                           x = "Crime Statistic", y="Standardized Frequency") +
  theme(text = element_text(size=18)) + theme(axis.text.x = element_text(angle = 60, hjust = 1))