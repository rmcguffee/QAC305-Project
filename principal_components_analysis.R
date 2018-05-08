install.packages("haven")
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
               PINTERST, FLICKR, VINE, CLSSMTES, INTWKDYH,
               INTWKENH)

g_df_selected <- select(g_df, NATHEAL, NATCITY, NATDRUG, NATEDUC, NATRACE,
                        NATAID, NATARMS, NATFARE, NATROAD, NATSOC, NATMASS,
                        NATPARK, NATCHLD, NATSCI, NATENRGY, TAX,
                        SPKATH, COLATH, LIBATH, SPKCOM, COLCOM, LIBCOM,
                        SPKMIL, COLMIL, LIBMIL, SPKHOMO, COLHOMO, LIBHOMO,
                        SPKRAC, COLRAC, LIBRAC, SPKMSLM, COLMSLM, LIBMSLM,
                        REVSPEAK, REVPUB)

select_df_scaled <- data.frame(scale(g_df_selected))

install.packages("VIM")
library(VIM)
df_imput <- kNN(select_df_scaled)
df_imput_only <- df_imput[,1:36]

fa <- df_imput_only
r <-cor(fa)

library(psych)
scree(r,factors = FALSE,pc=TRUE)

pcal<- princomp(fa,scores=TRUE, cor = TRUE)
summary(pcal)
loadings(pcal)

fit <- fa(fa,nfactors=6,rotate="varimax",fm="pa")
fit

#PA1- SPKATH, COLATH, LIBATH, SPKCOM, COLCOM, LIBCOM, -LIBMIL,-LIBHOMO,-LIBRACIST,-LIBMSLM
#	consideration of ideas as bad or good.    Against Freedom of Speech
#PA2- NATHEAL,NATEDUC, NATRACE, 
#Improving & protecting nations health, Improving nations education system, Improving the conditions of blacks
#Liberal
#PA3- SPKMSLM,COLMSLM
#consideration of muslim ideas as good or bad      Islamophobic
#PA5- SPKHOMO, COLHOMO, -LIBHOMO
#consideration of homosexual ideas as good or bad    Homophobic
#PA4- REVSPEAK,REVPUB
#Allow revolutionaries to publish books     Against Revolutionaries
#PA6- COLMIL,
#allow militarist to teachn               Against militarism

#highest commmunality spkhomo lowest- NATAID, NATROAD, NATSOC, NATPARK,
#foreignaid,

fit$scores
with_scores <- as.data.frame(cbind(fa, fit$scores))
with_scores <-rename(with_scores, "Against Freedom of Speech" = PA1,"Liberal" = PA2,"Islamophobic"=PA3,"Homophobic"=PA5, "Against Revolutionary"=PA4, "Against Militarism"=PA6)


