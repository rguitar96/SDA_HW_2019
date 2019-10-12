#Load the data
movies <- get(load("movies.RData"))
##################################################################################################################################################

#Checking for missing data and its number
sum(is.na(movies))
#Locate the specific missing data
summary(movies)
#Fill the missing information
#Runtime:
movies$runtime[movies$title == "The End of America"] <- 74
#https://www.imdb.com/title/tt1294790/
#Studio:
movies$studio <- as.character(movies$studio)
movies$studio[movies$title == "Oliver & Company"] <-
  "Walt Disney Pictures"
#https://es.wikipedia.org/wiki/Oliver_y_su_pandilla
movies$studio[movies$title == "Attack of the 50 Foot Woman"] <-
  "Woolner Brothers Pictures Inc."
#https://www.imdb.com/title/tt0051380/
movies$studio[movies$title == "Inbred"] <- "Melanie Light"
#https://www.imdb.com/title/tt1723124/fullcredits
movies$studio[movies$title == "Caveman"] <- "United Artists"
#https://es.wikipedia.org/wiki/El_cavern%C3%ADcola
movies$studio[movies$title == "Dirty Sanchez: The Movie"] <-
  "Vertigo Films"
#https://www.rottentomatoes.com/m/dirty_sanchez
movies$studio[movies$title == "The Man Who Sued God"] <-
  "Australian Film Finance Corporation (AFFC), New South Wales Film & Television Office, Showtime Australia See more"
#https://www.imdb.com/title/tt0268437/
movies$studio[movies$title == "Inserts"] <-
  "Film and General Productions"
#https://www.filmaffinity.com/es/film740308.html
movies$studio <- factor(movies$studio)

#Another way of checkinig which data is missing:
#DVD realease date
movies[!complete.cases(movies['dvd_rel_year']), ]
movies$dvd_rel_year[movies$title == "Charlie: The Life and Art of Charles Chaplin"] <-
  2003
movies$dvd_rel_month[movies$title == "Charlie: The Life and Art of Charles Chaplin"] <-
  11
movies$dvd_rel_day[movies$title == "Charlie: The Life and Art of Charles Chaplin"] <-
  5
#https://www.imdb.com/title/tt0379730/releaseinfo
movies$dvd_rel_year[movies$title == "The Squeeze"] <- 2015
movies$dvd_rel_month[movies$title == "The Squeeze"] <- 6
movies$dvd_rel_day[movies$title == "The Squeeze"] <- 9
#https://medium.com/@releasebandyal/producer-michael-doven-announces-release-of-the-squeeze-on-dvd-75a982d0a047
movies$dvd_rel_year[movies$title == "Electric Dreams"] <- 1984
#https://en.wikipedia.org/wiki/Electric_Dreams_(film)
movies$dvd_rel_year[movies$title == "The Last Remake of Beau Geste"] <-
  2010
movies$dvd_rel_month[movies$title == "The Last Remake of Beau Geste"] <-
  1
movies$dvd_rel_day[movies$title == "The Last Remake of Beau Geste"] <-
  11
#https://en.wikipedia.org/wiki/The_Last_Remake_of_Beau_Geste

movies[!complete.cases(movies['director']), ]
movies$director[movies$title == "Lorenzo's Oil"] <- "George Miller"
#https://es.wikipedia.org/wiki/Lorenzo%27s_Oil_(pel%C3%ADcula)
movies$director[movies$title == "The Ninth Gate"] <-
  "Roman Polanski"
#https://es.wikipedia.org/wiki/The_Ninth_Gate

movies[!complete.cases(movies['actor4']), ]
movies$actor4[movies$title == "Attack of the 50 Foot Woman"] <-
  "Roy Gordon"
#https://www.imdb.com/title/tt0051380/fullcredits/?ref_=tt_ov_st_sm

movies[!complete.cases(movies['actor5']), ]
movies$actor5[movies$title == "Attack of the 50 Foot Woman"] <-
  "George Douglas"
#https://www.imdb.com/title/tt0051380/fullcredits/?ref_=tt_ov_st_sm
movies$actor5[movies$title == "The Illusionist (L'illusionniste)"] <-
  "Eleanor Tomlinson"
#https://en.wikipedia.org/wiki/The_Illusionist_(2006_film)

summary(movies)
#Since the missing values do not belong to any a priori critical attribute, we will not delete any instances.
##################################################################################################################################################

#We delete the columns we are not interested in for the analysis.
movies <- subset(movies, select = -c(imdb_url, rt_url, top200))
##################################################################################################################################################

#1. What can we say about the relationship between audience scores and at least four of
#the other variables in this dataset?
#Audience score vs Critics score
library(ggplot2)
ggplot(movies, aes(x = audience_score, y = critics_score)) + geom_point() + geom_smooth(method =
                                                                                          lm)

#Audience score vs Genre and Audience Score vs Critics Score vs Genre
ggplot(movies, aes(
  x = reorder(genre, audience_score, FUN = median),
  y = audience_score,
  fill = genre
)) + geom_boxplot()
ggplot(movies, aes(
  x = reorder(genre, critics_score, FUN = median),
  y = critics_score,
  fill = genre
)) + geom_boxplot()

movies$score_diff <- movies$audience_score - movies$critics_score
ggplot(movies, aes(
  x = reorder(genre, score_diff, FUN = mean),
  y = score_diff,
  fill = genre
)) + geom_boxplot()

#Audience score vs Thtre_rel_month
ggplot(movies, aes(x = thtr_rel_month)) +   geom_histogram(color = "black", fill =
                                                             "blue")
ggplot(movies, aes(x = thtr_rel_month, y = mean(audience_score))) + geom_point() + geom_smooth(method =
                                                                                                 lm)

#Audience score vs Best_pic_win
ggplot(movies,
       aes(x = best_pic_win, y = audience_score, fill = best_pic_win)) + geom_boxplot()

#Audience score vs Best_dir_win
ggplot(movies,
       aes(x = best_dir_win, y = audience_score, fill = best_dir_win)) + geom_boxplot()

#Audience score vs Best_actor_win/Best_actress_win
ggplot(movies,
       aes(x = best_actor_win, y = audience_score, fill = best_actor_win)) +
  geom_boxplot()
ggplot(movies,
       aes(x = best_actress_win, y = audience_score, fill = best_actress_win)) +
  geom_boxplot()
##################################################################################################################################################

#2. Is there a difference in the score given by audience members (or critics) for movies
#of different genres? And for movies with different MPAA ratings? If so, which categories
#seem to be significantly different for each other?
ggplot(movies, aes(
  x = reorder(genre, audience_score, FUN = median),
  y = audience_score,
  fill = genre
)) + geom_boxplot()
ggplot(movies, aes(
  x = reorder(genre, critics_score, FUN = median),
  y = critics_score,
  fill = genre
)) + geom_boxplot()

ggplot(movies, aes(
  x = reorder(mpaa_rating, audience_score, FUN = median),
  y = audience_score,
  fill = mpaa_rating
)) + geom_boxplot()
ggplot(movies, aes(
  x = reorder(mpaa_rating, critics_score, FUN = median),
  y = critics_score,
  fill = mpaa_rating
)) + geom_boxplot()
##################################################################################################################################################


#Other questions:
#Is the month relevant to the score?
#Is there a relationship between the score and the awards a movie receives?

##################################################################################################################################################
##################################################################################################################################################

#1-is there a relationship between the type/genre and score?
#Audience score vs genre
plot_audience_vs_genre <- ggplot(movies, aes(
  x = reorder(genre, audience_score, FUN = median),
  y = audience_score,
  fill = genre
)) + geom_boxplot() + labs(title = "Audience score per genre", x = "Genre", y = "Audience score") + theme(axis.text.x = element_blank())
#Critics score vs genre
plot_critics_vs_genre <- ggplot(movies, aes(
  x = reorder(genre, critics_score, FUN = median),
  y = critics_score,
  fill = genre
)) + geom_boxplot() + labs(title = "Critics score per genre", x = "Genre", y = "Critics score") + theme(axis.text.x = element_blank())
#IMBD score vs genre
movies$imdb_rating <-
  movies$imdb_rating * 10 #x10 so that they can be compared with the rest of ratings.
plot_imdb_vs_genre <- ggplot(movies, aes(
  x = reorder(genre, imdb_rating, FUN = median),
  y = imdb_rating,
  fill = genre
)) + geom_boxplot() + labs(title = "IMDB score per genre", x = "Genre", y = "IMDB score") + theme(axis.text.x = element_blank())

#2-comparing different scores (critics/audience/imdb)
# -is there any difference on the audience/critics score by genre?
#For all genres
plot_number_movies_per_genre <-
  qplot(movies$genre, fill = movies$genre) + theme(legend.position = "none",
                                                   axis.text.x = element_text(angle = 45, hjust = 1)) + ggtitle("Number of movies per genre") +
  xlab("Genre") + ylab("Number of movies")


mean_score <-
  data.frame(
    "Score" = c(
      mean(movies$imdb_rating),
      mean(movies$audience_score),
      mean(movies$critics_score)
    ),
    "Source" = c("IMDB", "Audience", "Critics")
  )
plot_all <- ggplot(mean_score) + geom_linerange(aes(
  x = Source,
  ymin = max(c(0, min(Score) - 10)),
  ymax = Score,
  colour = Source
),
position = position_dodge(width = 1)) +  geom_point(
  aes(x = Source, y = Score, colour = Source),
  position = position_dodge(width = 1),
  size = 3
) + coord_flip() + theme(legend.position = "none") + ggtitle("All genres")

#Action and adventure
action_adventure <- subset(movies, genre == "Action & Adventure")
mean_score <-
  data.frame(
    "Score" = c(
      mean(action_adventure$imdb_rating),
      mean(action_adventure$audience_score),
      mean(action_adventure$critics_score)
    ),
    "Source" = c("IMDB", "Audience", "Critics")
  )
plot_action <- ggplot(mean_score) + geom_linerange(aes(
  x = Source,
  ymin = max(c(0, min(Score) - 10)),
  ymax = Score,
  colour = Source
),
position = position_dodge(width = 1)) +  geom_point(
  aes(x = Source, y = Score, colour = Source),
  position = position_dodge(width = 1),
  size = 3
) + coord_flip() + theme(legend.position = "none") + ggtitle("Action & Adventure")

#Animation
animation <- subset(movies, genre == "Animation")
mean_score <-
  data.frame(
    "Score" = c(
      mean(animation$imdb_rating),
      mean(animation$audience_score),
      mean(animation$critics_score)
    ),
    "Source" = c("IMDB", "Audience", "Critics")
  )
plot_animation <- ggplot(mean_score) + geom_linerange(aes(
  x = Source,
  ymin = max(c(0, min(Score) - 10)),
  ymax = Score,
  colour = Source
),
position = position_dodge(width = 1)) +  geom_point(
  aes(x = Source, y = Score, colour = Source),
  position = position_dodge(width = 1),
  size = 3
) + coord_flip() + theme(legend.position = "none") + ggtitle("Animation")

#Art House & International
art_house <- subset(movies, genre == "Art House & International")
mean_score <-
  data.frame(
    "Score" = c(
      mean(art_house$imdb_rating),
      mean(art_house$audience_score),
      mean(art_house$critics_score)
    ),
    "Source" = c("IMDB", "Audience", "Critics")
  )
plot_art_house <- ggplot(mean_score) + geom_linerange(aes(
  x = Source,
  ymin = max(c(0, min(Score) - 10)),
  ymax = Score,
  colour = Source
),
position = position_dodge(width = 1)) +  geom_point(
  aes(x = Source, y = Score, colour = Source),
  position = position_dodge(width = 1),
  size = 3
) + coord_flip() + theme(legend.position = "none") + ggtitle("Art House & International")

#Comedy
comedy <- subset(movies, genre == "Comedy")
mean_score <-
  data.frame(
    "Score" = c(
      mean(comedy$imdb_rating),
      mean(comedy$audience_score),
      mean(comedy$critics_score)
    ),
    "Source" = c("IMDB", "Audience", "Critics")
  )
plot_comedy <- ggplot(mean_score) + geom_linerange(aes(
  x = Source,
  ymin = max(c(0, min(Score) - 10)),
  ymax = Score,
  colour = Source
),
position = position_dodge(width = 1)) +  geom_point(
  aes(x = Source, y = Score, colour = Source),
  position = position_dodge(width = 1),
  size = 3
) + coord_flip() + theme(legend.position = "none") + ggtitle("Comedy")

#Documentary
documentary <- subset(movies, genre == "Documentary")
mean_score <-
  data.frame(
    "Score" = c(
      mean(documentary$imdb_rating),
      mean(documentary$audience_score),
      mean(documentary$critics_score)
    ),
    "Source" = c("IMDB", "Audience", "Critics")
  )
plot_documentary <- ggplot(mean_score) + geom_linerange(aes(
  x = Source,
  ymin = max(c(0, min(Score) - 10)),
  ymax = Score,
  colour = Source
),
position = position_dodge(width = 1)) +  geom_point(
  aes(x = Source, y = Score, colour = Source),
  position = position_dodge(width = 1),
  size = 3
) + coord_flip() + theme(legend.position = "none") + ggtitle("Documentary")

#Drama
drama <- subset(movies, genre == "Drama")
mean_score <-
  data.frame(
    "Score" = c(
      mean(drama$imdb_rating),
      mean(drama$audience_score),
      mean(drama$critics_score)
    ),
    "Source" = c("IMDB", "Audience", "Critics")
  )
plot_drama <- ggplot(mean_score) + geom_linerange(aes(
  x = Source,
  ymin = max(c(0, min(Score) - 10)),
  ymax = Score,
  colour = Source
),
position = position_dodge(width = 1)) +  geom_point(
  aes(x = Source, y = Score, colour = Source),
  position = position_dodge(width = 1),
  size = 3
) + coord_flip() + theme(legend.position = "none") + ggtitle("Drama")

#Horror
horror <- subset(movies, genre == "Horror")
mean_score <-
  data.frame(
    "Score" = c(
      mean(horror$imdb_rating),
      mean(horror$audience_score),
      mean(horror$critics_score)
    ),
    "Source" = c("IMDB", "Audience", "Critics")
  )
plot_horror <- ggplot(mean_score) + geom_linerange(aes(
  x = Source,
  ymin = max(c(0, min(Score) - 10)),
  ymax = Score,
  colour = Source
),
position = position_dodge(width = 1)) +  geom_point(
  aes(x = Source, y = Score, colour = Source),
  position = position_dodge(width = 1),
  size = 3
) + coord_flip() + theme(legend.position = "none") + ggtitle("Horror")

#Musical & Performing Arts
musical <- subset(movies, genre == "Musical & Performing Arts")
mean_score <-
  data.frame(
    "Score" = c(
      mean(musical$imdb_rating),
      mean(musical$audience_score),
      mean(musical$critics_score)
    ),
    "Source" = c("IMDB", "Audience", "Critics")
  )
plot_musical <- ggplot(mean_score) + geom_linerange(aes(
  x = Source,
  ymin = max(c(0, min(Score) - 10)),
  ymax = Score,
  colour = Source
),
position = position_dodge(width = 1)) +  geom_point(
  aes(x = Source, y = Score, colour = Source),
  position = position_dodge(width = 1),
  size = 3
) + coord_flip() + theme(legend.position = "none") + ggtitle("Musical & Performing Arts")

#Mystery & Suspense
mistery <- subset(movies, genre == "Mystery & Suspense")
mean_score <-
  data.frame(
    "Score" = c(
      mean(mistery$imdb_rating),
      mean(mistery$audience_score),
      mean(mistery$critics_score)
    ),
    "Source" = c("IMDB", "Audience", "Critics")
  )
plot_mistery <- ggplot(mean_score) + geom_linerange(aes(
  x = Source,
  ymin = max(c(0, min(Score) - 10)),
  ymax = Score,
  colour = Source
),
position = position_dodge(width = 1)) +  geom_point(
  aes(x = Source, y = Score, colour = Source),
  position = position_dodge(width = 1),
  size = 3
) + coord_flip() + theme(legend.position = "none") + ggtitle("Mistery & Suspense")

#Other
other <- subset(movies, genre == "Other")
mean_score <-
  data.frame(
    "Score" = c(
      mean(other$imdb_rating),
      mean(other$audience_score),
      mean(other$critics_score)
    ),
    "Source" = c("IMDB", "Audience", "Critics")
  )
plot_other <- ggplot(mean_score) + geom_linerange(aes(
  x = Source,
  ymin = max(c(0, min(Score) - 10)),
  ymax = Score,
  colour = Source
),
position = position_dodge(width = 1)) +  geom_point(
  aes(x = Source, y = Score, colour = Source),
  position = position_dodge(width = 1),
  size = 3
) + coord_flip() + theme(legend.position = "none") + ggtitle("Other")

#Science Fiction & Fantasy
science <- subset(movies, genre == "Science Fiction & Fantasy")
mean_score <-
  data.frame(
    "Score" = c(
      mean(science$imdb_rating),
      mean(science$audience_score),
      mean(science$critics_score)
    ),
    "Source" = c("IMDB", "Audience", "Critics")
  )
plot_science <- ggplot(mean_score) + geom_linerange(aes(
  x = Source,
  ymin = max(c(0, min(Score) - 10)),
  ymax = Score,
  colour = Source
),
position = position_dodge(width = 1)) +  geom_point(
  aes(x = Source, y = Score, colour = Source),
  position = position_dodge(width = 1),
  size = 3
) + coord_flip() + theme(legend.position = "none") + ggtitle("Science Fiction & Fantasy")

#Combine the plots...
library(ggpubr)
plot_combined_genre <-
  ggarrange(
    plot_all,
    plot_action,
    plot_animation,
    plot_art_house,
    plot_comedy,
    plot_documentary,
    plot_drama,
    plot_horror,
    plot_mistery,
    plot_musical,
    plot_other,
    plot_science,
    ncol = 3,
    nrow = 4
  )


#3-comparison best picture/oscar wins and score/oscar wins
#Select the movies awarded for best film
best_pic_movies <- subset(movies, best_pic_win == "yes")
#Add a column showing if they have won another award
best_pic_movies$other_win <- NA
if (best_pic_movies$best_actor_win == "yes" ||
    best_pic_movies$best_actress_win == "yes" ||
    best_pic_movies$best_dir_win == "yes") {
  best_pic_movies$other_win <- "yes"
} else{
  best_pic_movies$other_win <- "yes"
}

#Plots...
library(plyr)
#Best movies that have won another award
best_movie_another_award <- count(best_pic_movies$other_win)
best_movie_another_award <- best_movie_another_award %>%
  arrange((freq)) %>%
  mutate(lab.ypos = cumsum(freq) - 0.5 * freq)

pie_another_award <-
  ggplot(best_movie_another_award, aes(x = "", y = freq, fill = x)) +
  geom_bar(width = 1,
           stat = "identity",
           color = "white") +
  coord_polar("y", start = 0) +
  theme_void() +
  geom_text(aes(label = signif((freq / 7) * 100), digits = 2),
            position = position_stack(vjust = 0.5),
            color = "white") +
  labs(fill = "Another award") +
  ggtitle("Best movies with another award") +
  theme(plot.title = element_text(size = 20, hjust = 0.5)) +
  scale_fill_manual(values = "#00BFC4")

#Best movies that have won best actor award
best_movie_best_actor <- count(best_pic_movies$best_actor_win)
best_movie_best_actor <- best_movie_best_actor %>%
  arrange((freq)) %>%
  mutate(lab.ypos = cumsum(freq) - 0.5 * freq)

pie_best_actor <-
  ggplot(best_movie_best_actor, aes(x = "", y = freq, fill = x)) +
  geom_bar(width = 1,
           stat = "identity",
           color = "white") +
  coord_polar("y", start = 0) +
  theme_void() +
  geom_text(aes(label = signif((freq / 7) * 100), digits = 2),
            position = position_stack(vjust = 0.5),
            color = "white") +
  labs(fill = "Best actor") +
  ggtitle("Best movies with best actor") +
  theme(plot.title = element_text(size = 20, hjust = 0.5))

#Best movies that have won best actress award
best_movie_best_actress <- count(best_pic_movies$best_actress_win)
best_movie_best_actress <- best_movie_best_actress %>%
  arrange((freq)) %>%
  mutate(lab.ypos = cumsum(freq) - 1 * freq)

pie_best_actress <-
  ggplot(best_movie_best_actress, aes(x = "", y = freq, fill = x)) +
  geom_bar(width = 1,
           stat = "identity",
           color = "white") +
  coord_polar("y", start = 0) +
  theme_void() +
  geom_text(aes(label = signif((freq / 7) * 100), digits = 2),
            position = position_stack(vjust = 0.5),
            color = "white") +
  labs(fill = "Best actress") +
  ggtitle("Best movies with best actress") +
  theme(plot.title = element_text(size = 20, hjust = 0.5))

#Best movies that have won best director award
best_movie_best_director <- count(best_pic_movies$best_dir_win)
best_movie_best_director <- best_movie_best_director %>%
  arrange((freq)) %>%
  mutate(lab.ypos = cumsum(freq) - 0.9 * freq)

pie_best_director <-
  ggplot(best_movie_best_director, aes(x = "", y = freq, fill = x)) +
  geom_bar(width = 1,
           stat = "identity",
           color = "white") +
  coord_polar("y", start = 0) +
  theme_void() +
  geom_text(aes(label = signif((freq / 7) * 100), digits = 2),
            position = position_stack(vjust = 0.5),
            color = "white") +
  labs(fill = "Best director") +
  ggtitle("Best movies with best director") +
  theme(plot.title = element_text(size = 20, hjust = 0.5))
pie_best_director

#Combine plots!
plot_combined_awarded <-
  ggarrange(
    pie_another_award,
    pie_best_actor,
    pie_best_actress,
    pie_best_director,
    ncol = 2,
    nrow = 2
  )


# -are oscar-awarded films more liked?
#Audience score vs Best_pic_win
plot_audience_vs_best_pic <-
  ggplot(movies,
         aes(x = best_pic_win, y = audience_score, fill = best_pic_win)) + geom_boxplot() + theme(legend.position = "none") + labs(title = "Audience", x = "Best picture award", y = "Score")
#Critics score vs Best_pic_win
plot_critics_vs_best_pic <-
  ggplot(movies,
         aes(x = best_pic_win, y = critics_score, fill = best_pic_win)) + geom_boxplot() + theme(legend.position = "none") + labs(title = "Critics", x = "Best picture award", y = "Score")
#IMDB score vs Best_pic_win
plot_imdb_vs_best_pic <-
  ggplot(movies, aes(x = best_pic_win, y = imdb_rating, fill = best_pic_win)) +
  geom_boxplot() + theme(legend.position = "none") + labs(title = "IMDB", x = "Best picture award", y = "Score")
#Combine the plots...
plot_combined_best_pic <-
  ggarrange(
    plot_audience_vs_best_pic,
    plot_critics_vs_best_pic,
    plot_imdb_vs_best_pic,
    ncol = 3,
    nrow = 1
  )

#Audience score vs Best_dir_win
plot_audience_vs_best_dir <-
  ggplot(movies,
         aes(x = best_dir_win, y = audience_score, fill = best_dir_win)) + geom_boxplot() + theme(legend.position = "none") + labs(title = "Audience", x = "Best director award", y = "Score")
#Critics score vs Best_dir_win
plot_critics_vs_best_dir <-
  ggplot(movies,
         aes(x = best_dir_win, y = critics_score, fill = best_dir_win)) + geom_boxplot() + theme(legend.position = "none") + labs(title = "Critics", x = "Best director award", y = "Score")
#IMDB score vs Best_dir_win
plot_imdb_vs_best_dir <-
  ggplot(movies, aes(x = best_dir_win, y = imdb_rating, fill = best_dir_win)) +
  geom_boxplot() + theme(legend.position = "none") + labs(title = "IMDB", x = "Best director award", y = "Score")
#Combine the plots...
plot_combined_best_dir <-
  ggarrange(
    plot_audience_vs_best_dir,
    plot_critics_vs_best_dir,
    plot_imdb_vs_best_dir,
    ncol = 3,
    nrow = 1
  )

#Audience score vs Best_actor_win
plot_audience_vs_best_actor <-
  ggplot(movies,
         aes(x = best_actor_win, y = audience_score, fill = best_actor_win)) +
  geom_boxplot() + theme(legend.position = "none") + labs(title = "Audience", x = "Best actor award", y = "Score")
#Critics score vs Best_dir_win
plot_critics_vs_best_actor <-
  ggplot(movies,
         aes(x = best_actor_win, y = critics_score, fill = best_actor_win)) + geom_boxplot() + theme(legend.position = "none") + labs(title = "Critics", x = "Best actor award", y = "Score")
#IMDB score vs Best_dir_win
plot_imdb_vs_best_actor <-
  ggplot(movies,
         aes(x = best_actor_win, y = imdb_rating, fill = best_actor_win)) + geom_boxplot() + theme(legend.position = "none") + labs(title = "IMDB", x = "Best actor award", y = "Score")
#Combine the plots...
plot_combined_best_actor <-
  ggarrange(
    plot_audience_vs_best_actor,
    plot_critics_vs_best_actor,
    plot_imdb_vs_best_actor,
    ncol = 3,
    nrow = 1
  )

#Audience score vs Best_actress_win
plot_audience_vs_best_actress <-
  ggplot(movies,
         aes(x = best_actress_win, y = audience_score, fill = best_actress_win)) +
  geom_boxplot() + theme(legend.position = "none") + labs(title = "Audience", x = "Best actress award", y = "Score")
#Critics score vs Best_dir_win
plot_critics_vs_best_actress <-
  ggplot(movies,
         aes(x = best_actress_win, y = critics_score, fill = best_actress_win)) +
  geom_boxplot() + theme(legend.position = "none") + labs(title = "Critics", x = "Best actress award", y = "Score")
#IMDB score vs Best_dir_win
plot_imdb_vs_best_actress <-
  ggplot(movies,
         aes(x = best_actress_win, y = imdb_rating, fill = best_actress_win)) +
  geom_boxplot() + theme(legend.position = "none") + labs(title = "IMDB", x = "Best actress award", y = "Score")
#Combine the plots...
plot_combined_best_actress <-
  ggarrange(
    plot_audience_vs_best_actress,
    plot_critics_vs_best_actress,
    plot_imdb_vs_best_actress,
    ncol = 3,
    nrow = 1
  )



