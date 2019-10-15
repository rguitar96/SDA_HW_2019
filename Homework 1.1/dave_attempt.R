library(dplyr)
library(ggplot2)
library(vcd)
library(scales)
library(gridExtra)

movies = get(load("movies.RData"))

#2-comparing different scores (critics/audience/imdb)
#-is there any difference on the audience/critics score by genre?


#plot average score for each  score category over the years

#NA omitted
data_critics_score_rel_year = 
      na.omit(movies) %>%
      group_by(thtr_rel_year)  %>% 
      summarise(N=n(),
                avg=mean(critics_score), 
                s=sd(critics_score),se=s/sqrt(N),
                cil=avg-qt(0.975,N-1)*se,
                ciu=avg+qt(0.975,N-1)*se) 


#Remove where 2 or less movies
ggplot(subset(data_critics_score_rel_year, N>2), aes(x=thtr_rel_year,y=avg))+
  labs(x ="Film release year", y = "95% CI Critics Score")+ 
  geom_errorbar(aes(ymin=cil,ymax=ciu))+
  geom_point(size=1)+
  geom_line()


data_audience_score_rel_year = 
  na.omit(movies) %>%
  group_by(thtr_rel_year)  %>% 
  summarise(N=n(),
            avg=mean(audience_score), 
            s=sd(audience_score),se=s/sqrt(N),
            cil=avg-qt(0.975,N-1)*se,
            ciu=avg+qt(0.975,N-1)*se) 

#Remove where 2 or less movies
ggplot(subset(data_audience_score_rel_year, N>2), aes(x=thtr_rel_year,y=avg))+
  labs(x ="Film release year", y = "95% CI Audience Score")+
  geom_errorbar(aes(ymin=cil,ymax=ciu))+
  geom_point(size=1)+
  geom_line()

data_imdb_score_rel_year = 
  na.omit(movies) %>%
  group_by(thtr_rel_year)  %>% 
  summarise(N=n(),
            avg=mean(imdb_rating), 
            s=sd(imdb_rating),se=s/sqrt(N),
            cil=avg-qt(0.975,N-1)*se,
            ciu=avg+qt(0.975,N-1)*se) 

#Remove where 2 or less movies
ggplot(subset(data_imdb_score_rel_year, N>2), aes(x=thtr_rel_year,y=avg))+
  labs(x ="Film release year", y = "95% CI IMDB Score")+
  geom_errorbar(aes(ymin=cil,ymax=ciu))+
  geom_point(size=1)+
  geom_line()


score = list()

score$year_score_agg = list(
                  "critics" = aggregate(critics_score ~ thtr_rel_year , movies, mean),
                  "audience" = aggregate(audience_score ~ thtr_rel_year, movies, mean),
                  "imdb" = aggregate(imdb_rating ~ thtr_rel_year, movies, mean)
                  )


score$year_metrics_grid = expand.grid(year =sort(unique(movies$thtr_rel_year)),
                          metrics = c("Critic", "Audience", "IMDB"))

score$year_metrics_grid$year_score_agg =c(score$year_score_agg$critics$critics_score,
                                    score$year_score_agg$audience$audience_score, 
                                    score$year_score_agg$imdb$imdb_rating)

score$year_metrics_grid$year_score_agg_rescaled = 
                      c(
                      rescale(score$year_score_agg$critics$critics_score, from=(c(1,100))), 
                      rescale(score$year_score_agg$audience$audience_score, from=(c(1,100))), 
                      rescale(score$year_score_agg$imdb$imdb_rating, from=(c(1,10))))

ggplot(score$year_metrics_grid)+
  geom_tile(aes(x=metrics, y=year,fill=year_score_agg_rescaled))+
  scale_fill_distiller(palette = "RdBu") 


score$genre_score_agg = list(
  "critics" = aggregate(critics_score ~ genre , movies, mean),
  "audience" = aggregate(audience_score ~ genre, movies, mean),
  "imdb" = aggregate(imdb_rating ~ genre, movies, mean)
)


score$genre_metrics_grid = expand.grid(year =sort(unique(movies$genre)),
                                 metrics = c("Critic", "Audience", "IMDB"))

score$genre_metrics_grid$genre_score_agg =c(score$genre_score_agg$critics$critics_score,
                                     score$genre_score_agg$audience$audience_score, 
                                     score$genre_score_agg$imdb$imdb_rating)

score$genre_metrics_grid$genre_score_agg_rescaled = 
  c(
    rescale(score$genre_score_agg$critics$critics_score, from=(c(1,100))), 
    rescale(score$genre_score_agg$audience$audience_score, from=(c(1,100))), 
    rescale(score$genre_score_agg$imdb$imdb_rating, from=(c(1,10))))

ggplot(score$genre_metrics_grid)+
  geom_tile(aes(x=metrics, y=year,fill=genre_score_agg_rescaled))+
  scale_fill_distiller(palette = "RdPu") 


#5-do actors and directors have a higher score once they won? 
#  (find the first film and see the tendency, look for sudden change)
best_dirs =  subset(movies, movies$best_dir_win == "yes") %>%
  group_by(director) %>%
    filter(n() > 2)

win_years = data.frame(
        year = c(2010, 2007,1987, 1990, 1978),
        director=c("Kathryn Bigelow", "Martin Scorsese","Oliver Stone","Oliver Stone","Woody Allen")
)

win_year_intersects = 
  geom_vline(data = win_years, aes(xintercept=year, color = director))

p1 = ggplot(best_dirs, aes(y=critics_score, x=thtr_rel_year, color=director))+
  geom_point()+
  geom_line() +
  win_year_intersects    

p2 = ggplot(best_dirs, aes(y=audience_score, x=thtr_rel_year, color=director))+
  geom_point()+
  geom_line() +
  win_year_intersects

p3= ggplot(best_dirs, aes(y=imdb_rating, x=thtr_rel_year, color=director))+
  geom_point()+
  geom_line() +
  win_year_intersects

grid.arrange(p1, p2, p3, nrow = 3)


all_actors = table((c(movies$actor1, 
                      movies$actor2, 
                      movies$actor3, 
                      movies$actor4, 
                      movies$actor5)))

