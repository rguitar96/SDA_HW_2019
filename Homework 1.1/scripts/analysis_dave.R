library(dplyr)
library(ggplot2)
library(vcd)
library(scales)

movies = get(load("Homework 1.1/movies.RData"))

movies = na.omit(movies)

#Does having a higher number of imdb votes lead to a better score?

ggplot(movies, aes(x = as.numeric(imdb_rating), y = imdb_num_votes))+
  geom_point(size=1)+
  scale_y_continuous(name = "Num Votes (10,000)", labels = c(0,25,50,75,100))+
  xlab("Rating")+
  ggtitle("Number of IMDB user votes by movie rating")


#Does winning any other awards indicate winning best picture?

movies$awards = factor(ifelse((movies$best_actor_win == "yes" & movies$best_actress_win == "yes"),"Best actress and actor",
                 ifelse((movies$best_actor_win == "no" & movies$best_actress_win == "yes"),"Best actress",
                 ifelse((movies$best_actor_win == "yes" & movies$best_actress_win == "no"),"Best actor",
                        "None"))))

ggplot(subset(movies, movies$best_pic_nom == "yes"), aes(x = awards, fill=best_pic_win))+
  geom_bar(position=position_dodge())+
  labs(x = "Awards won (except best picture)", y = "Number of movies nominated for best picture", fill="Won best pciture")


#Do all the ratings match?
films_2011 = subset(movies, movies$thtr_rel_year> 2011)

film_scores = expand.grid(films = films_2011$title,
                          metrics = c("Critic", "Audience", "IMDB"))

film_scores$score = c(rescale(log(films_2011$critics_score)),
                      rescale(log(films_2011$audience_score)),
                      rescale(log(films_2011$imdb_rating)))

ggplot(film_scores,aes(x=metrics, y=films))+
  geom_tile(aes(fill=score))

#Does a title type perform better when released at a certian time of the year?

movies$thtr_rel_month_class= cut(movies$thtr_rel_month, breaks = 12, 
                                 labels=c("Jan", "Feb", "Mar", "Apr","May", "Jun", "Jul", "Aug", "Sept", "Oct", "Nov", "Dec"))


data_CS=na.omit(movies) %>% 
                        group_by(genre, thtr_rel_month_class) %>%  
                        summarise(N=n(), 
                                  avg=mean((audience_score)), 
                                  sf= ifelse(N == 1, 1,sd((audience_score))),
                                  se= sf/sqrt(N), 
                                  cil=ifelse(N == 1, avg, avg-qt(0.975,N-1)*se),
                                  ciu=ifelse(N == 1, avg,avg+qt(0.975,N-1)*se))


ggplot(data=data_CS, aes(x=thtr_rel_month_class, y=avg))+ 
  geom_errorbar(aes(ymin=cil,ymax=ciu),position=position_dodge())+
  geom_point(size=1, position=position_dodge(width=0.1))+
  facet_wrap(data_CS$genre)

#Does the film rating depend on the run time and genre?

movies$runtime_range = (ifelse(( movies$runtime < 60),"<1 hour",
                              ifelse((movies$runtime >= 60 & movies$runtime < 120),"1-2 hours",
                              ifelse((movies$runtime >= 120 & movies$runtime < 180),"2-3 hours",
                              ifelse((movies$runtime >= 180 & movies$runtime < 240),"3-4 hours",
                              ifelse((movies$runtime >= 240 & movies$runtime < 300),"4-5 hours",
                              ifelse((movies$runtime >= 300),">5 hours",
                              NA)))))))

ggplot(movies, aes(x =runtime_range, fill=genre))+
  geom_bar(position = position_dodge())+
  facet_wrap(movies$critics_rating)
