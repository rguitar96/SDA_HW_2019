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
movies$studio[movies$title == "Oliver & Company"] <- "Walt Disney Pictures"
#https://es.wikipedia.org/wiki/Oliver_y_su_pandilla
movies$studio[movies$title == "Attack of the 50 Foot Woman"] <- "Woolner Brothers Pictures Inc."
#https://www.imdb.com/title/tt0051380/
movies$studio[movies$title == "Inbred"] <- "Melanie Light"
#https://www.imdb.com/title/tt1723124/fullcredits
movies$studio[movies$title == "Caveman"] <- "United Artists"
#https://es.wikipedia.org/wiki/El_cavern%C3%ADcola
movies$studio[movies$title == "Dirty Sanchez: The Movie"] <- "Vertigo Films"
#https://www.rottentomatoes.com/m/dirty_sanchez
movies$studio[movies$title == "The Man Who Sued God"] <- "Australian Film Finance Corporation (AFFC), New South Wales Film & Television Office, Showtime Australia See more"
#https://www.imdb.com/title/tt0268437/
movies$studio[movies$title == "Inserts"] <- "Film and General Productions"
#https://www.filmaffinity.com/es/film740308.html
movies$studio <- factor(movies$studio)

#Another way of checkinig which data is missing:
#DVD realease date
movies[!complete.cases(movies['dvd_rel_year']),]
movies$dvd_rel_year[movies$title == "Charlie: The Life and Art of Charles Chaplin"] <- 2003
movies$dvd_rel_month[movies$title == "Charlie: The Life and Art of Charles Chaplin"] <- 11
movies$dvd_rel_day[movies$title == "Charlie: The Life and Art of Charles Chaplin"] <- 5
#https://www.imdb.com/title/tt0379730/releaseinfo
movies$dvd_rel_year[movies$title == "The Squeeze"] <- 2015
movies$dvd_rel_month[movies$title == "The Squeeze"] <- 6
movies$dvd_rel_day[movies$title == "The Squeeze"] <- 9
#https://medium.com/@releasebandyal/producer-michael-doven-announces-release-of-the-squeeze-on-dvd-75a982d0a047
movies$dvd_rel_year[movies$title == "Electric Dreams"] <- 1984
#https://en.wikipedia.org/wiki/Electric_Dreams_(film)
movies$dvd_rel_year[movies$title == "The Last Remake of Beau Geste"] <- 2010
movies$dvd_rel_month[movies$title == "The Last Remake of Beau Geste"] <- 1
movies$dvd_rel_day[movies$title == "The Last Remake of Beau Geste"] <- 11
#https://en.wikipedia.org/wiki/The_Last_Remake_of_Beau_Geste

movies[!complete.cases(movies['director']),]
movies$director[movies$title == "Lorenzo's Oil"] <- "George Miller"
#https://es.wikipedia.org/wiki/Lorenzo%27s_Oil_(pel%C3%ADcula)
movies$director[movies$title == "The Ninth Gate"] <- "Roman Polanski"
#https://es.wikipedia.org/wiki/The_Ninth_Gate

movies[!complete.cases(movies['actor4']),]
movies$actor4[movies$title == "Attack of the 50 Foot Woman"] <- "Roy Gordon"
#https://www.imdb.com/title/tt0051380/fullcredits/?ref_=tt_ov_st_sm

movies[!complete.cases(movies['actor5']),]
movies$actor5[movies$title == "Attack of the 50 Foot Woman"] <- "George Douglas"
#https://www.imdb.com/title/tt0051380/fullcredits/?ref_=tt_ov_st_sm
movies$actor5[movies$title == "The Illusionist (L'illusionniste)"] <- "Eleanor Tomlinson"
#https://en.wikipedia.org/wiki/The_Illusionist_(2006_film)

summary(movies)
#Since the missing values do not belong to any a priori critical attribute, we will not delete any instances.
##################################################################################################################################################

#We delete the columns we are not interested in for the analysis.
movies <- subset(movies, select = -c(imdb_url,rt_url))
##################################################################################################################################################

#1. What can we say about the relationship between audience scores and at least four of
#the other variables in this dataset?


