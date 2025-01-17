# Instructions

## Movies
You work for Paramount Pictures. Your boss has just acquired data about how much audiences and critics like movies as well as other variables. She is interested in learning what attributes make a movie popular. She wants your team to figure it all out.

The data set is comprised of 651 randomly sampled movies produced and released before 2106. Note that some of the variables are only there for informational purposes and it might not be the best idea to consider them in your analysis. For example, actor 1 through actor 5 variables were used to determined whether the movie casts an actor or actress who won a best actor or actress Oscar.

### Research Questions
These are some of the research questions that might lead your analysis. You might as well come up with other interesting ones. Based on this data set:
1. What can we say about the relationship between audience scores and at least four of the other variables in this dataset?
2. Is there a difference in the score given by audience members (or critics) for movies of different genres? And for movies with different MPAA ratings? If so, which categories seem to be significantly different for each other?

### Dataset
Click here to download the data: [movies.RData](https://github.com/rguitar96/SDA_HW_2019/blob/master/Homework%201.1/movies.RData)


### Codebook: *variable description*
+ **title**: Title of movie
+ **title_type**: Type of movie (Documentary, Feature Film, TV Movie)
+ **genre**: Genre of movie (Action & Adventure, Comedy, Documentary, Drama, Horror, Mystery & Suspense, Other)
+ **runtime**: Runtime of movie (in minutes)
+ **mpaa_rating**: MPAA rating of the movie (G, PG, PG-13, R, Unrated)
+ **studio**: Studio that produced the movie
+ **thtr_rel_year**: Year the movie is released in theaters
+ **thtr_rel_month**: Month the movie is released in theaters
+ **thtr_rel_day**: Day of the month the movie is released in theaters
+ **dvd_rel_year**: Year the movie is released on DVD
+ **dvd_rel_month**: Month the movie is released on DVD
+ **dvd_rel_day**: Day of the month the movie is released on DVD
+ **imdb_rating**: Rating on IMDB
+ **imdb_num_votes**: Number of votes on IMDB
+ **critics_rating**: Categorical variable for critics rating on Rotten Tomatoes (Certified Fresh, Fresh, Rotten)
+ **critics_score**: Critics score on Rotten Tomatoes
+ **audience_rating**: Categorical variable for audience rating on Rotten Tomatoes (Spilled, Upright)
+ **audience_score**: Audience score on Rotten Tomatoes
+ **best_pic_nom**: Whether or not the movie was nominated for a best picture Oscar (no, yes)
+ **best_pic_win**: Whether or not the movie won a best picture Oscar (no, yes)
+ **best_actor_win**: Whether or not one of the main actors in the movie ever won an Oscar (no, yes) – note that this is not necessarily whether the actor won an Oscar for their role in the given movie
+ **best_actress** win: Whether or not one of the main actresses in the movie ever won an Oscar (no, yes) – not that this is not necessarily whether the actresses won an Oscar for their role in the given movie
+ **best_dir_win**: Whether or not the director of the movie ever won an Oscar (no, yes) – not that this is not necessarily whether the director won an Oscar for the given movie
+ **top200_box**: Whether or not the movie is in the Top 200 Box Office list on BoxOfficeMojo (no, yes) director: Director of the movie
+ **actor1**: First main actor/actress in the abridged cast of the movie
+ **actor2**: Second main actor/actress in the abridged cast of the movie
+ **actor3**: Third main actor/actress in the abridged cast of the movie
+ **actor4**: Fourth main actor/actress in the abridged cast of the movie
+ **actor5**: Fifth main actor/actress in the abridged cast of the movie
+ **imdb_url**: Link to IMDB page for the movie
+ **rt_url**: Link to Rotten Tomatoes page for the movie

You can also visit Rotten Tomatoes webpage for more information on some variable categories (certified fresh, fresh or rotten status of a movie or TV show).
