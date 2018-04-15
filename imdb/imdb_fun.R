library(readr)
movie_metadata <- read_csv("C:/Users/Brendan/Desktop/Datasets/IMDB/movie_metadata.csv")

director_gross <- sort(by(movie_metadata$gross, movie_metadata$director_name, sum), decreasing = TRUE)

barplot(director_gross[1:10])
#The top 10 highest grossing directos are no suprise. Burton, Cameron, Lucas, Nolan...
#It would be interesting to see which directors stay on the list when we look at avg gross per movie
#I'm betting lucas, cameron, and nolan stay on the list but burton will drop out

director_avg_gross <- sort(by(movie_metadata$gross, movie_metadata$director_name, mean), decreasing = TRUE)
barplot(director_avg_gross[1:10])
#Ha! I was way off! Who is Lee Unkrich!?!?!?!
#He's a pixar director, no wonder he has such a high per movie gross.

#how many non color movies are in the list?
sum(movie_metadata$color != "Color", na.rm = TRUE)
#what proportion of the movies are in color?
mean(movie_metadata$color == "Color", na.rm = TRUE)

#what countries are represented and whats the dist?
country.counts <- sort(table(movie_metadata$country), decreasing = TRUE)
barplot(country.counts[1:25])
#ignoring USA movies lets look at the distribution of 10 largest producers of international flicks
barplot(country.counts[2:11])
#look at the largest international producer by gross instead of number of titles


#it seems strange that Steven Spielberg isn't in the top 10 gross
spiel.ind <- which(movie_metadata$director_name == "Steven Spielberg")
movie_metadata[spiel.ind, ]
#turns out an NA is messing it all up
sum(as.numeric(movie_metadata$gross[spiel.ind]), na.rm = TRUE)
#4.1 billion puts him in the right spot
#need to handle the NAs before any exploration
