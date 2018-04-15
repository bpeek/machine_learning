import requests
import lxml.html
import pandas as pd
import numpy as np
import csv
import re
"""
get the list of movie ids from the Kaggle 5000 movie dataset.
the keywords are incomplete so I'm trying to build a scraper to get all of the keywords.
it looks like the scraper Kaggle used only took the keywords off the main page instead 
of looking at the full list on the dedicated keyword page.
"""



#read movie_metadata.csv into a DataFrame
movies_csv = pd.read_csv('C:/Users/Brendan/Desktop/MovieDatasets/movie_metadata.csv')


movie_main_page_links = list(movies_csv['movie_imdb_link'])
#create a list of links with the end trimmed off to allow us to append keywords to the address
movie_main_page = []
for i in range(len(movie_main_page_links)):
    movie_main_page.append(movie_main_page_links[i][:36])
    
#now we have a list of links that we can feed into the get_keywords method

        
# a method that takes an movie's main page address returns all the keywords associated with that particular movie
def get_keywords(movie_main_page):
    page = requests.get(movie_main_page+'keywords')
    parsed_page = lxml.html.document_fromstring(page.text)
    keywords = parsed_page.find_class('sodatext')
    all_keywords = []
    end = len(keywords)
    for i in range(end): all_keywords.append(keywords[i].text_content().strip())
    return all_keywords

"""
create a loop to go through all the 5000+ keyword sites and stick the keyword
list that is returned in a dictionary with the FULL movie_imdb_link as the key.
I want to merge the results with the Kaggle dataset so I need the keys to match the
keys in the csv file.
"""

#create a dictionary with the unique movie page link as the key and a list of keywords as the value
#takes about one second per page
#I need to wait until I have 1.5 hours to let it run
"""
movies = {}
n = len(movie_main_page)
for i in pbar(range(n)):
        movies[movie_main_page_links[i]] = get_keywords(movie_main_page[i])
        print(str(i+1)+" of "+str(n))
        
"""
#scraped data is now saved in C:/Users/Brendan/Desktop/MovieDatasets/movies.csv

#export the movies dict to a csv so you don't have to rescrape everything again!
"""
w = csv.writer(open("C:/Users/Brendan/Desktop/MovieDatasets/movies.csv", "w"))
for key, val in movies.items(): w.writerow([key, val])
"""


#import movies.csv as a DataFrame and name the columns explicitly because there are no headers
#name the link column to match the other csv to allow an easy merge
movies_df = pd.read_csv("C:/Users/Brendan/Desktop/MovieDatasets/movies.csv", header=None, names = ['movie_imdb_link','keywords'])

#combine both DataFrames on movie_imdb_link column
movies_complete = movies_df.merge(movies_csv, how='inner', on = 'movie_imdb_link')

#export the complete DataFrame to a csv file 
w = csv.writer(open("C:/Users/Brendan/Desktop/MovieDatasets/movies_complete.csv", "w"))
for key, val in movies_complete.items(): w.writerow([key, val])

"""everything above this line worked as intended"""

"""
looks at the first list of keywords in the keywords column of the movies_complete DataFrame
use this to iterate through each list to compile a master list from which the number
of occurrences of each keyword can be counted
"""
x = movies_complete['keywords'][0]
y = re.split('\W+\s', x)


#remove non-alpha and non-whitespace characters from strings in y[0]
#need to generalize 
re.sub(r'[^\w\s]', '', y[0])

#create a list with keywords with unnecessary characters removed
clean_list = []
for i in range(len(y)):
    clean_list.append(re.sub(r'[^\w\s]', '', y[i]))
    

clean_list

#movies_complete_dummies = pd.get_dummies(movies_complete)
#darn, this made dummies out of each keyword list not each keyword
#find a way to flatten the keywords so the list isn't the dummy, the keyword is.

#this creates a csv file with 31486 columns. This needs to be narrowed to run a regression with 5000 obs
#maybe narrow it down to 50 most common keywords
"""
w = csv.writer(open("C:/Users/Brendan/Desktop/MovieDatasets/movies_complete_dummies.csv", "w"))
for key, val in movies_complete_dummies.items(): w.writerow([key, val])
"""