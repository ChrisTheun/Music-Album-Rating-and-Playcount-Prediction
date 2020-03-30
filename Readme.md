In this repository, I stored the code I wrote for my Pitchfork music album rating and Last.fm playcount prediction project, which can be found on my online portfolio.

In the project, I make use of Nolan Conaway's public Kaggle dataset containing data on more more than 18.000 Pitchfork reviews, which can be accessed through the following link: https://www.kaggle.com/nolanbconaway/pitchfork-data
Moreover, I used data retrieved from the Last.fm API to supplement the already existing Pitchfork album review data with even more features.

First, the pitchfork.R file contains code necessary to format the Pitchfork review dataset into a suitable format for analysis as well as perform various visualizations.
Second, the lastfm_API.R file contains code for retrieving the Last.fm data through their API.
Finally, the predict pf scores.R file contains code which merges the two datasets together and performs the analysis.