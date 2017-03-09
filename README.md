Project: NewSearch

**Introduction**

*Note: since we are using a very big dataset, the web page may be running a little slow, please stay patient*

Contributors: Manesh Jhawar, Kushal Jhunjhunwalla, Helly Shah, Alice Li

API: http://developer.nytimes.com/article_search_v2.json

Nowadays, when we are always looking forward to something new, always looking for new technology, we might want to rest a minute and look back to what we have been through. Many of the times, ideas are inspired by an incident from the past. We decided that it will be fascinating if we could build a web app for people to search back in time, a time that is special or of interest to them. With this app, an individual will be able to know a day back in time when he or she was not even born, or she could revisit a day that means a lot to her.

The dataset we used is the New York Times Archive API. NYT Archive API is a great data source because New York Times itself covers news and articles from worldwide, and is a rather believable source. With NYT data, we will be able to cover a wide range of events or incidents around the world on that day. NYT Archive API is very neatly organized, and contains enough information for our web apps, accordingly, it contains the dates that an article was posted. NYT API is categorized into sections like movies, books, top searched etc. We used the Archive API that provides lists of NYT articles by a month and goes back all the way to 1851. The archive API is similar to the article search API where we can simply pass the year and month we want, and the API will return a JSON object with articles of that month.

In order to access this API, we signed up for an API key and acknowledged that it is limited to 2000 requests per day. Though there is a limitation to access to this data, it will be enough for our current use and web building.
From the API, the columns we extracted from the dataset were: snippet, main headline, section, keywords, as well as the URL source. On our web page, we have five different tabs that take in different data inputs. The first tab is an About page that explains our web page. The next tab is an article search where the user will be able to select articles by date. A result will show indicating the publication date, headline, and section. By clicking on the headline of each result, a new page will open displaying the full article. Users will be about to change how many entries they want to see on this page, and there is also a search bar for interest searches.

The second tab shows the Section Visualization. This part includes four visualizations that are organized by Monty, Day, Top 10, and Popularity. Visualizations use colors to indicate different sections and displayed using hover-interactive bar plot, pie plot, and dot plot.

The third tab indicates section comparison where users will be able to choose two sections they want to compare. Our comparison visualization is indicated by a line, which clear show the comparing trend of the two sections. Each line represents the number of articles posted according to the day of the month. The line graph is also hovered interactive, showing major points and line labels.

The last tab gives a dot visualization on ten of the most discussed topics of the month of interest. Users will be able to choose a month from the sidebar and visualize the ten most discussed topics of that month. The popularity of each topic will be indicated by the size of the dot, the bigger the dot, the more popular the topic.

Our goal for this project is not just building a good looking app, we want our app to be as useful to the society as possible. With our app, people will be able to learn from the past, revisits and remember memorable moments from their lives. We are currently in a world with lots of issues, people might feel overwhelmed, but in fact, it has always been like this. Let’s look back and see how things were resolved. We have made great improvements till this point today, let’s have more faith going towards tomorrow.
