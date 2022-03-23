# Shiny_NBA

## URL TO PLAYWITH
https://brianzheng.shinyapps.io/shiny_2/?_ga=2.244598789.1640937978.1648018182-1638503067.1648018182

## Introduction
  I use the same data as in portfolio 1 and analyse the same problem about who is the best three points shooter. The improvement in the shiny app is that the app gives the user the permission to choose specific players and get all their shooting habits and players' pictures.
  
## What are some interesting facts that you learned through the visualization. Provide at least one unexpected finding
  The interesting facts I find out through the visualization is that players who have high 3 points percentage prefer to shoot three points than the mid-range. The way I find out the fact is by clicking the player in my scatter plot and analyzing the pie graph of the shooting area for the player. For example, Seth Curry, one of the best three points shooter, shoot more than 50% outside the three points line while he only shoots around 10% between 16 feet and three points line.
  The second interesting fact is that some players(especially players as Center) with extremely high three percentage will not shoot three points a lot. We can see Dwight Howard has highest three points percentage in that season. However, he barely shoots three points. The fact may be that he is not good at 3 points. In other words, his high 3 points percentages are from the luck. Maybe in the future, we need to filter players with more than 2 3-points attempts per game to better analyse who is the best three points shooter.
  

## How did you create the interface?
  I create the interface by thinking about how to help the user get the information about the players who they cared about. 
  1) The key graph in my app is the scatter plot in the first row because the app focus on finding the best three points shooter. By clicking the certain point in that graph, the user will get a player's picture and the players' shooting area (pie chart). It's hard to find picture for every player. However, I find out a URL, which each player's picture is connected to certain ID. By changing the Id of the URL, I will have access to most of the players. I think that's useful because the user can directly the player they are searching for. 
  2) The second input permission I give to the user is to select the statistics they cared about. The user is able to get the histogram of all the players in that statistics after choosing the statistics they want to learn.
  3) The third permission I give to the user is to brush from the histogram. By brushing in the histogram, the users are able to filter some players they want to investigate more. After choosing all the players they want to investigate, the users can use click event to pick specific player in the scatter plot.

## Were there any data preparation steps? What guided the style customizations and interface layout that you used
  Yes, there are a lot data preparation steps. As I said before, I need to find the specific player's ID to give the user the permission to access the picture of players. The IDs are stored in a .Json file, so it takes me lots of time to figure out how to transfer that to a data frame. However, I do love the result from the hard work. Finally, I set up some factor levels to make my graph's variables follow the order I want.
  
  I choose a specific theme to make my app more beautiful and decent. For the layout, I put two graphs about all players in the first row. Then, the second row include all the info about a specific player. The reason I set the layout is that I want to let the user see my data from general to specific points. Moreover, in the last part, I put the data table selected from the histogram because I want to give the user some info about all the players they brush in the histogram. 
