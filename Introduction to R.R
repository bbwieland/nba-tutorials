### PROJECT OVERVIEW ----

# install packages if necessary:
# remove the "#' and run the below three lines

# install.packages("tidyverse")
# install.packages("rvest")

# necessary libraries: 

library(tidyverse) # for lots of useful R functions!
library(rvest) # for web scraping

# Our research question: How can we predict a team's win total for next season using their current-season statistics?

# A few sub-questions:
# -- What variables are important for predicting team record?
# -- How accurate can our predictions be?

### DATA COLLECTION (WEB SCRAPING) & DATA WRANGLING ----

# Where to get team data from?
# Basketball Reference!

url = "https://www.basketball-reference.com/leagues/NBA_2021.html" # the URL we want —— team data from 2021
site = read_html(url) # reading the HTML page of our URL using rvest
site_tables = site %>% html_table() # finding all the tables on the page, again via rvest

team_data_2021 = site_tables[[11]] # selecting the table we want from our list of all tables on that page

## Now, we need to clean that table! 

colnames(team_data_2021) = team_data_2021[1,] # making the column names = the first row of the table

team_data_2021 = team_data_2021[-c(1,32),1:18] # now, removing the first row of the table (the column names)

# Looking at our data so far in a few different ways:

team_data_2021 # show the WHOLE dataframe (or, as much as our console will output)
head(team_data_2021) # show the "head" of our dataframe — just the first few rows (default: 6)
str(team_data_2021) # short for "structure" — a different way of looking at our data
View(team_data_2021) # opens the dataframe in a new window up top

# What else needs to be cleaned up?

team_data_2021_clean = team_data_2021 %>%
  select(-`NA`) %>% # note: why is NA like `NA`? R column naming rules!
  mutate(across(.cols = 3:17,as.numeric)) %>% # changing all our columns to numeric
  mutate(Team = gsub("\\*","",Team)) %>% # very basic regex in R... why not just "*"?
  mutate(W = W*82/72, L = L*82/72, PW = PW*82/72, PL = PL*82/72) # adjusting due to COVID-shortened season

## OK, now we have our data to use for predicting. How about the response variable? 

site_2022 = read_html("https://www.basketball-reference.com/leagues/NBA_2022.html")
team_data_2022 = (site_2022 %>% html_table())[[11]] # a more compact (but confusing) way of running this code

colnames(team_data_2022) = team_data_2022[1,]
team_data_2022 = team_data_2022[-c(1,32),1:18]

team_data_2022_clean = team_data_2022 %>%
  select(Team,W) %>% # We JUST need the response variable here! 
  mutate(Team = gsub("\\*","",Team)) %>% 
  mutate(W = as.numeric(W)) %>%
  rename(W_Next = W) # "rename" allows you to rename your columns using the syntax `new name = old name`

## How do we combine our data?

data = left_join(team_data_2021_clean,team_data_2022_clean,by = "Team") # *_join will combine your datasets "by" a common variable

# we're ready to proceed and start analyzing our dataset!

### EXPLORATORY DATA ANALYSIS ----

# In this section, we'll use base R (no additional packages) to look at the relationships in our data.
# -- Which variables might be useful predictors?
# -- Which might not provide much information for prediction?

# We can do this textually... 

cor(data$W,data$W_Next) # compute correlations using Pearson's R
cor(data$PW,data$W_Next) # Pearson's R: a value between -1 and 1. Positive = + correlation, negative = - correlation
cor(data$MOV,data$W_Next) # Values closer to 1 indicate a stronger correlation.
cor(data$SRS,data$W_Next) # Values closer to 1 indicate a stronger correlation.

# What should we expect this correlation to output?
cor(data$L,data$W_Next)

# What do we expect cor() to output for a less-useful predictor?
cor(data$Pace,data$W_Next)
cor(data$`3PAr`,data$W_Next) # note: the `3PAr` instead of 3PAr — column naming rules!

# Or visually by plotting...

plot(data$W,data$W_Next)
plot(data$PW,data$W_Next)

# One final option: a visual or textual "correlation matrix"
# If you don't have the corrplot package installed, remove the '#' below and run the line of code
# install.packages("corrplot")

library(corrplot)

cor(data[,3:18]) # computing the correlation matrix of the numeric columns in our data

correlation_matrix = cor(data[,3:18]) # saving that matrix as an object, so we can use it

corrplot(correlation_matrix) # this really showcases which correlations are strong/weak! 
corrplot(correlation_matrix,method = "pie",type = "upper") # messing w/ the output a bit... lots of customization!

# A note for model building: we can see lots of "multicollinearity" in these plots! Will need to take that into account when we get to the next stage.

# What does multicollinearity look like?
plot(data$PW,data$W) # high multicollinearity
plot(data$Age,data$Pace) # low multicollinearity

### MODEL BUILDING ----

model_W = lm(W_Next~W,data) # The most basic model type: a linear regression model using wins this year to predict wins next year
summary(model_W) # How do we interpret this?

model_PW = lm(W_Next~PW,data) # The same single-predictor model, this time using Pythagorean wins. 
summary(model_PW) # How does it compare to our last model?

model_combined = lm(W_Next ~ W + PW,data) # How do we expect this model to perform?
summary(model_combined) # Note the effects of multicollinearity on the individual p-values!

# What if we include ALL the numeric variables?
model_all = lm(W_Next ~.,data[,3:18]) # fitting a model using every numeric predictor we have. What's the `.` do?
summary(model_all) # note the NAs... byproduct of the matrix algebra

# Making an informed choice of variables for the model from our EDA:
model_our_choices = lm(W_Next~MOV + Age + `TS%` ,data)
summary(model_our_choices)

# Or... using an R package to choose our variables for us: 
# You might need the line of code below, just remove the `#`
# install.packages("MASS")

library(MASS)

best_model = stepAIC(model_all,direction = "both",trace = F) # Using Akaike Information Criteria to choose the "best" predictors

detach(package:MASS) # we need to "unload" MASS, so that select() continues to work!

# Now let's evaluate this "best" model: 

summary(best_model)

# OK, let's proceed with this to test our model. 

### MODEL VALIDATION ----

# Once you have a model, there are many different ways you can evaluate it:
# 1. Analyze if it is a 'good' model visually & look at problem points
# 2. Calculate some descriptive statistics for the model
# 3. Test its predictive power on out-of-sample data

# 1. ANALYZING VISUALLY & CHECKING OUR MODEL:

plot(best_model) # creates some plots to check the assumptions of a linear regression model

predicted_data = data %>%
  mutate(predicted_win_total = predict(best_model,data)) %>% # note use of the predict(model,data) function!
  mutate(error = W_Next - predicted_win_total) %>% # error = actual - predicted 
  arrange(-error) # sorting the data by a variable

head(predicted_data) %>%
  select(Team,W,W_Next,predicted_win_total,error) # Which teams did the model underestimate?

tail(predicted_data) %>%
  select(Team,W,W_Next,predicted_win_total,error) # Which teams did the model overestimate?

predicted_data[12:18,] %>%
  select(Team,W,W_Next,predicted_win_total,error) # Which teams did the model "get right"?

# 2. CALCULATING MODEL STATISTICS:

# This will be easiest with another third-party package: modelr
# Again, you might need this code below - just remove the `#`
# install.packages("modelr")

library(modelr)

mae(best_model,data) # Mean absolute error (between actual & predicted values)
qae(best_model,data) # Percentiles for that absolute error
rmse(best_model,data) # Square root of the errors squared... harsher on large errors!

# 3. TESTING THE MODEL ON OUT-OF-SAMPLE DATA: 

# This is kind of the gold standard of model evaluation.
# A lot of common errors (overfitting, most notably) will be "exposed" by this method. 

# Let's write a function that allows us to perform that web scraping from above for ANY season!
# Basically identical to the code from web scraping... but note the use of a keyword in the function!

seasonDataScraper = function(initialSeason) { # note the function(initialSeason) - we need to specify initialSeason!
  url = paste0("https://www.basketball-reference.com/leagues/NBA_",initialSeason,".html") # picking the URL based on initialSeason
  site = read_html(url) 
  site_tables = site %>% html_table() 
  
  team_data = site_tables[[11]] 
  
  colnames(team_data) = team_data[1,] 
  
  team_data = team_data[-c(1,32),1:18] 
  
  team_data_clean = team_data %>%
    select(-`NA`) %>%
    mutate(across(.cols = 3:17,as.numeric)) %>%
    mutate(Team = gsub("\\*","",Team))
  
  site_next = read_html(paste0("https://www.basketball-reference.com/leagues/NBA_",initialSeason + 1,".html")) # picking the URL based on initialSeason
  team_data_next = (site_next %>% html_table())[[11]] 
  
  colnames(team_data_next) = team_data_next[1,]
  team_data_next = team_data_next[-c(1,32),1:18]
  
  team_data_next_clean = team_data_next %>%
    select(Team,W) %>% 
    mutate(Team = gsub("\\*","",Team)) %>% 
    mutate(W = as.numeric(W)) %>%
    rename(W_Next = W) 
  
  data = left_join(team_data_clean,team_data_next_clean,by = "Team")
  return(data) # return() at the end of a function can be used to return an object.
}

data_2018 = seasonDataScraper(2018) # using our function to get a sample season. This will be used to predict 2018-19 win totals using 2017-18 data.

# Let's make a prediction from that data, and then validate it!

predicted_data_2018 = data_2018 %>%
  mutate(predicted_win_total = predict(best_model,data_2018)) %>% # note use of the predict(model,data) function!
  mutate(error = W_Next - predicted_win_total) %>% # error = actual - predicted 
  arrange(-error) # sorting the data by a variable

# How does the model hold up when predicting 'out-of-sample' data? 

mae(best_model,data_2018)
qae(best_model,data_2018)
rmse(best_model,data_2018)
rsquare(best_model,data_2018) # all the way down to 0.281... from 0.66! 

# How can we improve this? Increasing the sample used to fit the model. 
# A good independent project that you should have all the skills to do at this point: 
# Using our seasonDataScraper function, use the data from 2015-2019 (5 years worth) to fit a new model. See how it performs vs. out-of-sample data! 

### DATA VISUALIZATION ----

# We'll need one more package for this section — again, remove the `#` on the next line & run the next line if you need it installed!
# install.packages("teamcolors")

library(teamcolors) 

# OK, now we have our predictions from our model
# What if we want to visualize these?

# Here, we'll just work with a bar graph.
# However, keep in mind that there are DOZENS of different ways to do data visualization using the packages we currently have loaded!
# They're just a little bit outside the scope of this tutorial. 

# First, we need to scrape the 2022 season data in order to make our predictions for 2022-23.
# We can do this using a modified version of the seasonDataScraper function.

seasonDataScraper2 = function(initialSeason) { # note the function(initialSeason) - we need to specify initialSeason!
  url = paste0("https://www.basketball-reference.com/leagues/NBA_",initialSeason,".html") # picking the URL based on initialSeason
  site = read_html(url) 
  site_tables = site %>% html_table() 
  
  team_data = site_tables[[11]] 
  
  colnames(team_data) = team_data[1,] 
  
  team_data = team_data[-c(1,32),1:18] 
  
  team_data_clean = team_data %>%
    select(-`NA`) %>%
    mutate(across(.cols = 3:17,as.numeric)) %>%
    mutate(Team = gsub("\\*","",Team))
  
  return(team_data_clean) # return() at the end of a function can be used to return an object.
}

data_22 = seasonDataScraper2(2022) # scraping the 2022 full season data

# Now, let's use that predict(model,data) function to add a column of predicted 2023 win totals. 

data_22_predicted = data_22 %>%
  mutate(projected_wins = predict(best_model,data_22)) %>% # adding the predicted totals as a new column
  arrange(-projected_wins) # sorting by projected 2022-23 win totals

# Cool... but how can we visualize this?
# First, we're going to need a color palette for each team. We can get this using that teamcolors package. 

str(teamcolors) # taking a look at the 'teamcolors' dataset

# It has color hex codes for each team's primary & secondary colors! 
# We can combine it with our dataset using another left_join, like we did earlier. 

data_22_predicted = data_22_predicted %>%
  left_join(teamcolors,by = c("Team" = "name")) # note the syntax for when the "by" columns don't have the same name!

# Now onto the plotting! 
# Let's start with the bar graph.

ggplot(data = data_22_predicted, mapping = aes(y = Team, x = projected_wins)) + # in aes(), we put the plot's "aesthetics" — x, y, fill, color, etc. 
  geom_col() # adding the bars onto the chart — this is the SIMPLEST POSSIBLE VERSION of what we're doing! 

# Now let's start prettying this up. 
# First step: put it in "Pareto chart" style, with the bars stacked greatest-to-least
# This pattern makes the chart easier for viewers to interpret

ggplot(data = data_22_predicted, mapping = aes(y = reorder(Team,projected_wins), x = projected_wins)) + # note the use of reorder() to arrange the teams
  geom_col()

# Let's add some color!

ggplot(data = data_22_predicted, mapping = aes(y = reorder(Team,projected_wins), x = projected_wins)) +
  geom_col(aes(fill = primary,color = secondary),alpha = 0.8) + # note the addition of two new "aesthetics" — color and fill. Also, the use of "alpha" to edit the opacity of the bars
  scale_fill_identity() + # these two lines tell R to choose the color & fill of the bars based on the 'identity' of the color/fill aesthetics — AKA the hex codes!
  scale_color_identity()

# Having the text on the side eats a lot of valuable space on our graph. What if it was inside the bars instead? 

ggplot(data = data_22_predicted, mapping = aes(y = reorder(Team,projected_wins), x = projected_wins)) +
  geom_col(aes(fill = primary,color = secondary),alpha = 0.8) + 
  geom_text(aes(label = mascot),hjust = 0,x = 1) + # Adding a new layer, or "geom", with team names. note that putting the x = ... OUTSIDE aes() makes it the same value for every text label! "hjust" = horizontal justification of the text
  scale_fill_identity() + 
  scale_color_identity() +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank()) # removing the axis ticks & axis text from the y-axis

# Now let's clean up the theme of the chart. We'll go for a 538ish look... but know there's near-infinite customizability here!

ggplot(data = data_22_predicted, mapping = aes(y = reorder(Team,projected_wins), x = projected_wins)) +
  geom_col(aes(fill = primary,color = secondary),alpha = 0.8) + 
  geom_text(aes(label = mascot),hjust = 0,x = 1,family = "Chivo") + # note the addition of family = in here! That specifies the font family. 
  geom_text(aes(label = round(projected_wins,1),x = projected_wins - 1),hjust = 1,family = "Chivo") + # adding the projected win totals to the bar graph
  scale_fill_identity() + 
  scale_color_identity() +
  theme_minimal(base_family = "Chivo") + # adding a 'theme' with lots of presets. base_family sets the base font family!
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        plot.title = element_text(hjust = 0.5), # centering the plot title
        panel.grid = element_blank()) + # removing that background panel grid, because it looked messy
  labs(x = "Projected Wins", y = "", # relabeling the axes — making X look better, and removing Y's label
       title = "Projected 2022-23 Win Totals") # adding a title to the plot

# A quick note: use the "Zoom" button in the bottom-right panel to resize/adjust your graph!
# There are a variety of ways to save these plots, most notably ggsave()

### REVIEW: WHAT WE'VE COVERED ----

# 1. Data collection & web scraping using rvest
# 2. Data wrangling using the tidyverse
# 3. Exploratory data analysis in base R & using corrplot
# 4. Building a linear regression model using R's lm() syntax
# 5. Evaluating our models using plots, modelr, and out-of-sample validation data
# 6. Data visualization using ggplot