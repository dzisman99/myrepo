SDS 348 Project 1
================
Dylan Zisman
3/15/2021

## Dylan Zisman (dgz93) SDS 348 Project 1

INTRODUCTION

I grew up watching, reading about, and playing baseball from as early an
age as I can remember. Being from Houston, Texas, my favorite
professional team is the Astros. In recent years, the team has been
under fire (and understandably so) for cheating. A recent study came out
that begged the question: did this “cheating” actually yield a true
competitive advantage (strictly from a statistical standpoint)? The
findings of that study shocked the baseball world in that there was no
overbearing proof that any of the Astros’ actions that occurred during
the 2017 MLB season actually made the players on the team perform any
better than teams that hadn’t been cheating. This project proves to
substantiate this case.

This project uses data gathered from baseball-reference.com. I chose to
create an Excel spreadsheet of the data by individually researching what
players played on each team in the two divisions from which the teams
playing in the 2017 World Series hailed, those being the American League
West and the National League West divisions, during the 2019 and 2020
seasons. Only players who both played in multiple games in both 2019 and
2020 and batted above .000 in both 2019 and 2020 were chosen to be added
to the Excel data set. Players that either retired, didn’t play at the
major league level, or opted out of the 2020 season due to concerns
amidst the COVID-19 pandemic were omitted to ensure that every player in
the data set had valid data for every variable. Additionally, only
easy-to-explain-to-non-baseball-watchers statistics that can be
displayed in decimal form were chosen for this study (those being
batting average “AVG” and on-base percentage “OBP”).

The main focus of this study is to determine how a COVID-19-shortened
2020 season impacted player batting average across the Western divisions
of both the American and National Leagues in Major League Baseball
compared to the traditional-length 162-game 2019 season. Specifically,
the Houston Astros\*\* will be studied in comparison to other teams.
Some players listed spent time on multiple teams between 2019 and 2020,
but were counted in the data collection process if they played any part
of a season with the team for which they are identified in the data
sets.

\*\*The Houston Astros, who were penalized for stealing opposing teams’
signs during the 2017 season, are highlighted in this study. An
investigation performed by MLB in the winter offseason following the
2019 season concluded that the Astros used illegal methods to find out
what pitch an opposing pitcher was going to throw in real time using
illicitly-operated cameras. The fallout that resulted included the
1-year suspensions and subsequent firings of manager A.J. Hinch and
general manager Jeff Lunhow, an MLB-maximum $5 million fine, the loss of
both first and second-round draft picks in 2020 and 2021, and scorn from
around the league, players and fans alike. The toll on the team’s
current players was shown in terms of production, where players
seemingly struggled to hit as well as they had in seasons past, which
sparked a statistical debate at the conclusion of the 2020 season as to
whether or not the sign stealing really helped the players obtain a
significant advantage at the plate.

``` r
library(readxl)

# Teams in AL West: Houston Astros, Texas Rangers, Oakland Athletics, Los Angeles Angels, Seattle Mariners
ALWest = read_excel("AL_West.xlsx") # Imports data from the American League West

# Teams in NL West: Los Angeles Dodgers, San Diego Padres, Colorado Rockies, San Francisco Giants, Arizona Diamondbacks (D-Backs)
NLWest = read_excel("NL_West.xlsx") # Imports data from the National League West
```

Description of data set variables: - Name: Given name of player on
rosters during 2019 and/or 2020 MLB season - Team: Team name for which
player is on roster - Division: ALW = American League West, NLW =
National League West - AVG\_2019: Batting average (always listed as a
decimal by MLB) for the 162-game 2019 season - AVG\_2020: Batting
average (always listed as a decimal by MLB) for the 60-game 2020 season
- OBP\_2019: On-base percentage (always listed as a decimal by MLB) for
the 162-game 2019 season - OBP\_2020: On-base percentage (always listed
as a decimal by MLB) for the 60-game 2020 season

``` r
# Joining the data sets
library(dplyr)
```

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r
library(tidyverse)
```

    ## -- Attaching packages --------------------------------------- tidyverse 1.3.0 --

    ## v ggplot2 3.3.3     v purrr   0.3.4
    ## v tibble  3.0.5     v stringr 1.4.0
    ## v tidyr   1.1.2     v forcats 0.5.0
    ## v readr   1.4.0

    ## -- Conflicts ------------------------------------------ tidyverse_conflicts() --
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

``` r
baseball = full_join(ALWest, NLWest) # Creates data set baseball by joining all components of ALWest and NLWest
```

    ## Joining, by = c("Name", "Team", "Division", "AVG_2019", "AVG_2020", "OBP_2019", "OBP_2020")

Because the variables in common between the data sets were identical,
the `full_join()` function was most effective in combining the two. With
this in mind, there was no need for any rows or columns to be dropped.
Additionally, there are no NA values in the data set.

``` r
# Summary statistics data frame
Variable = c("Mean","Standard Deviation","Variance") # Column of types of summary statistic
AVG_2019 = c(mean(baseball$AVG_2019),sd(baseball$AVG_2019),var(baseball$AVG_2019)) # Obtains summary statistics for AVG_2019
AVG_2020 = c(mean(baseball$AVG_2020),sd(baseball$AVG_2020),var(baseball$AVG_2020)) # Obtains summary statistics for AVG_2020
OBP_2019 = c(mean(baseball$OBP_2019),sd(baseball$OBP_2019),var(baseball$OBP_2019)) # Obtains summary statistics for OBP_2019
OBP_2020 = c(mean(baseball$OBP_2020),sd(baseball$OBP_2020),var(baseball$OBP_2020)) # Obtains summary statistics for OBP_2020
baseball_summary_stats = data.frame(Variable, AVG_2019, OBP_2019, OBP_2020)
baseball_summary_stats
```

    ##             Variable    AVG_2019    OBP_2019    OBP_2020
    ## 1               Mean 0.243021622 0.318037838 0.299562162
    ## 2 Standard Deviation 0.044388472 0.050760239 0.065151226
    ## 3           Variance 0.001970336 0.002576602 0.004244682

The table above displays the basic summary statistics for each numeric
variable in the data set.

``` r
# Summary statistics by team
baseball_team_stats = baseball %>%
  group_by(Team) %>% # Groups by team
  summarize(`Mean_AVG_2019` = mean(AVG_2019), # Obtains summary statistics for AVG_2019
            `SD_AVG_2019` = sd(AVG_2019),
            `Var_AVG_2019` = var(AVG_2019),
            `Mean_AVG_2020` = mean(AVG_2020), # Obtains summary statistics for AVG_2020
            `SD_AVG_2020` = sd(AVG_2020),
            `Var_AVG_2020` = var(AVG_2020),
            `Mean_OBP_2019` = mean(OBP_2019), # Obtains summary statistics for OBP_2019
            `SD_OBP_2019` = sd(OBP_2019),
            `Var_OBP_2019` = var(AVG_2019),
            `Mean_OBP_2020` = mean(AVG_2020), # Obtains summary statistics for OBP_2020
            `SD_OBP_2020` = sd(AVG_2020),
            `Var_OBP_2020` = var(AVG_2020))
baseball_team_stats
```

    ## # A tibble: 10 x 13
    ##    Team  Mean_AVG_2019 SD_AVG_2019 Var_AVG_2019 Mean_AVG_2020 SD_AVG_2020
    ##  * <chr>         <dbl>       <dbl>        <dbl>         <dbl>       <dbl>
    ##  1 Ange~         0.232      0.0545      0.00297         0.233      0.0664
    ##  2 Astr~         0.238      0.0524      0.00274         0.225      0.0539
    ##  3 Athl~         0.240      0.0321      0.00103         0.201      0.0634
    ##  4 D-Ba~         0.252      0.0362      0.00131         0.211      0.0638
    ##  5 Dodg~         0.256      0.0405      0.00164         0.252      0.0433
    ##  6 Gian~         0.250      0.0520      0.00270         0.249      0.0631
    ##  7 Mari~         0.231      0.0432      0.00187         0.203      0.0516
    ##  8 Padr~         0.249      0.0368      0.00136         0.229      0.0607
    ##  9 Rang~         0.243      0.0353      0.00125         0.208      0.0467
    ## 10 Rock~         0.247      0.0495      0.00245         0.238      0.0575
    ## # ... with 7 more variables: Var_AVG_2020 <dbl>, Mean_OBP_2019 <dbl>,
    ## #   SD_OBP_2019 <dbl>, Var_OBP_2019 <dbl>, Mean_OBP_2020 <dbl>,
    ## #   SD_OBP_2020 <dbl>, Var_OBP_2020 <dbl>

The table above shows each of the summary statistics for each of the ten
teams from the baseball data set.

``` r
# Correlation calculations
cor(baseball$AVG_2019, baseball$OBP_2019) # Obtains correlation value between AVG and OBP in 2019
```

    ## [1] 0.8110287

``` r
cor(baseball$AVG_2019, baseball$OBP_2020) # Obtains correlation value between AVG in 2019 and OBP in 2020
```

    ## [1] 0.2310409

``` r
cor(baseball$AVG_2020, baseball$OBP_2019) # Obtains correlation value between AVG in 2020 and OBP in 2019
```

    ## [1] 0.2122162

``` r
cor(baseball$AVG_2020, baseball$OBP_2020) # Obtains correlation value between AVG and OBP in 2020
```

    ## [1] 0.8427912

OBP and AVG are strongly correlated, but only when comparing them within
the same season, and this makes perfect sense. OBP is calculated by OBP
= (Hits + Walks + Hit by Pitch) / (At Bats + Walks + Hit by Pitch +
Sacrifice Flies), while AVG is calculated by AVG = Hits / At Bats.

``` r
# Using dplyr core functions
astros = baseball %>% # Creates data set astros that obtains relevant data for players on the Houston Astros
  filter(Team=="Astros") %>% # Removes all rows that apply to players not on the Houston Astros
  select(-c(Team, Division)) %>% # Removes all columns named in function
  arrange(desc(AVG_2019)) %>% # Rearranges the data in descending order based on 2019 AVG
  mutate(`AVG_diff` = AVG_2019 - AVG_2020) # Creates column `AVG_diff` to explain difference between AVG in 2019 and 2020

astros_summary = baseball %>% # Creates new dataset astros_summary
  filter(Team=="Astros") %>% # Removes all rows that apply to players not on the Houston Astros
  select(-c(Team, Division)) %>% # Keeps `Name` as the only remaining non-numeric column
  summarize(Name, OBP_diff = OBP_2019 - OBP_2020,
            AVG_diff = AVG_2019 - AVG_2020) # Gives differences between OBP & AVG from the 2019 to the 2020 season

mean_diff = baseball %>% # Creates a data set of team name and mean AVG and OBP differential
  group_by(Team) %>% # Groups the subsequent data by team
  mutate(`AVG_diff` = AVG_2019 - AVG_2020, `OBP_diff` = OBP_2019 - OBP_2020) %>% # Creates the `AVG_diff` and `OBP_diff` variables
  summarize(AVG_team_diff = mean(AVG_diff), OBP_team_diff = mean(OBP_diff)) # Takes the mean of `AVG_diff` and `OBP_diff`
```

The data sets created above were done to clearly determine, on both a
by-player and by-team basis, whether or not the Houston Astros’ batting
averages and on-base percentages were adversely affected between the
fallout after the 2019 season and the 2020 season.

``` r
astros_summary # Note that a negative difference means that a player improved from 2019 to 2020
```

    ## # A tibble: 22 x 3
    ##    Name             OBP_diff AVG_diff
    ##    <chr>               <dbl>    <dbl>
    ##  1 Martin Maldonado  -0.0570  -0.002 
    ##  2 Yuli Gurriel       0.069    0.0660
    ##  3 Jose Altuve        0.067    0.0790
    ##  4 Carlos Correa      0.0320   0.015 
    ##  5 Alex Bregman       0.073    0.0540
    ##  6 Kyle Tucker       -0.006   -0.112 
    ##  7 George Springer    0.024   -0.04  
    ##  8 Josh Reddick       0.003    0.03  
    ##  9 Michael Brantley   0.008    0.011 
    ## 10 Abraham Toro       0.066    0.069 
    ## # ... with 12 more rows

The general consensus is that most players on the Houston Astros did in
fact see some sort of a drop in both their batting average and on-base
percentage during the COVID-shortened 2020 season.

While he doesn’t have the largest drop, the most notable drop comes from
Jose Altuve, who was the American League MVP in 2017 following a World
Series victory. People online gave him the most trouble following the
results of the investigation in 2019, saying that any accolades accrued
in 2017 were no longer valid, yet Astros players were quick to say that
Altuve played no part in cheating and that his title was legitimate.
Altuve suffered both physically on the field and mentally off the field
nonetheless. While it does not count towards the AVG and OBP variables
given in the data sets, Altuve’s postseason performance silenced any
doubt that he was relying on cheating to achieve elite status as a
hitter.

``` r
mean_diff # Note that a negative difference means that a team improved from 2019 to 2020
```

    ## # A tibble: 10 x 3
    ##    Team      AVG_team_diff OBP_team_diff
    ##  * <chr>             <dbl>         <dbl>
    ##  1 Angels        -0.000522      -0.0117 
    ##  2 Astros         0.0128         0.0218 
    ##  3 Athletics      0.0385         0.0340 
    ##  4 D-Backs        0.0409         0.0446 
    ##  5 Dodgers        0.00381        0.00444
    ##  6 Giants         0.000333      -0.00650
    ##  7 Mariners       0.0280         0.0185 
    ##  8 Padres         0.0203         0.0278 
    ##  9 Rangers        0.0349         0.0432 
    ## 10 Rockies        0.00912        0.0179

While at first one could have considered that, because players on the
Houston Astros performed worse during the 2020 season, that they were
truly obtaining a competitive advantage using their sign-stealing
system. However, the mean\_diff data set shows that just about every
team performed worse on average during the shortened season in 2020.
This data argues that the shortened-season was harder on everyone, and
that makes sense when you think about it: Spring Training was cut short
due to the COVID-19 outbreak in March of 2020, and the season resumed in
August with tweaks to the rules of the game for safety reasons. Attempts
by players and staff members to adapt to the changes that the new season
brought came with challenges in that players had to be at the top of
their game as quickly as possible, and many players who had spent the
onset of the pandemic in quarantine lacked the proper facilities to
prepare themselves fully for the season. As a result, players around the
league struggled to perform at their usual levels. This means that the
Astros, while possibly still having the mental tax of dealing with the
fallout of the 2017 scandal, were not the sole team that underperformed.
Thus, it cannot be confidently concluded that the sign-stealing
mechanism was effective in increasing batting performance.

``` r
# Making the correlation heat map
library(reshape2) # Contains the `melt` function needed to create the correlation matrix
```

    ## 
    ## Attaching package: 'reshape2'

    ## The following object is masked from 'package:tidyr':
    ## 
    ##     smiths

``` r
library(ggplot2)
heatMapData = baseball %>%
  select(-c(Name, Team, Division)) # Removes non-numeric columns from data set
cormatrix = cor(heatMapData) # Creates a correlation matrix
melted_cormatrix = melt(cormatrix) # Reshapes the data to have unique variable-ID combinations
melted_cormatrix = melted_cormatrix %>%
  rename("Correlation Coefficient" = `value`) # Renames the `value` variable from the `melt` function to change the name of the legend on the heat map
ggplot(melted_cormatrix, aes(x=Var1, y=Var2, fill=`Correlation Coefficient`)) + # Creates the correlation heat map
  geom_tile() + labs(x = "Variable 1", y = "Variable 2", title = "Correlation Heat Map")
```

![](Project-1_files/figure-gfm/unnamed-chunk-9-1.png)<!-- -->

The heat map depicted above explains exactly what was mentioned earlier
in that when variables from different seasons are compared, they have a
weak correlation (unless they are the same variable, in which case of
course they have a correlation coefficient of 1). When variables from
the same season are compared, they have a strong correlation for reasons
explained in the correlation calculations section of this project.

``` r
# Creating the second data visualization model
ggplot(baseball, aes(x = AVG_2019, y = AVG_2020, col = (OBP_2019 + OBP_2020)/2)) + # Sets aesthetic parameters
  geom_point(size = 4, pch = 19) + # Creates large circular points on scatter plot
  geom_smooth(method = lm, col = 'red') + # Creates a linear line of best fit
  labs(title = "Comparison of By-Season Batting Average", x = "2019 Batting Average", # Sets labels
       y = "2020 Batting Average", col = "Mean On-Base Percentage")
```

    ## `geom_smooth()` using formula 'y ~ x'

![](Project-1_files/figure-gfm/unnamed-chunk-10-1.png)<!-- -->

A simple scatter plot provides insight into the by-player trends from
one season to the next. The general trend is that a player who recorded
a higher batting average in 2019 performed slightly better on average in
2020. Additionally, as expected, players who had lower batting averages
also had lower on-base percentages on average than players who had
higher batting averages in 2019 and/or 2020.

``` r
# Creating the third data visualization model
library(RColorBrewer)
sample_size = baseball %>% # Creates data set sample_size for number of players on each team
  group_by(Team) %>%
  summarize(sample_size=n())

baseball_with_sample_size = left_join(sample_size, baseball) # left_joins to create new data set
```

    ## Joining, by = "Team"

``` r
# Creates a violin wrapping box plot for the 2019 season
ggplot(baseball_with_sample_size, aes(x = Team, y = AVG_2019, fill = sample_size)) +
  geom_violin(width=1) + geom_boxplot(width=0.15, color = "black", alpha=0.2) + # Creates violin and box plot
  scale_fill_gradient(low="blue", high="red") + # Adds a color gradient for team density
  labs(title = "Violin Wrapping Box Plot of 2019 Batting Average by Team", # Sets necessary labels
       y = "2019 Batting Average", fill = "# Batters on Team") # Kept x as "Team"
```

![](Project-1_files/figure-gfm/unnamed-chunk-11-1.png)<!-- -->

``` r
# Creates a second violin wrapping box plot for the 2020 season
ggplot(baseball_with_sample_size, aes(x = Team, y = AVG_2020, fill = sample_size)) +
  geom_violin(width=1) + geom_boxplot(width=0.15, color = "black", alpha=0.2) + # Creates violin and box plot
  scale_fill_gradient(low="yellow", high="green") + # Adds a color gradient for team density
  labs(title = "Violin Wrapping Box Plot of 2020 Batting Average by Team", # Sets necessary labels
       y = "2020 Batting Average", fill = "# Batters on Team") # Kept x as "Team"
```

![](Project-1_files/figure-gfm/unnamed-chunk-12-1.png)<!-- -->

A violin wrapping box plot is effective for multiple reasons when
substantiating the case of the sign stealing, and its subsequent
fallout, having any true advantage (or disadvantage) on the Astros
players’ batting average. In both 2019 and 2020, the Astros found
themselves in the middle of the pack in terms of batting average
distributions. They did this in spite of having more players hitting,
which, should the players be performing poorly, would drop them to or
near the bottom of the ten teams in the data set. One more thing to note
is that, in terms of median player batting average, the Astros were
ranked last of the 10 teams in 2019, but fourth in 2020.

``` r
# PAM clustering/k-means or PCA
library(cluster)
library(factoextra)
```

    ## Welcome! Want to learn more? See two factoextra-related books at https://goo.gl/ve3WBa

``` r
pca = baseball %>% # Creates new data set for PCA
  select_if(is.numeric) %>% # Removes categorical variables
  scale() %>% # Scales to 0
  prcomp # Standardizes the data such that mean(pca) = 0 and var(pca) = 1
pca # Visualize the rotated data
```

    ## Standard deviations (1, .., p=4):
    ## [1] 1.5270849 1.1500087 0.4860235 0.3305647
    ## 
    ## Rotation (n x k) = (4 x 4):
    ##                PC1        PC2        PC3        PC4
    ## AVG_2019 0.4892193 -0.5118235 -0.5677943  0.4198937
    ## AVG_2020 0.5007488  0.5051205 -0.4332328 -0.5535460
    ## OBP_2019 0.4950518 -0.5032657  0.5521752 -0.4435650
    ## OBP_2020 0.5146250  0.4791801  0.4301412  0.5661503

``` r
# Conclude number of principal components and % of variance explained by each PC
get_eig(pca) # Eigenvalues over 1 indicate principal components
```

    ##       eigenvalue variance.percent cumulative.variance.percent
    ## Dim.1  2.3319883        58.299706                    58.29971
    ## Dim.2  1.3225199        33.062998                    91.36270
    ## Dim.3  0.2362188         5.905471                    97.26817
    ## Dim.4  0.1092730         2.731825                   100.00000

There are 2 principal components that need to be considered in this PCA.
Additionally, PC1 accounts for about 58.3% of the variance and PC2
accounts for about 33.1% for a combined 91.4% of the variance explained
by the first two principal components.

``` r
# Continuation of PCA with visualization
pca_data = as.data.frame(pca$x) # Creates PCA values as a data frame
pca_data = bind_cols(pca_data, baseball) # Adds the baseball data set to the PCA
pca_data = pca_data %>%
  select(1,2,3,4,6) # Selects the 4 PCA columns, as well as Team from the baseball data set
ggplot(pca_data, aes(x = PC1, y = PC2, color = Team)) + geom_point() # Scatter plot of pca_data by team
```

![](Project-1_files/figure-gfm/unnamed-chunk-15-1.png)<!-- -->

The scatter plot of the PCA data shows that no one team is significantly
distinct from another in any way. This substantiates the idea that the
Houston Astros are not any different from another team, or the rest of
the league as a whole. Even teams that performed very poorly (in terms
of win-loss record) in 2019 and 2020 like the Rangers and Mariners don’t
differ significantly from the Astros. On the flip side, teams that
performed very well in 2019 and 2020 like the Dodgers and Athletics
aren’t notably different than the Astros.

``` r
# Visualize the 10 top contributions of the individuals to the PCs in a bar graph
pca_data2 = as.data.frame(pca$x) # Resets pca_data to remove `Team` and include `Name` as new data set pca_data2
pca_data2 = bind_cols(pca_data2, baseball)
pca_data2 = pca_data2 %>%
  select(1,2,3,4,5) # Selects PC data and `Name` column
# pca_data2 is used to match name to individual in the fviz_contrib plot

fviz_contrib(pca, choice = "ind", axes = 1:2, top = 10)
```

![](Project-1_files/figure-gfm/unnamed-chunk-16-1.png)<!-- --> The ten
individuals who contribute most to the PCs are (in descending order): 1.
Franklin Baretto (Athletics) 2. Erik Kratz (Giants) 3. Braden Bishop
(Mariners) 4. Anthony Bemboom (Angels) 5. Beau Taylor (Athletics) 6.
Cesar Puello (Angels) 7. Brendan Rodgers (Rockies) 8. Jake Fraley
(Mariners) 9. Jack Mayfield (Astros) 10. Francisco Mejia (Padres)

``` r
fviz_pca_biplot(pca)
```

![](Project-1_files/figure-gfm/unnamed-chunk-17-1.png)<!-- -->

``` r
# Creates a PCA variable plot
fviz_pca_var(pca, col.var = "black", repel = T) # repel parameter prevents variable names from touching on the graph
```

![](Project-1_files/figure-gfm/unnamed-chunk-18-1.png)<!-- -->

All 4 of the numeric variables used have a high quality of
representation. Additionally, the variables are grouped together by
year, proving that data collected from the same season will move
together, while data collected from different seasons will have a very
weak (if any) correlation.

SUMMARY:

While baseball players, coaches, and fans will all argue that the Astros
gained a truly unfair advantage by using an illegal signal-relaying
system, statistically, this is not the case. Studies done by the MLB
statistics teams have shown that there actually may have been a slight
disadvantage for the players who were relying on a cheating mechanism.
While in concept, knowing what pitch is coming should inherently make a
player more likely to hit the ball. However, the cheating system
involved people understanding what signs the catcher was giving, and
signs are subject to change. Staff members involved in guessing what
pitch was coming had lots of shortcomings. Human error and lack of full
knowledge of another team’s signs as they change throughout the game
meant that sometimes, the Astros would tell a batter that the wrong
pitch was coming. Rather than a professional player using their
knowledge of game-time situations to best prepare for whatever pitch was
coming their way next, they would instead try to time up a pitch that
was “probably” coming next. The conclusion of this study is that the
Houston Astros, in an attempt to gain an unfair advantage, didn’t have
any significant success, and the fallout from the scandal have made the
existence of the sign-relaying system nothing but a lingering regret.
