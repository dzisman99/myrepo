SDS 348 Project 2
================
Dylan Zisman
3/15/2021

## Dylan Zisman (dgz93) SDS 348 Project 1

INTRODUCTION

As of the time of this research, there are 898 Pokemon, including
regional variants and Mega Evolutions. Pokemon are differentiated from
one another not only on appearances, but by a plethora of both
categorical and numerical variables. Categorical variables include
weight, height, and typing, which includes both a primary and - for some
- a secondary typing, with 18 different types of Pokemon to choose from.
Pokemon are not only different combinations of types and sizes, but also
have specific base statistics that determine their fighting abilities in
comparison to one another.

The six base statistics are HP (lit: “Hit Points” \[indicator of
health\]), Attack (A.K.A. Physical Attack), Defense (A.K.A. Physical
Defense), Special Attack, Special Defense, and Speed. Ranging anywhere
between 1 and 255, each individual stat is combined into the aptly named
“Base Stat Total” by which most players of the games in the Pokemon
franchise evaluate the overall strength of their favorite Pokemon.

Pokemon are oftentimes based off of real-life animals, plants, people,
places, or things, so it’s no surprise that many of the things that
people would consider to be “strong” like dragons and wild beasts give
rise to strong Pokemon. With that in mind, this study aims to find out
whether or not type is indicative of overall strength (base stat total).

Many fans of the Pokemon franchise will be quick to come to the
conclusion that dragon type Pokemon will definitely have higher base
stat totals considering that many dragon types are considered
“legendary” or “pseudo-legendary” Pokemon, which are always stronger
than their non-legendary and non-pseudo-legendary counterparts. Many
Pokemon games are structured so that you can only capture dragon type
Pokemon towards the end of the game, when the other Pokemon you’ve been
training since the beginning are strong enough to fight among the
Dragons. However, there are also legendaries and pseudo-legendaries that
are not Dragon types. This study looks to see if the presence of the
many legendary Dragon types is enough to prove that there is a
significant difference between base stat totals between the types.

``` r
# Obtaining data set
library(readxl)
pokemon = read_excel("pokemon.xlsx")
```

All data was obtained from PokemonDB (short for Pokemon Database), which
is one of the 4 major websites involved in collecting Pokemon data
(Serebii, Bulbapedia, and Pokemon.com are the others) for public use.
Base stats are not values that can be found by Pokemon players just by
playing the games or collecting Pokemon cards, but rather they are
official numbers assigned to each Pokemon in the base code of the games
themselves, and have been verified by both the Pokemon Company and by
data miners, as base stats have changed throughout the generations
(Pokemon is currently in Generation VIII, with each new set of games
that features new Pokemon and a new region generally being the clear
indicators of the start of a new generation).

``` r
# Remove columns in the data set that won't be used
library(tidyverse)
```

    ## -- Attaching packages --------------------------------------- tidyverse 1.3.0 --

    ## v ggplot2 3.3.3     v purrr   0.3.4
    ## v tibble  3.0.5     v dplyr   1.0.3
    ## v tidyr   1.1.2     v stringr 1.4.0
    ## v readr   1.4.0     v forcats 0.5.0

    ## -- Conflicts ------------------------------------------ tidyverse_conflicts() --
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

``` r
library(dplyr)
pokemon = pokemon %>%
  select(NAME, TYPE1, TYPE2, HP, ATK,
         DEF, SP_ATK, SP_DEF, SPD, TOTAL)
```

Description of data set variables: - Name –&gt; Pokemon’s name with
variant type if applicable - TYPE1 –&gt; primary typing (every Pokemon
has this) - TYPE2 –&gt; secondary typing (not every Pokemon has this) -
HP –&gt; base HP stat - ATK –&gt; base Physical Attack stat - DEF –&gt;
base Physical Defense stat - SP\_ATK –&gt; base Special Attack stat -
SP\_DEF –&gt; base Special Defense stat - SPD –&gt; base Speed stat -
TOTAL –&gt; base stat total

``` r
# Obtaining summary statistics
# Finding the average base stat total for Pokemon by type
library(tidyr)
library(ggplot2)

pokemonByType = pokemon %>% # Create a data set with Pokemon types and mean base stat totals
  select(TYPE1, TYPE2, TOTAL) %>%
  pivot_longer(cols = c(TYPE1, TYPE2), names_to = "TYPE") %>%
  drop_na() %>%
  group_by(value) %>%
  summarize(meanTotal = mean(TOTAL), n = n()) %>%
  drop_na() # Removes the category that includes only Pokemon with one type,
            # as they have already been included in the other type values

ggplot(pokemonByType, aes(x = value, y = meanTotal, fill = value)) + geom_col() + geom_text(aes(label = n), vjust  = -.5) + labs (x = "Type", y = "Mean Base Stat Total")
```

![](Project-2_files/figure-gfm/unnamed-chunk-3-1.png)<!-- -->

The numbers above each column in the above bar plot indicates how many
Pokemon are present for each type. Although there are only 1048 Pokemon
in the data set, there are 1602 entries in this bar plot because there
is type overlap, meaning that a Pokemon that identifies as, for example,
a Pokemon like Torterra, who is both a Grass and a Ground type, would
count as one Pokemon in both the `Grass` and the `Ground` type groups.
An additional thing to note that is not explicitly stated is that there
are nearly 500 (497 to be exact) Pokemon that are monotype, meaning that
they do not have a secondary typing. Those Pokemon were initially set to
their own group in the data set `pokemonByType` as type = NA, but they
have been removed to avoid redundancy.

It is already apparent that one type sticks out, and, as stated in the
introduction, it is no surprise that the dragon type stands out above
the rest.

``` r
# MANOVA
pokemon = pokemon %>% # Creates new column `logTOTAL`
  mutate(logTOTAL = log(TOTAL))
# Testing assumptions
# Check assumptions visually
pokemon %>%
  pivot_longer(cols = c(TYPE1, TYPE2), names_to = "TYPE") %>%
  drop_na() %>%
  ggplot(aes(y = logTOTAL)) +
  geom_boxplot(aes(fill = as.factor(value)))
```

![](Project-2_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->

``` r
# Check for normality
pokemon %>%
  pivot_longer(cols = c(TYPE1, TYPE2), names_to = "TYPE") %>%
  group_by(value) %>%
  drop_na() %>%
  summarize(p.value = shapiro.test(logTOTAL)$p.value)
```

    ## # A tibble: 18 x 2
    ##    value      p.value
    ##  * <chr>        <dbl>
    ##  1 Bug      0.0000660
    ##  2 Dark     0.0107   
    ##  3 Dragon   0.0000439
    ##  4 Electric 0.000210 
    ##  5 Fairy    0.00450  
    ##  6 Fighting 0.00341  
    ##  7 Fire     0.00406  
    ##  8 Flying   0.000632 
    ##  9 Ghost    0.00432  
    ## 10 Grass    0.0000558
    ## 11 Ground   0.0355   
    ## 12 Ice      0.000421 
    ## 13 Normal   0.000172 
    ## 14 Poison   0.00406  
    ## 15 Psychic  0.000137 
    ## 16 Rock     0.0157   
    ## 17 Steel    0.00127  
    ## 18 Water    0.0000477

``` r
# Check for equal variance
pokemon %>%
  pivot_longer(cols = c(TYPE1, TYPE2), names_to = "TYPE") %>%
  drop_na() %>%
  group_by(value) %>%
  summarize(variance = var(logTOTAL))
```

    ## # A tibble: 18 x 2
    ##    value    variance
    ##  * <chr>       <dbl>
    ##  1 Bug        0.120 
    ##  2 Dark       0.0760
    ##  3 Dragon     0.0865
    ##  4 Electric   0.0664
    ##  5 Fairy      0.111 
    ##  6 Fighting   0.0671
    ##  7 Fire       0.0635
    ##  8 Flying     0.0782
    ##  9 Ghost      0.0684
    ## 10 Grass      0.0723
    ## 11 Ground     0.0770
    ## 12 Ice        0.0761
    ## 13 Normal     0.0848
    ## 14 Poison     0.0740
    ## 15 Psychic    0.0893
    ## 16 Rock       0.0550
    ## 17 Steel      0.0644
    ## 18 Water      0.0786

The p-value for base stat total (variable `TOTAL`) was not significant
for a handful of the types, so a log transformation was performed in
order to meet all the assumptions of ANOVA.

Hypothesis Testing: H0 –&gt; Pokemon type is not a significant indicator
of base stat total. H1 (two-tailed) –&gt; Pokemon type is a significant
indicator of base stat total.

``` r
# Run ANOVA to compare the length by different doses
summary(aov(logTOTAL ~ TYPE1 * TYPE2, data = pokemon))
```

    ##              Df Sum Sq Mean Sq F value   Pr(>F)    
    ## TYPE1        17  5.836  0.3433   5.067 5.44e-10 ***
    ## TYPE2        17  3.392  0.1995   2.945 8.75e-05 ***
    ## TYPE1:TYPE2 139  8.425  0.0606   0.895    0.778    
    ## Residuals   380 25.745  0.0677                     
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 494 observations deleted due to missingness

``` r
# Which means differ? Conduct post-hoc analysis
pairwise.t.test(pokemon$logTOTAL, pokemon$TYPE1, p.adj = "none")
```

    ## 
    ##  Pairwise comparisons using t tests with pooled SD 
    ## 
    ## data:  pokemon$logTOTAL and pokemon$TYPE1 
    ## 
    ##          Bug     Dark    Dragon  Electric Fairy   Fighting Fire    Flying 
    ## Dark     0.00591 -       -       -        -       -        -       -      
    ## Dragon   9.7e-11 0.00066 -       -        -       -        -       -      
    ## Electric 4.1e-05 0.34953 0.00621 -        -       -        -       -      
    ## Fairy    0.03114 0.97877 0.00579 0.47682  -       -        -       -      
    ## Fighting 0.00150 0.65276 0.00381 0.66761  0.73282 -        -       -      
    ## Fire     1.4e-05 0.27378 0.00856 0.87332  0.40514 0.56371  -       -      
    ## Flying   0.12211 0.87133 0.08082 0.74616  0.89359 0.92786  0.68863 -      
    ## Ghost    0.00120 0.64001 0.00355 0.67267  0.72328 0.99051  0.56696 0.92222
    ## Grass    0.00829 0.54484 7.1e-06 0.07518  0.62179 0.26974  0.04679 0.64019
    ## Ground   0.00689 0.94528 0.00127 0.41025  0.97579 0.71350  0.33017 0.90334
    ## Ice      0.00764 0.95319 0.00133 0.40824  0.98210 0.70844  0.32904 0.89932
    ## Normal   0.06532 0.15757 6.5e-08 0.00622  0.27340 0.05721  0.00291 0.39703
    ## Poison   0.03598 0.63799 0.00018 0.16203  0.68032 0.37113  0.11984 0.67059
    ## Psychic  1.3e-08 0.03712 0.07102 0.22733  0.11253 0.12872  0.28951 0.37739
    ## Rock     8.2e-05 0.41963 0.00439 0.89198  0.54132 0.75949  0.76671 0.79657
    ## Steel    5.9e-06 0.07504 0.13783 0.30757  0.14824 0.18741  0.37054 0.38977
    ## Water    0.00031 0.97165 3.2e-05 0.21779  0.95477 0.56225  0.14771 0.85117
    ##          Ghost   Grass   Ground  Ice     Normal  Poison  Psychic Rock   
    ## Dark     -       -       -       -       -       -       -       -      
    ## Dragon   -       -       -       -       -       -       -       -      
    ## Electric -       -       -       -       -       -       -       -      
    ## Fairy    -       -       -       -       -       -       -       -      
    ## Fighting -       -       -       -       -       -       -       -      
    ## Fire     -       -       -       -       -       -       -       -      
    ## Flying   -       -       -       -       -       -       -       -      
    ## Ghost    -       -       -       -       -       -       -       -      
    ## Grass    0.25600 -       -       -       -       -       -       -      
    ## Ground   0.70161 0.51245 -       -       -       -       -       -      
    ## Ice      0.69663 0.52330 0.99271 -       -       -       -       -      
    ## Normal   0.05117 0.32293 0.15511 0.16252 -       -       -       -      
    ## Poison   0.35945 0.96987 0.60247 0.61122 0.43402 -       -       -      
    ## Psychic  0.12620 0.00115 0.05576 0.05654 1.2e-05 0.01195 -       -      
    ## Rock     0.76610 0.10479 0.48343 0.48061 0.01029 0.20271 0.17842 -      
    ## Steel    0.18638 0.00981 0.09741 0.09770 0.00071 0.03027 0.96567 0.25670
    ## Water    0.54519 0.44091 0.90741 0.91733 0.05449 0.59400 0.00522 0.28632
    ##          Steel  
    ## Dark     -      
    ## Dragon   -      
    ## Electric -      
    ## Fairy    -      
    ## Fighting -      
    ## Fire     -      
    ## Flying   -      
    ## Ghost    -      
    ## Grass    -      
    ## Ground   -      
    ## Ice      -      
    ## Normal   -      
    ## Poison   -      
    ## Psychic  -      
    ## Rock     -      
    ## Steel    -      
    ## Water    0.03112
    ## 
    ## P value adjustment method: none

``` r
pairwise.t.test(pokemon$logTOTAL, pokemon$TYPE2, p.adj = "none")
```

    ## 
    ##  Pairwise comparisons using t tests with pooled SD 
    ## 
    ## data:  pokemon$logTOTAL and pokemon$TYPE2 
    ## 
    ##          Bug     Dark    Dragon  Electric Fairy   Fighting Fire    Flying 
    ## Dark     0.00846 -       -       -        -       -        -       -      
    ## Dragon   0.00078 0.33227 -       -        -       -        -       -      
    ## Electric 0.19475 0.25328 0.06068 -        -       -        -       -      
    ## Fairy    0.11916 0.07482 0.00317 0.95036  -       -        -       -      
    ## Fighting 0.00059 0.28641 0.92518 0.05142  0.00209 -        -       -      
    ## Fire     0.01424 0.97869 0.37778 0.29562  0.12848 0.33421  -       -      
    ## Flying   0.03918 0.16280 0.00586 0.71546  0.43710 0.00367  0.25570 -      
    ## Ghost    0.01810 0.72002 0.18286 0.38531  0.17197 0.15277  0.76962 0.35663
    ## Grass    0.20329 0.03707 0.00130 0.74090  0.67583 0.00085  0.07160 0.22325
    ## Ground   0.06796 0.16100 0.01046 0.83066  0.65797 0.00729  0.23477 0.80344
    ## Ice      0.00101 0.29689 0.84476 0.05851  0.00620 0.90824  0.33301 0.01291
    ## Normal   0.49420 0.04746 0.00577 0.50858  0.43192 0.00453  0.06854 0.19604
    ## Poison   0.45528 0.00356 4.0e-05 0.36905  0.18693 2.2e-05  0.01186 0.01987
    ## Psychic  0.01065 0.76150 0.16264 0.32396  0.09561 0.13151  0.81396 0.21690
    ## Rock     0.09060 0.34373 0.07283 0.77212  0.64226 0.06034  0.39914 0.99357
    ## Steel    0.00319 0.74268 0.48328 0.15503  0.02104 0.42163  0.75222 0.04516
    ## Water    0.24880 0.06088 0.00457 0.73123  0.68382 0.00333  0.09586 0.29762
    ##          Ghost   Grass   Ground  Ice     Normal  Poison  Psychic Rock   
    ## Dark     -       -       -       -       -       -       -       -      
    ## Dragon   -       -       -       -       -       -       -       -      
    ## Electric -       -       -       -       -       -       -       -      
    ## Fairy    -       -       -       -       -       -       -       -      
    ## Fighting -       -       -       -       -       -       -       -      
    ## Fire     -       -       -       -       -       -       -       -      
    ## Flying   -       -       -       -       -       -       -       -      
    ## Ghost    -       -       -       -       -       -       -       -      
    ## Grass    0.09244 -       -       -       -       -       -       -      
    ## Ground   0.32313 0.40004 -       -       -       -       -       -      
    ## Ice      0.17267 0.00281 0.01643 -       -       -       -       -      
    ## Normal   0.09021 0.62424 0.28258 0.00678 -       -       -       -      
    ## Poison   0.01260 0.39605 0.07840 0.00018 0.92906 -       -       -      
    ## Psychic  0.92518 0.04529 0.21583 0.16041 0.06145 0.00330 -       -      
    ## Rock     0.52686 0.44426 0.88636 0.07211 0.30531 0.15165 0.44629 -      
    ## Steel    0.47902 0.00918 0.05771 0.41975 0.02108 0.00043 0.48551 0.20420
    ## Water    0.12726 0.96013 0.44340 0.00659 0.67803 0.50009 0.07888 0.46205
    ##          Steel  
    ## Dark     -      
    ## Dragon   -      
    ## Electric -      
    ## Fairy    -      
    ## Fighting -      
    ## Fire     -      
    ## Flying   -      
    ## Ghost    -      
    ## Grass    -      
    ## Ground   -      
    ## Ice      -      
    ## Normal   -      
    ## Poison   -      
    ## Psychic  -      
    ## Rock     -      
    ## Steel    -      
    ## Water    0.02204
    ## 
    ## P value adjustment method: none

Instead of a MANOVA, this data set requires 2 separate ANOVAs to compare
the significance of `TOTAL` on both `TYPE1` and `TYPE2`. The ANOVA shows
that type is in fact a significant indicator of base stat total (both
p-values are &lt; .05), regardless of TYPE1 or TYPE2. Additionally, it
makes sense that the interaction between the two typing variables is not
significant (p-value &gt; .05), because there is no true interaction
between `TYPE1` and `TYPE2`, as they are the more or less the same
thing. The test yields F values of 5.067 and 2.945, and an interaction F
value of .895. Also, based on the paired t-tests, it seems that the
means are significantly different between a fair amount of type pairs,
indicating that there may be some type of base stat total hierarchy in
which certain types of Pokemon yield significantly higher base stat
totals from Pokemon of other specific types, who in turn also yield
significantly higher base stat totals from Pokemon of more distinct
types. For example, the p-value for the comparison of base stat total
means between the types Grass and Psychic is significant, and since the
earlier bar plot of mean base stat totals shows that Psychic type
Pokemon have a noticeably higher mean base stat total, this indicates
that Psychic type Pokemon have a significantly higher mean base stat
total than Grass type Pokemon. Seasoned players of the Pokemon games
would immediately recognize this significance to be true because there
are many more legendary and pseudo-legendary Pokemon that possess the
Psychic typing than the Grass typing. The presence of these p-values
that are &lt; .05 indicates that the null hypothesis is to be rejected.

The two key points to notice here are the two types that have more
p-values &lt; .05 than others. Those two types are the Bug type and the
Dragon type. Based on knowledge of the game that is now backed up by
statistical evidence, Bug type Pokemon have significantly lower mean
base stat totals than most other types. Additionally, as mentioned in
the introduction, it is now statistically proven that Dragon type
Pokemon have a significantly higher mean base stat total than most other
types.

``` r
# Randomization Testing
sample1 = pokemon %>%
  select(TYPE1, TYPE2, TOTAL) %>%
  pivot_longer(cols = c(TYPE1, TYPE2), names_to = "TYPE") %>%
  drop_na() %>%
  select(value, TOTAL) %>%
  rename(type = value) %>%
  arrange(type)

# Reorganize the data for future use
pokemon = pokemon %>%
  pivot_longer(cols = c(TYPE1, TYPE2), names_to = "TYPE") %>%
  drop_na()

# Represent the distribution of base stat total for each type
ggplot(sample1, aes(TOTAL, fill = type)) +
  geom_histogram(bins = 100) +
  facet_wrap(~ type, ncol = 6) +
  theme(legend.position = "none")
```

![](Project-2_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->

``` r
# Calculate the mean difference between the two conditions
true_diff = sample1 %>%
  group_by(type) %>%
  summarize(means = mean(TOTAL)) %>%
  summarize(mean_diff = diff(means)) %>%
  pull
true_diff
```

    ##  [1]  71.915340  79.523670 -81.960290  -9.287726  36.702544 -14.944704
    ##  [7] -10.568585   2.901081 -35.772876  21.827160  23.319038 -59.897683
    ## [13]   5.706751  74.844920 -37.342409  42.639249 -59.806428

``` r
Fs <- replicate(5000,{
  # Randomly permute the response variable across doses
  new = pokemon %>%
    mutate(TOTAL = sample(TOTAL))
  # Compute variation within groups
  SSW = new %>%
    group_by(value) %>%
    summarize(SSW = sum((TOTAL - mean(TOTAL))^2)) %>%
    summarize(sum(SSW)) %>% 
    pull
  # Compute variation between groups
  SSB = new %>% 
    mutate(mean = mean(TOTAL)) %>%
    group_by(value) %>% 
    mutate(groupmean = mean(TOTAL)) %>%
    summarize(SSB = sum((mean - groupmean)^2)) %>%
    summarize(sum(SSB)) %>%
    pull
  # Compute the F-statistic (ratio of MSB and MSW)
  # df for SSB is 3 groups - 1 = 2
  # df for SSW is 1602 observations - 3 groups = 1599
   (SSB / 2) / (SSW / 1599)
})

# Represent the distribution of the F-statistics for each randomized sample
hist(Fs, prob=T); abline(v = 5.067, col = "red"); abline(v = 2.945, col = "red")
```

![](Project-2_files/figure-gfm/unnamed-chunk-6-2.png)<!-- -->

This randomization test shows that there is a fairly normal distribution
of F-statistics in the random sample. Like in the real sample, there are
a few mean differences that are much higher than the others and a few
that are much lower than the others. This shows that, even if there were
5000 types of Pokemon, some types would yield Pokemon with significantly
higher base stat totals than others.

``` r
# Linear Regression
sample1 = sample1 %>%
  mutate(logTOTAL = log(TOTAL)) # Adds the logTOTAL column for sample1
                                # in order to standardize the data for lm
fit = lm(logTOTAL ~ type, data = sample1)
summary(fit)
```

    ## 
    ## Call:
    ## lm(formula = logTOTAL ~ type, data = sample1)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -0.88514 -0.21252  0.07176  0.20185  0.61922 
    ## 
    ## Coefficients:
    ##              Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)   5.88556    0.02928 201.019  < 2e-16 ***
    ## typeDark      0.19328    0.04436   4.357 1.40e-05 ***
    ## typeDragon    0.35161    0.04419   7.957 3.33e-15 ***
    ## typeElectric  0.19304    0.04436   4.351 1.44e-05 ***
    ## typeFairy     0.15189    0.04592   3.307 0.000963 ***
    ## typeFighting  0.25163    0.04402   5.716 1.30e-08 ***
    ## typeFire      0.22034    0.04265   5.166 2.69e-07 ***
    ## typeFlying    0.19061    0.03913   4.871 1.22e-06 ***
    ## typeGhost     0.20148    0.04491   4.486 7.78e-06 ***
    ## typeGrass     0.11807    0.03851   3.066 0.002208 ** 
    ## typeGround    0.16585    0.04279   3.876 0.000111 ***
    ## typeIce       0.21994    0.04734   4.646 3.66e-06 ***
    ## typeNormal    0.07590    0.03820   1.987 0.047096 *  
    ## typePoison    0.09528    0.04338   2.197 0.028195 *  
    ## typePsychic   0.25632    0.03899   6.574 6.61e-11 ***
    ## typeRock      0.19153    0.04369   4.384 1.24e-05 ***
    ## typeSteel     0.27870    0.04436   6.282 4.30e-10 ***
    ## typeWater     0.14161    0.03692   3.836 0.000130 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.2808 on 1584 degrees of freedom
    ## Multiple R-squared:  0.07321,    Adjusted R-squared:  0.06326 
    ## F-statistic:  7.36 on 17 and 1584 DF,  p-value: < 2.2e-16

``` r
# Checking assumptions for linear regression
# Residuals against fitted values plot to check for any problematic patterns (nonlinear, equal variance)
plot(fit, which = 1)
```

![](Project-2_files/figure-gfm/unnamed-chunk-8-1.png)<!-- -->

``` r
# Q-Q plot to check for normality of the residuals
plot(fit, which = 2)
```

![](Project-2_files/figure-gfm/unnamed-chunk-8-2.png)<!-- -->

This linear regression model shows that, using the standardized
`logTOTAL` values, every type significantly affects base stat total. The
appropriate assumptions have been checked. While some types have
p-values below, but closer to .05 like Fairy, Grass, Ground, Normal,
Poison, and Water, we still see that the same types have extremely low
p-values, with Dragon type’s p-value being raised to the power of -15.
With each test, the idea that the Dragon type is a significant indicator
of a high base stat total is being further solidified.

``` r
# Logistic Regression
# Divide clump thickness into 3 levels: small, medium, large
pokemon = pokemon %>%
  mutate(clump_logTOTAL = ntile(logTOTAL, 3)) %>%
  mutate(clump_logTOTAL = factor(clump_logTOTAL, labels = c("S", "M", "L"))) %>%
  mutate(y = ifelse(TOTAL >= 600, 1, 0))

# Fit a new regression model
# Variable `value` indicates typing, regardless of if it is primary or secondary
fit1 = glm(y ~ value, data = pokemon, family = binomial(link = "logit"))
summary(fit1)
```

    ## 
    ## Call:
    ## glm(formula = y ~ value, family = binomial(link = "logit"), data = pokemon)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -1.0612  -0.5134  -0.3640  -0.2938   2.5557  
    ## 
    ## Coefficients:
    ##               Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)   -3.09104    0.51116  -6.047 1.47e-09 ***
    ## valueDark      1.28275    0.61456   2.087 0.036862 *  
    ## valueDragon    2.81146    0.56386   4.986 6.16e-07 ***
    ## valueElectric -0.02985    0.78051  -0.038 0.969490    
    ## valueFairy     1.01160    0.64962   1.557 0.119416    
    ## valueFighting  1.25049    0.61413   2.036 0.041731 *  
    ## valueFire      1.11696    0.61251   1.824 0.068218 .  
    ## valueFlying    1.09537    0.58517   1.872 0.061222 .  
    ## valueGhost     1.21073    0.62398   1.940 0.052338 .  
    ## valueGrass     0.09531    0.66050   0.144 0.885263    
    ## valueGround    1.13095    0.61267   1.846 0.064904 .  
    ## valueIce       0.50704    0.72812   0.696 0.486191    
    ## valueNormal   -0.13580    0.68489  -0.198 0.842824    
    ## valuePoison   -0.11441    0.77969  -0.147 0.883339    
    ## valuePsychic   2.09087    0.55139   3.792 0.000149 ***
    ## valueRock      0.45199    0.68962   0.655 0.512200    
    ## valueSteel     1.85630    0.58478   3.174 0.001502 ** 
    ## valueWater     0.41002    0.60674   0.676 0.499183    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 1162.7  on 1601  degrees of freedom
    ## Residual deviance: 1046.8  on 1584  degrees of freedom
    ## AIC: 1082.8
    ## 
    ## Number of Fisher Scoring iterations: 5

``` r
# Interpret coefficients as odds ratios
table(pokemon$y, pokemon$value)
```

    ##    
    ##     Bug Dark Dragon Electric Fairy Fighting Fire Flying Ghost Grass Ground Ice
    ##   0  88   61     41       68    56       63   72    103    59   120     71  53
    ##   1   4   10     31        3     7       10   10     14     9     6     10   4
    ##    
    ##     Normal Poison Psychic Rock Steel Water
    ##   0    126     74      87   70    55   146
    ##   1      5      3      32    5    16    10

``` r
# Odds of 600 club membership by type
odds_bug = (4/92) / (88/92)
odds_dark = (10/71) / (61/71)
odds_dragon = (31/72) / (41/72)
odds_electric = (3/71) / (68/71)
odds_fairy = (7/63) / (56/63)
odds_fighting = (10/73) / (63/73)
odds_fire = (10/82) / (72/82)
odds_flying = (14/117) / (103/117)
odds_ghost = (9/68) / (59/68)
odds_grass = (6/126) / (120/126)
odds_ground = (10/81) / (71/81)
odds_ice = (4/57) / (53/57)
odds_normal = (5/131) / (126/131)
odds_poison = (3/77) / (74/77)
odds_psychic = (32/119) / (87/119)
odds_rock = (5/75) / (70/75)
odds_steel = (16/71) / (55/71)
odds_water = (10/156) / (146/156)

odds_ratio_dragon =  odds_dragon / odds_psychic
odds_ratio_dragon
```

    ## [1] 2.05564

``` r
odds_ratio_psychic = odds_psychic / odds_steel
odds_ratio_psychic
```

    ## [1] 1.264368

``` r
odds_ratio_dragon2 = odds_dragon / odds_steel
odds_ratio_dragon2
```

    ## [1] 2.599085

``` r
# Compare the exponentiated coefficients of the model
exp(coef(fit1))
```

    ##   (Intercept)     valueDark   valueDragon valueElectric    valueFairy 
    ##    0.04545455    3.60655720   16.63414553    0.97058826    2.74999987 
    ## valueFighting     valueFire   valueFlying    valueGhost    valueGrass 
    ##    3.49206332    3.05555541    2.99029112    3.35593204    1.09999996 
    ##   valueGround      valueIce   valueNormal   valuePoison  valuePsychic 
    ##    3.09859140    1.66037728    0.87301607    0.89189204    8.09195363 
    ##     valueRock    valueSteel    valueWater 
    ##    1.57142849    6.39999969    1.50684924

The big takeaways from the logistic regression are that Dragon type
Pokemon are roughly 16.63 times more likely to be a member of the 600
Club than any other type, and types like Steel and Psychic are roughly
6.40 and 8.09 times likely to be members respectively. When comparing
these 3 types, Dragon type Pokemon are 2.06x more likely to have a base
stat total greater than or equal to 600 than Psychic types, and are
2.60x more likely to have a base stat total greater than or equal to 600
than Steel types. Psychic types are 1.26x more likely to have a base
stat total equal to or greater than 600 than Steel types. Other types
like Fighting, Dark, Ghost, Ground, and Fire also have more than a 3
times likelihood of being members of the 600 Club compared to Pokemon
who do not possess any of those typings., but that value of over 16x
likelihood for a Pokemon to be a Dragon type if it is a member of the
600 Club is overwhelming. Additionally, Dragon types have the highest
individual odds of being members of the 600 Club, with about 43.06% (or
31 of 72) Dragon type Pokemon containing a base stat total at or above
600. No other types comes close in terms of this kind of frequency.

``` r
# Based on predicted probabilities,
pokemon$prob1 = predict(fit1, type = "response")

# we can classify a base stat total as at/above the pseudo-legendary line or not
pokemon$predicted = ifelse(pokemon$prob1 > .25, "600 Club", "Not 600 Club")

# Confusion matrix: compare true to predicted condition
table(true_condition = pokemon$y, predicted_condition = pokemon$predicted) %>% 
  addmargins
```

    ##               predicted_condition
    ## true_condition 600 Club Not 600 Club  Sum
    ##            0        128         1285 1413
    ##            1         63          126  189
    ##            Sum      191         1411 1602

``` r
# Sensitivity (true positive rate)
print("Sensitivity:")
```

    ## [1] "Sensitivity:"

``` r
mean(pokemon[pokemon$y == 1, ]$prob1 > .25)
```

    ## [1] 0.3333333

``` r
# Specificity (true negative rate)
print("Specificity:")
```

    ## [1] "Specificity:"

``` r
mean(pokemon[pokemon$y == 0, ]$prob1 <= .25)
```

    ## [1] 0.9094126

``` r
# Predicted log odds 
pokemon$logit = predict(fit1, type = "link")
```

The confusion matrix makes some interesting points. The first is that,
with a sensitivity value of .333, the prediction model can only
correctly predict if a Pokemon is a member of the 600 Club one-third of
the time. However, with a specificity value of .909, it can correctly
predict that a Pokemon is not a member of the 600 Club roughly 91% of
the time.

``` r
# Density plot of log-odds for each outcome

# Define functions to calculate sensitivity and specificity for different cutoffs
sens = function(p, data = pokemon, y = y) mean(data[pokemon$y == 1, ]$prob1 > p)
spec = function(p, data = pokemon, y = y) mean(data[pokemon$y == 0, ]$prob1 <= p)

# Apply the functions to our data
sensitivity = sapply(seq(0, 1, .01), sens, pokemon)
specificity = sapply(seq(0, 1, .01), spec, pokemon)

# Store values of sensitivity and specificity in a data frame with cutoff values
ROC = data.frame(sensitivity, specificity, cutoff = seq(0,1, .01))

# Represent the relationship between sensitivity and specificity for different cutoffs
ROC %>%
  pivot_longer(-cutoff, names_to = "key", values_to = "rate") %>%
  ggplot(aes(cutoff, rate, color = key)) + 
  geom_path() +
  geom_vline(xintercept = c(.1, .5, .9), lty = 2, color = "gray50") +
  labs(title = "Relationship Between Sensitivity and Specificity")
```

![](Project-2_files/figure-gfm/unnamed-chunk-11-1.png)<!-- -->

``` r
# Instead plot Sensitivity (TPR) against 1 - Specificity (FPR): this is called a ROC curve!
ROC$TPR = sensitivity
ROC$FPR = 1 - specificity 

ROC %>%
  ggplot(aes(FPR, TPR)) + 
  geom_path(size = 1.5) + 
  geom_segment(aes(x = 0, y = 0, xend = 1, yend = 1), lty = 2) +
  scale_x_continuous(limits = c(0,1))
```

![](Project-2_files/figure-gfm/unnamed-chunk-11-2.png)<!-- -->

``` r
library(plotROC)
```

    ## Warning: package 'plotROC' was built under R version 4.0.5

``` r
# Plot ROC depending on values of y and its probabilities displaying some cutoff values
ROCplot = ggplot(pokemon) + 
  geom_roc(aes(d = y, m = prob1), cutoffs.at = list(0.1, 0.5, 0.9)) +
  labs(title = "ROC Plot")
ROCplot
```

![](Project-2_files/figure-gfm/unnamed-chunk-12-1.png)<!-- -->

``` r
# Calculate area under curve (AUC)
calc_auc(ROCplot)
```

    ##   PANEL group       AUC
    ## 1     1    -1 0.7212618

The area under the curve is roughly .72. According to the rule of thumb
from class, this means that the model has fair prediction power. The
explanation for why this plot only has fair prediction power is because
of the low true positive rate. Based on just looking through the data,
when running through the data set in descending order by `TOTAL`, there
seemed to be no good cutoff point for the prediction model to determine
which Pokemon would be good fits to be members of the 600 Club.

CONCLUSION

It appears that the common fan would be right to assume that Dragon type
Pokemon tend to have significantly higher base stat totals than any
other type, and this makes sense. Most of the legendary and
pseudo-legendary Pokemon are Dragon types. In East Asian culture, the
dragon is a symbol of balance, freedom, power, strength, and good
fortune, and this idea seems to perfectly encapsulate what it would mean
to be a strong Pokemon. That being said, it’s no surprise that the
Dragon type yields a vast majority of the stronger Pokemon. The Dragon
and the Psychic type were statistically significant in every test
conducted throughout the study, and it again is no surprise that
Rayquaza (a Dragon/Flying type) and Mewtwo (a Psychic type) top the list
in the `TOTAL` category. Psychic type Pokemon are associated with the
mind, and like English playwright Edward Bulwer-Lytton said, “The pen is
mightier than the sword,” meaning that the mind is stronger than brute
force. So it once again makes sense that Pokemon with strong minds not
only identify with the Psychic type, but also have high base stat
totals. The third most significant type was the Steel type, and there
isn’t an in-depth analysis as to why. The logical explanation comes from
the knowledge by the average Pokemon fan that Steel type Pokemon are
very defensive Pokemon, meaning that, while their DEF and SP\_DEF stats
are significantly higher than most, their ATK, SP\_ATK, and SPD stats
are more middle of the pack/bottom tier.

So the conclusion is that type is in fact a significant indicator of
base stat total. A Pokemon with an extremely high base stat total is
more likely to be a Dragon type, while a Pokemon with an extremely low
base stat total is more likely to be a Bug type.
