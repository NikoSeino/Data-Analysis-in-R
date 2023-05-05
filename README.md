DTSC 650: Data Analysis in R
================
Niko Seino
2023-05-05

<a href="https://rpubs.com/nseino/1038302">View on RPubs</a>

- <a href="#about-this-project" id="toc-about-this-project">About This
  Project</a>
- <a href="#data-preparation" id="toc-data-preparation">Data
  Preparation</a>
- <a href="#data-cleaning" id="toc-data-cleaning">Data Cleaning</a>
- <a href="#data-visualization" id="toc-data-visualization">Data
  Visualization</a>
- <a href="#data-exploration" id="toc-data-exploration">Data
  Exploration</a>
- <a href="#using-data-to-make-predictions"
  id="toc-using-data-to-make-predictions">Using Data to Make
  Predictions</a>

## About This Project

This project contains an analysis of survey data containing 37 variables
related to the health and demographics of survey participants.

My goal was to explore and visualize the data, then determine the best
demographic variables for predicting depression.

To do this, I focused on four specific variables:

*ADDEPEV2*: Shows whether or not participants have been diagnosed with a
depressive disorder. In the dataset, this variable is numeric but will
need to be treated as nominal. The possible responses are: 1=Yes, 2=No,
7=Don’t know/Not sure, 9=Refused

*POORHLTH*: Shows how many days in the past 30 that poor physical or
mental health kept participant from doing usual day-to-day activities.

The values in this variable are integer values referring to days between
1-30, and ‘88’ is used to indicate “None”, or 0 days. Other values
include 77, meaning “Don’t know”,and 99, meaning “refused”.

*EDUCA* Shows participants’ level of education.This is a numeric
variable with the numbers representing categories of education level.
There are 6 categories 1-6 starting with 1 representing no formal
education or only kindergarten, up to 6 representing 4 or more years of
college. An additional category, 9, represents participants who did not
give an answer.

*CHILDREN* Shows the number of children in each participant’s household.
This is a numeric variable representing the number of children. The
value has a range of 1-87, and 88 represents “none” or 0, which was
important to note when doing statistical calculations. ‘99’ represents
those who did not give an answer.

## Data Preparation

``` r
# Clear memory
rm(list = ls())

# Load packages
suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(ggplot2))
suppressPackageStartupMessages(library(lm.beta))

# Set working directory and load dataset
#setwd("~/DTSC650")
brfss <- read_csv("BRFSS2015_650.csv", show_col_types = FALSE)
```

## Data Cleaning

1.  Transforming variables

``` r
#Viewing the "Poor Health" data:
options(scipen = 999)
ggplot(brfss, aes(x = brfss$POORHLTH, na.rm = TRUE)) +
  geom_histogram(binwidth = 3)
```

![](650-project_files/figure-gfm/unnamed-chunk-2-1.png)<!-- -->

``` r
# The graph shows that the data is varied between 1-30 with no real outliers.
#   I can also see that the majority of responses were 88, or 0 days. 
#   To better represent this and clean up the graph, I will update the data
#   to show the zeros and also eliminate the 77s and 99s.

brfss$POORHLTH <- replace(brfss$POORHLTH, brfss$POORHLTH == 88, 0)
brfss <- brfss %>%
  filter(POORHLTH <= 30, na.rm = TRUE)

# Updated graph:
ggplot(brfss, aes(x = POORHLTH)) +
  geom_histogram(binwidth = 2)
```

![](650-project_files/figure-gfm/unnamed-chunk-2-2.png)<!-- -->
![unnamed-chunk-2-2](https://user-images.githubusercontent.com/102825218/236534182-7ea42e0d-5cc5-4249-a992-246151acfa9d.png)


``` r
# Now I can clearly see the most responses are at 0, and there are no tails on
#   either end of the graph indicating no outliers. 
```

``` r
# ADDEPEV2: Transforming into a binary variable with options "Yes" or "no"
# Eliminating the non responses ('7's and '9's)
# Converting variables to factors with levels

brfss <- brfss %>%
  filter(ADDEPEV2 == 1 | ADDEPEV2 == 2)
brfss$ADDEPEV2 <- factor(brfss$ADDEPEV2, levels = c(1, 2), labels = c("Yes", "No"))
```

``` r
# Converting variables to factors and removing non responses
brfss <- brfss %>%
  filter(EDUCA > 0, EDUCA < 7)

# Adding labels to decrease confusion
brfss$EDUCA <- factor(brfss$EDUCA,
                         levels = c("1", "2", "3", "4", "5", "6"),
                         labels = c("None", "Elementary", "Some High School",
                                    "High School Graduate", "Some College",
                                    "College Graduate"))
```

2.  Removing Outliers

``` r
# Creating a graph to view the "Children" data:
ggplot(brfss, aes(x = brfss$CHILDREN, na.rm = TRUE)) +
  geom_histogram(binwidth = 3)
```

![](650-project_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->
![unnamed-chunk-5-1](https://user-images.githubusercontent.com/102825218/236534228-35a2e5e9-cf90-44a1-abd0-d00fcf2bf6c1.png)


``` r
# The 88 and 99 values are making the data difficult to read, so I need to
#   transform the 88 to 0 and remove the 99's since they are not relevant.
# I also want to know what the max is so I can better fit the graph.
brfss$CHILDREN <- replace(brfss$CHILDREN, brfss$CHILDREN == 88, 0)
brfss <- brfss %>%
  filter(CHILDREN <= 87)
max(brfss$CHILDREN) 
```

    ## [1] 77

``` r
#max number of children in a household is 77. This seems odd and a possible outlier.

# Creating a boxplot to view quantiles
ggplot(brfss, aes(x = CHILDREN)) +
  geom_boxplot()
```

![](650-project_files/figure-gfm/unnamed-chunk-5-2.png)<!-- -->
![unnamed-chunk-5-2](https://user-images.githubusercontent.com/102825218/236534265-263a772a-4f47-4e97-b0b1-a3cf650c9164.png)


``` r
# Here we can see several possible outliers.

childrenupper <- quantile(brfss$CHILDREN, .9985, na.rm = TRUE)
# This shows that 99.85% of the values are at 6 or below. 
# Values above 6 are more than 3 standard deviations from the mean, indicating 
#   they are outliers. 

# Update the dataset with outliers removed 
children_out <- which(brfss$CHILDREN > 6)
length(children_out) 
```

    ## [1] 217

``` r
brfss <- brfss[-children_out, ]
```

## Data Visualization

``` r
# Visualizing poor health days

Q12a <- ggplot(brfss, aes(x = POORHLTH)) +
  geom_bar() + 
  labs(title = "Days in the Past 30 in which Poor Health 
       Prevented Usual Daily Activities")
Q12a
```

![](650-project_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->
![unnamed-chunk-6-1](https://user-images.githubusercontent.com/102825218/236534306-f61ba428-675f-47aa-be6b-57c77bbe3dae.png)


``` r
# Poor health days by depression dx

ggplot(brfss, aes(x = POORHLTH)) +
  geom_bar() +
  facet_wrap(~ ADDEPEV2) +
  labs(title = "Days in the Past 30 in which Poor Health
       Affected Daily Activities of People 
       With and Without Depression")
```

![](650-project_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->
![unnamed-chunk-7-1](https://user-images.githubusercontent.com/102825218/236534327-98838f7d-a8e5-4c10-9056-530032853f92.png)


Interpretation:

We can see that there are peaks at both ends, 0 and 30, meaning most
people either were unaffected by poor health in the last 30 days, or
were affected every day. This pattern is similar in those who have been
told they have depression, however in this population the proportion of
0 poor health days is much lower, and the proportion of 30 poor health
days is higher.

``` r
# Simple bar graph to view counts of participants with depression dx
ggplot(brfss, aes(x = ADDEPEV2, fill = ADDEPEV2)) +
  geom_bar() +
  labs(title = "Number of People in Survey Who Have Had Depression",
       x = "Had Depression")
```

![](650-project_files/figure-gfm/unnamed-chunk-8-1.png)<!-- -->
![unnamed-chunk-8-1](https://user-images.githubusercontent.com/102825218/236534378-f3292c24-9052-4650-b7ed-e45156cefb97.png)


``` r
# Depression in relation to education level
Q12b <- ggplot(brfss, aes(x = ADDEPEV2, fill = EDUCA)) +
  geom_bar(position = "dodge") + 
  labs(title = 'Education Level of People Who Have Had Depression')
Q12b
```

![](650-project_files/figure-gfm/unnamed-chunk-9-1.png)<!-- -->
![unnamed-chunk-9-1](https://user-images.githubusercontent.com/102825218/236534405-319a96f2-03e1-430b-b237-a2c09dba6555.png)


Interpretation:

Among Respondents who reported they have NOT been diagnosed with
depression, there is a higher proportion of people who have completed 4
or more years of college. The proportions of other education levels are
similar.

``` r
# Graphing education levels and number of children
Q12c <- ggplot(brfss) +
  geom_bar(aes(x = EDUCA, fill = EDUCA)) +
  labs(title = "Education Level of Survey Participants", x = "Education Level") +
  theme(axis.text.x = element_text(angle = 90))
Q12c
```

![](650-project_files/figure-gfm/unnamed-chunk-10-1.png)<!-- -->
![unnamed-chunk-10-1](https://user-images.githubusercontent.com/102825218/236534446-a726cd0b-e359-4332-a019-612811d5f8fc.png)


``` r
Q12d <- ggplot(brfss) +
  geom_bar(aes(x = CHILDREN)) +
  labs(title = "Number of Children in Households of Survey Participants") +
  scale_x_continuous(breaks = seq(0, 6, 1))
Q12d
```

![](650-project_files/figure-gfm/unnamed-chunk-10-2.png)<!-- -->
![unnamed-chunk-10-2](https://user-images.githubusercontent.com/102825218/236534491-8447e48a-16b2-44c1-9e7b-adfce44597b3.png)


``` r
# Creating a table to show frequencies of education level per # of children
edu_kids <- table(brfss$EDUCA, brfss$CHILDREN)
edu_kids
```

    ##                       
    ##                            0     1     2     3     4     5     6
    ##   None                   197    41    30    20     9     1     2
    ##   Elementary            4369   601   473   317   149    46    21
    ##   Some High School      9049  1482  1053   648   270    92    36
    ##   High School Graduate 46587  6579  4647  2229   818   275   115
    ##   Some College         45940  7347  5681  2489  1021   304   116
    ##   College Graduate     53195  8527  8706  3212  1051   318    98

``` r
# Plotting a comparison of education level vs number of children
ggplot(brfss, aes(x = EDUCA, fill = EDUCA)) +
  geom_bar() +
  scale_fill_brewer("blues") +
  facet_grid(~ CHILDREN) +
  labs(title = "Education Level vs Number of Children", x = "Education Level") +
  theme(axis.text.x = element_text(angle = 90))
```

![](650-project_files/figure-gfm/unnamed-chunk-12-1.png)<!-- -->
![unnamed-chunk-12-1](https://user-images.githubusercontent.com/102825218/236534527-12af3521-6b5c-4587-8b6f-97d2b29b2371.png)


Interpretation:

The majority of participants had 0 children in their households, and for
all numbers of children (0-6), counts increased as education level
increased, with “College Graduate” being the most common education
level. The one exception was participants who had 6 children–“Some
College” was the most frequent education level and “High School
Graduate” the second most frequent.

## Data Exploration

Conducting descriptive statistical calculations to gain further insights
and answer some questions.

1.  What is the average number of days poor health affected daily
    activities of people who completed only elementary school, vs those
    who graduated college?

``` r
Q13a <- brfss %>%
  filter(EDUCA == "Elementary" | EDUCA == "College Graduate") %>%
  group_by(EDUCA) %>%
  summarize(mean_poorhlth = round(mean(POORHLTH), 2))
Q13a
```

    ## # A tibble: 2 × 2
    ##   EDUCA            mean_poorhlth
    ##   <fct>                    <dbl>
    ## 1 Elementary                8.51
    ## 2 College Graduate          3.65

2.  What are the frequencies of each education level for participants
    who had depression?

``` r
Q13b <- brfss %>%
  filter(ADDEPEV2 == "Yes") %>%
  group_by(EDUCA) %>%
  summarize(count = n()) %>%
  arrange(desc(count))
Q13b
```

    ## # A tibble: 6 × 2
    ##   EDUCA                count
    ##   <fct>                <int>
    ## 1 Some College         20549
    ## 2 College Graduate     19877
    ## 3 High School Graduate 19125
    ## 4 Some High School      4721
    ## 5 Elementary            1971
    ## 6 None                    95

3.  What is the most common education level for participants who
    reported poor health affected daily activities for more than 15 out
    of the past 30 days?

``` r
Q13c <- brfss %>%
  filter(POORHLTH > 15) %>%
  group_by(EDUCA) %>%
  summarize(count = n()) %>%
  arrange(desc(count)) %>%
  head(1)
Q13c
```

    ## # A tibble: 1 × 2
    ##   EDUCA                count
    ##   <fct>                <int>
    ## 1 High School Graduate  9596

4.  What is the mean and standard deviation of number of children for
    those who did vs did not report having depression?

``` r
Q13d <- brfss %>%
  group_by(ADDEPEV2) %>%
  summarize(mean_children = mean(CHILDREN), sd_children = sd(CHILDREN)) %>%
  as.data.frame() %>%
  mutate_if(is.numeric, round, 2)
Q13d
```

    ##   ADDEPEV2 mean_children sd_children
    ## 1      Yes          0.50        0.98
    ## 2       No          0.53        1.02

## Using Data to Make Predictions

1.  Creating a dataframe with just the variables I want to focus on

``` r
df_lr <- brfss %>%
  select(POORHLTH, ADDEPEV2, EDUCA, CHILDREN)
```

2.  Looking at summary stats

``` r
summary(df_lr)
```

    ##     POORHLTH      ADDEPEV2                      EDUCA          CHILDREN     
    ##  Min.   : 0.000   Yes: 66338   None                :  300   Min.   :0.0000  
    ##  1st Qu.: 0.000   No :151823   Elementary          : 5976   1st Qu.:0.0000  
    ##  Median : 0.000                Some High School    :12630   Median :0.0000  
    ##  Mean   : 5.262                High School Graduate:61250   Mean   :0.5193  
    ##  3rd Qu.: 5.000                Some College        :62898   3rd Qu.:1.0000  
    ##  Max.   :30.000                College Graduate    :75107   Max.   :6.0000

3.  Performing logistic regressions to predict the likelihood of a
    participant having depression:

``` r
# First, predicting depression from poor health days:
mod1 <- glm(ADDEPEV2 ~ POORHLTH, binomial(link = "logit"), df_lr)
summary(mod1)
```

    ## 
    ## Call:
    ## glm(formula = ADDEPEV2 ~ POORHLTH, family = binomial(link = "logit"), 
    ##     data = df_lr)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -1.6727  -1.2361   0.7530   0.7692   1.3301  
    ## 
    ## Coefficients:
    ##               Estimate Std. Error z value            Pr(>|z|)    
    ## (Intercept)  1.1154794  0.0056256   198.3 <0.0000000000000002 ***
    ## POORHLTH    -0.0489213  0.0004765  -102.7 <0.0000000000000002 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 268024  on 218160  degrees of freedom
    ## Residual deviance: 257348  on 218159  degrees of freedom
    ## AIC: 257352
    ## 
    ## Number of Fisher Scoring iterations: 4

``` r
# Converting coefficients from log odds to odds ratio
exp(coef(mod1))
```

    ## (Intercept)    POORHLTH 
    ##    3.051030    0.952256

Interpretation:

POORHLTH odds ratio of .95 indicates that for each additional day of
poor health, the odds of not having had depression decreases by about
5%. So there is a negative relationship between poor health days and not
having had depression. The POORHLTH variable is statistically
significant in this model, however the AIC appears to be quite high.
Will test other models to see if we can get a lower AIC.

4.  Adding another predictor variable

``` r
mod2 <- glm(ADDEPEV2 ~ POORHLTH + EDUCA, binomial(), df_lr)
summary(mod2)
```

    ## 
    ## Call:
    ## glm(formula = ADDEPEV2 ~ POORHLTH + EDUCA, family = binomial(), 
    ##     data = df_lr)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -1.7206  -1.1986   0.7495   0.7884   1.3954  
    ## 
    ## Coefficients:
    ##                            Estimate Std. Error z value            Pr(>|z|)    
    ## (Intercept)                1.153513   0.128359   8.987 <0.0000000000000002 ***
    ## POORHLTH                  -0.047969   0.000482 -99.515 <0.0000000000000002 ***
    ## EDUCAElementary            0.004921   0.131439   0.037              0.9701    
    ## EDUCASome High School     -0.213689   0.129705  -1.648              0.0995 .  
    ## EDUCAHigh School Graduate -0.038998   0.128609  -0.303              0.7617    
    ## EDUCASome College         -0.144379   0.128595  -1.123              0.2615    
    ## EDUCACollege Graduate      0.068568   0.128584   0.533              0.5939    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 268024  on 218160  degrees of freedom
    ## Residual deviance: 256958  on 218154  degrees of freedom
    ## AIC: 256972
    ## 
    ## Number of Fisher Scoring iterations: 4

The EDUCA variable does not appear to be a good fit for this model as
none of the categories have statistically significant coefficients.

5.  Predicting depression from poor health days and number of children

``` r
mod3 <- glm(ADDEPEV2 ~ POORHLTH + CHILDREN, binomial(), df_lr)
summary(mod3)
```

    ## 
    ## Call:
    ## glm(formula = ADDEPEV2 ~ POORHLTH + CHILDREN, family = binomial(), 
    ##     data = df_lr)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -1.6761  -1.2212   0.7505   0.7667   1.3637  
    ## 
    ## Coefficients:
    ##               Estimate Std. Error z value             Pr(>|z|)    
    ## (Intercept)  1.1230464  0.0062528  179.61 < 0.0000000000000002 ***
    ## POORHLTH    -0.0490393  0.0004785 -102.49 < 0.0000000000000002 ***
    ## CHILDREN    -0.0133104  0.0047708   -2.79              0.00527 ** 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 268024  on 218160  degrees of freedom
    ## Residual deviance: 257340  on 218158  degrees of freedom
    ## AIC: 257346
    ## 
    ## Number of Fisher Scoring iterations: 4

``` r
exp(coef(mod3))
```

    ## (Intercept)    POORHLTH    CHILDREN 
    ##   3.0742051   0.9521437   0.9867778

The CHILDREN coefficient is statistically significant and has a negative
relationship with the outcome variable.

5.  Choosing the model with the best predictor variables

``` r
# Calculating AIC
AIC(mod3)
```

    ## [1] 257345.8

``` r
AIC(mod1)
```

    ## [1] 257351.6

Result:

The AIC of our model with 2 predictors and the first model with just one
predictor are very close, but the AIC of the model with both POORHLTH
and CHILDREN is slightly lower, indicating it as the better model.
