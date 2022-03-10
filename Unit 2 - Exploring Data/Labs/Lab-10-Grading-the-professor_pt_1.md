Lab 10 - Grading the professor, Pt. 1
================
John Adams
3/10/22

### Load packages and data

``` r
library(tidyverse) 
library(tidymodels)
library(openintro)
library(skimr)
library(gridExtra)
```

### Data

``` r
glimpse(evals)
```

    ## Rows: 463
    ## Columns: 23
    ## $ course_id     <int> 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 1…
    ## $ prof_id       <int> 1, 1, 1, 1, 2, 2, 2, 3, 3, 4, 4, 4, 4, 4, 4, 4, 4, 5, 5,…
    ## $ score         <dbl> 4.7, 4.1, 3.9, 4.8, 4.6, 4.3, 2.8, 4.1, 3.4, 4.5, 3.8, 4…
    ## $ rank          <fct> tenure track, tenure track, tenure track, tenure track, …
    ## $ ethnicity     <fct> minority, minority, minority, minority, not minority, no…
    ## $ gender        <fct> female, female, female, female, male, male, male, male, …
    ## $ language      <fct> english, english, english, english, english, english, en…
    ## $ age           <int> 36, 36, 36, 36, 59, 59, 59, 51, 51, 40, 40, 40, 40, 40, …
    ## $ cls_perc_eval <dbl> 55.81395, 68.80000, 60.80000, 62.60163, 85.00000, 87.500…
    ## $ cls_did_eval  <int> 24, 86, 76, 77, 17, 35, 39, 55, 111, 40, 24, 24, 17, 14,…
    ## $ cls_students  <int> 43, 125, 125, 123, 20, 40, 44, 55, 195, 46, 27, 25, 20, …
    ## $ cls_level     <fct> upper, upper, upper, upper, upper, upper, upper, upper, …
    ## $ cls_profs     <fct> single, single, single, single, multiple, multiple, mult…
    ## $ cls_credits   <fct> multi credit, multi credit, multi credit, multi credit, …
    ## $ bty_f1lower   <int> 5, 5, 5, 5, 4, 4, 4, 5, 5, 2, 2, 2, 2, 2, 2, 2, 2, 7, 7,…
    ## $ bty_f1upper   <int> 7, 7, 7, 7, 4, 4, 4, 2, 2, 5, 5, 5, 5, 5, 5, 5, 5, 9, 9,…
    ## $ bty_f2upper   <int> 6, 6, 6, 6, 2, 2, 2, 5, 5, 4, 4, 4, 4, 4, 4, 4, 4, 9, 9,…
    ## $ bty_m1lower   <int> 2, 2, 2, 2, 2, 2, 2, 2, 2, 3, 3, 3, 3, 3, 3, 3, 3, 7, 7,…
    ## $ bty_m1upper   <int> 4, 4, 4, 4, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 6, 6,…
    ## $ bty_m2upper   <int> 6, 6, 6, 6, 3, 3, 3, 3, 3, 2, 2, 2, 2, 2, 2, 2, 2, 6, 6,…
    ## $ bty_avg       <dbl> 5.000, 5.000, 5.000, 5.000, 3.000, 3.000, 3.000, 3.333, …
    ## $ pic_outfit    <fct> not formal, not formal, not formal, not formal, not form…
    ## $ pic_color     <fct> color, color, color, color, color, color, color, color, …

### Exercise 1

Visualize the distribution of score. a. Is the distribution skewed? What
does that tell you about how students rate courses? Is this what you
expected to see? Why, or why not? Include any summary statistics and
visualizations you use in your response.

The distribution of scores is skewed to the left, which tells you that
students tend to rate courses really high (median = 4.3) and occasion
rate courses lower. 75% of courses are rated higher than a 3.8. This
matches my expectation because most classes I’ve had tend to be good and
then there is always one or two that were not so good.

``` r
evals %>%
  ggplot(aes(x = score)) +
  geom_histogram(binwidth = 0.1, color = "#FFFFFF", fill = "navy")
```

![](Lab-10-Grading-the-professor_pt_1_files/figure-gfm/exercise%201-1.png)<!-- -->

``` r
evals %>%
  select(score) %>%
  skim()
```

|                                                  |            |
|:-------------------------------------------------|:-----------|
| Name                                             | Piped data |
| Number of rows                                   | 463        |
| Number of columns                                | 1          |
| \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_   |            |
| Column type frequency:                           |            |
| numeric                                          | 1          |
| \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_ |            |
| Group variables                                  | None       |

Data summary

**Variable type: numeric**

| skim_variable | n_missing | complete_rate | mean |   sd |  p0 | p25 | p50 | p75 | p100 | hist  |
|:--------------|----------:|--------------:|-----:|-----:|----:|----:|----:|----:|-----:|:------|
| score         |         0 |             1 | 4.17 | 0.54 | 2.3 | 3.8 | 4.3 | 4.6 |    5 | ▁▁▅▇▇ |

``` r
evals %>%
  select(bty_avg) %>%
  skim()
```

|                                                  |            |
|:-------------------------------------------------|:-----------|
| Name                                             | Piped data |
| Number of rows                                   | 463        |
| Number of columns                                | 1          |
| \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_   |            |
| Column type frequency:                           |            |
| numeric                                          | 1          |
| \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_ |            |
| Group variables                                  | None       |

Data summary

**Variable type: numeric**

| skim_variable | n_missing | complete_rate | mean |   sd |   p0 |  p25 |  p50 | p75 | p100 | hist  |
|:--------------|----------:|--------------:|-----:|-----:|-----:|-----:|-----:|----:|-----:|:------|
| bty_avg       |         0 |             1 | 4.42 | 1.53 | 1.67 | 3.17 | 4.33 | 5.5 | 8.17 | ▃▇▇▃▂ |

### Exercises 2 and 3

Geom_Jitter prevents overplotting by moving dots that overlap a random
amount left and right. Given that score is descrete and there are 463
observations, some of those observations overlap, which means when
looking at the visualization, you are not seeing all of the data. I
plotted a geom_point with an alpha of 0.25 to also uncover the
overlapping.

``` r
score_point <- evals %>%
                ggplot(aes(y = score, x = bty_avg)) +
                geom_point()

score_point_alpha <- evals %>%
                      ggplot(aes(y = score, x = bty_avg)) +
                      geom_point(alpha = 0.25)

score_jitter <- evals %>%
                  ggplot(aes(y = score, x = bty_avg)) +
                  geom_jitter()

grid.arrange(score_point, score_point_alpha, score_jitter, nrow = 2, ncol = 2)
```

![](Lab-10-Grading-the-professor_pt_1_files/figure-gfm/score%20vs%20bty%20avg-1.png)<!-- -->

### Exercise 4

Let’s see if the apparent trend in the plot is something more than
natural variation. Fit a linear model called score_bty_fit to predict
average professor evaluation score by average beauty rating (bty_avg).
Based on the regression output, write the linear model.

score = 3.88 + 0.06664 x bty_avg

``` r
score_bty_fit <- linear_reg() %>%
  set_engine("lm") %>%
  fit(score ~ bty_avg, data = evals) %>%
  tidy()

score_bty_fit
```

    ## # A tibble: 2 × 5
    ##   term        estimate std.error statistic   p.value
    ##   <chr>          <dbl>     <dbl>     <dbl>     <dbl>
    ## 1 (Intercept)   3.88      0.0761     51.0  1.56e-191
    ## 2 bty_avg       0.0666    0.0163      4.09 5.08e-  5

### Exercise 5

Recreate the scatterplot from Exercise 2, and add the regression line to
this plot in orange colour, with shading for the uncertainty of the line
turned off.

``` r
score_jitter <- evals %>%
                  ggplot(aes(y = score, x = bty_avg)) +
                  geom_jitter() +
                  geom_smooth(method = lm, se = FALSE)
score_jitter
```

    ## `geom_smooth()` using formula 'y ~ x'

![](Lab-10-Grading-the-professor_pt_1_files/figure-gfm/plot%20with%20line-1.png)<!-- -->

### Exercises 6-7

Based on this model, we can expect that a professors average evaluation
score will rise on average by 0.066 points for each additional point
increase in their average beauty rating.

The y-intercept would mean that a teacheer will earn an average
evaluation score of 3.88 if they have a 0 beauty rating. The lowest
beauty score in this dataset is a 1.67 and, in general, it’s hard to
imagine a professor getting a beauty rating of 0, which makes the
y-intercept meaningless in this case.

### Exercise 8

3.5% of the variation in a professors average evaluation score can be
explained by their average beauty rating.

``` r
evals %>%
  summarise(
    r = cor(score, bty_avg), 
    r_squared = (cor(score, bty_avg))^2
  )
```

    ## # A tibble: 1 × 2
    ##       r r_squared
    ##   <dbl>     <dbl>
    ## 1 0.187    0.0350
