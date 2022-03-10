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

# Linear regression with a categorical predictor

### Exercises 9 & 10

Fit a new linear model called score_bty_gender to predict average
professor evaluation score based on gender of the professor. Based on
the regression output, write the linear model and interpret the slope
and intercept in context of the data.

score = 4.23 - 0.1415 x gender

male = 0 female = 1

score_male = 4.23 score_female = 4.23 - 0.1415 = 4.1585

Female professors are expected, on average, to receive an evaluation
score that is 0.14 points lower than male professors.

``` r
evals <- evals %>%
          mutate(gender_num = ifelse(gender == "male", 0, 1))

score_gender_fit <- linear_reg() %>%
  set_engine("lm") %>%
  fit(score ~ factor(gender_num), data = evals) %>%
  tidy()

score_gender_fit
```

    ## # A tibble: 2 × 5
    ##   term                estimate std.error statistic p.value
    ##   <chr>                  <dbl>     <dbl>     <dbl>   <dbl>
    ## 1 (Intercept)            4.23     0.0330    128.   0      
    ## 2 factor(gender_num)1   -0.142    0.0508     -2.78 0.00558

### Exercise 11 - 13

Tenure track professors are expected, on average, to earn an average
evaluation score of 4.15. Teaching professors are expected, on average,
to earn an average evaluation score that is 0.13 points higher than a
tenure track professor. Tenured professors are expected, on average, to
earn an average evaluation score that is 0.015 points lower than a
tenure track professor.

1.1 percent of the variation in average evaluation score can be
explained by the professor rank.

``` r
evals %>%
  group_by(rank) %>%
  summarise(
    n = n()
  )
```

    ## # A tibble: 3 × 2
    ##   rank             n
    ##   <fct>        <int>
    ## 1 teaching       102
    ## 2 tenure track   108
    ## 3 tenured        253

``` r
score_rank_fit <- linear_reg() %>%
  set_engine("lm") %>%
  fit(score ~ factor(rank), data = evals) %>%
  tidy()

score_rank_fit
```

    ## # A tibble: 3 × 5
    ##   term                     estimate std.error statistic   p.value
    ##   <chr>                       <dbl>     <dbl>     <dbl>     <dbl>
    ## 1 (Intercept)                 4.28     0.0537     79.9  1.02e-271
    ## 2 factor(rank)tenure track   -0.130    0.0748     -1.73 8.37e-  2
    ## 3 factor(rank)tenured        -0.145    0.0636     -2.28 2.28e-  2

``` r
evals <- evals %>%
          mutate(rank_level = relevel(evals$rank, ref = "tenure track"))

score_rank_eval_fit <- linear_reg() %>%
  set_engine("lm") %>%
  fit(score ~ rank_level, data = evals) %>%
  tidy()

score_rank_eval_fit
```

    ## # A tibble: 3 × 5
    ##   term               estimate std.error statistic   p.value
    ##   <chr>                 <dbl>     <dbl>     <dbl>     <dbl>
    ## 1 (Intercept)          4.15      0.0521    79.7   2.58e-271
    ## 2 rank_levelteaching   0.130     0.0748     1.73  8.37e-  2
    ## 3 rank_leveltenured   -0.0155    0.0623    -0.249 8.04e-  1

``` r
summary(lm(formula = evals$score ~ evals$rank_level))
```

    ## 
    ## Call:
    ## lm(formula = evals$score ~ evals$rank_level)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -1.8546 -0.3391  0.1157  0.4305  0.8609 
    ## 
    ## Coefficients:
    ##                          Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)               4.15463    0.05214  79.680   <2e-16 ***
    ## evals$rank_levelteaching  0.12968    0.07482   1.733   0.0837 .  
    ## evals$rank_leveltenured  -0.01550    0.06228  -0.249   0.8036    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.5419 on 460 degrees of freedom
    ## Multiple R-squared:  0.01163,    Adjusted R-squared:  0.007332 
    ## F-statistic: 2.706 on 2 and 460 DF,  p-value: 0.06786

### Exercises 14 & 15

Create another new variable called tenure_eligible that labels
“teaching” faculty as “no” and labels “tenure track” and “tenured”
faculty as “yes”.

Professors with the rank of teaching are expected to, on average, earn
an average evaluation score of 4.28. On the other hand, professors with
any other rank are expected, on average, to earn an average evaluation
score that is 0.14 poins lower than the professors with the rank of
teaching.

1.15 percent of the variation in evaluation score can be explained by
whether or not the professor has a rank of teaching.

``` r
evals <- evals %>%
          mutate(tenure_eligible = if_else(evals$rank == "teaching", "no", "yes")) 

score_ten_eli_fit <- linear_reg() %>%
  set_engine("lm") %>%
  fit(score ~ tenure_eligible, data = evals) %>%
  tidy()

score_ten_eli_fit
```

    ## # A tibble: 2 × 5
    ##   term               estimate std.error statistic   p.value
    ##   <chr>                 <dbl>     <dbl>     <dbl>     <dbl>
    ## 1 (Intercept)           4.28     0.0536     79.9  2.72e-272
    ## 2 tenure_eligibleyes   -0.141    0.0607     -2.32 2.10e-  2

``` r
summary(lm(formula = evals$score ~ evals$tenure_eligible))
```

    ## 
    ## Call:
    ## lm(formula = evals$score ~ evals$tenure_eligible)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -1.8438 -0.3438  0.1157  0.4360  0.8562 
    ## 
    ## Coefficients:
    ##                          Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)                4.2843     0.0536  79.934   <2e-16 ***
    ## evals$tenure_eligibleyes  -0.1406     0.0607  -2.315    0.021 *  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.5413 on 461 degrees of freedom
    ## Multiple R-squared:  0.0115, Adjusted R-squared:  0.009352 
    ## F-statistic: 5.361 on 1 and 461 DF,  p-value: 0.02103
