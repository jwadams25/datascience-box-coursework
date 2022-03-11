Lab 11 - Grading the professor, Pt. 2
================
John Adams
3/10/22

### Load packages and data

``` r
library(tidyverse) 
library(tidymodels)
library(openintro)
library(broom)
```

### Exercise 1

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

### Exercise 2

Fit a linear model (one you have fit before): score_bty_fit, predicting
average professor evaluation score based on average beauty rating
(bty_avg) only. Write the linear model, and note the R2 and the adjusted
R2.

score = 3.88 + 0.06664 x bty_avg

Multiple R-squared: 0.03502, Adjusted R-squared: 0.03293

``` r
score_bty_fit <- linear_reg() %>%
  set_engine("lm") %>%
  fit(score ~ bty_avg, data = evals)

score_bty_fit
```

    ## parsnip model object
    ## 
    ## Fit time:  5ms 
    ## 
    ## Call:
    ## stats::lm(formula = score ~ bty_avg, data = data)
    ## 
    ## Coefficients:
    ## (Intercept)      bty_avg  
    ##     3.88034      0.06664

``` r
summary(lm(formula = evals$score ~ evals$bty_avg))
```

    ## 
    ## Call:
    ## lm(formula = evals$score ~ evals$bty_avg)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -1.9246 -0.3690  0.1420  0.3977  0.9309 
    ## 
    ## Coefficients:
    ##               Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)    3.88034    0.07614   50.96  < 2e-16 ***
    ## evals$bty_avg  0.06664    0.01629    4.09 5.08e-05 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.5348 on 461 degrees of freedom
    ## Multiple R-squared:  0.03502,    Adjusted R-squared:  0.03293 
    ## F-statistic: 16.73 on 1 and 461 DF,  p-value: 5.083e-05

``` r
linear_reg() %>%
  set_engine("lm") %>%
  fit(score ~ bty_avg, data = evals) %>%
  tidy()
```

    ## # A tibble: 2 × 5
    ##   term        estimate std.error statistic   p.value
    ##   <chr>          <dbl>     <dbl>     <dbl>     <dbl>
    ## 1 (Intercept)   3.88      0.0761     51.0  1.56e-191
    ## 2 bty_avg       0.0666    0.0163      4.09 5.08e-  5

### Exercises 3 & 4

Fit a linear model (one you have fit before): score_bty_gen_fit,
predicting average professor evaluation score based on average beauty
rating (bty_avg) and gender. Write the linear model, and note the R2 and
the adjusted R2.

All else held constant, for each additional point increase in a
professors average beauty rating, we would expect their average
evaluation score to increase by 0.074 points.

All else held constant, males earn, on average, roughly 0.17 higher
average evaluation scores than female professors.

The doesn’t make sense in this context because it would be a female with
a beauty rating of 0.

``` r
score_bty_gen_fit <- linear_reg() %>%
  set_engine("lm") %>%
  fit(score ~ bty_avg + gender, data = evals)

glance(score_bty_gen_fit)$r.squared
```

    ## [1] 0.05912279

``` r
linear_reg() %>%
  set_engine("lm") %>%
  fit(score ~ bty_avg + gender, data = evals) %>%
  tidy()
```

    ## # A tibble: 3 × 5
    ##   term        estimate std.error statistic   p.value
    ##   <chr>          <dbl>     <dbl>     <dbl>     <dbl>
    ## 1 (Intercept)   3.75      0.0847     44.3  6.23e-168
    ## 2 bty_avg       0.0742    0.0163      4.56 6.48e-  6
    ## 3 gendermale    0.172     0.0502      3.43 6.52e-  4

### Exercise 5

5.9% of the variation in scores is explained by our model.

Visualize above relationship

``` r
evals %>%
  ggplot(aes(x = bty_avg, y = score, color = gender)) +
  geom_jitter() +
  labs(
    title = "Is there a relationship between a professors gender and beauty rating \non their average evaluation score?", 
    x = "Average Beauty Rating",
    y = "Average Evaluation Score",
    color = "Gender of Professor"
  ) +
  theme_minimal()
```

![](Lab-11-Grading-the-professor_pt_2_files/figure-gfm/score%20bty%20gender%20viz-1.png)<!-- -->

``` r
evals %>%
  ggplot(aes(x = bty_avg, y = score, color = gender)) +
  geom_jitter() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(
    title = "Is there a relationship between a professors gender and beauty rating \non their average evaluation score?", 
    x = "Average Beauty Rating",
    y = "Average Evaluation Score",
    color = "Gender of Professor"
  ) +
  theme_minimal() +
  facet_wrap(~gender, ncol = 2)
```

    ## `geom_smooth()` using formula 'y ~ x'

![](Lab-11-Grading-the-professor_pt_2_files/figure-gfm/score%20bty%20gender%20viz-2.png)<!-- -->

### Exercise 6

score = 3.7473 + 0.0741 x bty_avg + 0.172

### Exercise 7

For two professors who received the same beauty rating, which gender
tends to have the higher course evaluation score?

Male professors

### Exercise 8

### Exercise 9

Since the model with gender has a higher adjusted r-squared than the
model without gender tells us that gender is in fact useful in
explaining the variability of a professors average evaluation score.

``` r
glance(score_bty_fit)$adj.r.squared > glance(score_bty_gen_fit)$adj.r.squared
```

    ## [1] FALSE

### Exercise 10

The slope of bty_avg is slightly more in the model that includes both
gender and bty_avg than the one that only includes bty_avg (0.074 vs
0.067)

### Exercise 11

Create a new model called score_bty_rank_fit with gender removed and
rank added in. Write the equation of the linear model and interpret the
slopes and intercept in context of the data.

score = 3.98155 + 0.06783 x bty_avg - 0.16070 x rank_tenture_track -
0.12623 x rank_tenure

All else held constant, for each additional point increase in a
professors average beauty rating, we would expect their average
evaluation score to increase by 0.068 points.

All else held constant, we would exp

``` r
score_bty_rank_fit <- linear_reg() %>%
  set_engine("lm") %>%
  fit(score ~ bty_avg + rank, data = evals)

score_bty_rank_fit
```

    ## parsnip model object
    ## 
    ## Fit time:  2ms 
    ## 
    ## Call:
    ## stats::lm(formula = score ~ bty_avg + rank, data = data)
    ## 
    ## Coefficients:
    ##      (Intercept)           bty_avg  ranktenure track       ranktenured  
    ##          3.98155           0.06783          -0.16070          -0.12623

``` r
glance(score_bty_rank_fit)$r.squared > glance(score_bty_fit)$adj.r.squared
```

    ## [1] TRUE
