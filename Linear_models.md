Linear models
================
2024-10-30

## Getting Started!

In general, remember linear regression (lm) is for continuous variables
and logistic regression (generalized linear model (glm)) is for
categorial variables.

So I put a couple of packages into my library so I can do my work and
the functions I want will work. The knitr used to specify information
about graphics. I also used the theme_set to tell the code to make plots
black and white and where to put the legend, respectively. Viridis is a
color scheme and I told ggplot to use that palette.

## Import Data

So we open this data set about NYC AirBnBs. You can always view the file
first using `view(nyc_airbnb)`. We changed the rating from a scale of 10
to 1-5 by dividing each number in half. We renamed a column borough to
neighborhood gorup. We removed Staten Island from borough column, and
then we only output price, stars, borough, neighborhood and room type
from the listed columns( the original doc had info about latitude and
longitude etc.– from here: <https://p8105.com/dataset_airbnb.html>)

``` r
data("nyc_airbnb")

nyc_airbnb = 
  nyc_airbnb |> 
  mutate(stars = review_scores_location / 2) |> 
  rename(
    borough = neighbourhood_group,
    neighborhood = neighbourhood) |> 
  filter(borough != "Staten Island") |> 
  select(price, stars, borough, neighborhood, room_type)

nyc_airbnb
```

    ## # A tibble: 40,492 × 5
    ##    price stars borough neighborhood room_type      
    ##    <dbl> <dbl> <chr>   <chr>        <chr>          
    ##  1    99   5   Bronx   City Island  Private room   
    ##  2   200  NA   Bronx   City Island  Private room   
    ##  3   300  NA   Bronx   City Island  Entire home/apt
    ##  4   125   5   Bronx   City Island  Entire home/apt
    ##  5    69   5   Bronx   City Island  Private room   
    ##  6   125   5   Bronx   City Island  Entire home/apt
    ##  7    85   5   Bronx   City Island  Entire home/apt
    ##  8    39   4.5 Bronx   Allerton     Private room   
    ##  9    95   5   Bronx   Allerton     Entire home/apt
    ## 10   125   4.5 Bronx   Allerton     Entire home/apt
    ## # ℹ 40,482 more rows

## Fit a model

Good! so now that we have our data set and the variables that matter, we
can create a model for the variables of interest. Here I will create a
model that considers how price relates to the rating and the borough
where the airbnb is located.

``` r
nyc_airbnb %>% 
  ggplot(aes( x= stars, y= price, color = borough)) +
  geom_point()
```

    ## Warning: Removed 9962 rows containing missing values or values outside the scale range
    ## (`geom_point()`).

<img src="Linear_models_files/figure-gfm/unnamed-chunk-2-1.png" width="90%" />

Ok so no we know what the data looks like. We tell R to get the linear
model equation, since the ratings, price etc are all continuous
variables. We can set that function equal to fit.Note! The borough
options are Bronx, Brooklyn, Manhattan, Queens, and I think it might be
because Bronx is entered first, OR it could be alphabetical order, but
the regression is comparing all other boroughs to Bronx.

``` r
fit = lm(price ~ stars + borough, data = nyc_airbnb)
fit
```

    ## 
    ## Call:
    ## lm(formula = price ~ stars + borough, data = nyc_airbnb)
    ## 
    ## Coefficients:
    ##      (Intercept)             stars   boroughBrooklyn  boroughManhattan  
    ##           -70.41             31.99             40.50             90.25  
    ##    boroughQueens  
    ##            13.21

Then we can also run the summary function to get more info like degrees
of freedom. The F statistic also tells us about the overall goodness of
fit.

``` r
summary(fit)
```

    ## 
    ## Call:
    ## lm(formula = price ~ stars + borough, data = nyc_airbnb)
    ## 
    ## Residuals:
    ##    Min     1Q Median     3Q    Max 
    ## -169.8  -64.0  -29.0   20.2 9870.0 
    ## 
    ## Coefficients:
    ##                  Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)       -70.414     14.021  -5.022 5.14e-07 ***
    ## stars              31.990      2.527  12.657  < 2e-16 ***
    ## boroughBrooklyn    40.500      8.559   4.732 2.23e-06 ***
    ## boroughManhattan   90.254      8.567  10.534  < 2e-16 ***
    ## boroughQueens      13.206      9.065   1.457    0.145    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 181.5 on 30525 degrees of freedom
    ##   (9962 observations deleted due to missingness)
    ## Multiple R-squared:  0.03423,    Adjusted R-squared:  0.03411 
    ## F-statistic: 270.5 on 4 and 30525 DF,  p-value: < 2.2e-16

I can also pull out only the coefficient from the summary function

``` r
summary(fit)$coef
```

    ##                   Estimate Std. Error   t value     Pr(>|t|)
    ## (Intercept)      -70.41446  14.020697 -5.022180 5.137589e-07
    ## stars             31.98989   2.527500 12.656733 1.269392e-36
    ## boroughBrooklyn   40.50030   8.558724  4.732049 2.232595e-06
    ## boroughManhattan  90.25393   8.567490 10.534465 6.638618e-26
    ## boroughQueens     13.20617   9.064879  1.456850 1.451682e-01

You can also use the broom package to get a more condensed description.
I can see the p value for each variable. We know that the regression is
using Bronx as a standard and comparing the other values to this. Thus,
what the coefficient estimate means is that staying in brooklyn is 40
dollars more expensive relative to the bronx, manhattan is 90 etc. ALSO
notice this tells you that queens is not significantly influencing the
price– its p value is 0.145!

``` r
broom::tidy(fit)
```

    ## # A tibble: 5 × 5
    ##   term             estimate std.error statistic  p.value
    ##   <chr>               <dbl>     <dbl>     <dbl>    <dbl>
    ## 1 (Intercept)         -70.4     14.0      -5.02 5.14e- 7
    ## 2 stars                32.0      2.53     12.7  1.27e-36
    ## 3 boroughBrooklyn      40.5      8.56      4.73 2.23e- 6
    ## 4 boroughManhattan     90.3      8.57     10.5  6.64e-26
    ## 5 boroughQueens        13.2      9.06      1.46 1.45e- 1

Now usually, people are only interested in the estimate and p value—
there is a way to put that out directly. I’m telling it to only select
the 2 columns I care about; then I’m telling it that when it looks in
the term column above and sees something that starts (^) with borough,
capitalize it, add a colon and add some space before the next character
appears.

``` r
broom::tidy(fit) %>% 
  select(term, estimate, p.value) %>% 
  mutate(term = str_replace(term, "^borough", "Borough: "))
```

    ## # A tibble: 5 × 3
    ##   term               estimate  p.value
    ##   <chr>                 <dbl>    <dbl>
    ## 1 (Intercept)           -70.4 5.14e- 7
    ## 2 stars                  32.0 1.27e-36
    ## 3 Borough: Brooklyn      40.5 2.23e- 6
    ## 4 Borough: Manhattan     90.3 6.64e-26
    ## 5 Borough: Queens        13.2 1.45e- 1

Then if we wanted to see this in an actual table, we can use the
`kable()` function that’s used in r markdown files. Remember the knitr
function is about displaying data. I can display the entire estimate and
P values, or I can say hey I’m only interested in the first 3 decimals
for both the estimate and p.value.

``` r
broom::tidy(fit) %>% 
  select(term, estimate, p.value) %>% 
  mutate(term = str_replace(term, "^borough", "Borough: ")) %>% 
knitr::kable(digits =3)
```

| term               | estimate | p.value |
|:-------------------|---------:|--------:|
| (Intercept)        |  -70.414 |   0.000 |
| stars              |   31.990 |   0.000 |
| Borough: Brooklyn  |   40.500 |   0.000 |
| Borough: Manhattan |   90.254 |   0.000 |
| Borough: Queens    |   13.206 |   0.145 |

Remember the `factor` function? Factor takes a string and attaches a
numeric value to it based on its position.

## How to change the reference category for your covariates!

Recall, I mentioned that the Bronx was automatically set as the
reference category. And that’s because the linear model by default,
treats the variables not as characters but factors, meaning it takes a
character and attaches a numeric value to it based on its position. So
it uses whatever is first as the reference.

Well! I can decide which variable should be the reference.

## First, Let’s say I want the reference column to be the most frequent borough

Here I take the nyc_airbnb modified data set from above and tell it to
organize the borough column by which borough is more frequent, and also
to reorder the room type by which is more frequent. So that the most
frequent borough appears first. And that’s what we see in the plot
below.

``` r
nyc_airbnb = 
nyc_airbnb %>% 
  mutate(
    borough = fct_infreq(borough),
    room_type = fct_infreq(room_type))
```

Now let’s see what it did to the data using the plot

``` r
nyc_airbnb %>% 
  ggplot(aes( x= stars, y= price, color = borough)) +
  geom_point()
```

    ## Warning: Removed 9962 rows containing missing values or values outside the scale range
    ## (`geom_point()`).

<img src="Linear_models_files/figure-gfm/unnamed-chunk-10-1.png" width="90%" />
Now I rereun the regression.

I’ve already defined how borough should be analyzed. So note code
earlier in this segment must be run prior to this for it to work. See
now I have Bronx listed as a variable, and as expected, Bronx will be 90
dollars cheaper relative to Manhattan (the opposite of what we saw
above).

``` r
fit = lm(price ~ stars + borough, data = nyc_airbnb)

broom::tidy(fit)
```

    ## # A tibble: 5 × 5
    ##   term            estimate std.error statistic   p.value
    ##   <chr>              <dbl>     <dbl>     <dbl>     <dbl>
    ## 1 (Intercept)         19.8     12.2       1.63 1.04e-  1
    ## 2 stars               32.0      2.53     12.7  1.27e- 36
    ## 3 boroughBrooklyn    -49.8      2.23    -22.3  6.32e-109
    ## 4 boroughQueens      -77.0      3.73    -20.7  2.58e- 94
    ## 5 boroughBronx       -90.3      8.57    -10.5  6.64e- 26

## Diagnostics: starting with looking at residuals.

I use the `add_residuals` function in the modelr package to add a column
of the residuals to my condensed data frame, fit. The residual tells you
the difference between your value of interest (let’s say price) and
where it is relative to what your model would predict. So it shows that
the 99\$ private room in the Bronx is about 9 dollars more expensive
than my model predicts.

It’s good to look at residuals because that gives us an idea as to how
well the model fits the data and if there are outliers. The taller the
line/size of the squiggly space (maybe?), the more outliers there are. I
reset the limits of the y axis for better visualization.

``` r
nyc_airbnb %>% 
  modelr::add_residuals(fit) %>% 
  ggplot(aes(x = borough, y = resid)) + geom_violin() +
  ylim(-500,1500)
```

    ## Warning: Removed 9993 rows containing non-finite outside the scale range
    ## (`stat_ydensity()`).

<img src="Linear_models_files/figure-gfm/unnamed-chunk-12-1.png" width="90%" />

I can also plot the residuals for each borough and have a better idea of
where the model might be failing some values— you want to see where
majority of values are– that means they are well represented by the
model. For values that are somewhat outliers, like there are a lot in
manhattan, see the extra data points further away.I’m telling it to plot
each borough separately (using the facet function– I think it means all
rows) and plot the residuals for each star rating

``` r
nyc_airbnb |> 
  modelr::add_residuals(fit) |> 
  ggplot(aes(x = stars, y = resid)) + geom_point() +
  facet_wrap(. ~borough)
```

    ## Warning: Removed 9962 rows containing missing values or values outside the scale range
    ## (`geom_point()`).

<img src="Linear_models_files/figure-gfm/unnamed-chunk-13-1.png" width="90%" />

\##Comparing models

So it’s important to be sure that your complex model is complex because
it represents the data well and is better than a simpler model (with
fewer indepdent variables). The standard for comparing models is using
an anova test. The original linear model
(`fit = lm(price ~ stars + borough, data = nyc_airbnb)` ) looked at
stars and borough as the independent variables. But let’s say my null
hypothesis is borough doesn’t have an influence. The P value is get from
the anova will tell you if there’s a significant difference in the more
complex plot you have, and if so, that means accounting for those extra
variables DOES make a difference in how well the model estimates the
effects. It’s a little hard to understand/read at first, so I can add
the `broom:: tidy()` function to improve readability.

``` r
fit_null = lm(price ~ stars, data = nyc_airbnb)
fit_alt = lm(price ~ stars + borough, data = nyc_airbnb)
anova(fit_null, fit_alt) %>% 
  broom:: tidy()
```

    ## # A tibble: 2 × 7
    ##   term                    df.residual     rss    df   sumsq statistic    p.value
    ##   <chr>                         <dbl>   <dbl> <dbl>   <dbl>     <dbl>      <dbl>
    ## 1 price ~ stars                 30528  1.03e9    NA NA            NA  NA        
    ## 2 price ~ stars + borough       30525  1.01e9     3  2.53e7      256.  7.84e-164

## How to create models that further stratify an indepedent variable of interest using nested data

So this is important because let’s say I want to know not only whether
star rating influences price, but what if it’s star rating within in
each borough. I can tell the code to consider the interaction of stars
and borough (and in stats means multiply). Importantly you can see p
values for each interaction.

``` r
fit = lm(price ~ stars* borough + room_type * borough, data = nyc_airbnb) 

broom:: tidy(fit)
```

    ## # A tibble: 16 × 5
    ##    term                                  estimate std.error statistic  p.value
    ##    <chr>                                    <dbl>     <dbl>     <dbl>    <dbl>
    ##  1 (Intercept)                              95.7      19.2     4.99   6.13e- 7
    ##  2 stars                                    27.1       3.96    6.84   8.20e-12
    ##  3 boroughBrooklyn                         -26.1      25.1    -1.04   2.99e- 1
    ##  4 boroughQueens                            -4.12     40.7    -0.101  9.19e- 1
    ##  5 boroughBronx                             -5.63     77.8    -0.0723 9.42e- 1
    ##  6 room_typePrivate room                  -124.        3.00  -41.5    0       
    ##  7 room_typeShared room                   -154.        8.69  -17.7    1.42e-69
    ##  8 stars:boroughBrooklyn                    -6.14      5.24   -1.17   2.41e- 1
    ##  9 stars:boroughQueens                     -17.5       8.54   -2.04   4.09e- 2
    ## 10 stars:boroughBronx                      -22.7      17.1    -1.33   1.85e- 1
    ## 11 boroughBrooklyn:room_typePrivate room    32.0       4.33    7.39   1.55e-13
    ## 12 boroughQueens:room_typePrivate room      54.9       7.46    7.37   1.81e-13
    ## 13 boroughBronx:room_typePrivate room       71.3      18.0     3.96   7.54e- 5
    ## 14 boroughBrooklyn:room_typeShared room     47.8      13.9     3.44   5.83e- 4
    ## 15 boroughQueens:room_typeShared room       58.7      17.9     3.28   1.05e- 3
    ## 16 boroughBronx:room_typeShared room        83.1      42.5     1.96   5.03e- 2

## Nesting data

So I’m telling it to take this data and form a list of data frames.
Truly not quite sure what this code is doing in terms of the -borough bc
that means exclude borough.

``` r
nyc_airbnb %>% 
  nest(data = -borough)
```

    ## # A tibble: 4 × 2
    ##   borough   data                 
    ##   <fct>     <list>               
    ## 1 Bronx     <tibble [649 × 4]>   
    ## 2 Queens    <tibble [3,821 × 4]> 
    ## 3 Brooklyn  <tibble [16,810 × 4]>
    ## 4 Manhattan <tibble [19,212 × 4]>

Now I’m going to create a model for each borough. Again, not quite sure
what syntax needs are for the map function. Data refers to the data
column.

``` r
nyc_airbnb %>% 
  nest(data = -borough) %>% 
  mutate(
    models = map(.x = data, ~lm(price ~ stars, data = .x))
  ) 
```

    ## # A tibble: 4 × 3
    ##   borough   data                  models
    ##   <fct>     <list>                <list>
    ## 1 Bronx     <tibble [649 × 4]>    <lm>  
    ## 2 Queens    <tibble [3,821 × 4]>  <lm>  
    ## 3 Brooklyn  <tibble [16,810 × 4]> <lm>  
    ## 4 Manhattan <tibble [19,212 × 4]> <lm>

I can pull out the models column to see the coefficients. The map
function takes an input (.x = the information of interest, which is
data, then applies a function to elements in the list, which in this
case is the linear model, and returns an object of the same length as
the input, which is what I think data =.x does).

``` r
nyc_airbnb %>% 
  nest(data = -borough) %>% 
  mutate(
    models = map(.x = data, ~lm(price ~ stars, data = .x))
  ) %>% 
  pull(models)
```

    ## [[1]]
    ## 
    ## Call:
    ## lm(formula = price ~ stars, data = .x)
    ## 
    ## Coefficients:
    ## (Intercept)        stars  
    ##      49.925        4.913  
    ## 
    ## 
    ## [[2]]
    ## 
    ## Call:
    ## lm(formula = price ~ stars, data = .x)
    ## 
    ## Coefficients:
    ## (Intercept)        stars  
    ##        18.1         15.8  
    ## 
    ## 
    ## [[3]]
    ## 
    ## Call:
    ## lm(formula = price ~ stars, data = .x)
    ## 
    ## Coefficients:
    ## (Intercept)        stars  
    ##      -11.35        27.99  
    ## 
    ## 
    ## [[4]]
    ## 
    ## Call:
    ## lm(formula = price ~ stars, data = .x)
    ## 
    ## Coefficients:
    ## (Intercept)        stars  
    ##       -34.3         43.3

I can apply the tiny data and add a results column of the broom tidy
function that provides the stats we care about.

``` r
stats_of_interest = nyc_airbnb %>% 
  nest(data = -borough) %>% 
  mutate(
    models = map(.x = data, ~lm(price ~ stars, data = .x)),
    results = map(models, broom::tidy)
  )
stats_of_interest
```

    ## # A tibble: 4 × 4
    ##   borough   data                  models results         
    ##   <fct>     <list>                <list> <list>          
    ## 1 Bronx     <tibble [649 × 4]>    <lm>   <tibble [2 × 5]>
    ## 2 Queens    <tibble [3,821 × 4]>  <lm>   <tibble [2 × 5]>
    ## 3 Brooklyn  <tibble [16,810 × 4]> <lm>   <tibble [2 × 5]>
    ## 4 Manhattan <tibble [19,212 × 4]> <lm>   <tibble [2 × 5]>

Now I can unnest it to show me columns and rows. I removed the stars
columns

``` r
stats_of_interest = nyc_airbnb %>% 
  nest(data = -borough) %>% 
  mutate(
    models = map(.x = data, ~lm(price ~ stars, data = .x)),
    results = map(models, broom::tidy)
  ) %>% 
   select(-data, -models) %>% 
  unnest(results)
stats_of_interest
```

    ## # A tibble: 8 × 6
    ##   borough   term        estimate std.error statistic  p.value
    ##   <fct>     <chr>          <dbl>     <dbl>     <dbl>    <dbl>
    ## 1 Bronx     (Intercept)    49.9      18.3      2.72  6.71e- 3
    ## 2 Bronx     stars           4.91      4.10     1.20  2.31e- 1
    ## 3 Queens    (Intercept)    18.1      26.3      0.688 4.92e- 1
    ## 4 Queens    stars          15.8       5.63     2.81  5.06e- 3
    ## 5 Brooklyn  (Intercept)   -11.3      14.5     -0.784 4.33e- 1
    ## 6 Brooklyn  stars          28.0       3.10     9.02  2.13e-19
    ## 7 Manhattan (Intercept)   -34.3      22.9     -1.50  1.35e- 1
    ## 8 Manhattan stars          43.3       4.78     9.07  1.39e-19

I can remove the stars column.

``` r
stats_of_interest = nyc_airbnb %>% 
  nest(data = -borough) %>% 
  mutate(
    models = map(.x = data, ~lm(price ~ stars, data = .x)),
    results = map(models, broom::tidy)
  ) %>% 
   select(-data, -models) %>% 
  unnest(results) %>% 
 filter(term == "stars")
stats_of_interest
```

    ## # A tibble: 4 × 6
    ##   borough   term  estimate std.error statistic  p.value
    ##   <fct>     <chr>    <dbl>     <dbl>     <dbl>    <dbl>
    ## 1 Bronx     stars     4.91      4.10      1.20 2.31e- 1
    ## 2 Queens    stars    15.8       5.63      2.81 5.06e- 3
    ## 3 Brooklyn  stars    28.0       3.10      9.02 2.13e-19
    ## 4 Manhattan stars    43.3       4.78      9.07 1.39e-19
