# Purpose

Purpose of this work folder.

Ideally store a minimum working example data set in data folder.

Add binary files in bin, and closed R functions in code. Human Readable
settings files (e.g. csv) should be placed in settings/

``` r
## Set-up

# Clearing environment and loading functions:
rm(list = ls()) 
gc() 
library(tidyverse)
```

    ## ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
    ## ✔ dplyr     1.2.1     ✔ readr     2.1.6
    ## ✔ forcats   1.0.0     ✔ stringr   1.6.0
    ## ✔ ggplot2   4.0.1     ✔ tibble    3.3.1
    ## ✔ lubridate 1.9.5     ✔ tidyr     1.3.2
    ## ✔ purrr     1.2.2     
    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()
    ## ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors

``` r
list.files('code/', full.names = T, recursive = T) %>% .[grepl('.R', .)] %>% as.list() %>% walk(~source(.))

# Setting up folder structure:
CHOSEN_LOCATION <- "/Users/samtheron/Desktop/Data Sc./22921893_solution"
# Texevier::create_template(directory = glue::glue("{CHOSEN_LOCATION}/"), template_name = "22921893Question1")
# Texevier::create_template(directory = glue::glue("{CHOSEN_LOCATION}/"), template_name = "22921893Question2")
# Texevier::create_template(directory = glue::glue("{CHOSEN_LOCATION}/"), template_name = "22921893Question3")
# Texevier::create_template(directory = glue::glue("{CHOSEN_LOCATION}/"), template_name = "22921893Question4")
```

# Purpose

# Question 1

``` r
# Clearing environment:
gc() 
library(pacman)

# Loading packages:
p_load(tidyverse, lubridate, stringi, ggridges, patchwork)

# Sourcing functions:
list.files('22921893Question1/code/', full.names = T, recursive = T) %>% as.list() %>% walk(~source(.))

# Loading data:
raw_coffee <- read_csv("22921893Question1/data/Coffee/Coffee.csv")
```

    ## Rows: 2095 Columns: 12
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr (10): name, roaster, roast, loc_country, origin_1, origin_2, review_date...
    ## dbl  (2): Cost_Per_100g, Rating
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

## Loading and cleaning data

First, I want to inspect the data to check its format and how messy it
is

``` r
raw_count <- raw_coffee |> 
    count(roast)

raw_country_counts <- raw_coffee |> 
    count(loc_country) |> 
    arrange(desc(n))

raw_origin_pairs <- raw_coffee |> 
    distinct(origin_1, origin_2) |> 
    slice_head(n = 15)
```

In order to handle uppercase column names and load the coffee data the
load_data function is used:

``` r
coffee <- load_data("22921893Question1/data/Coffee/Coffee.csv")
```

    ## Rows: 2095 Columns: 12
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr (10): name, roaster, roast, loc_country, origin_1, origin_2, review_date...
    ## dbl  (2): Cost_Per_100g, Rating
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

Additional issue with the data: the dates are in (Mon-YY) format; the
country column has a typo where it reads “United States and Floyd”; and
there are missing values in the roast column. Therefore:

``` r
coffee_clean <- clean_coffee_data(coffee)
```

## Cost and rating

First, we will investigate the relationship between cost and rating by
the location of the roaster. As rating is a discrete variable, we will
use a boxplot.

``` r
box <- plot_box_cost_rating(coffee_clean)

box
```

<img src="README_files/figure-markdown_github/unnamed-chunk-6-1.png" alt="Cost and rating."  />
<p class="caption">
Cost and rating.
</p>

``` r
save_plot(box, "Cost and rating.png")
```

The plot displays an upward trend, where higher ratings cluster at a
higher cost. The mid-range of ratings overlap significantly, with long
tails in both directions. As the United States (n = 1332) and Taiwan (n
= 549) are the countries which roast the most coffee beans, they are the
predominant countries in the data. To visualise the cost distribution of
these countries the following functions are run:

``` r
# Density of cost
cost_density <- plot_country_density(coffee_clean, cost_per_100g, c("United States", "Taiwan")) 
cost_density
```

<img src="README_files/figure-markdown_github/unnamed-chunk-7-1.png" alt="The US and Taiwan."  />
<p class="caption">
The US and Taiwan.
</p>

``` r
save_plot(cost_density, "The US and Taiwan.jpg")
```

As the goal should ultimately be to find good value coffee, I define a
value metric as rating divided by cost per 100g. This tells the buyer
how much rating she is getting per dollar spent. The function also
filters coffees with implausibly low cost.

``` r
coffee_clean <- add_value(coffee_clean)
```

After removing the two implausible entries, the value metric now ranges
from 0.7 to 81.4, with a mean of 15.4 and a median of 16.0. Coffee costs
are skewed right, indicating most coffees are reasonably priced with a
few expensive coffees also present in the sample. In contrast, ratings
are tightly clustered, ranging between 84 and 98.

``` r
desc_stats <- numeric_summary(coffee_clean) |> 
    knitr::kable(caption = "Descriptive statistics")

desc_stats
```

| variable      | mean | median |   sd |  min |   max |
|:--------------|-----:|-------:|-----:|-----:|------:|
| cost_per_100g |  9.1 |    5.9 | 10.3 |  1.1 | 132.3 |
| rating        | 93.1 |   93.0 |  1.6 | 84.0 |  98.0 |
| value         | 15.4 |   16.0 |  7.6 |  0.7 |  81.4 |

Descriptive statistics

The following table depicts the Top 10 roasters by median value.
Inclusion as a candidate for best value roaster requires at least 5
coffees. Mr. Chao Coffee, El Gran Cafe, and Jackrabbit Java lead with
median values around 22-25. These roasters deliver substantially more
rating per dollar spent than the sample median of 16.0.

``` r
candidate_roasters <- best_value_roasters(coffee_clean, min_n = 5)

t10candidates <- candidate_roasters |> 
  slice_head(n = 10) |> 
  knitr::kable(digits = 2, caption = "Top 10 roasters by value (median)")

t10candidates
```

| roaster                  |   n | median_cost | median_rating | median_value |
|:-------------------------|----:|------------:|--------------:|-------------:|
| Mr. Chao Coffee          |   5 |        3.67 |            92 |        24.80 |
| El Gran Cafe             |  29 |        3.82 |            91 |        23.56 |
| Jackrabbit Java          |  23 |        4.12 |            92 |        22.33 |
| Coffee By Design         |   7 |        4.08 |            93 |        22.30 |
| Kakalove Cafe            | 142 |        4.25 |            94 |        22.00 |
| Mystic Monk Coffee       |   9 |        4.11 |            91 |        21.90 |
| Propeller Coffee         |  10 |        4.32 |            93 |        21.67 |
| San Francisco Bay Coffee |   9 |        4.17 |            90 |        21.10 |
| Espresso Republic        |  11 |        4.41 |            90 |        20.63 |
| Creation Food Co.        |   5 |        4.51 |            91 |        19.96 |

Top 10 roasters by value (median)

## Indicators of good coffee 

Per the practical instructions, the most important keywords that
Stelenbosch students have used as indicators of good coffee are: -
sweet - finish - mouthfeel - structure - aroma - chocolate - toned

Therefore, I first generate a new dataset, which includes a count of
these important keywords individually as well as a total column.

``` r
coffee_keywords <- count_keywords(coffee_clean, c("mouthfeel", "aroma", "chocolate", "sweet", "finish", "toned", "structure"))
```

This new dataset is more complete, as it includes valuable information:
the frequency of keywords which Stellenbosch students regard as
desirable. This is valuable information, as it provides an indicator of
what qualities may make a coffee in Stellenbosch a best seller.

## Which roaster is the ‘best’?

To see whether the roasters with the best value also score well on
keyword indicators, I add the average keyword count per roaster to the
top 10 value rankings. This indicates there a tradeoff when choosing a
roaster to supply coffee. The roasters with the highest rating
(e.g. Kakalove Cafe) are more expensive, and do not necessarily have a
higher frequency of desirable keywords. is Two roasters stand out in the
table as having a particularly high keyword average, Mystic Monk Coffee
and Creation Food Co.

``` r
keyword_avg <- coffee_keywords |> 
    summarise(total_keywords_mean = mean(total_keywords, na.rm = TRUE), .by = roaster)

candidate_roasters <- candidate_roasters |> 
    left_join(keyword_avg, by = "roaster")

t10complete <- candidate_roasters |> 
    slice_head(n = 10) |> 
    knitr::kable(digits = 2, caption = "Top 10 roasters by value, with average keywords")

t10complete
```

| roaster | n | median_cost | median_rating | median_value | total_keywords_mean |
|:-------------------|---:|---------:|-----------:|----------:|---------------:|
| Mr. Chao Coffee | 5 | 3.67 | 92 | 24.80 | 8.20 |
| El Gran Cafe | 29 | 3.82 | 91 | 23.56 | 8.59 |
| Jackrabbit Java | 23 | 4.12 | 92 | 22.33 | 8.65 |
| Coffee By Design | 7 | 4.08 | 93 | 22.30 | 9.29 |
| Kakalove Cafe | 142 | 4.25 | 94 | 22.00 | 8.61 |
| Mystic Monk Coffee | 9 | 4.11 | 91 | 21.90 | 11.33 |
| Propeller Coffee | 10 | 4.32 | 93 | 21.67 | 9.00 |
| San Francisco Bay Coffee | 9 | 4.17 | 90 | 21.10 | 9.11 |
| Espresso Republic | 11 | 4.41 | 90 | 20.63 | 9.36 |
| Creation Food Co. | 5 | 4.51 | 91 | 19.96 | 10.60 |

Top 10 roasters by value, with average keywords

The following plot depicts the trade-off when choosing a roaster to
supply coffee. As high-rating-roasters are often more expensive, and do
not necessarily have more desirable characteristics (as proxied by the
frequency of desirable words in the reviews). The following plot
displays the relationship between rating (mean) and cost (mean) for each
roasters. A clear positive relationship can be seen between rating and
cost. The colour corresponds to the mean frequency of desirable words
that these roasters have in their coffee (per the reviews). The three
roasters that most often have desirable words in their coffee’s
descriptions are Mystic Monk Coffee, Creation Food Co., and Corvus
Coffee Roasters. Of these three, only two appeared in the top value
rankings: Mystic Monk Coffee and Creation Food Co. Therefore, it is
contended these are the ‘best’ roasters, as they provide provide great
value while having a high frequency of desirable keywords.

``` r
roaster_counts <- coffee_clean |> 
    count(roaster) |> 
    arrange(desc(n))

roaster_stats <- summarise_numeric(coffee_keywords, roaster) |> 
    left_join(roaster_counts, by = "roaster") |> 
    filter(n >= 5) |> 
    arrange(desc(rating_mean))

roaster_scatter <- plot_roaster_scatter(roaster_stats)

roaster_scatter
```

<img src="README_files/figure-markdown_github/unnamed-chunk-13-1.png" alt="Roaster Scatterplot."  />
<p class="caption">
Roaster Scatterplot.
</p>

``` r
save_plot(roaster_scatter, "Roaster Scatterplot.png" )
```

# Question 2

``` r
# Clearing environment:
gc() 
library(pacman)

# Loading packages:
p_load(tidyverse, lubridate)

# Sourcing functions:
list.files('22921893Question2/code/', full.names = T, recursive = T) %>% as.list() %>% walk(~source(.))

# Loading data:
```

# Question 3

``` r
# Clearing environment:
gc() 
library(pacman)

# Loading packages:
p_load(tidyverse, lubridate)

# Sourcing functions:
list.files('22921893Question3/code/', full.names = T, recursive = T) %>% as.list() %>% walk(~source(.))

# Loading data:
```
