Lab 06 - Ugly charts and Simpson’s paradox
================
Benjamin Egan
2-17-25

### Exercise 1

``` r
staff_long <- staff %>%
  pivot_longer(cols = -faculty_type, names_to = "year") %>%
  mutate(value = as.numeric(value))
```

### Staff by Year Graph

``` r
staff_long %>%
  ggplot(aes(
    x = year,
    y = value,
    group = faculty_type,
    color = faculty_type
  )) +
  geom_line()+
  theme_bw()+
  labs(
    x = "Year",
    y = "Percent of Total Instructional Staff",
    title = "Trends in Instructional Staff Employee Status",
    color = "Faculty Status"
  )
```

![](lab-06_files/figure-gfm/graph-1.png)<!-- -->

### Fisheries Graph

``` r
fisheries <- read_csv("data/fisheries.csv")
```

    ## Rows: 216 Columns: 4
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr (1): country
    ## dbl (3): capture, aquaculture, total
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
view(fisheries)
```

Looking at this data set, we have four variables. Country, capture,
aquaculture, and total. Based on their data, it’s clear that China leads
aquaculture by a landslide. One way we could visualize this is by taking
out China and individually reporting their statistics. It would help
with the visual. Another way to help would be to create two separate
graphs, one for aquaculture and one for capture. The third way (the one
I am picking to visualize) is to show the top 10 fisheries in the total
column. From there, I would indicate the proportion of the total fish
obtained via aquaculture and capture. There is probably a better way to
visualize it, but for now I overlay a bar plot of the amount obtained by
aquaculture with a blank bar plot of the total amount of fish (to give
the illusion of the rest being by capture.)

``` r
top_10 <- fisheries %>%
  arrange(desc(total)) %>%
    slice(1:10) 


top_10 %>% ggplot(aes(
  x = fct_reorder(country, total),
  y = total
)) +
  geom_bar(stat = "identity", aes(y = aquaculture), alpha = .6, fill = "salmon")+
  geom_bar(stat = "identity", fill = NA, color = "Black")+
  coord_flip()+
  labs(
    x = "Country",
    y = "Amount in Tons",
    title = "Tonnage of Fish captured",
    subtitle = "Salmon indicates the proportion farmed via aquaculture instead of capture"
  ) 
```

![](lab-06_files/figure-gfm/visual%20for%20fisheries-1.png)<!-- -->

## Mosaic Data

### Q/A answers

``` r
#Smoking Visual
Whickham %>%
ggplot(aes(
   x=age,
   fill = fct_rev(smoker)
   )) + 
    geom_histogram(color = "black")+
  theme_bw()+
  labs(
    x = "Age",
    y = "",
    title = "Smokers and non smokers at baseline",
    fill = "Are they a smoker?"
  )+
    theme(plot.title = element_text(size = 20),
        axis.title = element_text(size = 15),
        axis.text = element_text(size = 12),
        legend.title = element_text(size = 15),
        legend.text = element_text(size = 15),
        )
```

![](lab-06_files/figure-gfm/Q/A%20answers-1.png)<!-- -->

``` r
#Death Visual
Whickham %>%
ggplot(aes(
   x=age,
   fill = fct_rev(outcome)
   )) + 
    geom_histogram(color = "black")+
  theme_bw()+
    labs(
    x = "Age",
    y = "",
    title = "Survival status after 20 years",
    fill = "Are they still alive?"
  )+
    theme(plot.title = element_text(size = 20),
        axis.title = element_text(size = 15),
        axis.text = element_text(size = 12),
        legend.title = element_text(size = 15),
        legend.text = element_text(size = 15),
        )
```

![](lab-06_files/figure-gfm/Q/A%20answers-2.png)<!-- -->

``` r
#create distinguishing variable
Whickham_alter <- Whickham %>%
  mutate(Specific = case_when(
    smoker == "Yes" & outcome == "Dead" ~ "Dead smoker",
    smoker == "Yes" & outcome == "Alive" ~ "Alive smoker",
    smoker == "No" & outcome == "Dead" ~ "Dead non-smoker",
    smoker == "No" & outcome == "Alive" ~ "Alive non-smoker"
      ))

#visual for all groups
Whickham_alter %>%
ggplot(aes(
   x=age,
   fill = Specific
   )) + 
    geom_histogram(color = "black")+
  theme_bw()+
  facet_wrap(~Specific)+
    labs(
    x = "Age",
    y = "",
    title = "Survival status after 20 years",
    fill = "Survival status and baseline smoking"
  )+
    theme(plot.title = element_text(size = 20),
        axis.title = element_text(size = 15),
        axis.text = element_text(size = 12),
        legend.title = element_text(size = 15),
        legend.text = element_text(size = 15),
        )
```

![](lab-06_files/figure-gfm/Q/A%20answers-3.png)<!-- -->

1.  This was an observational study, since they weren’t trying out an
    intervention for smoking.

2.  There were 1314 observations in this data set (all women)

3.  There are three variables; outcome, smoker, and age. Outcome is a
    categorical variable with answers (Alive or Dead). Smoker is a
    categorical variable with answers (No or Yes). Age is a continuous
    numerical variable that recorded their age in years.

4.  based on the two simple graphs I made, I would expect that smoking
    decreasing your health, leading to earlier death. The more complex
    graph shows me that there is a more spread out distriubtion of death
    in the smokers than htere is for the non-smokers.

``` r
Whickham %>%
  count(smoker, outcome) %>%
  group_by(outcome) %>%
  mutate(percent_outcome = (n / sum(n))*100)
```

    ## # A tibble: 4 × 4
    ## # Groups:   outcome [2]
    ##   smoker outcome     n percent_outcome
    ##   <fct>  <fct>   <int>           <dbl>
    ## 1 No     Alive     502            53.1
    ## 2 No     Dead      230            62.3
    ## 3 Yes    Alive     443            46.9
    ## 4 Yes    Dead      139            37.7

``` r
ggplot(Whickham, aes(y = smoker, 
                     fill = outcome)) +
  geom_bar(position = "fill") + 
  labs(title = "Smoking outcome",
       y = "Did they smoke?", x = NULL)
```

![](lab-06_files/figure-gfm/visuals%20asked%20for-1.png)<!-- -->

``` r
Whickham_age_breakdown <- Whickham_alter %>%
 mutate(age_cat = case_when(
    age <= "44" ~ "18-44",
    age > "44" & age <= "64" ~ "45-64",
    age > "64" ~ "65+",
      ))

 ggplot(Whickham_age_breakdown, aes(y = fct_rev(smoker), 
                     fill = fct_rev(outcome))) +
  geom_bar(position = "fill") + 
   facet_wrap(~age_cat)+
  labs(title = "Smoking outcome",
       y = "Did they Smoke?", x = NULL)
```

![](lab-06_files/figure-gfm/age%20breakdown-1.png)<!-- -->

Have to say, it’s hard to see the difference in this graph. I included
the table below. It looks like there are more people alive longer based
on the counts for the 65+ category.

``` r
Whickham_age_breakdown %>%
  count(age_cat, smoker, outcome) %>%
  group_by(outcome) %>%
  mutate(percent_outcome = (n / sum(n))*100)
```

    ## # A tibble: 12 × 5
    ## # Groups:   outcome [2]
    ##    age_cat smoker outcome     n percent_outcome
    ##    <chr>   <fct>  <fct>   <int>           <dbl>
    ##  1 18-44   No     Alive     327          34.6  
    ##  2 18-44   No     Dead       12           3.25 
    ##  3 18-44   Yes    Alive     270          28.6  
    ##  4 18-44   Yes    Dead       15           4.07 
    ##  5 45-64   No     Alive     147          15.6  
    ##  6 45-64   No     Dead       53          14.4  
    ##  7 45-64   Yes    Alive     167          17.7  
    ##  8 45-64   Yes    Dead       80          21.7  
    ##  9 65+     No     Alive      28           2.96 
    ## 10 65+     No     Dead      165          44.7  
    ## 11 65+     Yes    Alive       6           0.635
    ## 12 65+     Yes    Dead       44          11.9

## Additional Plot

A suggestion for how to make the visual easier to see? You can more
clearly see in this plot that the distribution of people who’s status is
different based on smoking status. Smokers have a more even distribution
while non-smokers are clearly skewed.

``` r
Whickham_age_breakdown %>%
ggplot(aes(
   x=age_cat,
   fill = Specific
   )) + 
    geom_histogram(stat = "count", color = "black")+
  theme_bw()+
  facet_wrap(~Specific)+
    labs(
    x = "Age",
    y = "",
    title = "Survival status after 20 years",
    fill = "Survival status and baseline smoking"
  )+
    theme(plot.title = element_text(size = 20),
        axis.title = element_text(size = 15),
        axis.text = element_text(size = 12),
        legend.title = element_text(size = 15),
        legend.text = element_text(size = 15),
        )
```

![](lab-06_files/figure-gfm/additional%20plot-1.png)<!-- -->
