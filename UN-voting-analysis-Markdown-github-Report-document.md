THE UNITED NATIONS VOTE ANALYSIS
================
Karanja David
December 14, 2021

# Analysis Objectives

The analysis answers questions related to international relations. The
United Nations has grown from the original 51 in 1945 to the current 193
members. The UN advocates for peace, dignity, equality and a healthy
planet. [About UN](https://www.un.org/en/about-us).

## Questions the analysis answers.

1.  What is the trend of yes, abstain and no votes over the years?
2.  Which regions agree with UN resolutions and which ones don’t?
3.  Which five countries have the highest percentage of agree ability,
    disagree ability and abstinence from taking the vote?
4.  How do Kenya’s key allies vote compared to Kenya? The United States
    of America, The United Kingdom and China.
5.  What is Kenya’s opinion on the six UN topics? The Palestinian
    conflict, nuclear weapons and material, arms control and
    disarmament, colonialism, economic development and human rights.
6.  How is Afghanistan, Israel and United State view on the Palestinian
    conflict.
7.  Which nations have the sharpest yes or no voting trends in the vote
    in recent years?

# Data sources used

1.  votes
2.  descriptions

## Votes data set

Votes data set contains united nations voting data. There are 4 original
columns.

1.  rcid - roll call id.
2.  session - session number the voting was done.
3.  vote- a code or country’s choice. Under code, we have 1 for yes, 2
    for abstain, 3 for no, 8 for not present and 9 for not a member.
4.  ccode - unique code for a particular country. first session was held
    in 1946.

## Descriptions data set

descriptions data frame has 10 columns. Me, nu,di, hr, co, ec are about
topics that UN member countries cast votes on.

1.  rcid - roll call id. Also in the votes data set.
2.  session -session number the voting was done. Also in the votes data
    set.
3.  date of voting.
4.  unres- UN resolution.
5.  me - Palestinian conflict.
6.  nu - nuclear weapons and nuclear material.
7.  di- arms and disarmament
8.  hr- human rights.
9.  co - colonialism.
10. ec- economic development.

#### Set up the enviroment

``` r
# loading libraries
library(broom)
library(countrycode)
library(knitr)
library(scales)
library(tidyverse)
```

#### Load data sets

``` r
#import data sets
votes <- readRDS("votes.rds")
descriptions <- readRDS("descriptions.rds")
```

#### Check the columns, data types and number of observations.

``` r
#glimpse the data
glimpse(votes)
```

    ## Rows: 508,929
    ## Columns: 4
    ## $ rcid    <dbl> 46, 46, 46, 46, 46, 46, 46, 46, 46, 46, 46, 46, 46, 46, 46, 46~
    ## $ session <dbl> 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2,~
    ## $ vote    <dbl> 1, 1, 9, 1, 1, 1, 9, 9, 9, 9, 9, 9, 9, 9, 9, 1, 9, 1, 1, 1, 1,~
    ## $ ccode   <int> 2, 20, 31, 40, 41, 42, 51, 52, 53, 54, 55, 56, 57, 58, 60, 70,~

``` r
glimpse(descriptions)
```

    ## Rows: 2,589
    ## Columns: 10
    ## $ rcid    <dbl> 46, 47, 48, 49, 50, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60, 61~
    ## $ session <dbl> 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2,~
    ## $ date    <dttm> 1947-09-04, 1947-10-05, 1947-10-06, 1947-10-06, 1947-10-06, 1~
    ## $ unres   <chr> "R/2/299", "R/2/355", "R/2/461", "R/2/463", "R/2/465", "R/2/56~
    ## $ me      <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,~
    ## $ nu      <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,~
    ## $ di      <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,~
    ## $ hr      <dbl> 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,~
    ## $ co      <dbl> 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0,~
    ## $ ec      <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,~

#### A standard format for consitency of the plots

``` r
#plot format
plot_format <- theme(plot.title = element_text(face = "bold", size = 18),
                      plot.subtitle = element_text(margin = margin(b = 20)),
                      axis.title.y = element_text(margin = margin(r = 10)),
                      axis.title.x = element_text(margin = margin(t = 10)),
                      plot.margin = margin(10,10,10,10),
                      legend.title = element_text(face = "bold"))
```

# Data processing

``` r
# filter countries that participated in the vote. 
# mutate to add year, country and region columns. Filter out code 260 which has no corresponding country name or region. The warning is due to the unavailability  of ccode 260 from country code package.

votes_processed <- votes %>% 
  filter(vote <= 3) %>% 
  mutate(year = session + 1945) %>% 
  mutate(country = countrycode(ccode, "cown", "country.name")) %>% 
  mutate(region = countrycode(sourcevar = country, 
                              origin = "country.name",
                              destination = "region")) %>% 
  filter(!is.na(country))

#view the data frame 
head(votes_processed)
```

    ## # A tibble: 6 x 7
    ##    rcid session  vote ccode  year country            region                   
    ##   <dbl>   <dbl> <dbl> <int> <dbl> <chr>              <chr>                    
    ## 1    46       2     1     2  1947 United States      North America            
    ## 2    46       2     1    20  1947 Canada             North America            
    ## 3    46       2     1    40  1947 Cuba               Latin America & Caribbean
    ## 4    46       2     1    41  1947 Haiti              Latin America & Caribbean
    ## 5    46       2     1    42  1947 Dominican Republic Latin America & Caribbean
    ## 6    46       2     1    70  1947 Mexico             Latin America & Caribbean

## Data Analysis and Visualization

### Question one

#### 1. What is the trend of yes, abstain and no votes from the vote over the years?

##### 1a. Yes

``` r
yes_by_year <- votes_processed %>% 
  group_by(year) %>% 
  summarize(total = n(), percent_yes = mean(vote == 1))

head(yes_by_year, n = 3)
```

    ## # A tibble: 3 x 3
    ##    year total percent_yes
    ##   <dbl> <int>       <dbl>
    ## 1  1947  2039       0.569
    ## 2  1949  3469       0.438
    ## 3  1951  1434       0.585

``` r
#yes votes in percentage visualization 
ggplot(data = yes_by_year, aes(x = year, y = percent_yes))+
  geom_line()+
  labs(title = "percent of YES votes United Nations", 
       caption = "Trend from 1946")+
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10))+
  scale_y_continuous(labels = percent)+
  xlab("Year")+
  ylab("Percent Yes")+
  plot_format
```

![](UN-voting-analysis-Markdown-github-Report-document_files/figure-gfm/percent%20yes%20votes%20over%20the%20years-1.png)<!-- -->

Result: Roughly over 80% Yes votes. There has been an upward trend of
agree ability with UN resolutions from late 1940s to late 1970s. Peak
yes percentage in 1989. In recent years, the graph has been fairly
constant from 1993 at 82%.

#### 1b. Abstain

``` r
abstain_by_year <- votes_processed %>% 
  group_by(year) %>% 
  summarize(total = n(), percent_abstain = mean(vote == 2))

head(abstain_by_year, n = 3)
```

    ## # A tibble: 3 x 3
    ##    year total percent_abstain
    ##   <dbl> <int>           <dbl>
    ## 1  1947  2039           0.137
    ## 2  1949  3469           0.237
    ## 3  1951  1434           0.169

``` r
#Abstain from the vote percentage over the years.
ggplot(data = abstain_by_year, aes(x = year, y = percent_abstain))+
  geom_line()+
  labs(title = "percent of ABSTAIN from vote United Nations", 
       caption = "Trend from 1946")+
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10))+
  scale_y_continuous(labels = percent)+
  xlab("Year")+
  ylab("Percent Abstain")+
  plot_format
```

![](UN-voting-analysis-Markdown-github-Report-document_files/figure-gfm/percent%20Abstain%20votes%20over%20the%20years-1.png)<!-- -->

Result: Highest percent to abstain recorded is 24% in the late 1940s.
Fewer countries have abstained over the years. In 2010, only 10%
abstained from the vote.

#### 1c. No

``` r
no_by_year <- votes_processed %>% 
  group_by(year) %>% 
  summarize(total = n(), percent_no = mean(vote == 3))

head(no_by_year, n = 3)
```

    ## # A tibble: 3 x 3
    ##    year total percent_no
    ##   <dbl> <int>      <dbl>
    ## 1  1947  2039      0.294
    ## 2  1949  3469      0.326
    ## 3  1951  1434      0.246

``` r
#No votes in percentage visualization 
ggplot(data = no_by_year, aes(x = year, y = percent_no))+
  geom_line()+
  labs(title = "percent of NO votes United Nations", 
       caption = "Trend from 1946")+
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10))+
  scale_y_continuous(labels = percent)+
  xlab("Year")+
  ylab("Percent No")+
  plot_format
```

![](UN-voting-analysis-Markdown-github-Report-document_files/figure-gfm/percent%20No%20votes%20over%20the%20years-1.png)<!-- -->

Result: No votes are all less than 30%. The No votes have been on a
decline since 1950s. An significantly low rise is recorded from 1990
onwards.

#### 1d. Combined Yes, Abstain and No. 

``` r
#gather the votes into a single column 
 votes_combined <- votes_processed %>% 
   group_by(year) %>% 
   summarize(percent_yes = mean(vote == 1), 
             percent_abstain = mean(vote == 2),
             percent_no = mean(vote == 3))%>%
   pivot_longer(
     cols = c(starts_with("percent")),
     names_to = "percent_name",
     values_to = "percent_value"
     )

head(votes_combined)
```

    ## # A tibble: 6 x 3
    ##    year percent_name    percent_value
    ##   <dbl> <chr>                   <dbl>
    ## 1  1947 percent_yes             0.569
    ## 2  1947 percent_abstain         0.137
    ## 3  1947 percent_no              0.294
    ## 4  1949 percent_yes             0.438
    ## 5  1949 percent_abstain         0.237
    ## 6  1949 percent_no              0.326

``` r
#visualization 
   votes_combined_plot <- votes_combined %>% 
     ggplot(aes(x = year, y = percent_value, color = percent_name))+
     geom_line()+
     labs(title = "Total votes percentage United Nations", 
        caption = "Trend from 1946")+
     xlab("Year")+
     ylab("Percent")+
     scale_x_continuous(breaks = scales::pretty_breaks(n = 10))+
     scale_y_continuous(labels = percent)+
     plot_format
   
   votes_combined_plot
```

![](UN-voting-analysis-Markdown-github-Report-document_files/figure-gfm/combined%20votes%20plots-1.png)<!-- -->

Resut: In the overall summary as of 2010, Yes votes account for over 75%
of votes cast. A distant second is abstinence from the vote at 12%. No
votes are at 10%.

### Question 2

#### 2. Which regions agree with UN resolutions and which ones don’t?

``` r
yes_regions <- votes_processed %>%
  group_by(region) %>%
  summarize(percent_yes = mean(vote == 1)) %>% 
  ggplot(aes(x = reorder(region, -percent_yes), y = percent_yes, fill = region))+
  geom_col()+
  labs(title = "Regional Yes percent United Nations votes", 
       caption = "Trend from 1946")+
  xlab("Region")+
  ylab("Percentage")+
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10))+
  scale_y_continuous(labels = percent)+
  plot_format
  
yes_regions
```

<img src="UN-voting-analysis-Markdown-github-Report-document_files/figure-gfm/region yes percentages-1.png" width="95%" />

Result: Sub Saharan Africa has the highest percentage of yes votes.
Closely followed by South Asia, Middle East & North Africa, Latin
America & Caribbean, East Asia & Pacific, Europe & Central Asia. The
only region with less than 50% yes votes is North America.

### Question 3

#### 3. Which five countries have the highest percentage of agree ability, disagree ability and abstinence from taking the vote?

#### 3a. Highest Yes Percent countries.

``` r
yes_by_country <- votes_processed %>%
  group_by(country) %>%
  summarize(total = n(), percent_yes = mean(vote == 1)) %>% 
  filter(total > 100) %>% 
  arrange(desc(percent_yes))

yes_top_5 <- head(yes_by_country, n = 5)

#table  
kable(yes_top_5, col.names = c("Country", "Total votes", "Percent Yes"),
      align = "ccc", caption = "Top 5 highest yes percentage nations.")
```

|       Country       | Total votes | Percent Yes |
|:-------------------:|:-----------:|:-----------:|
| São Tomé & Príncipe |    1091     |  0.9761687  |
|     Seychelles      |     881     |  0.9750284  |
|      Djibouti       |    1598     |  0.9612015  |
|    Guinea-Bissau    |    1538     |  0.9603381  |
|     Timor-Leste     |     326     |  0.9570552  |

Top 5 highest yes percentage nations.

Result:

1.  Sao Tome and Principle 97.62%
2.  Seychelles 97.50%
3.  Djibouti 96.12%
4.  Guinea-Bissau 96.03%
5.  Timor-Leste 95.71%

#### 3b. Top 5 countries with high Abstain percentages.

``` r
abstain_by_country <- votes_processed %>%
  group_by(country) %>%
  summarize(total = n(), percent_abstain = mean(vote == 2)) %>% 
  filter(total > 100) %>% 
  arrange(desc(percent_abstain))

abstain_top_5 <- head(abstain_by_country, n = 5)

#table  
kable(abstain_top_5, col.names = c("Country", "Total votes", "Percent Yes"),
      align = "ccc", caption = "Top 5 highest Abstain percentage nations.")
```

| Country | Total votes | Percent Yes |
|:-------:|:-----------:|:-----------:|
| France  |    2527     |  0.3462604  |
|  Italy  |    2382     |  0.3240974  |
| Georgia |     713     |  0.3029453  |
|  Japan  |    2384     |  0.3028523  |
| Belgium |    2568     |  0.2998442  |

Top 5 highest Abstain percentage nations.

Result:

1.  France 34.63%
2.  Italy 32.41%
3.  Georgia 30.30%
4.  Japan 30.29%
5.  Belgium 29.98%

#### 3c. Top 5 countries with highest No percentages.

``` r
no_by_country <- votes_processed %>%
  group_by(country) %>%
  summarize(total = n(), percent_abstain = mean(vote == 3)) %>% 
  filter(total > 100) %>% 
  arrange(desc(percent_abstain))

no_top_5 <- head(no_by_country, n = 5)

#table  
kable(no_top_5, col.names = c("Country", "Total votes", "Percent Yes"),
      align = "ccc", caption = "Top 5 highest No percentage nations.")
```

|             Country              | Total votes | Percent Yes |
|:--------------------------------:|:-----------:|:-----------:|
|          United States           |    2568     |  0.5003894  |
|              Palau               |     369     |  0.4878049  |
|              Israel              |    2380     |  0.3789916  |
| Micronesia (Federated States of) |     724     |  0.2859116  |
|          United Kingdom          |    2558     |  0.2834246  |

Top 5 highest No percentage nations.

Result: United States 50.04% Palau 48.78% Israel 37.90% Micronesia
(Federated States of) 28.59% United Kingdom 28.34%

### Question 4

#### 4. How do Kenya’s key allies vote compared to Kenya? The United States of America, The United Kingdom and China.

``` r
#summarize yes votes by year and country
by_year_country <- votes_processed %>% 
  group_by(year, country) %>% 
  summarize(total = n(), percent_yes = mean(vote == 1))

by_year_country
```

    ## # A tibble: 4,735 x 4
    ## # Groups:   year [34]
    ##     year country     total percent_yes
    ##    <dbl> <chr>       <int>       <dbl>
    ##  1  1947 Afghanistan    34       0.382
    ##  2  1947 Argentina      38       0.579
    ##  3  1947 Australia      38       0.553
    ##  4  1947 Belarus        38       0.5  
    ##  5  1947 Belgium        38       0.605
    ##  6  1947 Bolivia        37       0.595
    ##  7  1947 Brazil         38       0.658
    ##  8  1947 Canada         38       0.605
    ##  9  1947 Chile          38       0.658
    ## 10  1947 Colombia       35       0.543
    ## # ... with 4,725 more rows

``` r
#Do major Kenya's allies agree with Un resolutions?
#data viz US and UK, China, Kenya. 
countries <- c("United States", "United Kingdom", "China", "Kenya")
labels <- data.frame(country = countries, x = c(2005,2000, 2010,2005 ), 
                     y = c(0.25, 0.60, 0.82, 0.95))

US_UK_China_Kenya <- by_year_country %>% 
  filter(country %in% c("United States", "United Kingdom", "China", "Kenya"))

US_UK_China_Kenya_plot <- US_UK_China_Kenya %>% 
  ggplot(aes(x = year, y = percent_yes, color = country))+
  geom_line()+
  geom_text(data = labels, aes(x,y, label = country), size = 4)+
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10))+
  theme(legend.position = "none")+
  labs(title = "US, UK, China and Kenya percent Yes votes", 
       caption = "Trend from 1946")+
  xlab("year")+
  ylab("Percent_yes")+
  scale_y_continuous(labels = percent)+
  plot_format

US_UK_China_Kenya_plot
```

![](UN-voting-analysis-Markdown-github-Report-document_files/figure-gfm/Kenya%20allies-1.png)<!-- -->

``` r
#data viz US and UK, China, Kenya.
US_UK_China_Kenya %>% 
  ggplot(aes(x = year, y = percent_yes))+
  geom_line()+
  facet_wrap(~country)+
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10))+
  theme(legend.position = "none")+
  labs(title = "US, UK, China and Kenya percent Yes votes", 
       caption = "Trend from 1946")+
  xlab("year")+
  ylab("Percent_yes")+
  scale_y_continuous(labels = percent)+
  plot_format
```

![](UN-voting-analysis-Markdown-github-Report-document_files/figure-gfm/Kenya%20allies%20facet%20wrapped-1.png)<!-- -->

Result: Kenya and China have a yes percent of over 75%. They have
similar curve trends. Us and UK have similar patterns till 1980. From
1980, US averages 25% while UK’s percentage rises to 50%

## Join votes and descriptions for futhur analysis

``` r
#an inner join to join the two data sets
votes_descriptions <- votes_processed %>% 
  inner_join(descriptions, by = c("rcid", "session"))

#view if the new data frame 
glimpse(votes_descriptions)
```

    ## Rows: 352,472
    ## Columns: 15
    ## $ rcid    <dbl> 46, 46, 46, 46, 46, 46, 46, 46, 46, 46, 46, 46, 46, 46, 46, 46~
    ## $ session <dbl> 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2,~
    ## $ vote    <dbl> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,~
    ## $ ccode   <int> 2, 20, 40, 41, 42, 70, 90, 91, 92, 93, 94, 95, 100, 101, 130, ~
    ## $ year    <dbl> 1947, 1947, 1947, 1947, 1947, 1947, 1947, 1947, 1947, 1947, 19~
    ## $ country <chr> "United States", "Canada", "Cuba", "Haiti", "Dominican Republi~
    ## $ region  <chr> "North America", "North America", "Latin America & Caribbean",~
    ## $ date    <dttm> 1947-09-04, 1947-09-04, 1947-09-04, 1947-09-04, 1947-09-04, 1~
    ## $ unres   <chr> "R/2/299", "R/2/299", "R/2/299", "R/2/299", "R/2/299", "R/2/29~
    ## $ me      <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,~
    ## $ nu      <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,~
    ## $ di      <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,~
    ## $ hr      <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,~
    ## $ co      <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,~
    ## $ ec      <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,~

### Question 5

#### 5. What is Kenya’s opinion on the six UN topics? The Palestinian conflict, nuclear weapons and material, arms control and disarmament, colonialism, economic development and human rights.

``` r
#collapse the topics, rename them for clarity, filter 1 for topic. 
votes_gathered <- votes_descriptions %>%
  pivot_longer(
    cols = c(me:ec),
    names_to = "topic",
    values_to = "has_topic")%>%
  mutate(topic = recode(topic,
                 me = "Palestinian conflict",
                 nu = "Nuclear weapons and nuclear material",
                 di = "arms control and disarmament",
                 hr = "Human rights",
                 co = "Colonialism",
                 ec = "Economic development")) %>% 
  filter(has_topic == 1)
 
#view the tidy data 
head(votes_gathered)
```

    ## # A tibble: 6 x 11
    ##    rcid session  vote ccode  year country region date                unres topic
    ##   <dbl>   <dbl> <dbl> <int> <dbl> <chr>   <chr>  <dttm>              <chr> <chr>
    ## 1    47       2     1     2  1947 United~ North~ 1947-10-05 00:00:00 R/2/~ Huma~
    ## 2    47       2     1    20  1947 Canada  North~ 1947-10-05 00:00:00 R/2/~ Huma~
    ## 3    47       2     1    40  1947 Cuba    Latin~ 1947-10-05 00:00:00 R/2/~ Huma~
    ## 4    47       2     3    41  1947 Haiti   Latin~ 1947-10-05 00:00:00 R/2/~ Huma~
    ## 5    47       2     1    42  1947 Domini~ Latin~ 1947-10-05 00:00:00 R/2/~ Huma~
    ## 6    47       2     2    70  1947 Mexico  Latin~ 1947-10-05 00:00:00 R/2/~ Huma~
    ## # ... with 1 more variable: has_topic <dbl>

``` r
by_country_year_topic <- votes_gathered %>%
  group_by(country, year, topic) %>%
  summarize(total = n(), percent_yes = mean(vote == 1)) %>%
  ungroup()
```

Kenya’s yes vote on different UN topics

``` r
#Kenya's views on the six topics
kenya_by_country_year_topic <- by_country_year_topic %>% 
  filter(country == "Kenya") %>% 
  ggplot(aes(x = year, y = percent_yes))+
  geom_line()+
  facet_wrap(~topic)+
  labs(title = "Kenya's Views on the 6 UN topics",
       subtitle = "percent Yes votes",
       caption = "trend from 1946")+
  xlab("Year")+
  ylab("Percent")+
  scale_y_continuous(labels = percent)+
  plot_format


kenya_by_country_year_topic
```

<img src="UN-voting-analysis-Markdown-github-Report-document_files/figure-gfm/Kenya on the topics-1.png" width="95%" />

Result: Kenya has maintained a \>75% yes vote on arms control and
disarmament, colonialism and economic development. Vote on Human rights
decreased significantly between 1990 to 1995. This was during President
Moi’s regime when the country fought for a multi party democracy. Kenya
also had decreased yes votes on the Palestine conflict during the 60s
and 90s.

### Question 6

#### 6. What is Afghanistan, Israel and United State’s view on the Palestinian conflict.

``` r
#filter for nations yes vote on the Palestinian conflict 
nations <-  c("Afghanistan", "Israel", "United States")

afghan_israel_us <- votes_descriptions %>% 
  filter(country %in% nations, me == 1) %>% 
  group_by(year, country) %>% 
  summarize(percent_yes = mean(vote == 1))

afghan_israel_us %>% 
  ggplot(aes(x = year, y = percent_yes))+
  geom_line()+
  facet_wrap(~country)+
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10))+
  scale_y_continuous(labels = percent)+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  plot_format
```

<img src="UN-voting-analysis-Markdown-github-Report-document_files/figure-gfm/Palestinian Conflict-1.png" width="95%" />

The Palestinian conflict dates back to 1910 and is one between Jews and
Arabs. Both Israel and Palestine have been in a territorial dispute for
decades. The UN tried ending the conflict in 1947 by partitioning a
Jewish state which the Arabs highly opposed. In a recent 2012 vote, the
UN general assembly voted overwhelmingly to give Palestine a non member
observer status. [The general assembly
vote](https://www.un.org/press/en/2012/ga11317.doc.htm) Afghanistan,
supports UN’s resolutions on the Palestinian conflict. The US and Israel
do not support the deccision.

### Question 7

#### 7. Which nations have the sharpest yes or no voting trends in the vote in recent years?

Use linear regression to map out trends

``` r
#nesting by country and topic
country_topic_coefficients  <- by_country_year_topic %>% 
  nest(-country, -topic) %>% 
  mutate(model = map(data, ~lm(percent_yes ~ year, data = .))) %>%
  mutate(tidied = map(model, tidy)) %>% 
  unnest(tidied)

head(country_topic_coefficients)
```

    ## # A tibble: 6 x 9
    ##   country     topic    data    model  term  estimate std.error statistic p.value
    ##   <chr>       <chr>    <list>  <list> <chr>    <dbl>     <dbl>     <dbl>   <dbl>
    ## 1 Afghanistan Colonia~ <tibbl~ <lm>   (Int~ -9.20e+0  1.96         -4.70 4.76e-5
    ## 2 Afghanistan Colonia~ <tibbl~ <lm>   year   5.11e-3  0.000989      5.17 1.23e-5
    ## 3 Afghanistan Economi~ <tibbl~ <lm>   (Int~ -1.15e+1  3.62         -3.17 3.49e-3
    ## 4 Afghanistan Economi~ <tibbl~ <lm>   year   6.24e-3  0.00183       3.42 1.85e-3
    ## 5 Afghanistan Human r~ <tibbl~ <lm>   (Int~ -7.27e+0  4.37         -1.66 1.06e-1
    ## 6 Afghanistan Human r~ <tibbl~ <lm>   year   4.08e-3  0.00221       1.85 7.43e-2

The slope is the estimated change each year of voting “yes”. The p.value
assesses whether a trend could be due to chance or is of statistical
significance. P.values that are less than 0.05 are statistically
significant hence give correct trend predictions.

``` r
country_topic_filtered <- country_topic_coefficients %>%
  filter(term == "year") %>%
  mutate(p.adjusted = p.adjust(p.value))%>%
  filter(p.adjusted < .05)

head(country_topic_filtered)
```

    ## # A tibble: 6 x 10
    ##   country     topic    data     model term  estimate std.error statistic p.value
    ##   <chr>       <chr>    <list>   <lis> <chr>    <dbl>     <dbl>     <dbl>   <dbl>
    ## 1 Afghanistan Colonia~ <tibble~ <lm>  year   0.00511  0.000989      5.17 1.23e-5
    ## 2 Austria     Colonia~ <tibble~ <lm>  year   0.00835  0.00152       5.49 8.10e-6
    ## 3 Barbados    Colonia~ <tibble~ <lm>  year   0.00669  0.00120       5.60 1.25e-5
    ## 4 Barbados    Palesti~ <tibble~ <lm>  year   0.0166   0.00295       5.61 1.21e-5
    ## 5 Belgium     Colonia~ <tibble~ <lm>  year   0.00731  0.00155       4.73 4.36e-5
    ## 6 Bolivia     Colonia~ <tibble~ <lm>  year   0.00852  0.00118       7.23 3.26e-8
    ## # ... with 1 more variable: p.adjusted <dbl>

``` r
#steepest downward trend
country_topic_filtered %>% 
  arrange(estimate) %>% 
  head(n = 5)
```

    ## # A tibble: 5 x 10
    ##   country       topic    data   model term  estimate std.error statistic p.value
    ##   <chr>         <chr>    <list> <lis> <chr>    <dbl>     <dbl>     <dbl>   <dbl>
    ## 1 Vanuatu       Palesti~ <tibb~ <lm>  year   -0.0327   0.00516     -6.33 2.60e-5
    ## 2 Vanuatu       Colonia~ <tibb~ <lm>  year   -0.0179   0.00271     -6.60 2.53e-5
    ## 3 Malta         Nuclear~ <tibb~ <lm>  year   -0.0112   0.00137     -8.15 3.14e-8
    ## 4 Cyprus        Human r~ <tibb~ <lm>  year   -0.0108   0.00196     -5.48 1.22e-5
    ## 5 United States Palesti~ <tibb~ <lm>  year   -0.0107   0.00194     -5.51 6.85e-6
    ## # ... with 1 more variable: p.adjusted <dbl>

``` r
#steepest upward trend

country_topic_filtered %>% 
  arrange(desc(estimate)) %>% 
  head(n = 5)
```

    ## # A tibble: 5 x 10
    ##   country      topic    data    model term  estimate std.error statistic p.value
    ##   <chr>        <chr>    <list>  <lis> <chr>    <dbl>     <dbl>     <dbl>   <dbl>
    ## 1 Malawi       Palesti~ <tibbl~ <lm>  year    0.0201   0.00289      6.95 5.64e-7
    ## 2 Nepal        Palesti~ <tibbl~ <lm>  year    0.0187   0.00221      8.46 8.28e-9
    ## 3 Barbados     Palesti~ <tibbl~ <lm>  year    0.0166   0.00295      5.61 1.21e-5
    ## 4 South Africa Colonia~ <tibbl~ <lm>  year    0.0166   0.00188      8.82 1.13e-8
    ## 5 Malawi       Colonia~ <tibbl~ <lm>  year    0.0150   0.00280      5.34 2.31e-5
    ## # ... with 1 more variable: p.adjusted <dbl>

From the code, the country with the steepest downward trend is Vanuatu
with an estimate of -0.0327. The country with the highest upward trend
is Malawi with an estimate of 0.0201

#### Vanuatu’s downward trend

``` r
#visualize top downward trend.
vanuatu_by_year_topic <- by_country_year_topic %>% 
  filter(country == "Vanuatu")

steepest_downward_trend <- by_country_year_topic %>% 
  filter(country == "Vanuatu", topic == "Palestinian conflict") %>% 
  ggplot(aes(x = year, y = percent_yes))+
  geom_line()+
  labs(title = "Vanuatu - Sharpest downward trend in voting", 
       caption = "Trend from 1946")+
  xlab("Year")+
  ylab("Percent")+
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10))+
  scale_y_continuous(labels = percent)+
  plot_format

steepest_downward_trend
```

![](UN-voting-analysis-Markdown-github-Report-document_files/figure-gfm/visualize%20top%20downward%20trend-1.png)<!-- -->

#### Malawi’s upward trend

``` r
#visualize top upward trend
steepest_upward_trend <- by_country_year_topic %>% 
  filter(country == "Malawi", topic == "Palestinian conflict") %>% 
  ggplot(aes(x = year, y = percent_yes))+
  geom_line()+
  labs(title = "Malawi - Sharpest upward trend in voting", 
       caption = "Trend from 1946")+
  xlab("Year")+
  ylab("Percent")+
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10))+
  scale_y_continuous(labels = percent)+
  plot_format

steepest_upward_trend
```

![](UN-voting-analysis-Markdown-github-Report-document_files/figure-gfm/top%20upward%20trend-1.png)<!-- -->
