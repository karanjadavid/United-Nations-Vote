# UNITED NATIONS VOTE ANALYSIS 


#load packages
library(broom)
library(countrycode)
library(knitr)
library(scales)
library(tidyverse)


#import data sets
votes <- readRDS("votes.rds")
descriptions <- readRDS("descriptions.rds")


#data overview
glimpse(votes)
glimpse(descriptions)


# data sets explained
# There are two data sets 
    # a)votes
    # b)descriptions

#Votes data set 
#Votes data set contains united nations voting data. There are 4 original 
#columns.
# 1) rcid - roll call id.
#2)session - session number the voting was done.
#3) vote- a code or country's choice. Under code, we have 1 for yes, 2
# for abstain, 3 for no, 8 for not present, 9 for not a member. 
#4) ccode - unique code for a particular country.
#first session was held in 1946.


#descriptions data set
# descriptions data frame has 10 columns about topics that UN member countries 
#cast votes on.
#rcid and session columns are also in the votes data set. Other columns include
#date of voting, UN resolution and different topics. 
#me - Palestinian conflict, nu - nuclear weapons and nuclear material, 
#di- arms and disarmament, hr- human rights, co - colonialism, 
#ec- economic development.


#standard plot format throughout the analysis
plot_format <- theme(plot.title = element_text(face = "bold", size = 18),
                      plot.subtitle = element_text(margin = margin(b = 20)),
                      axis.title.y = element_text(margin = margin(r = 10)),
                      axis.title.x = element_text(margin = margin(t = 10)),
                      plot.margin = margin(10,10,10,10),
                      legend.title = element_text(face = "bold"))


#data cleaning and transformation
#remove data not needed while adding country name and its Continent columns.  
votes_processed <- votes %>% 
  filter(vote <= 3) %>% 
  mutate(year = session + 1945) %>% 
  mutate(country = countrycode(ccode, "cown", "country.name")) %>% 
  mutate(region = countrycode(sourcevar = country, 
                              origin = "country.name",
                              destination = "region")) %>% 
  filter(!is.na(country))

head(votes_processed)


#percent yes votes over the years
yes_by_year <- votes_processed %>% 
  group_by(year) %>% 
  summarize(total = n(), percent_yes = mean(vote == 1))

head(yes_by_year, n = 3)

ggplot(data = yes_by_year, aes(x = year, y = percent_yes))+
  geom_line()+
  labs(title = "percent of YES votes United Nations", 
       caption = "Trend from 1946")+
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10))+
  scale_y_continuous(labels = percent)+
  xlab("Year")+
  ylab("Percent Yes")+
  plot_format
  

#percent abstained votes over the years
abstain_by_year <- votes_processed %>% 
  group_by(year) %>% 
  summarize(total = n(), percent_abstain = mean(vote == 2))

head(abstain_by_year, n = 3)

#abstain viz
ggplot(data = abstain_by_year, aes(x = year, y = percent_abstain))+
  geom_line()+
  labs(title = "percent of ABSTAIN from vote United Nations", 
       caption = "Trend from 1946")+
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10))+
  scale_y_continuous(labels = percent)+
  xlab("Year")+
  ylab("Percent Abstain")+
  plot_format


#percent no votes over the years
no_by_year <- votes_processed %>% 
  group_by(year) %>% 
  summarize(total = n(), percent_no = mean(vote == 3))

head(no_by_year, n = 3)

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


#yes, no and abstain votes 
 votes_processed %>% 
   group_by(year) %>% 
   summarize(percent_yes = mean(vote == 1), 
             percent_abstain = mean(vote == 2),
             percent_no = mean(vote == 3))%>%
   pivot_longer(
     cols = c(starts_with("percent")),
     names_to = "percent_name",
     values_to = "percent_value"
     ) %>% 
   ggplot(aes(x = year, y = percent_value, color = percent_name))+
   geom_line()+
   labs(title = "Total votes percentage United Nations", 
        caption = "Trend from 1946")+
   xlab("Year")+
   ylab("Percent")+
   scale_x_continuous(breaks = scales::pretty_breaks(n = 10))+
   scale_y_continuous(labels = percent)+
   plot_format
 
 
 #representation of different regions of the world and how much they agree
 #and vote yes in UN decisions.
yes_regions <- votes_processed %>%
  group_by(region) %>%
  summarize(percent_yes = mean(vote == 1)) %>% 
  ggplot(aes(x = reorder(region, -percent_yes), y = percent_yes, fill = region))+
  geom_col()+
  labs(title = "Regional Yes percent United Nations votes", 
       caption = "Trend from 1946")+
  xlab("Region")+
  ylab("Percentage")+
  scale_x_discrete(labels = function(x) str_wrap(x, width = 15))+
  scale_y_continuous(labels = percent)+
  plot_format
  
yes_regions


#summarize by country . Filter countries with less than 100 votes out
#arrange in descending order
yes_by_country <- votes_processed %>%
  group_by(country) %>%
  summarize(total = n(), percent_yes = mean(vote == 1)) %>% 
  filter(total > 100) %>% 
  arrange(desc(percent_yes))

yes_by_country

abstain_by_country <- votes_processed %>%
  group_by(country) %>%
  summarize(total = n(), percent_abstain = mean(vote == 2)) %>% 
  filter(total > 100) %>% 
  arrange(desc(percent_abstain))

abstain_by_country

no_by_country <- votes_processed %>%
  group_by(country) %>%
  summarize(total = n(), percent_no = mean(vote == 3)) %>% 
  filter(total > 100) %>% 
  arrange(desc(percent_no))

no_by_country


#summarize by country and year
by_year_country <- votes_processed %>% 
  group_by(year, country) %>% 
  summarize(total = n(), percent_yes = mean(vote == 1))

by_year_country


#Do major Kenyan allies agree with Un resolutions?
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


###################################################################
#linear regression
###################################################################
#linear regression on United states
#find out how percent yes changes with time

us_by_year <- by_year_country %>% 
  filter(country == "United States")

us_fit <- lm(percent_yes~year, data = us_by_year)


summary(us_fit)

# slope is -0.0062393. this is the estimated change each year of the US
#voting "yes"
#The p.value assesses whether a trend could be due to chance 
#US p.value is 1.37 * 10-7 which is in this case less than 0.05 hence significant


#linear regression on Afghanistan
#find out how percent yes changes with time

afghanistan_by_year <- by_year_country %>% 
  filter(country == "Afghanistan")

afghanistan_fit <- lm(percent_yes~year, data = afghanistan_by_year)


summary(afghanistan_fit)
# slope is 6.009e-03. this is the estimated change each year of Afghanistan
#voting "yes"
#The p.value assesses whether a trend could be due to chance 
#Afghanistan's p.value is 3.06*10^-9 which is in this case less than 0.05 
#hence significant


israel_by_year <- by_year_country %>% 
  filter(country == "Israel")

israel_fit <- lm(percent_yes~year, data = israel_by_year)


summary(israel_fit)

# slope is -0.006853. this is the estimated change each year of Israel
#voting "yes"
#The p.value assesses whether a trend could be due to chance 
#Israel's p.value is 1.89*10^-6 which is in this case less than 0.05 
#hence significant



# tidy the linear regression models
afghanistan_fit <- lm(percent_yes~year, data = afghanistan_by_year)
afghanistan_fit
israel_fit <- lm(percent_yes~year, data = israel_by_year)
israel_fit
us_fit <- lm(percent_yes~year, data = us_by_year)
us_fit


tidied_afghanistan <- tidy(afghanistan_fit)
tidied_afghanistan
tidied_israel <- tidy(israel_fit)
tidied_israel
tidied_us <- tidy(us_fit)
tidied_us

#combine the models
tidied_models <- bind_rows(tidied_afghanistan, tidied_israel, tidied_us)
tidied_models



#statistical country coefficients
nested_country <- by_year_country %>%
  nest(-country) %>% 
  mutate(models = map(data, ~lm(percent_yes~year, .))) %>% 
  mutate(tidied = map(models, tidy)) %>% 
  unnest(tidied)
 

significant_country_coefficients <- nested_country %>% 
  filter(term == "year") %>%
  mutate(p.adjusted = p.adjust(p.value))%>%
  filter(p.adjusted < .05)




###########################################################
#join data sets
###########################################################
votes_descriptions <- votes_processed %>% 
  inner_join(descriptions, by = c("rcid", "session"))

votes_descriptions


#filter for nations yes vote on the Palestinian conflict 
nations <-  c("Afghanistan", "Israel", "Palestine", "United States")

afghan_israel_palestine_us <- votes_descriptions %>% 
  filter(country %in% nations, me == 1) %>% 
  group_by(year, country) %>% 
  summarize(percent_yes = mean(vote == 1))



afghan_israel_palestine_us %>% 
  ggplot(aes(x = year, y = percent_yes))+
  geom_line()+
  facet_wrap(~country)



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
  
head(votes_gathered)

by_country_year_topic <- votes_gathered %>%
  group_by(country, year, topic) %>%
  summarize(total = n(), percent_yes = mean(vote == 1)) %>%
  ungroup()

head(by_country_year_topic)




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




#nesting by country year and topic
country_topic_coefficients  <- by_country_year_topic %>% 
  nest(-country, -topic) %>% 
  mutate(model = map(data, ~lm(percent_yes ~ year, data = .))) %>%
  mutate(tidied = map(model, tidy)) %>% 
  unnest(tidied)

country_topic_coefficients



country_topic_filtered <- country_topic_coefficients %>%
  filter(term == "year") %>%
  mutate(p.adjusted = p.adjust(p.value))%>%
  filter(p.adjusted < .05)


#steepest downward trend
country_topic_filtered %>% 
  arrange(estimate)

#steepest upward trend
country_topic_filtered %>% 
  arrange(desc(estimate))

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
  
