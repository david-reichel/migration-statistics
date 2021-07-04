#################################################
##      R code

# This is the computer code, using the software R, showing the analysis of 
# the book chapter 'Migration statistics' by Albert Kraler and David Reichel. 
#################################################

# Author of the script: David Reichel
# Twitter: david_reichel
# ResearchGate: https://www.researchgate.net/profile/David-Reichel

library(tidyverse)
library(eurostat)
library(openxlsx)
library(countrycode)
library(ggrepel)
library(scales)
library(WDI)
library(knitr)

EUlist2 <- c("AT","BE","BG","HR","CY", "CZ","DK","EE","FI", "FR","DE","EL","HU",
"IE","IT","LV","LT", "LU","MT","NL","PL", "PT","RO","SK","SI", "ES","SE")

# path to computer if you work locally. I put the data in a sub-folder 'data' on the path below
pth <- 'ENTER HERE THE PATH TO THE FILES ON YOUR COMPUTER'

# Migration flows in Europe by country of previous and country of next residence

# The first code block, below, replicates the analysis provided in the book chapter 
# on migrant flows in Europe.

# It first downloads the two relevant tables from Eurostat on emigration and immigration. 
# Then it matches the tables and rearranges the data for analysis. 

d1 <- get_eurostat("migr_imm5prv") # Immigration by age group, sex and country of previous residence
# local backup
# write.csv(d1, file = paste0(pth, "migr_imm5prv.csv"), row.names = FALSE)

d2 <- get_eurostat("migr_emi3nxt") # Emigration by age group, sex and country of next usual residence
# local backup
# write.csv(d2, file = paste0(pth, "migr_emi3nxt.csv"), row.names = FALSE)


d1 <- d1 %>%
  mutate(direction = "immigration") %>%
  rename(from = partner, to = geo)

d2 <- d2 %>%
  mutate(direction = "emigration") %>%
  rename(from = geo, to = partner)

d1 <- d1 %>%
  bind_rows(d2)

d <- d1 %>%
  filter(sex == "T") %>%
  filter(age == "TOTAL") %>%
  filter(agedef == "REACH") %>%
  filter(unit == "NR") %>%
  filter(from %in% EUlist2) %>%
  filter(to %in% EUlist2) %>%
  pivot_wider(names_from = direction, values_from = values) %>%
  filter(from != to) %>%
  mutate(diff = immigration-emigration,
         emigration = emigration+0.0000001, # add a tiny number to allow for logarithms and avoid 0s
         prp = immigration/emigration) %>%
  filter(!is.na(immigration) & !is.na(emigration)) %>%
  arrange(from, to)

# quick plot to check correlations of immigration and emigration data among countries
d %>%
  filter(time  == "2018-01-01") %>% 
  ggplot(aes(to, from, fill = log(prp))) + 
  geom_tile() +
  geom_text(aes(label = round(prp, 1)))


p1 <- ggplot() +
  geom_point(data = filter(d, time > "2014-01-01"), 
             aes(immigration, emigration, size = immigration, colour = str_sub(time, 1, 4)),
             alpha = 0.8) +
  geom_text_repel(data = filter(d, time > "2014-01-01" & (immigration > 10000 | emigration > 10000)), aes(immigration, emigration, label = paste0(from, sprintf('➔'), to)), size = 3) +
  geom_abline(intercept = 0, slope = 1, colour = grey(0.2)) +
  coord_cartesian(xlim = c(0, 35000), ylim = c(0, 35000)) +
  scale_size(guide = FALSE) +
  labs(colour = "year",
       x = "Migration measured as immigration by destination country",
       y = "Migration measured as emigration by origin country") +
  theme_bw() +
  theme(legend.position = c(0.9, 0.2))

p1

# filter(d, from == "AT", to == "DE", time == "2008-01-01", age == "TOTAL")
# filter(d2, from == "AT", to == "DE", time == "2008-01-01", age == "TOTAL")

# main difference in emigration
d1 <- d %>%
  filter(time > "2014-01-01") %>%
  group_by(from, to) %>%
  summarise(immigration = mean(immigration),
            emigration = mean(emigration),
            prp = mean(prp),
            diff = mean(diff),
            n_years = n()) %>%
  ungroup()

# check how many countries of origin and how many countries of destination
length(unique(d1$from)); length(unique(d1$to))
nrow(d1)

# average flows across pairs
summary(d1$emigration)
filter(d1, emigration == min(d1$emigration))
filter(d1, emigration == max(d1$emigration))
summary(d1$immigration)
filter(d1, immigration == min(d1$immigration))
filter(d1, immigration == max(d1$immigration))


## correlations
filter(d, time > "2014-01-01") %>%
  select(immigration, emigration) %>% cor()

d1 %>%
  select(immigration, emigration) %>% cor()

# check if the correlations among emigration and immigration numbers changes over time
d %>%
  group_by(time) %>%
  summarise(cor1 = cor(immigration, emigration)) %>% 
  ggplot() +
  geom_col(aes(time, cor1)) +
  geom_text(aes(time, cor1, label = round(cor1, 2)), vjust = -0.3)


## differences
## average difference
summary(d1$prp)
summary(d1$diff)
filter(d1, diff == max(d1$diff))
filter(d1, diff == min(d1$diff))

d1 %>%
  group_by(from) %>%
  summarise(diff_mn = mean(diff),
            diff_sm = sum(diff),
            diff_sd = sd(diff)) %>%
  mutate(diff_rank = rank(diff_mn),
         rank_group = ifelse(diff_rank < 6, "top",
                             ifelse(diff_rank > (max(diff_rank)-6), "bot",
                             "other"))) %>%
  filter(rank_group != "other") %>%
  arrange(diff_mn)

# main difference in immigration
d1 %>%
  group_by(to) %>%
  summarise(diff_mn = mean(diff),
            diff_sm = sum(diff),
            diff_sd = sd(diff),
            prp_mn = mean(prp)) %>%
  mutate(diff_rank = rank(diff_mn),
         rank_group = ifelse(diff_rank < 6, "top",
                             ifelse(diff_rank > (max(diff_rank)-6), "bot",
                             "other"))) %>%
  filter(rank_group != "other") %>%
  arrange(diff_mn)

## focus on Italy as an interesting example
d2 <- filter(d, from == "IT" | to == "IT") %>% 
  filter(time == "2018-01-01") %>%
  mutate(fromto = paste0(from, "-", to),
         fromto = fct_reorder2(fromto, from, diff))


# remove objects
rm(d);rm(d1);rm(d2)

#################################################
# Internet access
# Global internet access of total population and internet use by migrants in Europe
#################################################


# global internet use
inds <- c("IT.NET.USER.ZS", "NY.GDP.PCAP.KD", "SP.POP.TOTL")
# "Individuals using the Internet (% of population)"
# source: International Telecommunication Union ( ITU ) World Telecommunication/ICT Indicators Database, see: https://data.worldbank.org/indicator/IT.NET.USER.ZS

wd1 <- WDI(country = "all", indicator = inds, start = 2017, end = 2019, 
           extra = FALSE)

wd1 <- wd1 %>% 
  filter(!str_sub(iso2c, 1, 1) %in% c("1", "2", "3", "4", "5", "6", "7", "8")) %>%
  filter(!is.na(IT.NET.USER.ZS)) %>%
  group_by(country, iso2c) %>%
  mutate(maxyear = year == max(year)) %>%
  ungroup() %>%
  filter(maxyear == TRUE)

nrow(wd1); length(unique(wd1$iso2c))
count(wd1, year)
summary(wd1$IT.NET.USER.ZS)
filter(wd1, IT.NET.USER.ZS == min(IT.NET.USER.ZS)) # %>% pull(country)
filter(wd1, IT.NET.USER.ZS == max(IT.NET.USER.ZS)) # %>% pull(country)
ggplot(wd1, aes(IT.NET.USER.ZS)) + geom_histogram(binwidth = 5)
with(wd1, cor(NY.GDP.PCAP.KD, IT.NET.USER.ZS, use = "complete.obs"))
summary(lm(IT.NET.USER.ZS~NY.GDP.PCAP.KD, data = wd1))


#################################################
### Internet access data from Eurostat

#################################################

d <- get_eurostat("isoc_ci_ifp_iu")

d <- d %>%
  filter(ind_type %in% c("CB_NAT", "CB_FOR")) %>%
  filter(time == "2019-01-01") %>%
  filter(unit == "PC_IND") %>%
  filter(indic_is == "I_IMT12") %>% # or never use the internet "I_IUX"
  filter(!str_detect(geo, "EU")) %>%
  filter(geo != "EA")

lvs <- filter(d, ind_type == "CB_NAT") %>% arrange(values) %>% pull(geo) %>% as.character()

d <- d %>%
  mutate(geo = fct_relevel(geo, lvs))

d1 <- d %>%
  pivot_wider(geo, names_from = ind_type, values_from = values) %>%
  mutate(diff = CB_NAT-CB_FOR)

#################################################
# This plot creates figure 26.2

p1 <- ggplot(data = d) +
  geom_segment(data = d1, 
               aes(x = geo, xend = geo, y = CB_NAT, yend = CB_FOR),
               size = 2, alpha = 0.8, colour = "grey") +
  geom_point(aes(geo, values, colour = ind_type), size = 5.5) +
  geom_text(aes(geo, values, label = values), size = 3.5, colour = "white") +
  scale_colour_manual("Country of birth", labels = c("Foreign", "Native"),
                      values = c("darkblue", "darkred")) +
  labs(x = "Country", y = "Percentage of individuals: last internet use more than one year ago or never") +
  coord_flip() +
  theme_bw() +
  theme(legend.position = c(0.85, 0.15))
p1


#################################################
# Global migration stocks. 

#################################################

# Data from the [United Nations Population Division]
# (https://www.un.org/en/development/desa/population/migration/data/estimates2/estimates19.asp). 
# It includes several tables on the total international migration stock, 
# by age and sex and by destination and origin. 

# With the below code, we load the data from the United Nations website, and 
# rearrange them into one file that contains observations by country of destination and origin.

d <- read.xlsx("https://www.un.org/en/development/desa/population/migration/data/estimates2/data/UN_MigrantStockByOriginAndDestination_2019.xlsx",
               sheet = "Table 1", startRow = 16, na.strings = c("..", ""))

# there is a newer one available, which, however, requires different recoding etc. so we stick to the old version used when writing this chapter
# d <- read.xlsx("https://www.un.org/development/desa/pd/sites/www.un.org.development.desa.pd/files/undesa_pd_2020_ims_stock_by_sex_destination_and_origin.xlsx",
#                sheet = "Table 1", startRow = 15, na.strings = c("..", ""))


# this code does a lot of data cleaning and rearrangements to make the dataset ready for analysis
d <- d %>%
  rename(dest_country = X3, year = X1) %>%
  filter(X5 < 900) %>%
  filter(!is.na(X5)) %>%
  filter(year == 2019) %>%
  dplyr::select(-year, -X2, -X4, -X5, -X6) %>%
  mutate_all(as.character) %>%
  pivot_longer(-dest_country, 
               names_to = "orig_country", 
               values_to = "value", values_ptypes = list(val = 'character')) %>%
  filter(!is.na(value)) %>% 
  mutate(value = as.numeric(value),
         orig_country = str_replace_all(orig_country, "\\.", " ")) %>%
  dplyr::select(orig_country, dest_country, value) %>%
  mutate(orig_country = ifelse(orig_country == "Eswatini", "Swaziland", 
                               orig_country),
         dest_country = ifelse(dest_country == "Eswatini", "Swaziland", 
                               dest_country),
         orig_country2 = countrycode(orig_country, "country.name", "iso2c"),
         dest_country2 = countrycode(dest_country, "country.name", "iso2c"))

# check if total number is the same as the sum of all origin countries

d %>%
  filter(orig_country == "Total") %>%
  arrange(desc(value)) %>%
  head(25)

d %>%
  filter(orig_country != "Total") %>%
  group_by(dest_country) %>%
  summarise(value = sum(value, na.rm = TRUE)) %>%
  arrange(desc(value)) %>%
  head(25)


## total number and countries with highest numbers of migrants

d %>%
  filter(orig_country == "Total") %>%
  summarise(total = sum(value))

# most important destination countries
d %>%
  filter(orig_country == "Total") %>% 
  arrange(desc(value)) %>%
  mutate(prp = value/sum(value),
         cs_prp = cumsum(prp)) %>%
  head(10)

# most important origin countries
d %>%
  filter(orig_country != "Total") %>% 
  group_by(orig_country, orig_country2) %>%
  summarise(sum_value = sum(value, na.rm = TRUE)) %>%
  ungroup() %>%
  arrange(desc(sum_value)) %>%
  mutate(prp = sum_value/sum(sum_value),
         cs_prp = cumsum(prp)) %>%
  head(10)

t1 <- d %>%
  filter(orig_country != "Total") %>%
  group_by(dest_country, dest_country2) %>%
  filter(!is.na(value)) %>%
  filter(value > 100) %>%
  summarise(sum_value = sum(value, na.rm = TRUE),
            mean_value = mean(value, na.rm = TRUE),
            min_value = min(value, na.rm = TRUE),
            max_value = max(value, na.rm = TRUE),
            n_value = n()) %>%
  arrange(desc(sum_value))

# in which countries do we have most migrants
t1

# share of migrants in those countries with more than 2.5 million migrants
d %>%
  filter(orig_country != "Total") %>%
  group_by(dest_country, dest_country2) %>%
  filter(!is.na(value)) %>%
  summarise(sum_value = sum(value, na.rm = TRUE),
            mean_value = mean(value, na.rm = TRUE),
            min_value = min(value, na.rm = TRUE),
            max_value = max(value, na.rm = TRUE),
            n_value = n()) %>%
  mutate(over2_5 = sum_value > 2500000) %>%
  group_by(over2_5) %>%
  summarise(sum_value = sum(sum_value)) %>%
  ungroup() %>%
  mutate(prp = sum_value/sum(sum_value))

# countries with highest numbers of different origin countries
t1 %>% arrange(-n_value)
t1 %>% arrange(n_value)


##
# This code creates figure 26.3

p1 <- ggplot(t1) + 
  geom_vline(xintercept = 2500000, linetype = 2, colour = "grey") +
  geom_point(aes(sum_value, n_value)) + 
  scale_x_log10(labels = comma) +
  geom_label_repel(data = filter(t1, sum_value > 2500000), 
            aes(sum_value, n_value, label = dest_country),
            vjust = 1.1, hjust = 1.1,
            colour = grey(0.15), size = 3) +
  labs(x = "Number of migrants in 2018 (logarithmic scale)",
       y = "Number of different countries of origin") +
  theme_classic()
p1

# share of origin countries among all migrants, and how much of all migrants make up the top 3 countries within a state
t2 <- d %>%
  filter(orig_country != "Total") %>%
  group_by(dest_country, dest_country2) %>%
  filter(!is.na(value)) %>%
  filter(value > 100) %>%
  mutate(prp_orig = value/sum(value),
         sum_value = sum(value),
         n_value = n()) %>%
  arrange(dest_country, desc(prp_orig)) %>%
  mutate(cum_prp_orig = cumsum(prp_orig),
         rank_prp = rank(cum_prp_orig)) %>% 
  filter(rank_prp < 3) %>%
  filter(sum_value > 1500000) %>%
  arrange(desc(prp_orig))
t2

t2 %>% filter(n_value > 100)


#################################################
# Show the cross sectional 'migration hump'
#################################################

emigs <- d %>%
  filter(orig_country != "Total") %>%
  filter(!is.na(orig_country2)) %>%
  group_by(orig_country2) %>%
  summarise(emigrants = sum(value)) %>%
  ungroup()

imigs <- d %>%
  filter(orig_country == "Total") %>%
  select(immigrants = value, dest_country2, -orig_country, -orig_country2, -dest_country)

inds <- c("NY.GDP.PCAP.KD", "SP.POP.TOTL")

# here we load GDP and total population numbers from the Word Development Indicators database
d1 <- WDI(country = "all", indicator = inds, start = 2019, end = 2019, 
           extra = FALSE) %>%
  dplyr::select(-year) %>%
  mutate(iso2c = as.character(iso2c))

nrow(d1)
d1 <- left_join(d1, emigs, by = c("iso2c" = "orig_country2"))
nrow(d1)
d1 <- left_join(d1, imigs, by = c("iso2c" = "dest_country2"))
nrow(d1)
d1 <- d1 %>%
  mutate(emigrant_rate = emigrants/SP.POP.TOTL,
         immigrant_rate = immigrants/SP.POP.TOTL)
head(d1)


##
# This code creates figure 26.4

p1 <- ggplot() +
  geom_point(data = d1, aes(NY.GDP.PCAP.KD, immigrant_rate, colour = "immigrant rate"),
             size = 3, alpha = 0.8) +
  geom_point(data = d1, aes(NY.GDP.PCAP.KD, emigrant_rate, colour = "emigrant rate"),
             size = 3, alpha = 0.8) +
  geom_smooth(data = d1, 
              aes(NY.GDP.PCAP.KD, immigrant_rate, colour = "immigrant rate"), 
              se = FALSE, size = 2) +
  geom_smooth(data = d1, 
              aes(NY.GDP.PCAP.KD, emigrant_rate, colour = "emigrant rate"), 
              se = FALSE, size = 2) +
  geom_text_repel(data = filter(d1, immigrant_rate > 0.4), 
                  aes(NY.GDP.PCAP.KD, immigrant_rate, label = country),
                  size = 3) +
  geom_text_repel(data = filter(d1, emigrant_rate > 0.4), 
                  aes(NY.GDP.PCAP.KD, emigrant_rate, label = country),
                  size = 3) +
  scale_colour_manual("", values = c("darkgrey", "black")) +
  scale_x_log10(labels = comma) +
  labs(x = "GDP per capita",
       y = "Immigrant rate / Emigrant rate") + 
  theme_bw() +
  theme(legend.position = c(0.2, 0.9))
p1

#################################################
# Data on migrant stocks by country combined with population data and refugee data from UNHCR.


# Let's filter to the total number of migrants
d <- d %>%
  filter(orig_country == "Total")


# read total population data and match with migrant stock data
# from https://population.un.org/wpp/Download/Files/1_Indicators%20(Standard)/EXCEL_FILES/1_Population/WPP2019_POP_F01_1_TOTAL_POPULATION_BOTH_SEXES.xlsx

# I saved the data locally, but also available here on github

tmp <- read.xlsx(paste0(pth, "/data/WPP2019_POP_F01_1_TOTAL_POPULATION_BOTH_SEXES.xlsx"), 
                 sheet = "ESTIMATES", startRow = 17, na.strings = c("..", ""))

tmp <- tmp %>%
  dplyr::select(country = `Region,.subregion,.country.or.area.*`, 
                code = `Country.code`, total_population = `2019`) %>%
  filter(code < 900) %>% 
  mutate(country = ifelse(country == "Eswatini", "Swaziland", country),
         total_population = round(as.numeric(total_population)),
         country2 = countrycode(country, "country.name", "iso2c")) %>%
  dplyr::select(-code, -country) %>%
  filter(!is.na(country2))

# nrow(d)
d <- d %>%
  left_join(tmp, by = c("dest_country2" = "country2"))
# nrow(d)

##### read in data from UNHCR on refugees and match with migrant stock

d2 <- read.csv(paste0(pth, "/data/unhcr_popstats_export_persons_of_concern_2020_06_10_163440.csv"), 
               skip = 3, stringsAsFactors = FALSE)

d2 <- d2 %>%
  filter(Year == 2018) %>%
  dplyr::select(country = `Country...territory.of.asylum.residence`,
                orig_country = `Origin`,
                refugees = `Refugees..incl..refugee.like.situations.`,
                asylum_seekers = `Asylum.seekers..pending.cases.`) %>%
  mutate(country = ifelse(country == "Central African Rep.", "Central African Republic",
                          country),
         country = ifelse(country == "CuraÃ§ao", "Curacao",
                          country),
         orig_country = ifelse(orig_country == "Central African Rep.", "Central African Republic",
                               orig_country),
         orig_country = ifelse(orig_country == "CuraÃ§ao", "Curacao", orig_country),
         country2 = countrycode(country, "country.name", "iso2c"),
         orig_country2 = countrycode(orig_country, "country.name", "iso2c"),
         refugees = ifelse(refugees == "*", 1, refugees),
         asylum_seekers = ifelse(asylum_seekers == "*", 1, asylum_seekers),
         refugees = as.numeric(refugees),
         asylum_seekers = as.numeric(asylum_seekers)) %>%
  dplyr::select(-country, -orig_country) %>%
  mutate(orig_country2 = ifelse(is.na(orig_country2), "OTH_STL",
                                orig_country2))


d2 <- d2 %>% 
  group_by(country2) %>%
  summarise(refugees = sum(refugees, na.rm = TRUE),
            asylum_seekers = sum(asylum_seekers, na.rm = TRUE),
            n_orig_ref = n()) %>%
  ungroup()


# before matching data, it is useful to check the number of rows before and afterwards to see if 
# it worked our properly

# nrow(d)
d <- left_join(d, d2, by = c("dest_country2" = "country2"))
# nrow(d)

# finally add gdp data from world bank indicators
inds <- c("NY.GDP.PCAP.KD")

wd1 <- WDI(country = "all", indicator = inds, start = 2019, end = 2019, 
           extra = FALSE) %>%
  dplyr::select(-year, -country) %>%
  mutate(iso2c = as.character(iso2c))

# nrow(d)
d <- left_join(d, wd1, by = c("dest_country2" = "iso2c"))
# nrow(d)

#################################################
# migrant stocks
#################################################

# calculate the migrant share in the total population 
# (we need to multiply the population by 1000, because the numbers are provided in thousands)
# calculate the share of refugees in the total population and among migrants

d <- d %>%
  mutate(migrant_share = round(100*(value/(1000*total_population)), 1),
         refugees_share = refugees/(1000*total_population),
         refugees_among_migrants = refugees/value)

# counting the overall share of migrants
d %>% summarise(migrants = sum(value, na.rm = TRUE),
                total_population = 1000*sum(total_population, na.rm = TRUE)) %>%
  mutate(migrant_share = round(100*(migrants/total_population), 3)) %>%
  kable(caption = "Global stats on migrants")

# summary stats of the migrant share across countries
d$migrant_share %>% summary()

# Country with largest share of migrants in the total population
d %>% filter(migrant_share == max(d$migrant_share, na.rm = TRUE)) %>% select(dest_country, migrant_share, value)

# distribution of migrants shares across countries
p1 <- ggplot(d) +
  geom_histogram(aes(migrant_share), binwidth = 2) +
  labs(title = "distribution of migrants shares across countries")
p1

# Now we look into the number and shares of refugees among migrants

t1 <- d %>%
  summarise(migrants = sum(value, na.rm = TRUE),
            total_population = 1000*sum(total_population, na.rm = TRUE),
            refugees = sum(refugees, na.rm = TRUE),
            asylum_seekers = sum(asylum_seekers, na.rm = TRUE),
            migrants_share = migrants/total_population,
            refugees_share = refugees/total_population,
            refugees_among_migrants = refugees/migrants)

t1 %>%
  select(refugees, refugees_share, refugees_among_migrants) %>%
  kable(caption = "Global refugee numbers")


# check share of migrants against share of refugees
# p1 <- ggplot(d, aes(refugees_among_migrants, migrant_share, size = total_population)) +
#   geom_point()
# p1

# check share of refugees in total population against share of refugees among migrants
# p1 <- ggplot(d, aes(refugees_among_migrants, refugees_share, size = total_population)) +
#   geom_point() +
#   geom_smooth()
# p1

d$refugees_share %>% summary()
d$refugees_among_migrants %>% summary()

d %>% arrange(-refugees_share) %>% select(refugees_share, refugees, dest_country, refugees_among_migrants)
d %>% arrange(-refugees_among_migrants) %>% select(refugees_share, refugees, dest_country, refugees_among_migrants)

quantile(d$refugees_among_migrants, p = c(0.01, 0.99), na.rm = TRUE)

filter(d, refugees_among_migrants > 0.5) %>%
  select(dest_country, refugees, refugees_share, refugees_among_migrants) %>%
  arrange(-refugees_among_migrants)

d %>% mutate(ref_above_half_mig = as.integer(refugees_among_migrants > 0.5)) %>%
  group_by(ref_above_half_mig) %>%
  summarise(n = n(),
            refugees = sum(refugees, na.rm = TRUE),
            mn_gdp = mean(NY.GDP.PCAP.KD, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(prp = refugees/sum(refugees))


d <- d %>%
  mutate(log_gdp = log(NY.GDP.PCAP.KD),
         log_gdp_scaled = log_gdp-mean(log_gdp, na.rm = TRUE),
         log_gdp_scaled_2p = log_gdp_scaled*log_gdp_scaled)

m1 <- lm(refugees_among_migrants ~ log_gdp_scaled,
         data = d)
summary(m1)

##
# This code creates figure 26.5

p1 <- ggplot() +
  geom_point(data = d, aes(refugees_among_migrants, NY.GDP.PCAP.KD, size = refugees), 
             alpha = 0.6) +
  geom_text(data = dplyr::filter(d, refugees_among_migrants > 0.5),
            aes(refugees_among_migrants, NY.GDP.PCAP.KD, label = dest_country),
            vjust = 1.5, size = 3) +
  scale_x_continuous(limits = c(0,1)) +
  scale_y_log10(labels = comma) +
  labs(x = "Proportion of refugees in the total number of migrants in a country",
       y = "GDP per capita\n(log 10 scale)") +
  theme_bw() +
  theme(legend.position = "none")
p1

### end



#################################################
# Session info

# R version 4.0.3 (2020-10-10)
# Platform: x86_64-w64-mingw32/x64 (64-bit)
# Running under: Windows 10 x64 (build 19041)
# 
# Matrix products: default
# 
# locale:
#   [1] LC_COLLATE=German_Austria.1252  LC_CTYPE=German_Austria.1252   
# [3] LC_MONETARY=German_Austria.1252 LC_NUMERIC=C                   
# [5] LC_TIME=German_Austria.1252    
# 
# attached base packages:
#   [1] stats     graphics  grDevices utils     datasets  methods   base     
# 
# other attached packages:
#   [1] knitr_1.31        WDI_2.7.2         scales_1.1.1      ggrepel_0.9.1    
# [5] countrycode_1.2.0 openxlsx_4.2.3    eurostat_3.6.84   forcats_0.5.1    
# [9] stringr_1.4.0     dplyr_1.0.3       purrr_0.3.4       readr_1.4.0      
# [13] tidyr_1.1.2       tibble_3.0.5      ggplot2_3.3.3     tidyverse_1.3.0  
# 
# loaded via a namespace (and not attached):
#   [1] Rcpp_1.0.6         lubridate_1.7.9.2  lattice_0.20-41    class_7.3-17      
# [5] utf8_1.1.4         assertthat_0.2.1   digest_0.6.27      R6_2.5.0          
# [9] cellranger_1.1.0   plyr_1.8.6         backports_1.2.0    reprex_1.0.0      
# [13] e1071_1.7-4        highr_0.8          httr_1.4.2         pillar_1.4.7      
# [17] rlang_0.4.10       curl_4.3           readxl_1.3.1       rstudioapi_0.13   
# [21] Matrix_1.2-18      splines_4.0.3      labeling_0.4.2     RefManageR_1.3.0  
# [25] munsell_0.5.0      broom_0.7.4        compiler_4.0.3     modelr_0.1.8      
# [29] xfun_0.20          pkgconfig_2.0.3    mgcv_1.8-33        tidyselect_1.1.0  
# [33] fansi_0.4.2        crayon_1.3.4       dbplyr_2.0.0       withr_2.4.1       
# [37] sf_0.9-7           grid_4.0.3         nlme_3.1-149       jsonlite_1.7.2    
# [41] gtable_0.3.0       lifecycle_1.0.0    DBI_1.1.1          magrittr_2.0.1    
# [45] units_0.6-7        KernSmooth_2.23-17 zip_2.1.1          cli_2.5.0         
# [49] stringi_1.5.3      farver_2.0.3       fs_1.5.0           sp_1.4-5          
# [53] xml2_1.3.2         ellipsis_0.3.1     generics_0.1.0     vctrs_0.3.6       
# [57] RColorBrewer_1.1-2 tools_4.0.3        RJSONIO_1.3-1.4    glue_1.4.2        
# [61] hms_1.0.0          colorspace_2.0-0   classInt_0.4-3     rvest_0.3.6       
# [65] haven_2.3.1  

