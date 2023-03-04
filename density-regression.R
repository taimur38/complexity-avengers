library(tidyverse)
library(lfe)
library(ggthemes)
library(stargazer)

colomb_df <- read_csv("data/big/colombia-complexity-df.csv") 

colomb_df <- colomb_df %>%
    select(-country)

# density regression

# the question is - if a place has RCA 0 in 2012, but a high density
# what is the likelihood that that place has an RCA 1 in 2022? 
# does density tell us whether the product appeared later

# lets try this for 2012 - 2019 first 
comparison_years1 <- colomb_df %>%
    filter(year %in% c(2012, 2019)) %>%
    select(year, Dpt_name, product, rca, density) %>%
    pivot_wider(names_from = year, values_from=c("density", "rca")) %>%
    mutate(
           rca_diff = rca_2019 - rca_2012
    ) %>%
    mutate(
           rca_factor_2012 = as.factor(rca_2012)
    )

felm(rca_2019 ~ log(density_2012) + rca_2012, comparison_years1) %>%
    stargazer(
              type = "text"
    )

# we can also try for 2022
comparison_years2 <- colomb_df %>%
    filter(year %in% c(2012, 2022)) %>%
    select(year, Dpt_name, product, rca, density) %>%
    pivot_wider(names_from = year, values_from=c("density", "rca")) %>%
    mutate(
           rca_diff = rca_2022 - rca_2012
    ) 

felm(rca_2022 ~ log(density_2012), comparison_years2) %>%
    stargazer()


## Some Visualizations ----

colomb_df %>%
    ggplot(aes(x=density)) +
    geom_histogram(data = . %>% filter(year == 2019), fill="red", alpha = 0.4) +
    geom_histogram(data = . %>% filter(year == 2012), fill="blue", alpha = 0.4) +
    facet_wrap(~Dpt_name) +
    labs(
         title = "Colombia Density histogram",
         subtitle = "blue = 2012, red = 2019"
    ) +
    theme_few() 

colomb_df %>%
    filter(year %in% c(2012, 2019)) %>%
    ggplot(aes(x=density, color=factor(year))) +
    geom_density() +
    facet_wrap(~Dpt_name) +
    labs(
         title = "Colombia Density histogram"
    ) +
    theme_few() 

ggsave("density-distribution.png")

colomb_df %>%
    filter(year == 2022) %>%
    ggplot(aes(x=density)) +
    geom_histogram() +
    facet_wrap(~Dpt_name) +
    labs(
         title = "Colombia Density histogram, 2022"
    ) +
    theme_few() 
