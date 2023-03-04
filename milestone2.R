library(tidyverse)
library(ggthemes)
library(treemap)
library(knitr)

colomb_df <- read_csv("data/big/colombia-complexity-df.csv") %>%
    select(-country)

c_df <- read_csv("data/big/complexity-colombia-all.csv")
gdp_dat <- read_csv("data/gdp_state.csv")


# distribution of RCAs 

colomb_df %>%
    filter(rca2 < 3) %>%
    ggplot(aes(x=rca2)) +
    geom_density() +
    geom_vline(xintercept = 1, linetype = "dotted") +
    facet_wrap(~year, scales="free_x") +
    theme_few() +
    labs(
         title = "Colombia Industry RCA distribution by Year",
         x = "Continuous RCA"
    )

ggsave("rca-year-distribution.png")

colomb_df %>%
    filter(rca2 < 3) %>%
    filter(year == 2022) %>%
    ggplot(aes(x=rca2)) +
    geom_density() +
    geom_vline(xintercept = 1, linetype = "dotted") +
    facet_wrap(~Dpt_name) +
    theme_few() +
    labs(
         title = "Colombia Industry RCA distribution by State",
         subtitle = "Cross-Section at 2022",
         x = "Continuous RCA"
    )

ggsave("rca-state-distribution.png")

names(colomb_df)

colomb_df %>%
    filter(year == 2019) %>%
    group_by(Dpt_name) %>%
    summarise(
              num_industries = n(),
              total_wages = sum(value)
    ) %>%
    mutate(
           Wage_Fraction = paste0(round(total_wages / sum(total_wages) * 100, 2), "%")
    )
              

c_df %>%
    filter(year == 2019) %>%
    group_by(state) %>%
    summarise(
              industries = n(), 
              population = sum(people),
              total_wages = sum(wage)
    ) %>%
    mutate(
           wage_fraction = total_wages / sum(total_wages) * 100,
           population_fraction = population / sum(population) * 100,
           wage_per_capita = total_wages / population 
    ) %>%
    kable(digits = 2)

gdp_dat %>% 
    left_join(colomb_df) %>%
    filter(year == 2022) %>%
    group_by(Dpt_name) %>%
    summarise(
              gdp_percent = mean(gdp_percentage_state),
              eci = mean(eci)
    ) %>%
    ggplot(aes(x=eci, y=gdp_percent)) +
    geom_smooth(method="lm", se = F, linetype = "dashed") +
    geom_label(aes(label=Dpt_name)) +
    theme_few() +
    labs(
         title = "State ECI vs GDP Share",
         subtitle = "2022"
    )

ggsave("eci-vs-gdp.png")


gdp_eci_df <- gdp_dat %>% 
    left_join(colomb_df) %>%
    filter(year == 2022) %>%
    group_by(Dpt_name) %>%
    summarise(
              gdp_percent = mean(gdp_percentage_state),
              eci = mean(eci)
    ) 

lm(gdp_percent ~ eci, gdp_eci_df) %>%
    summary()

colomb_df

colomb_df %>%
    group_by(year, product) %>%
    summarise(
              ubiquity = sum(rca)
    ) %>%
    left_join(colomb_df) %>%
    group_by(year, Dpt_name) %>%
    summarise(
              diversity = sum(rca, na.rm = T),
              avg_ubiquity = mean(ubiquity, na.rm = T)
    ) %>%
    ungroup() %>%
    group_by(year) %>%
    mutate(
           mean_diversity = mean(diversity, na.rm = T),
           mean_ubiquities = mean(avg_ubiquity, na.rm = T)
    ) %>%
    ungroup() %>%
    ggplot(aes(x=diversity, avg_ubiquity)) +
    geom_point() +
    geom_vline(aes(xintercept = mean_diversity)) +
    geom_hline(aes(yintercept = mean_ubiquities)) +
    facet_wrap(~year) +
    theme_few() +
    labs(
         title = "Ubiquity vs Diversity for Colombian States"
    )
ggsave("ubiquity-vs-diversity.png")

prod_order <- (colomb_df %>%
    group_by(year, product) %>%
    summarise(
              ubiquity = sum(rca)
    ) %>% 
    filter(year == 2012) %>%
    arrange(-ubiquity))$product

state_order <- colomb_df %>%
    group_by(year, Dpt_name) %>%
    summarise(
              diversity = sum(rca)
    ) %>% 
    filter(year == 2012) %>%
    arrange(-diversity) %>%
    select(Dpt_name)

ordered_df <- (colomb_df %>%
    filter(year == 2012) %>%
    select(year, Dpt_name, product, rca) %>% 
    pivot_wider(names_from=product, values_from=rca) %>%
    select(-year))[, c("Dpt_name", prod_order)]

