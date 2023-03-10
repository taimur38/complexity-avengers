library(tidyverse)
library(ggthemes)
library(treemap)
library(knitr)

colomb_df <- read_csv("data/big/colombia-complexity-df.csv") %>%
    rename(rpca = rpca_wage)

colomb_ppl_df <- read_csv("data/big/colombia-complexity-df-people.csv") 

names(colomb_ppl_df)

c_df <- read_csv("data/big/complexity-colombia-all.csv")
gdp_dat <- read_csv("data/gdp_state.csv")

colomb_ppl_df %>%
    select(product, rca2, rpca)


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

colomb_ppl_df %>%
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

colomb_ppl_df %>%
    filter(rca2 < 3) %>%
    filter(year == 2019) %>%
    ggplot(aes(x=rca2)) +
    geom_density() +
    geom_vline(xintercept = 1, linetype = "dotted") +
    facet_wrap(~Dpt_name) +
    theme_few() +
    labs(
         title = "Colombia Industry RCA distribution by State",
         subtitle = "Cross-Section at 2019",
         x = "Continuous RCA"
    )

ggsave("rca-state-distribution.png")

names(colomb_df)

colomb_ppl_df %>%
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
    left_join(colomb_ppl_df) %>%
    filter(year == 2019) %>%
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
         subtitle = "2019"
    )

ggsave("eci-vs-gdp.png")


gdp_eci_df <- gdp_dat %>% 
    left_join(colomb_ppl_df) %>%
    filter(year == 2019) %>%
    group_by(Dpt_name) %>%
    summarise(
              gdp_percent = mean(gdp_percentage_state),
              eci = mean(eci)
    ) 

lm(gdp_percent ~ eci, gdp_eci_df) %>%
    stargazer(type="text")

colomb_df

colomb_ppl_df %>%
    group_by(year, product) %>%
    summarise(
              ubiquity = sum(rca)
    ) %>%
    left_join(colomb_ppl_df) %>%
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

prod_order <- (colomb_ppl_df %>%
    group_by(year, product) %>%
    summarise(
              ubiquity = sum(rca)
    ) %>% 
    filter(year == 2012) %>%
    arrange(-ubiquity))$product

state_order <- colomb_ppl_df %>%
    group_by(year, Dpt_name) %>%
    summarise(
              diversity = sum(rca)
    ) %>% 
    filter(year == 2012) %>%
    arrange(-diversity) %>%
    select(Dpt_name)

(colomb_ppl_df %>%
    filter(year == 2012) %>%
    select(year, Dpt_name, product, rca) %>% 
    pivot_wider(names_from=product, values_from=rca, values_fill=0) %>%
    select(-year))[, c("Dpt_name", prod_order)] %>%
    pivot_longer(-c("Dpt_name"), values_to="rca", names_to = "product") %>%
    mutate(
           product = factor(product, levels=prod_order),
           Dpt_name = factor(Dpt_name, levels=state_order$Dpt_name),
           rca = factor(rca)
    ) %>%
    ggplot(aes(x=product, y=Dpt_name, fill=rca)) +
    geom_tile() +
    theme_few() +
    scale_fill_viridis_d() +
    labs(
         title = "Industries exhibit a block-diagonal structure",
         subtitle = "Colombia, 2012 using RCA",
         y = "State",
         x = "Industry",
    ) +
    theme(
          axis.ticks.x = element_blank(),
          axis.text.x = element_blank()
    )

ggsave("imgs/block-diagonal.png")

prod_order2 <- (colomb_df %>%
    mutate(rpca = ifelse(rpca >= 1, 1, 0)) %>%
    group_by(year, product) %>%
    summarise(
              ubiquity = sum(rpca, na.rm = T)
    ) %>% 
    filter(year == 2012) %>%
    arrange(-ubiquity))$product

state_order2 <- colomb_df %>%
    mutate(rpca = ifelse(rpca >= 1, 1, 0)) %>%
    group_by(year, Dpt_name) %>%
    summarise(
              diversity = sum(rpca, na.rm = T)
    ) %>% 
    filter(year == 2012) %>%
    arrange(-diversity) 


(colomb_df %>%
    filter(year == 2012) %>%
    select(year, Dpt_name, product, rpca) %>% 
    pivot_wider(names_from=product, values_from=rpca, values_fill=0) %>%
    select(-year))[, c("Dpt_name", prod_order2)] %>% 
    pivot_longer(-c("Dpt_name"), values_to="rpca", names_to = "product") %>%
    mutate(
           product = factor(product, levels=prod_order2),
           Dpt_name = factor(Dpt_name, levels=state_order3$Dpt_name),
           rpca = factor(ifelse(rpca >= 1, 1, 0))
    ) %>%
    ggplot(aes(x=product, y=Dpt_name, fill=rpca)) +
    geom_tile() +
    theme_few() +
    scale_fill_viridis_d() +
    labs(
         title = "Industries exhibit a block-diagonal structure",
         subtitle = "Colombia, 2012 Using RpCA",
         y = "State",
         x = "Industry",
    ) +
    theme(
          axis.ticks.x = element_blank(),
          axis.text.x = element_blank()
    )

ggsave("imgs/block-diagonal-rpca.png")

prod_order3 <- (colomb_df %>%
    group_by(year, product) %>%
    summarise(
              ubiquity = sum(rca, na.rm = T)
    ) %>% 
    filter(year == 2012) %>%
    arrange(-ubiquity))$product

state_order3 <- colomb_df %>%
    group_by(year, Dpt_name) %>%
    summarise(
              diversity = sum(rca, na.rm = T)
    ) %>% 
    filter(year == 2012) %>%
    arrange(-diversity) 

(colomb_df %>%
    filter(year == 2012) %>%
    select(year, Dpt_name, product, rca) %>% 
    pivot_wider(names_from=product, values_from=rca, values_fill=0) %>%
    select(-year))[, c("Dpt_name", prod_order3)] %>% 
    pivot_longer(-c("Dpt_name"), values_to="rca", names_to = "product") %>%
    mutate(
           product = factor(product, levels=prod_order3),
           Dpt_name = factor(Dpt_name, levels=state_order3$Dpt_name),
           rca = factor(ifelse(rca >= 1, 1, 0))
    ) %>%
    ggplot(aes(x=product, y=Dpt_name, fill=rca)) +
    geom_tile() +
    theme_few() +
    scale_fill_viridis_d() +
    labs(
         title = "Industries exhibit a block-diagonal structure",
         subtitle = "Colombia, 2012 Using RCA and Wages",
         y = "State",
         x = "Industry",
    ) +
    theme(
          axis.ticks.x = element_blank(),
          axis.text.x = element_blank()
    )

ggsave("imgs/block-diagonal-rca-wages.png")
