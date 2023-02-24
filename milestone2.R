library(tidyverse)
library(ggthemes)

colomb_df <- read_csv("data/big/colombia-complexity-df.csv") %>%
    select(-country)

colomb_df %>%
    arrange(-rca2) %>%
    select(rca2)

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
