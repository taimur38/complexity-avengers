library(tidyverse)
library(haven)
library(economiccomplexity)

all_years <- read_stata("data/big/df_all_select.dta")
dept_names <- read_csv("data/dept_names.csv")

names(all_years)

# potential issue? lot of wage = na info here
all_years %>%
    group_by(YEAR, is.na(wage)) %>%
    summarise(cnt = n()) %>%
    pivot_wider(names_from="is.na(wage)", values_from=cnt) %>%
    mutate(
           pct = `TRUE` / (`TRUE` + `FALSE`)
    )

    # get yomna a list of the 


# make a dataframe which just has year, state, industry, number of people, and the total wage for it. 
# the 'fundamental' values from the huge dataset 

complexity_colombia <- all_years %>%
    filter(!is.na(wage)) %>%
    group_by(YEAR, Dpt_name, industry_4) %>%
    mutate(
           effective_wage = weights * wage,
    ) %>%
    summarise(
            people = sum(weights),
            wage = sum(effective_wage),
            # educ = sum(effective_educ)
    ) %>%
    rename(
           year = YEAR,
           state = Dpt_name,
           industry = industry_4,
    )

complexity_colombia

complex_df <- tibble( 
                         year = as.integer(),
                         Dpt_name = "",
                         product = "", 
                         value = 0,
                         rca = 0,
                         rca2 = 0,
                         rpca = 0,
                         eci = 0,
                         coi = 0,
                         pci = 0,
                         cog = 0,
                         density = 0
                )


# subset_df <- complexity_colombia %>% 
#     filter(year == 2012) %>% 
#     rename(
#            country = state,
#            product = industry,
#            value = wage 
#     )

for(y in unique(complexity_colombia$year)) {

    # first get things into the way the complexity package likes.
    big_subset_df <- complexity_colombia %>% 
        filter(year == y) %>%
        rename(
               country = state,
               product = industry, 
               value = people 
        )

    big_subset_df

    subset_df <- big_subset_df %>%
        select(country, product, value)

    mcp <- balassa_index(subset_df)
    mcp2 <- balassa_index(subset_df, discrete = F)

    mcp_p <- balassa_index(subset_df_people)
    mcp_p2 <- balassa_index(subset_df_people, discrete = F)
    
    complexity <- complexity_measures(mcp, method='eigenvalues')
    eci <- complexity$complexity_index_country 
    pci <- complexity$complexity_index_product 

    prox <- proximity(mcp)
    prox_p <- prox$proximity_product 

    outlook <- complexity_outlook(mcp, prox_p, pci) 
    coi <- outlook$complexity_outlook_index 
    cog <- outlook$complexity_outlook_gain 

    df_cog <- data.frame(as.matrix(cog))
    df_cog <- df_cog %>%
        mutate(country = row.names(.)) %>%
        pivot_longer(!country, names_to = "product", values_to = "cog") %>%
        mutate(
               product = ifelse(str_detect(product, "^X"),
                                str_sub(product, start=2),
                                product)
        )


        # now we are looking at avg wage bill for this industry here
        # versus average wage bill 

    df_rca <- data.frame(as.matrix(mcp))
    df_rca2 <- data.frame(as.matrix(mcp2))
    df_rpca <- big_subset_df %>% 
        group_by(product) %>% 
        mutate(
               worker_total = sum(value, na.rm = T),
        ) %>%
        ungroup() %>%
        group_by(country) %>%
        mutate(
               pop_state = sum(value, na.rm = T)
        ) %>%
        ungroup() %>%
        mutate(pop_total = sum(value, na.rm = T)) %>%
        mutate(
               rpca = (value / pop_state) / (worker_total / pop_total),   
        ) %>%
        select(country, product, rpca) 

    df_rca <- df_rca %>%
        mutate(country = row.names(.)) %>%
        pivot_longer(!country, names_to = "product", values_to = "rca") %>%
        mutate(
               product = ifelse(str_detect(product, "^X"),
                                str_sub(product, start=2),
                                product)
        )

    df_rca2 <- df_rca2 %>%
        mutate(country = row.names(.)) %>%
        pivot_longer(!country, names_to = "product", values_to = "rca2") %>%
        mutate(
               product = ifelse(str_detect(product, "^X"),
                                str_sub(product, start=2),
                                product)
        )

    df_rpca <- df_rpca %>%
        mutate(
               product = ifelse(str_detect(product, "^X"),
                                str_sub(product, start=2),
                                product)
        )

    prox_df <- data.frame(as.matrix(prox_p))
    prox_denom <- prox_df %>% 
        mutate_if(is.numeric, funs(sum(.)))
    
    df_density <- data.frame(as.matrix(mcp %*% as.matrix(prox_df)))
    df_density <- df_density / head(prox_denom, nrow(df_density))
    df_density <- df_density %>% 
        mutate(country = row.names(.)) %>% 
        pivot_longer(!country, names_to = "product", values_to = "density") %>% 
        mutate(
               product = ifelse(str_detect(product, "^X"), 
                                str_sub(product, start=2), 
                                product)
        )

    df_merge_y = subset_df %>% 
        left_join(df_rca, by=c("country", "product")) %>%
        left_join(df_rca2, by=c("country", "product")) %>%
        left_join(df_rpca, by=c("country", "product")) %>%
        left_join(
                  data.frame(eci, coi) %>% 
                      mutate(country = rownames(.)), 
                  by="country"
        ) %>%
        left_join(
                  data.frame(pci) %>% 
                      mutate(product = rownames(.)),
                  by = "product"
        ) %>%
        left_join(df_cog, by=c("country", "product")) %>%
        left_join(df_density, by=c("country", "product"))

    df_merge_y$coi <- scale(df_merge_y$coi)[,1]
    df_merge_y$cog <- scale(df_merge_y$cog)[,1]

    df_merge_y <- df_merge_y %>%
        rename(Dpt_name = country) %>%
        mutate(year = y)

    complex_df <- bind_rows(complex_df, df_merge_y)

}

complex_df

write_csv(complex_df, "data/big/colombia-complexity-df-people.csv")
