library("PISA2000lite")
library("PISA2003lite")
library("PISA2006lite")
library("PISA2009lite")
library("PISA2012lite")
library(tidyverse)
library(cimentadaj)
library(haven)
library(intsvy)
library(ggrepel)
library(SAScii)

student2015 <- read_sav("/Users/cimentadaj/Downloads/PISA/PISA2015/CY6_MS_CMB_STU_QQQ.sav")
pisa2012_dir <- "/Users/cimentadaj/Downloads/PISA/PISA2012/"
dic_student <- parse.SAScii(sas_ri = paste0(pisa2012_dir, 'PISA2012_SAS_student.sas'))

student2012 <- read_fwf(file = paste0(pisa2012_dir, 'INT_STU12_DEC03.txt'),
                        col_positions = fwf_widths(dic_student$width), progress = T)
colnames(student2012) <- dic_student$varname

pisa_data_names <- c("math2000", paste0("student", seq(2003, 2015, 3)))

pisa <-
  map(pisa_data_names, get) %>%
  setNames(pisa_data_names) %>%
  enframe()

pisa$selected_vars <-
  map(seq_along(pisa$value), function(row) {
  immigrant_var <-
    switch(pisa$name[row],
         "math2000" = "ST16Q01",
         "student2003" = "ST15Q02",
         "student2006" = "ST11Q02",
         "student2009" = "ST17Q02",
         "student2012" = "ST20Q02",
         "student2015" = "ST019BQ01T")
  
  if ("hisei" %in% names(pisa$value[[row]])) pisa$value[[row]]$HISEI <- pisa$value[[row]]$hisei
  
  pisa$value[[row]] %>%
    rename_(mom_imm = immigrant_var)
})

pisa$selected_vars <-
  pisa$selected_vars %>%
  map(function(data) {
    data %>%
      mutate(country = pisa_countrynames[as.character(CNT)],
             MISCED = car::recode(MISCED, "1:5 = 0; 6:7 = '1'")) %>%
      select(country, MISCED, mom_imm, HISEI, ends_with("READ"), ends_with("MATH")) %>%
      as_tibble()
})

pisa$reg_models <- map(seq_along(pisa$selected_vars), function(index) {
  print(pisa$name[index])
  
  if (pisa$name[index] == "student2015") {
  pisa2015.reg.pv(x = c("MISCED", "mom_imm", "HISEI"),
                  pvlabel = "MATH", by = "country", data = pisa$selected_vars[[index]])
  } else {
  pisa.reg.pv(x = c("MISCED", "mom_imm", "HISEI"),
              pvlabel = "MATH", by = "country", data = pisa$selected_vars[[index]])
  }
})

pisa$reg_models_read <- map(seq_along(pisa$selected_vars), function(index) {
  print(pisa$name[index])
  pisa.reg.pv(x = c("MISCED", "mom_imm", "HISEI"),
              pvlabel = "READ", by = "country", data = pisa$selected_vars[[index]])
})
beepr::beep()

# write_rds(pisa, path = "./pisa")
pisa <- read_rds("./pisa")

pisa$r_sq <-
  map(pisa$reg_models, ~ map(.x, ~ .x$reg[grep("squared", row.names(.x$reg)), 1:2]))

pisa$r_sq_read <-
  map(pisa$reg_models_read, ~ map(.x, ~ .x$reg[grep("squared", row.names(.x$reg)), 1:2]))

pisa$r_sq_df <- map(pisa$r_sq, ~ enframe(.x) %>% unnest(value))
pisa$r_sq_df_read <- map(pisa$r_sq_read, ~ enframe(.x) %>% unnest(value))

pisa_unnested <-
  pisa %>%
  unnest(r_sq_df, r_sq_df_read)

vars <- c("estimate", "std.error")
colnames(pisa_unnested) <-
  c("type", "country",paste0(vars, "_math"), paste0(c("country", vars), "_read"))

pisa_to_nest <-
  pisa_unnested %>%
  mutate(type = ifelse(type == "math2000", "student2000", type),
         region = countrycode::countrycode(country, "country.name", "region"),
         continent = countrycode::countrycode(country, "country.name", "continent")) %>%
  separate(type, c("resp", "year"), sep = 7)

gini <-
  read_csv("./gini.csv") %>%
  select(Country, Year, Gini) %>%
  mutate_if(is_integer, as.character) %>%
  group_by(Country, Year) %>%
  summarise(avg_gini = mean(Gini, na.rm = T),
            median_gini = median(Gini, na.rm = T))

pisa_to_nest_2 <-
  gini %>%
  right_join(pisa_to_nest, by = c("Country" = "country", "Year" = "year"))

pisa_to_nest_2 %>%
  filter(continent == "Europe") %>%
  ggplot(aes(avg_gini, estimate_math)) +
  geom_point(aes(colour = region), alpha = 0.7) +
  # geom_text_repel(aes(Value, estimate_math, label = country)) +
  geom_smooth(method = "lm") +
  facet_wrap(~ Year)

correlation_tests <-
  pisa_to_nest_2 %>%
  filter(continent %in% c("Europe")) %>%
  split(interaction(.$Year, .$continent)) %>%
  na.omit() %>%
  map(~ cor.test(.x$estimate_math, .x$avg_gini))

cor.extract <- function(cor_object) {
  data.frame(cor = unname(cor_object$estimate), p_val = cor_object$p.value)
}

map(correlation_tests, cor.extract) %>%
  enframe() %>%
  unnest(value) %>%
  separate(name, c("year", "country")) %>%
  ggplot(aes(year, cor, fill = country)) +
  geom_col(aes(fill = p_val <= 0.1)) +
  coord_flip()

##### All income inequality indicators
all_indicators <-
  read_csv("./income_inequality.csv")

# You left of here, choosing which indicators you will use:
setNames(unique(all_indicators$Measure), unique(all_indicators$MEASURE))
# The above code gives you all the measured available with its code.

income_inequality <-
  all_indicators %>%
  select(Country, Year, AGE, Value, MEASURE, Measure, Unit, METHODO) %>%
  mutate_if(is_integer, as.character) %>%
  filter(MEASURE %in% c("GINI", "GINIB", "GINIG", "PALMA", paste0("P90P", c("10", "50")),
                        "P50P10", "S80S20", "S90S10"))

vector_filler <- function(vector_to_fill, vector_to_fill_from) {
  
  which_inside_which <- function(vector_to_fill, vector_to_fill_from) {
    
    names_to_fill_from <- names(vector_to_fill_from)
    
    # Years which are present in both vectors (those we will keep as is)
    already_filled <- vector_to_fill %in% names_to_fill_from
    
    # Years which are present from the vector_to_fill_from perspective
    available_to_fill_from <- names_to_fill_from %in% vector_to_fill
    
    # Values we need to actually fill
    yrs_to_subtract <- vector_to_fill[!already_filled]
    
    # Values available to fill the yrs_to_subtract vector
    yrs_to_loop <- as.numeric(names_to_fill_from[!available_to_fill_from])
    
    list(already_filled, available_to_fill_from, yrs_to_subtract, yrs_to_loop)
  }
  
  indexes <- which_inside_which(vector_to_fill, vector_to_fill_from)
  
  indices_to_match <-
    map(indexes[[3]], function(.x) {
      subtraction <- .x - indexes[[4]]
      abs_subtr <- abs(subtraction)
      abs_subtr == min(abs_subtr)
    })
  
  new_values <-
    map_dbl(indices_to_match, ~ mean(vector_to_fill_from[!indexes[[2]]][which(.x)])) %>%
    setNames(indexes[[3]]) %>%
    `c`(vector_to_fill_from[indexes[[2]]])
  
  correct_order <-
    names(new_values) %>%
    as.numeric() %>%
    order()
  
  new_values[correct_order]
}

country_names <- unique(income_inequality$Country)
inequality_indicator <- unique(income_inequality$MEASURE)[-3]
# Excluding GINIG which is not found in the data

all_indicators <-
  map(country_names, function(country_name) {
    map(inequality_indicator, function(inequality_indicator) {
      
      specific_country <- with(income_inequality,
                               income_inequality[MEASURE == inequality_indicator &
                                                   Country == country_name &
                                                   METHODO == "METH2011" &
                                                   AGE == "TOT", c("Country", "Year", "Value")])
      
      vector_to_fill_from <- with(specific_country, setNames(Value, Year))
      
      vector_filler(vector_to_fill, vector_to_fill_from)
      
    })}) %>%
  setNames(country_names)

reduce_df <-
  all_indicators %>%
  map(~ reduce(.x, function(x, y) as.data.frame(cbind(x, y)))) %>%
  reduce(rbind)

colnames(reduce_df) <- inequality_indicator

final_inequality <-
  reduce_df %>%
  as_tibble() %>%
  mutate(year = rep(vector_to_fill, length(country_names)),
         country = rep(country_names, each = length(vector_to_fill))) %>%
  gather(indicators, value, GINI:S90S10)

pisa_to_nest_2 <-
  final_inequality %>%
  mutate(year = as.character(year)) %>%
  right_join(pisa_to_nest, by = c("country", "year")) %>%
  filter(country != "Russia")

my_plots <-
  map(na.omit(unique(pisa_to_nest_2$indicators)), function(inequality_measure) {
  pisa_to_nest_2 %>%
  filter(continent == "Europe" & indicators == inequality_measure) %>%
  ggplot(aes(value, estimate_read)) +
  geom_point(alpha = 0.7) +
  # geom_text_repel(aes(Value, estimate_math, label = country)) +
  geom_smooth(method = "lm") +
  xlab(inequality_measure) +
  ylab("") +
  facet_grid(~ year) +
  theme_minimal()
})

source("http://peterhaschke.com/Code/multiplot.R")

multiplot(my_plots[[1]], my_plots[[2]],
          my_plots[[3]], my_plots[[4]], cols = 1)

multiplot(my_plots[[5]], my_plots[[6]],
          my_plots[[7]], my_plots[[8]], cols = 1)


correlation_tests_second <-
  pisa_to_nest_2 %>%
  filter(continent %in% c("Europe")) %>%
  split(interaction(.$year, .$continent, .$indicators)) %>%
  na.omit() %>%
  map(~ cor.test(.x$estimate_math, .x$value))

cor.extract <- function(cor_object) {
  data.frame(cor = unname(cor_object$estimate), p_val = cor_object$p.value)
}

 my_corrs <-
  map(inequality_indicator, function(ineq_indi) {
  map(correlation_tests_second, cor.extract) %>%
  enframe() %>%
  unnest(value) %>%
  separate(name, c("year", "country", "indicators")) %>%
  filter(indicators == ineq_indi)  %>%
  ggplot(aes(year, cor)) +
  geom_line(aes(group = indicators)) +
  geom_smooth(aes(group = indicators), method = "lm", se = F) +
  geom_point(aes(colour = p_val <= 0.1), size = 2) +
  scale_colour_discrete(guide = F) +
  xlab("") + ylab(ineq_indi) +
  theme_minimal()
})

multiplot(my_corrs[[1]], my_corrs[[2]],
          my_corrs[[3]], my_corrs[[4]], cols = 1)

multiplot(my_corrs[[5]], my_corrs[[6]],
          my_corrs[[7]], my_corrs[[8]], cols = 1)

##### Tracking indicator
tracking <-
  read_csv("./tracking.csv") %>%
  select(-bwid, -cntry)

pisa_to_nest_3 <-
  pisa_to_nest %>%
  left_join(tracking, c("country" = "cntry_name"))

pisa_to_nest_3 %>%
  filter(continent == "Europe") %>%
  ggplot(aes(ztrack, estimate_math, alpha = std.error_math)) +
  geom_point() +
  geom_smooth(method = "lm") +
  facet_wrap(~ year) +
  scale_alpha_continuous(guide = F)

correlation_tests_third <-
  pisa_to_nest_3 %>%
  filter(continent %in% c("Europe")) %>%
  split(interaction(.$year, .$continent)) %>%
  na.omit() %>%
  map(~ cor.test(.x$estimate_math, .x$ztrack))

corr_df <-
  map(correlation_tests_third, cor.extract) %>%
  enframe() %>%
  unnest(value) %>%
  separate(name, c("year", "country"))

corr_df %>%
  ggplot(aes(year, cor)) +
  geom_line(aes(group = country)) +
  geom_smooth(aes(group = country), method = "lm") +
  geom_point() +
  ylab("Tracking * Social inequality correlation") +
  xlab("") +
  theme_minimal() +
  coord_fixed(3)

#### New Modeling ####
pisa2 <- pisa[-1, ]

pisa2$selected_vars <-
  map(seq_along(pisa2$value), function(row) {
    immigrant_var <-
      switch(pisa2$name[row],
             "student2003" = "ST15Q02",
             "student2006" = "ST11Q02",
             "student2009" = "ST17Q02",
             "student2012" = "ST20Q02",
             "student2015" = "ST019BQ01T")
    
    if ("hisei" %in% names(pisa2$value[[row]])) pisa2$value[[row]]$HISEI <- pisa2$value[[row]]$hisei
    
    pisa2$value[[row]] %>%
      rename_(mom_imm = immigrant_var)
})

quants_math <- map_dbl(pisa2$value, ~ quantile(.x$PV1MATH, prob = 0.7, na.rm = T))
quants_read <- map_dbl(pisa2$value, ~ quantile(.x$PV1READ, prob = 0.7, na.rm = T))

pisa2$selected_vars <-
  seq_along(pisa2$selected_vars) %>%
  map(function(data) {
    pisa2$selected_vars[[data]] %>%
      mutate(country = pisa_countrynames[as.character(CNT)],
             highest_edu = as.character(car::recode(as.numeric(HISCED), "1:3 = 1; 4:5 = 2; 6:7 = 3")),
             distance_from_mean_math = PV1MATH - quants_math[data],
             distance_from_mean_read = PV1READ - quants_read[data]) %>%
      select(country,
             distance_from_mean_math,
             distance_from_mean_read,
             highest_edu,
             HISCED,
             MISCED,
             mom_imm,
             HISEI,
             PARED,
             HEDRES,
             HISEI,
             HOMEPOS,
             ESCS,
             ends_with("READ"),
             ends_with("MATH")) %>%
      as_tibble()
  })

pisa2$reg_models_new_dv_math <- map(seq_along(pisa2$selected_vars), function(index) {
  print(pisa2$name[index])
  if (pisa2$name[index] == "student2015") {
    pisa2015.reg(y = "distance_from_mean_math", x = c("MISCED", "mom_imm", "HISEI"),
                 by = "country", data = pisa2$selected_vars[[index]])
  } else {
    pisa.reg(y = "distance_from_mean_math", x = c("MISCED", "mom_imm", "HISEI"),
                 by = "country", data = pisa2$selected_vars[[index]])
  }
})

# pisa2$reg_models_new_dv_read <- map(seq_along(pisa2$selected_vars), function(index) {
#   print(pisa2$name[index])
#   if (pisa2$name[index] == "student2015") {
#     pisa2015.reg(y = "distance_from_mean_read", x = c("MISCED", "mom_imm", "HISEI"),
#                  by = "country", data = pisa2$selected_vars[[index]])
#   } else {
#     pisa.reg(y = "distance_from_mean_read", x = c("MISCED", "mom_imm", "HISEI"),
#              by = "country", data = pisa2$selected_vars[[index]])
#   }
# })


# Create variable with language spoken at home the same at school or not
# MISCED variable
# HISEI and ESCS models separate
# Extract R2 and use GINI index scatterplot.

# Try their tracking index, as well as wefhorst's.
# Repeat on TIMSS and PIRLS.