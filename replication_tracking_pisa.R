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

student2015 <- read_sav("/Users/cimentadaj/Downloads/PISA/PISA2015/CY6_MS_CMB_STU_QQQ.sav")

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

write_rds()

pisa$r_sq <-
  map(pisa$reg_models, ~ map(.x, ~ .x$reg[grep("squared", row.names(.x$reg)), 1:2]))

pisa$r_sq_read <-
  map(pisa$reg_models_read, ~ map(.x, ~ .x$reg[grep("squared", row.names(.x$reg)), 1:2]))


pisa$r_sq_df <- map(pisa$r_sq, ~ enframe(.x) %>% unnest(value))
pisa$r_sq_df_read <- map(pisa$r_sq_read, ~ enframe(.x) %>% unnest(value))

gini <-
  read_csv("./Downloads/gini.csv") %>%
  select(Country, Year, Gini) %>%
  mutate_if(is_integer, as.character) %>%
  group_by(Country, Year) %>%
  summarise(avg_gini = mean(Gini, na.rm = T),
            median_gini = median(Gini, na.rm = T))

# You left of here, choosing which indicators you will use:
setNames(unique(income_inequality$Measure), unique(income_inequality$MEASURE))
#The above code gives you all the measured available with its code.

income_inequality <-
  read_csv("./Downloads/income_inequality.csv") %>%
  select(Country, Year, Value, MEASURE, Measure, Unit, METHODO) %>%
  mutate_if(is_integer, as.character) %>%
  filter(MEASURE %in% c("GINI", "GINIB", "GINIG", "PALMA", paste0("P90P", c("10", "50")),
                        "P50P10", "S80S20", "S90S10"))
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

pisa_to_nest_2 <-
  income_inequality %>%
  group_by(Country, Year, MEASURE, METHODO) %>%
  summarize(avg_value = mean(Value, na.rm = T)) %>%
  right_join(pisa_to_nest, by = c("Country" = "country", "Year" = "year"))


pisa_to_nest_2 %>%
  filter(MEASURE == "GINI" & METHODO == "METH2011" & Year == "2015") %>%
  ggplot(aes(avg_value, estimate_math)) +
  geom_point(alpha = 0.5) +
# geom_text_repel(aes(Value, estimate_math, label = country)) +
  geom_smooth(method = "lm", se = F) +
  facet_wrap(~ year) +
  scale_alpha_continuous(guide = F) +
  ylim(c(0, 0.4)) +
  xlim(c(20, 50))

correlation_tests <-
  pisa_to_nest %>%
  filter(continent %in% c("Europe")) %>%
  split(interaction(.$year, .$continent)) %>%
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
  geom_col() +
  coord_flip()

tracking <-
  read_csv("./Downloads/tracking.csv") %>%
  select(-bwid, -cntry)

pisa_to_nest <-
  pisa_to_nest %>%
  left_join(tracking, c("country" = "cntry_name"))

pisa_to_nest %>%
  ggplot(aes(zstand_timss, estimate_read, alpha = std.error_read)) +
  geom_point() +
  geom_smooth(method = "lm") +
  facet_wrap(~ year) +
  scale_alpha_continuous(guide = F)


# Create variable with language spoken at home the same at school or not
# MISCED variable
# HISEI and ESCS models separate
# Extract R2 and use GINI index scatterplot.

# Try 2015 PISA
# Try other GINI-type coefficients
# Try their tracking index, as well as wefhorst's.
# Repeat on TIMSS and PIRLS.

"MISED"
"ESCS" # SES index
"HISEI" # SES index not in 2015
