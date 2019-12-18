library(tidyverse)
library(survey)
library(modelsummary)
library(gt)

# read the CPS data (https://usa.ipums.org)
cps <- read_csv("acs2017.csv")

# compute the percentage of immigrants in each CBSA
cps_cbsa <- cps %>%
  mutate(immigrant = CITIZEN %in% c(2,3),
         immigrant_latin_america = CITIZEN %in% c(2,3) & BPL %in% c(200, 210, 250, 299, 300)) %>%
  group_by(MET2013) %>%
  summarize(percent_immigrant = sum(immigrant * PERWT) / sum(PERWT)) %>%
  rename(CBSA = MET2013)

# read the AHS data (https://www.census.gov/programs-surveys/ahs/data/2017/ahs-2017-public-use-file--puf-/ahs-2017-national-public-use-file--puf-.html)
ahs <- read_csv("ahs2017.csv") %>%
  select(HHNATVTY,
         HHRACE,
         HHSPAN,
         HHCITSHP,
         NEARTRASH,
         HINCP,
         HHGRAD,
         OMB13CBSA,
         WEIGHT,
         starts_with("REPWEIGHT"))

# for some reason, the values of those variables are between single
# quotes, so we need to extract them and convert them to integer
ahs$NEARTRASH <- as.integer(str_match(ahs$NEARTRASH, "\'(.*)\'")[, 2])
ahs$HHNATVTY <- as.integer(str_match(ahs$HHNATVTY, "\'(.*)\'")[, 2])
ahs$HHRACE <- as.integer(str_match(ahs$HHRACE, "\'(.*)\'")[, 2])
ahs$HHSPAN <- as.integer(str_match(ahs$HHSPAN, "\'(.*)\'")[, 2])
ahs$HHCITSHP <- as.integer(str_match(ahs$HHCITSHP, "\'(.*)\'")[, 2])
ahs$HHGRAD <- as.integer(str_match(ahs$HHGRAD, "\'(.*)\'")[, 2])
ahs$OMB13CBSA <- as.integer(str_match(ahs$OMB13CBSA, "\'(.*)\'")[, 2])

# create a variable indicating whether there was a small or large amount of
# trash in the streets within 1/2 block of where the householder lives
ahs$ANYTRASH <- case_when(
  ahs$NEARTRASH %in% c(1, 2) ~ TRUE,
  ahs$NEARTRASH == 3 ~ FALSE,
  TRUE ~ NA
)

# compute the percentage of people who say they live near trash in each CBSA
ahs_cbsa <- ahs %>%
  group_by(OMB13CBSA) %>%
  summarize(percent_anytrash = sum(ANYTRASH * WEIGHT, na.rm = TRUE) / sum(WEIGHT, na.rm = TRUE)) %>%
  rename(CBSA = OMB13CBSA)

# create a data frame that combines information from the CPS and the AHS on the percentage of immigrants
# and the percentage of people who say there is trash near their home in each CBSA
cbsa <- inner_join(cps_cbsa, ahs_cbsa, by = "CBSA")

# replication of Cato's analysis
replication_cato <- lm(percent_anytrash ~ percent_immigrant, cbsa)

cm <- c(
  "(Intercept)" = "(Intercept)",
  "percent_immigrant" = "Proportion of immigrants"
  )

models <- list()
models[["Model replicating Cato's result"]] <- replication_cato

msummary(models,
         title = "Summary of linear regression analysis of the proportion of people in a CBSA who say there is a small or large amount of trash, litter or junk in streets, lots or properties within 1/2 block of where they live on the proportion of immigrants in that CBSA",
         coef_map = cm,
         gof_omit = ".*",
         statistic = "std.error",
         stars = TRUE) %>%
  tab_style(
    style = list(
      cell_text(align = "center")
    ),
    locations = cells_data(
      columns = vars(
        `term`,
        `Model replicating Cato's result`
      )
    )
  ) %>%
  tab_style(
    style = list(
      cell_text(align = "center")
    ),
    locations = cells_column_labels(
      columns = vars(
        `Model replicating Cato's result`
      )
    )
  ) %>%
  gtsave("Do immigrants litter more - Table 1.png")

ggplot(cbsa, aes(x = percent_immigrant, y = percent_anytrash)) + 
  geom_point() +
  stat_smooth(method = "lm", col = "blue") +
  theme_bw() +
  ggtitle("Proportion of people who say there is a small or large amount of trash, litter or junk in streets, lots or properties\nwithin 1/2 block of where they live in each CBSA by proportion of immigrants in that CBSA") +
  xlab("Proportion of immigrants") +
  ylab("Proportion of people who say there is trash near where they live") +
  scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  theme(plot.title = element_text(hjust = 0.5)) +
  ggsave("Do immigrants litter more - Figure 1.png", width = 10, height = 7.5)

# recode the HHRACE variable
ahs$HHRACE <- as.factor(recode(ahs$HHRACE,
                               `1` = "White",
                               `2` = "Black",
                               `4` = "Asian",
                               `-6` = NA_character_,
                               `-9` = NA_character_,
                               .default = "Other"))
ahs$HHRACE <- relevel(ahs$HHRACE, ref = "White")

# recode the HHSPAN variable
ahs$HHSPAN <- as.factor(recode(ahs$HHSPAN,
                               `1` = "Hispanic",
                               `2` = "Non-hispanic",
                               `-6` = NA_character_))
ahs$HHSPAN <- relevel(ahs$HHSPAN, ref = "Non-hispanic")

# create a variable combining information on race and ethnicity
ahs$RACE <- case_when(
  ahs$HHRACE == "White" & ahs$HHSPAN == "Non-hispanic" ~ "Non-Hispanic White",
  ahs$HHRACE == "Black" & ahs$HHSPAN == "Non-hispanic" ~ "Non-Hispanic Black",
  ahs$HHRACE == "Asian" & ahs$HHSPAN == "Non-hispanic" ~ "Asian",
  ahs$HHSPAN == "Hispanic" ~ "Hispanic",
  TRUE ~ "Other"
)
ahs$RACE <- factor(ahs$RACE,
                   levels = c(
                     "Non-Hispanic White",
                     "Non-Hispanic Black",
                     "Hispanic",
                     "Asian"
                   )
)

# recode the HHCITSHP variable
ahs$HHCITSHP <- as.factor(recode(ahs$HHCITSHP,
                                 `1` = "Native",
                                 `2` = "Native",
                                 `3` = "Native",
                                 `4` = "Foreign-born, naturalized",
                                 `5` = "Foreign-born, non-citizen",
                                 `-6` = NA_character_,
                                 `-9` = NA_character_))
ahs$HHCITSHP <- relevel(ahs$HHCITSHP, ref = "Native")

# recode the HINCP variable
ahs$HINCP <- replace(ahs$HINCP, ahs$HINCP == -6, NA_real_) / 1000

# recode the HHGRAD variable
ahs$HHGRAD <- replace(ahs$HHGRAD, ahs$HHGRAD < 0, NA_integer_)
ahs$HHGRAD <- cut(ahs$HHGRAD,
                  breaks = c(31, 39, 44, 47),
                  labels = c("Less than high school", "High school", "College"),
                  right = FALSE,
                  include.lowest = TRUE)
ahs$HHCITSHP <- relevel(ahs$HHCITSHP, ref = "Native")

# create a variable indicating whether the householder was born a non-citizen abroad
ahs$IMMIGRANT <- (ahs$HHCITSHP == "Foreign-born, non-citizen" | ahs$HHCITSHP == "Foreign-born, naturalized")

# create a variable indicating whether the householder was born a non-citizen abroad and is not a non-hispanic white
ahs$WHITE_IMMIGRANT <- (ahs$HHCITSHP == "Foreign-born, non-citizen" | ahs$HHCITSHP == "Foreign-born, naturalized") &
  ahs$RACE == "Non-Hispanic White"

# create a variable indicating whether the householder was born a non-citizen abroad and is not a non-hispanic white
ahs$NONWHITE_IMMIGRANT <- (ahs$HHCITSHP == "Foreign-born, non-citizen" | ahs$HHCITSHP == "Foreign-born, naturalized") &
  ahs$RACE != "Non-Hispanic White"

# create a variable indicating whether the householder is native-born and, if not, his region of origin
ahs$REGION_OF_ORIGIN <- case_when(
  ahs$IMMIGRANT == FALSE ~ "Native",
  ahs$HHNATVTY %in% 303:374 ~ "South/Central America or Caribbean",
  ahs$HHNATVTY %in% c(100:168, 301, 501, 515) ~ "Europe, Australia, Canada or New-Zealand",
  ahs$HHNATVTY %in% c(200, 212, 213, 214, 216, 218, 222, 224, 235, 239, 243, 245, 246, 248, 400, 414, 430, 436) ~ "Middle East, North-Africa or Central Asia",
  ahs$HHNATVTY %in% c(407, 408, 412, 416:429, 440:461) ~ "Sub-Saharan Africa",
  ahs$HHNATVTY %in% c(202, 203, 210, 229, 231, 238) ~ "South Asia",
  ahs$HHNATVTY %in%  c(205, 206, 211, 226, 233, 242, 247) ~ "South East Asia",
  ahs$HHNATVTY %in% c(207, 209, 215, 217, 220, 228, 236, 240) ~ "East Asia",
  TRUE ~ "Other"
)
ahs$REGION_OF_ORIGIN <- factor(
  ahs$REGION_OF_ORIGIN,
  levels = c("Native",
             "South/Central America or Caribbean",
             "Europe, Australia, Canada or New-Zealand",
             "Middle East, North-Africa or Central Asia",
             "Sub-Saharan Africa",
             "South Asia",
             "South East Asia",
             "East Asia")
)

# recode the CBSA variable as a factor for the logistic regression analysis
ahs$OMB13CBSA <- factor(ahs$OMB13CBSA)

# https://stackoverflow.com/questions/59376992/how-to-use-survey-to-analyze-the-american-housing-survey-data-using-replicate-we
svy <- svrepdesign(data = ahs,
                   weight = ~WEIGHT,
                   repweights = "REPWEIGHT[0-9]+",
                   type = "Fay",
                   rho = 0.5,
                   mse = TRUE)

natives <- subset(svy, IMMIGRANT == FALSE)
immigrants <- subset(svy, IMMIGRANT == TRUE)
white_immigrants <- subset(svy, WHITE_IMMIGRANT == TRUE)
nonwhite_immigrants <- subset(svy, NONWHITE_IMMIGRANT == TRUE)

natives_anytrash <- svyciprop(~I(ANYTRASH == TRUE), natives, na.rm = TRUE)
immigrants_anytrash <- svyciprop(~I(ANYTRASH == TRUE), immigrants, na.rm = TRUE)
white_immigrants_anytrash <- svyciprop(~I(ANYTRASH == TRUE), white_immigrants, na.rm = TRUE)
nonwhite_immigrants_anytrash <- svyciprop(~I(ANYTRASH == TRUE), nonwhite_immigrants, na.rm = TRUE)

anytrash_natives_immigrants <- tribble(
  ~group, ~proportion, ~ci_lower, ~ci_higher,
  "Natives", as.numeric(natives_anytrash), as.numeric(attr(natives_anytrash, "ci")[1]), as.numeric(attr(natives_anytrash, "ci")[2]),
  "Immigrants", as.numeric(immigrants_anytrash), as.numeric(attr(immigrants_anytrash, "ci")[1]), as.numeric(attr(immigrants_anytrash, "ci")[2]),
  "White Immigrants", as.numeric(white_immigrants_anytrash), as.numeric(attr(white_immigrants_anytrash, "ci")[1]), as.numeric(attr(white_immigrants_anytrash, "ci")[2]),
  "Non-White Immigrants", as.numeric(nonwhite_immigrants_anytrash), as.numeric(attr(nonwhite_immigrants_anytrash, "ci")[1]), as.numeric(attr(nonwhite_immigrants_anytrash, "ci")[2])
  )
anytrash_natives_immigrants$group <- factor(
  anytrash_natives_immigrants$group,
  levels = c("Natives",
             "Immigrants",
             "White Immigrants",
             "Non-White Immigrants")
  )

ggplot(filter(anytrash_natives_immigrants, group %in% c("Natives", "Immigrants")), aes(x = reorder(group, proportion), y = proportion)) +
  geom_bar(position = position_dodge(), stat = "identity", color = "black", fill = "steelblue") +
  geom_text(aes(label = paste0(round(proportion * 100, 1), "%")),
            position = position_stack(vjust = 0.5),
            color = "white",
            size = 5) +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_higher), width = 0.2, position = position_dodge(0.9)) +
  theme_bw() +
  ggtitle("Proportion of people who say there is a small or large amount of trash, litter or junk in streets,\nlots or properties within 1/2 block of where they live") +
  xlab("Group") +
  ylab("Proportion") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  theme(plot.title = element_text(hjust = 0.5)) +
  ggsave("Do immigrants litter more - Figure 2.png", width = 10, height = 7.5)

ggplot(filter(anytrash_natives_immigrants, group %in% c("Natives", "White Immigrants", "Non-White Immigrants")), aes(x = reorder(group, proportion), y = proportion)) +
  geom_bar(position = position_dodge(), stat = "identity", color = "black", fill = "steelblue") +
  geom_text(aes(label = paste0(round(proportion * 100, 1), "%")),
            position = position_stack(vjust = 0.5),
            color = "white",
            size = 5) +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_higher), width = 0.2, position = position_dodge(0.9)) +
  theme_bw() +
  ggtitle("Proportion of people who say there is a small or large amount of trash, litter or junk in streets,\nlots or properties within 1/2 block of where they live") +
  xlab("Group") +
  ylab("Proportion") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  theme(plot.title = element_text(hjust = 0.5)) +
  ggsave("Do immigrants litter more - Figure 3.png", width = 10, height = 7.5)

immigrants_greater_latin_america <- subset(svy, REGION_OF_ORIGIN == "South/Central America or Caribbean")
immigrants_greater_europe <- subset(svy, REGION_OF_ORIGIN == "Europe, Australia, Canada or New-Zealand")
immigrants_greater_middle_east <- subset(svy, REGION_OF_ORIGIN == "Middle East, North-Africa or Central Asia")
immigrants_africa <- subset(svy, REGION_OF_ORIGIN == "Sub-Saharan Africa")
immigrants_south_asia <- subset(svy, REGION_OF_ORIGIN == "South Asia")
immigrants_south_east_asia <- subset(svy, REGION_OF_ORIGIN == "South East Asia")
immigrants_east_asia <- subset(svy, REGION_OF_ORIGIN == "East Asia")

immigrants_greater_latin_america_anytrash <- svyciprop(~I(ANYTRASH == TRUE), immigrants_greater_latin_america, na.rm = TRUE)
immigrants_greater_europe_anytrash <- svyciprop(~I(ANYTRASH == TRUE), immigrants_greater_europe, na.rm = TRUE)
immigrants_greater_middle_east_anytrash <- svyciprop(~I(ANYTRASH == TRUE), immigrants_greater_middle_east, na.rm = TRUE)
immigrants_africa_anytrash <- svyciprop(~I(ANYTRASH == TRUE), immigrants_africa, na.rm = TRUE)
immigrants_south_asia_anytrash <- svyciprop(~I(ANYTRASH == TRUE), immigrants_south_asia, na.rm = TRUE)
immigrants_south_east_asia_anytrash <- svyciprop(~I(ANYTRASH == TRUE), immigrants_south_east_asia, na.rm = TRUE)
immigrants_east_asia_anytrash <- svyciprop(~I(ANYTRASH == TRUE), immigrants_east_asia, na.rm = TRUE)

anytrash_immigrants <- tribble(
  ~group, ~proportion, ~ci_lower, ~ci_higher,
  "Latin America and the Caribbean", as.numeric(immigrants_greater_latin_america_anytrash), as.numeric(attr(immigrants_greater_latin_america_anytrash, "ci")[1]), as.numeric(attr(immigrants_greater_latin_america_anytrash, "ci")[2]),
  "Europe, Canada, Australia and New-Zealand", as.numeric(immigrants_greater_europe_anytrash), as.numeric(attr(immigrants_greater_europe_anytrash, "ci")[1]), as.numeric(attr(immigrants_greater_europe_anytrash, "ci")[2]),
  "Middle-East, North Africa and Central Asia", as.numeric(immigrants_greater_middle_east_anytrash), as.numeric(attr(immigrants_greater_middle_east_anytrash, "ci")[1]), as.numeric(attr(immigrants_greater_middle_east_anytrash, "ci")[2]),
  "Sub-Saharan Africa", as.numeric(immigrants_africa_anytrash), as.numeric(attr(immigrants_africa_anytrash, "ci")[1]), as.numeric(attr(immigrants_africa_anytrash, "ci")[2]),
  "South Asia", as.numeric(immigrants_south_asia_anytrash), as.numeric(attr(immigrants_south_asia_anytrash, "ci")[1]), as.numeric(attr(immigrants_south_asia_anytrash, "ci")[2]),
  "South East Asia", as.numeric(immigrants_south_east_asia_anytrash), as.numeric(attr(immigrants_south_east_asia_anytrash, "ci")[1]), as.numeric(attr(immigrants_south_east_asia_anytrash, "ci")[2]),
  "East Asia", as.numeric(immigrants_east_asia_anytrash), as.numeric(attr(immigrants_east_asia_anytrash, "ci")[1]), as.numeric(attr(immigrants_east_asia_anytrash, "ci")[2])
)
anytrash_immigrants$group <- factor(
  anytrash_immigrants$group,
  levels = c("Latin America and the Caribbean",
             "Europe, Canada, Australia and New-Zealand",
             "Middle-East, North Africa and Central Asia",
             "Sub-Saharan Africa",
             "South Asia",
             "South East Asia",
             "East Asia")
)

ggplot(anytrash_immigrants, aes(x = reorder(group, proportion), y = proportion)) +
  geom_bar(position = position_dodge(), stat = "identity", color = "black", fill = "steelblue") +
  geom_text(aes(label = paste0(round(proportion * 100, 1), "%")),
            position = position_stack(vjust = 0.5),
            color = "white",
            size = 5) +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_higher), width = 0.2, position = position_dodge(0.9)) +
  theme_bw() +
  ggtitle("Proportion of immigrants who say there is a small or large amount of trash, litter or junk in streets,\nlots or properties within 1/2 block of where they live by region of origin") +
  xlab("Group") +
  ylab("Proportion") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  theme(plot.title = element_text(hjust = 0.5)) +
  ggsave("Do immigrants litter more - Figure 4.png", width = 15, height = 7.5)

native_nonhispanic_whites <- subset(natives, IMMIGRANT == FALSE & RACE == "Non-Hispanic White")
native_nonhispanic_blacks <- subset(natives, IMMIGRANT == FALSE & RACE == "Non-Hispanic Black")
native_hispanics <- subset(natives, IMMIGRANT == FALSE & RACE == "Hispanic")
native_asians <- subset(natives, IMMIGRANT == FALSE & RACE == "Asian")

native_nonhispanic_whites_anytrash <- svyciprop(~I(ANYTRASH == TRUE), native_nonhispanic_whites, na.rm = TRUE)
native_nonhispanic_blacks_anytrash <- svyciprop(~I(ANYTRASH == TRUE), native_nonhispanic_blacks, na.rm = TRUE)
native_hispanics_anytrash <- svyciprop(~I(ANYTRASH == TRUE), native_hispanics, na.rm = TRUE)
native_asians_anytrash <- svyciprop(~I(ANYTRASH == TRUE), native_asians, na.rm = TRUE)

anytrash_natives <- tribble(
  ~group, ~proportion, ~ci_lower, ~ci_higher,
  "Non-Hispanic Whites", as.numeric(native_nonhispanic_whites_anytrash), as.numeric(attr(native_nonhispanic_whites_anytrash, "ci")[1]), as.numeric(attr(native_nonhispanic_whites_anytrash, "ci")[2]),
  "Non-Hispanic Blacks", as.numeric(native_nonhispanic_blacks_anytrash), as.numeric(attr(native_nonhispanic_blacks_anytrash, "ci")[1]), as.numeric(attr(native_nonhispanic_blacks_anytrash, "ci")[2]),
  "Hispanics", as.numeric(native_hispanics_anytrash), as.numeric(attr(native_hispanics_anytrash, "ci")[1]), as.numeric(attr(native_hispanics_anytrash, "ci")[2]),
  "Asians", as.numeric(native_asians_anytrash), as.numeric(attr(native_asians_anytrash, "ci")[1]), as.numeric(attr(native_asians_anytrash, "ci")[2])
)
anytrash_natives$group <- factor(
  anytrash_natives$group,
  levels = c("Non-Hispanic Whites",
             "Non-Hispanic Blacks",
             "Hispanics",
             "Asians")
  )

ggplot(anytrash_natives, aes(x = reorder(group, proportion), y = proportion)) +
  geom_bar(position = position_dodge(), stat = "identity", color = "black", fill = "steelblue") +
  geom_text(aes(label = paste0(round(proportion * 100, 1), "%")),
            position = position_stack(vjust = 0.5),
            color = "white",
            size = 5) +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_higher), width = 0.2, position = position_dodge(0.9)) +
  theme_bw() +
  ggtitle("Proportion of natives who say there is a small or large amount of trash, litter or junk in streets,\nlots or properties within 1/2 block of where they live by race/ethnicity") +
  xlab("Group") +
  ylab("Proportion") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  theme(plot.title = element_text(hjust = 0.5)) +
  ggsave("Do immigrants litter more - Figure 5.png", width = 10, height = 7.5)

model1 <- svyglm(ANYTRASH ~ IMMIGRANT,
                         design = svy,
                         family = quasibinomial(link = "logit"))

model2 <- svyglm(ANYTRASH ~ REGION_OF_ORIGIN,
                         design = svy,
                         family = quasibinomial(link = "logit"))

model3 <- svyglm(ANYTRASH ~ REGION_OF_ORIGIN + OMB13CBSA,
                         design = svy,
                         family = quasibinomial(link = "logit"))

model4 <- svyglm(ANYTRASH ~ REGION_OF_ORIGIN + OMB13CBSA + HINCP + HHGRAD,
                         design = svy,
                         family = quasibinomial(link = "logit"))

models <- list()
models[["Basic model"]] <- model1
models[["With region of origin"]] <- model2
models[["With region of origin and CBSA fixed effects"]] <- model3
models[["With region of origin, CBSA fixed effects, income and education"]] <- model4

cm <- c(
  "IMMIGRANTTRUE" = "Immigrant",
  "REGION_OF_ORIGINSouth/Central America or Caribbean" = "South/Central America or Caribbean",
  "REGION_OF_ORIGINEurope, Australia, Canada or New-Zealand" = "Europe, Australia, Canada or New-Zealand",
  "REGION_OF_ORIGINMiddle East, North-Africa or Central Asia" = "Middle East, North-Africa or Central Asia",
  "REGION_OF_ORIGINSub-Saharan Africa" = "Sub-Saharan Africa",
  "REGION_OF_ORIGINSouth Asia" = "South Asia",
  "REGION_OF_ORIGINSouth East Asia" = "South East Asia",
  "REGION_OF_ORIGINEast Asia" = "East Asia",
  "RACENon-Hispanic Black" = "Non-Hispanic Black",
  "RACEHispanic" = "Hispanic",
  "RACEAsian" = "Asian",
  "IMMIGRANTTRUE:RACENon-Hispanic Black" = "Immigrant x Non-Hispanic Black",
  "IMMIGRANTTRUE:RACEHispanic" = "Immigrant x Hispanic",
  "IMMIGRANTTRUE:RACEAsian" = "Immigrant x Asian",
  "HINCP" = "Income in $1,000",
  "HHGRADHigh school" = "High school",
  "HHGRADCollege" = "College"
)

gof <- modelsummary::gof_map
gof$omit[gof$raw != "nobs"] <- TRUE
gof$clean[gof$raw == "nobs"] <- "N"
gof$omit[gof$raw == "nobs"] <- FALSE

msummary(models,
         title = "Summary of logistic regression analysis for variables predicting that a householder will say there is a small or large amount of trash, litter or junk in streets, lots or properties within 1/2 block of where they live",
         coef_map = cm,
         notes = list("The reference category for region of origin is Europe, Australia, Canada or New-Zealand and, for education, it's people who didn't graduate high school."),
         gof_map = gof,
         statistic = "std.error",
         stars = TRUE) %>%
  tab_style(
    style = list(
      cell_text(align = "center")
    ),
    locations = cells_data(
      columns = vars(
        `term`,
        `Basic model`,
        `With region of origin`,
        `With region of origin and CBSA fixed effects`,
        `With region of origin, CBSA fixed effects, income and education`
        )
    )
  ) %>%
  tab_style(
    style = list(
      cell_text(align = "center")
    ),
    locations = cells_column_labels(
      columns = vars(
        `Basic model`,
        `With region of origin`,
        `With region of origin and CBSA fixed effects`,
        `With region of origin, CBSA fixed effects, income and education`
      )
    )
  ) %>%
  gtsave("Do immigrants litter more - Table 2.png")

# %>%
#   tab_footnote(                                                                                                                                                                               
#     footnote = "The reference category is native.",                                                                                           
#     locations = cells_stub(rows = vars(`Immigrant`))
#   ) %>%
#   tab_footnote(                                                                                                                                                                               
#     footnote = "The reference category is less than high school.",                                                                                           
#     locations = cells_stub(rows = vars(`High school`, `College`))
#   ) %>%
#   tab_footnote(                                                                                                                                                                               
#     footnote = "The reference category is white.",                                                                                           
#     locations = cells_stub(rows = c(11, 13, 15, 17))
#   )

model5 <- svyglm(ANYTRASH ~ IMMIGRANT + RACE,
                 design = svy,
                 family = quasibinomial(link = "logit"))

model6 <- svyglm(ANYTRASH ~ IMMIGRANT * RACE,
                 design = svy,
                 family = quasibinomial(link = "logit"))

model7 <- svyglm(ANYTRASH ~ IMMIGRANT * RACE + OMB13CBSA,
                 design = svy,
                 family = quasibinomial(link = "logit"))

model8 <- svyglm(ANYTRASH ~ IMMIGRANT * RACE + OMB13CBSA + HINCP + HHGRAD,
       design = svy,
       family = quasibinomial(link = "logit"))

models <- list()
models[["Basic model"]] <- model1
models[["With race/ethnicity"]] <- model5
models[["With race/ethnicity and interaction between immigrant and race/ethnicity"]] <- model6
models[["With race/ethnicity, interaction between immigrant and race/ethnicity and CBSA fixed effects"]] <- model7
models[["With race/ethnicity, interaction between immigrant and race/ethnicity, CBSA fixed effects, income and education"]] <- model8

msummary(models,
         title = "Summary of logistic regression analysis for variables predicting that a householder will say there is a small or large amount of trash, litter or junk in streets, lots or properties within 1/2 block of where they live",
         coef_map = cm,
         notes = list("The reference category for race/ethnicity is Non-Hispanic White and, for education, it's people who didn't graduate high school."),
         gof_map = gof,
         statistic = "std.error",
         stars = TRUE) %>%
  tab_style(
    style = list(
      cell_text(align = "center")
    ),
    locations = cells_data(
      columns = vars(
        `term`,
        `Basic model`,
        `With race/ethnicity`,
        `With race/ethnicity and interaction between immigrant and race/ethnicity`,
        `With race/ethnicity, interaction between immigrant and race/ethnicity and CBSA fixed effects`,
        `With race/ethnicity, interaction between immigrant and race/ethnicity, CBSA fixed effects, income and education`
      )
    )
  ) %>%
  tab_style(
    style = list(
      cell_text(align = "center")
    ),
    locations = cells_column_labels(
      columns = vars(
        `Basic model`,
        `With race/ethnicity`,
        `With race/ethnicity and interaction between immigrant and race/ethnicity`,
        `With race/ethnicity, interaction between immigrant and race/ethnicity and CBSA fixed effects`,
        `With race/ethnicity, interaction between immigrant and race/ethnicity, CBSA fixed effects, income and education`
      )
    )
  ) %>%
  gtsave("Do immigrants litter more - Table 3.png")
