# forms.new.tri.b <- map(forms.new, ~ add_predictors(formula(.x, env = .GlobalEnv), 
#                                                    ~ chain_type*Age.years*sector))
# fit.tri.b <- comp12.noLA %>% 
#   fit_with(clm, forms.new.tri.b, Hess = TRUE) 
# 
# hc.tri.b <- map(fit.tri.b, ~ get_CL_vcov(.x, comp12.noLA$care.home.group.new))
# map2(fit.tri.b, hc.tri.b, ~tidy(.x, cluster = TRUE, vcov = .y))
# 
# map2(fit.tri, fit.tri.b, anova)
# 
# library(effects)
# 
# ef.plot(comp12.noLA, Age.years, chain_type, .model = fit.tri.b[[5]])
# ef.plot(comp12.noLA, sector, chain_type, .model = fit.tri.b[[5]])
# ef.plot(comp12.noLA, Age.years, sector, .model = fit.tri.b[[5]])
# ef.plot(comp12.noLA, chain_type,  .model = fit.tri.b[[5]])


comp12.noLA.2 <- comp12.noLA %>%
  group_by(care.home.group.new) %>%
  summarise(group.mean.age = mean(Age.years, na.rm = TRUE)) %>%
  inner_join(., comp12.noLA, by = "care.home.group.new") %>%
  ungroup() %>%
  mutate(age.deviation = Age.years - group.mean.age,
         abs.age.deviation = abs(age.deviation),
         chain_type2 = forcats::fct_recode(chain_type, Small = "Independent"))
  
  
forms.new.tri.c <- map(forms.new, ~add_predictors(formula(.x, env = .GlobalEnv),
                                                  ~chain_type2 * age.deviation))
fit.tri.c <- comp12.noLA.2 %>%
  fit_with(clm, forms.new.tri.c, Hess = TRUE)

hc.tri.c <- map(fit.tri.c, ~get_CL_vcov(.x, comp12.noLA.2$care.home.group.new))
map2(fit.tri.c, hc.tri.c, ~tidy(.x, cluster = TRUE, vcov = .y))

ef.plot(comp12.noLA.2, age.deviation, chain_type2, .model = fit.tri.c[[5]])

map(fit.tri.c, ~ ef.plot(comp12.noLA.2, age.deviation, chain_type2, .model = .x))
