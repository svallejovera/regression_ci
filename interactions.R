rm(list=ls(all=TRUE))

library(tidyverse)
library(sjPlot)
library(marginaleffects)
library(modelsummary)

stevedata::election_turnout
data(election_turnout, package = "stevedata")

election_turnout$ss <-factor(election_turnout$ss)
election_turnout$trumpw <-factor(election_turnout$trumpw)
election_turnout$gdppercap_ln <-log(election_turnout$gdppercap)

election_turnout %>% 
  ggplot(aes(x=turnoutho)) +
  geom_density()

summary(election_turnout$turnoutho)
sd(election_turnout$turnoutho)
## No interactions:
model_simple <- lm(turnoutho ~ perhsed + gdppercap_ln + trumpw + ss, data = election_turnout)
modelsummary(model_simple,
             # output = "kableExtra",
             title = "2016 Election Turnout",
             coef_rename = c("(Intercept)" = "Intercept",
                             "perhsed" = "% Completed High School",
                             "gdppercap_ln" = "GDP pc (log)",
                             "trumpw1" = "Trump Won",
                             "ss1" = "Swing State"),
             gof_map = c("nobs", "r.squared"),
             stars = TRUE,
             notes = list('OLS standard errors in parentheses.',
                          '*** p<0.001, ** p<0.01, * p<0.05'))

plot_model(model_simple, type = "pred", terms = c("perhsed"))
## Dummy x Dummy:
model_dumdum <- lm(turnoutho ~ perhsed + gdppercap_ln + ss + trumpw +
                      ss*trumpw, data = election_turnout)
modelsummary(model_dumdum,
             # output = "kableExtra",
             title = "2016 Election Turnout",
             coef_rename = c("(Intercept)" = "Intercept",
                             "perhsed" = "% Completed High School",
                             "gdppercap_ln" = "GDP pc (log)",
                             "trumpw1" = "Trump Won",
                             "ss1" = "Swing State",
                             "ss1:trumpw1" = "Trump Won * Swing State"),
             gof_map = c("nobs", "r.squared"),
             stars = TRUE,
             notes = list('OLS standard errors in parentheses.',
                          '*** p<0.001, ** p<0.01, * p<0.05'))

values_pred <- plot_model(model_dumdum, type = "int")
values_pred
values_pred$data$predicted

values_marg <- plot_slopes(model_dumdum, variables = "trumpw", condition = "ss") +
  geom_hline(yintercept = 0, linetype = "dotted")
values_marg$data$estimate

## Dummy x Continious:
model_dumcont <- lm(turnoutho ~ perhsed + gdppercap_ln + ss + trumpw +
                     ss*perhsed, data = election_turnout)
modelsummary(model_dumcont,
             # output = "kableExtra",
             title = "2016 Election Turnout",
             coef_rename = c("(Intercept)" = "Intercept",
                             "perhsed" = "% Completed High School",
                             "gdppercap_ln" = "GDP pc (log)",
                             "trumpw1" = "Trump Won",
                             "ss1" = "Swing State",
                             "ss1:perhsed" = "% Completed High School * Swing State"),
             gof_map = c("nobs", "r.squared"),
             stars = TRUE,
             notes = list('OLS standard errors in parentheses.',
                          '*** p<0.001, ** p<0.01, * p<0.05'))

values_pred <- plot_model(model_dumcont, type = "int")
values_pred
values_pred$data$predicted

values_marg <- plot_slopes(model_dumcont, variables = "ss", condition = "perhsed") +
  geom_hline(yintercept = 0, linetype = "dotted") 
values_marg
values_marg$data$estimate

## Continuous x Continious:
model_contcont <- lm(turnoutho ~ perhsed + gdppercap_ln + ss + trumpw +
                      gdppercap_ln*perhsed, data = election_turnout)
modelsummary(model_contcont,
             # output = "kableExtra",
             title = "2016 Election Turnout",
             coef_rename = c("(Intercept)" = "Intercept",
                             "perhsed" = "% Completed High School",
                             "gdppercap_ln" = "GDP pc (log)",
                             "trumpw1" = "Trump Won",
                             "ss1" = "Swing State",
                             "perhsed:gdppercap_ln" = "GDP pc (log) * % Completed High School"),
             gof_map = c("nobs", "r.squared"),
             stars = TRUE,
             notes = list('OLS standard errors in parentheses.',
                          '*** p<0.001, ** p<0.01, * p<0.05'))

values_pred <- plot_model(model_contcont, type = "pred", terms = c("gdppercap_ln","perhsed [82,88,92]"))
values_pred
values_pred$data$predicted

values_marg <- plot_slopes(model_dumcont, variables = "ss", condition = "perhsed") +
  geom_hline(yintercept = 0, linetype = "dotted") 
values_marg
values_marg$data$estimate
