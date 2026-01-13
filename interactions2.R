library(tidyverse)
library(sjPlot)
library(marginaleffects)
library(modelsummary)

stevedata::TV16
data(TV16, package = "stevedata")

TV16$is_white <- ifelse(TV16$racef=="White", 1, 0)
TV16$is_white <-factor(TV16$is_white)
TV16$female <-factor(TV16$female)
TV16$collegeed <-factor(TV16$collegeed)

table(TV16$votetrump)

## No interactions:
model_simple <- lm(votetrump ~ female + collegeed + is_white + lemprac, data = TV16)
summary(model_simple)

plot_model(model_simple,show.values = TRUE, value.offset = .3) +
  geom_hline(yintercept = 0, linetype = "dashed", alpha = 0.6) +
  scale_color_manual(values = c("peru","lightblue")) +
  theme_minimal() +
  scale_y_continuous(breaks = seq(-0.25,.25,0.05))

plot_model(model_simple, type = "pred", terms = c("lemprac")) +
  theme_minimal() +
  scale_y_continuous(labels=scales::percent) 

## Dummy x Dummy:
model_dumdum <- lm(votetrump ~ female + collegeed + is_white + lemprac +
                     is_white*female, data = TV16)
summary(model_dumdum)
values_pred <- plot_model(model_dumdum, type = "int")
values_pred
values_pred$data$predicted

values_marg <- plot_slopes(model_dumdum, variables = "female", condition = "is_white") +
  geom_hline(yintercept = 0, linetype = "dotted")
values_marg
values_marg$data$estimate

## Dummy x Continious:
model_dumdum2 <- lm(votetrump ~ female + is_white + collegeed + lemprac +
                      collegeed*is_white, data = TV16)
summary(model_dumdum2)

values_pred <- plot_model(model_dumdum2, type = "int")
values_pred
values_pred$data$predicted

values_marg <- plot_slopes(model_dumdum2, variables = "collegeed", condition = "is_white") +
  geom_hline(yintercept = 0, linetype = "dotted") 
values_marg
values_marg$data$estimate

## Continuous x Continious:
TV16$racef <- factor(TV16$racef)
model_dumcont <- lm(votetrump ~ female + collegeed + lemprac + racef +
                      lemprac*racef, data = TV16)
summary(model_dumcont)

values_pred <- plot_model(model_dumcont, type = "pred", terms = c('lemprac','racef'))
values_pred +
  facet_wrap(~group)
values_pred$data$predicted

values_marg <- plot_slopes(model_dumcont, variables = "lemprac", condition = "racef") +
  geom_hline(yintercept = 0, linetype = "dotted") 
values_marg
values_marg$data$estimate
