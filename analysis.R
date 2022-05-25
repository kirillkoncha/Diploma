library(tidyverse)
library(ggplot2)
library(readr)
library(lme4)
library(lmerTest)
library(dplyr)
library(readxl)
my_data <- read_tsv("data_small.tsv")
# Беру только свои стимулы и убираю филлеры
my_data %>% 
  filter(author=="Kirill",
         type == "Literal_m" | type == "Literal_b" | type == "Metaphor" | type == "Bleaching") -> my_data
my_data$IA_FIRST_FIXATION_DURATION <- as.numeric(my_data$IA_FIRST_FIXATION_DURATION)
my_data$IA_FIRST_RUN_DWELL_TIME <- as.numeric(my_data$IA_FIRST_RUN_DWELL_TIME)
my_data$IA_REGRESSION_IN <- as.numeric(my_data$IA_REGRESSION_IN)
my_data$IA_DWELL_TIME <- as.numeric(my_data$IA_DWELL_TIME)
my_data$IA_LABEL <- gsub('[[:punct:] ]+','',my_data$IA_LABEL)
my_data$IA_LABEL <- as.factor(my_data$IA_LABEL)
my_data$Adjective <- gsub('[[:punct:] ]+','',my_data$Adjective)
my_data$Adjective <- as.factor(my_data$Adjective)
my_data$RECORDING_SESSION_LABEL <- as.factor(my_data$RECORDING_SESSION_LABEL)
#Подсчет accuracy
my_data %>% 
  filter(ACCURACY != -1) %>%
  group_by(RECORDING_SESSION_LABEL) %>%
  summarise(ACC = mean(ACCURACY)) -> accuracy

#######Делю датасет на сабсеты#########
#Прилагательные для пар с метафорой
my_data %>% 
  filter((IA_LABEL == "крепкими" | 
            IA_LABEL == "лёгкий" |
            IA_LABEL == "слабые" |
            IA_LABEL == "сладкий" |
            IA_LABEL == "ледяной" |
            IA_LABEL == "холодный" |
            IA_LABEL == "светлая" |
            IA_LABEL == "грязные" |
            IA_LABEL == "острые" |
            IA_LABEL == "жуткий" |
            IA_LABEL == "дикий" |
            IA_LABEL == "страшную" |
            IA_LABEL == "жёсткий" |
            IA_LABEL == "безумные" |
            IA_LABEL == "горячий" |
            IA_LABEL == "бешеных"),
  ) -> adjectives
adjectives %>%
  filter( SHOULD_WE_RECALIBRATE != 1) -> adjectives
adjectives %>% 
  filter (type == "Literal_m" | type == "Metaphor") -> adjectives_m

adjectives_m$type <- as.factor(adjectives_m$type)
levels(adjectives_m$type)
adjectives_m <- within(adjectives_m, type <- relevel(type, ref = "Literal_m"))

adjectives_m.ffd <- adjectives_m[! is.na(adjectives_m$IA_FIRST_FIXATION_DURATION) & 
                                   adjectives_m$IA_FIRST_FIXATION_DURATION> 60,]
adjectives_m.gd <- adjectives_m[! is.na(adjectives_m$IA_FIRST_RUN_DWELL_TIME) & 
                                  adjectives_m$IA_FIRST_RUN_DWELL_TIME> 60,]
adjectives_m.tt <- adjectives_m[! is.na(adjectives_m$IA_DWELL_TIME) & 
                                  adjectives_m$IA_DWELL_TIME> 60,]
adjectives_m.regr <- adjectives_m[! is.na(adjectives_m$IA_REGRESSION_IN),]
#Прилигательные для бличинга#
adjectives %>% 
  filter (type == "Literal_b" | type == "Bleaching") -> adjectives_b

adjectives_b$type <- as.factor(adjectives_b$type)
levels(adjectives_b$type)
adjectives_b <- within(adjectives_b, type <- relevel(type, ref = "Literal_b"))

adjectives_b.ffd <- adjectives_b[! is.na(adjectives_b$IA_FIRST_FIXATION_DURATION) & 
                                   adjectives_b$IA_FIRST_FIXATION_DURATION> 60,]
adjectives_b.gd <- adjectives_b[! is.na(adjectives_b$IA_FIRST_RUN_DWELL_TIME) & 
                                  adjectives_b$IA_FIRST_RUN_DWELL_TIME> 60,]
adjectives_b.tt <- adjectives_b[! is.na(adjectives_b$IA_DWELL_TIME) & 
                                  adjectives_b$IA_DWELL_TIME> 60,]
adjectives_b.regr <- adjectives_b[! is.na(adjectives_b$IA_REGRESSION_IN),]
#Существительные для пар с метафорой
my_data %>% 
  filter((IA_LABEL == "учёные" | 
            IA_LABEL == "деньги" |
            IA_LABEL == "собак" |
            IA_LABEL == "денег" |
            IA_LABEL == "слон" |
            IA_LABEL == "ужас" |
            IA_LABEL == "зубами" |
            IA_LABEL == "винами" |
            (IA_LABEL == "руки" & list != "2") | #второе условие, потому что во втором листе есть еще одно тако слово, но не после прилагательного
            IA_LABEL == "боли" |
            IA_LABEL == "вопль" |
            IA_LABEL == "мороз" |
            IA_LABEL == "корпус" |
            IA_LABEL == "акцент" |
            IA_LABEL == "ночь" |
            IA_LABEL == "жару" |
            IA_LABEL == "матрас" |
            IA_LABEL == "взгляд" |
            IA_LABEL == "ботинки" |
            IA_LABEL == "делишки" |
            IA_LABEL == "ножи" |
            IA_LABEL == "боли" |
            IA_LABEL == "сироп" |
            IA_LABEL == "голос" |
            IA_LABEL == "кубик" |
            IA_LABEL == "голос" |
            IA_LABEL == "камень" |
            IA_LABEL == "взгляд" |
            IA_LABEL == "завтрак" |
            IA_LABEL == "поцелуй" |
            IA_LABEL == "ткань" |
            IA_LABEL == "мысль"),
  ) -> nouns
nouns %>%
  filter( SHOULD_WE_RECALIBRATE != 1) -> nouns
nouns %>% 
  filter (type == "Literal_m" | type == "Metaphor") -> nouns_m

nouns_m$type <- as.factor(nouns_m$type)
levels(nouns_m$type)
nouns_m <- within(nouns_m, type <- relevel(type, ref = "Literal_m"))

nouns_m.ffd <- nouns_m[! is.na(nouns_m$IA_FIRST_FIXATION_DURATION) & 
                         nouns_m$IA_FIRST_FIXATION_DURATION> 60,]
nouns_m.gd <- nouns_m[! is.na(nouns_m$IA_FIRST_RUN_DWELL_TIME) & 
                        nouns_m$IA_FIRST_RUN_DWELL_TIME> 60,]
nouns_m.tt <- nouns_m[! is.na(nouns_m$IA_DWELL_TIME) & 
                        nouns_m$IA_DWELL_TIME> 60,]
nouns_m.regr <- nouns_m[! is.na(nouns_m$IA_REGRESSION_IN),]
#Существительные для пар с бличингом
nouns %>% 
  filter (type == "Literal_b" | type == "Bleaching") -> nouns_b

nouns_b$type <- as.factor(nouns_b$type)
levels(nouns_b$type)
nouns_b <- within(nouns_b, type <- relevel(type, ref = "Literal_b"))

nouns_b.ffd <- nouns_b[! is.na(nouns_b$IA_FIRST_FIXATION_DURATION) & 
                         nouns_b$IA_FIRST_FIXATION_DURATION> 60,]
nouns_b.gd <- nouns_b[! is.na(nouns_b$IA_FIRST_RUN_DWELL_TIME) & 
                        nouns_b$IA_FIRST_RUN_DWELL_TIME> 60,]
nouns_b.tt <- nouns_b[! is.na(nouns_b$IA_DWELL_TIME) & 
                        nouns_b$IA_DWELL_TIME> 60,]
nouns_b.regr <- nouns_b[! is.na(nouns_b$IA_REGRESSION_IN),]
#Postdisambg-регион с метафорой
my_data %>% 
  filter(((IA_LABEL == "сводили" | 
             IA_LABEL == "потому" |
             IA_LABEL == "внезапно" |
             IA_LABEL == "которые" |
             IA_LABEL == "позволяют" |
             IA_LABEL == "который" |
             (IA_LABEL == "когда" & IA_LABEL == "страшную") | #потому что есть еще когда, которое не идет после снимающего контекст существительного
             IA_LABEL == "который" |
             IA_LABEL == "которые" |
             IA_LABEL == "когда" |
             IA_LABEL == "пронзили" |
             IA_LABEL == "очень" |
             IA_LABEL == "которая")) & Adjective != "Петя"
  ) -> disambg
disambg %>%
  filter( SHOULD_WE_RECALIBRATE != 1) -> disambg
disambg %>% 
  filter (type == "Literal_m" | type == "Metaphor") -> disambg_m
disambg_m.ffd <- disambg_m[! is.na(disambg_m$IA_FIRST_FIXATION_DURATION) & 
                             disambg_m$IA_FIRST_FIXATION_DURATION> 60,]
disambg_m.gd <- disambg_m[! is.na(disambg_m$IA_FIRST_RUN_DWELL_TIME) & 
                            disambg_m$IA_FIRST_RUN_DWELL_TIME> 60,]
disambg_m.tt <- disambg_m[! is.na(disambg_m$IA_DWELL_TIME) & 
                            disambg_m$IA_DWELL_TIME> 60,]
#Postdisambg-регион
disambg %>% 
  filter (type == "Literal_b" | type == "Bleaching") -> disambg_b

disambg_b$type <- as.factor(disambg_b$type)
levels(disambg_b$type)
disambg_b <- within(disambg_b, type <- relevel(type, ref = "Literal_b"))

disambg_b.ffd <- disambg_b[! is.na(disambg_b$IA_FIRST_FIXATION_DURATION) & 
                             disambg_b$IA_FIRST_FIXATION_DURATION> 60,]
disambg_b.gd <- disambg_b[! is.na(disambg_b$IA_FIRST_RUN_DWELL_TIME) & 
                            disambg_b$IA_FIRST_RUN_DWELL_TIME> 60,]
disambg_b.tt <- disambg_b[! is.na(disambg_b$IA_DWELL_TIME) & 
                            disambg_b$IA_DWELL_TIME> 60,]

#######Дескрептивная статистика#########
adjectives_m.ffd %>%
  group_by(RECORDING_SESSION_LABEL, type) %>%
  summarise(dur = mean(IA_FIRST_FIXATION_DURATION)) %>%
  group_by(type) %>%
  summarise(mean_dur = mean(dur),
            sd_dur = sd(dur))

adjectives_m.gd %>%
  group_by(RECORDING_SESSION_LABEL, type) %>%
  summarise(dur = mean(IA_FIRST_RUN_DWELL_TIME)) %>%
  group_by(type) %>%
  summarise(mean_dur = mean(dur),
            sd_dur = sd(dur))

adjectives_m.tt %>%
  group_by(RECORDING_SESSION_LABEL, type) %>%
  summarise(dur = mean(IA_DWELL_TIME)) %>%
  group_by(type) %>%
  summarise(mean_dur = mean(dur),
            sd_dur = sd(dur))

adjectives_m.regr %>%
  group_by(RECORDING_SESSION_LABEL, type) %>%
  summarise(dur = mean(IA_REGRESSION_IN)) %>%
  group_by(type) %>%
  summarise(mean_dur = mean(dur),
            sd_dur = sd(dur))

adjectives_b.ffd %>%
  group_by(RECORDING_SESSION_LABEL, type) %>%
  summarise(dur = mean(IA_FIRST_FIXATION_DURATION)) %>%
  group_by(type) %>%
  summarise(mean_dur = mean(dur),
            sd_dur = sd(dur))

adjectives_b.gd %>%
  group_by(RECORDING_SESSION_LABEL, type) %>%
  summarise(dur = mean(IA_FIRST_RUN_DWELL_TIME)) %>%
  group_by(type) %>%
  summarise(mean_dur = mean(dur),
            sd_dur = sd(dur))

adjectives_b.tt %>%
  group_by(RECORDING_SESSION_LABEL, type) %>%
  summarise(dur = mean(IA_DWELL_TIME)) %>%
  group_by(type) %>%
  summarise(mean_dur = mean(dur),
            sd_dur = sd(dur))

adjectives_b.regr %>%
  group_by(RECORDING_SESSION_LABEL, type) %>%
  summarise(dur = mean(IA_REGRESSION_IN)) %>%
  group_by(type) %>%
  summarise(mean_dur = mean(dur),
            sd_dur = sd(dur))

nouns_m.ffd %>%
  group_by(RECORDING_SESSION_LABEL, type) %>%
  summarise(dur = mean(IA_FIRST_FIXATION_DURATION)) %>%
  group_by(type) %>%
  summarise(mean_dur = mean(dur),
            sd_dur = sd(dur))

nouns_m.gd %>%
  group_by(RECORDING_SESSION_LABEL, type) %>%
  summarise(dur = mean(IA_FIRST_RUN_DWELL_TIME)) %>%
  group_by(type) %>%
  summarise(mean_dur = mean(dur),
            sd_dur = sd(dur))

nouns_m.tt %>%
  group_by(RECORDING_SESSION_LABEL, type) %>%
  summarise(dur = mean(IA_DWELL_TIME)) %>%
  group_by(type) %>%
  summarise(mean_dur = mean(dur),
            sd_dur = sd(dur))

nouns_m.regr %>%
  group_by(RECORDING_SESSION_LABEL, type) %>%
  summarise(dur = mean(IA_REGRESSION_IN)) %>%
  group_by(type) %>%
  summarise(mean_dur = mean(dur),
            sd_dur = sd(dur))

nouns_b.ffd %>%
  group_by(RECORDING_SESSION_LABEL, type) %>%
  summarise(dur = mean(IA_FIRST_FIXATION_DURATION)) %>%
  group_by(type) %>%
  summarise(mean_dur = mean(dur),
            sd_dur = sd(dur))

nouns_b.gd %>%
  group_by(RECORDING_SESSION_LABEL, type) %>%
  summarise(dur = mean(IA_FIRST_RUN_DWELL_TIME)) %>%
  group_by(type) %>%
  summarise(mean_dur = mean(dur),
            sd_dur = sd(dur))

nouns_b.tt %>%
  group_by(RECORDING_SESSION_LABEL, type) %>%
  summarise(dur = mean(IA_DWELL_TIME)) %>%
  group_by(type) %>%
  summarise(mean_dur = mean(dur),
            sd_dur = sd(dur))

nouns_b.regr %>%
  group_by(RECORDING_SESSION_LABEL, type) %>%
  summarise(dur = mean(IA_REGRESSION_IN)) %>%
  group_by(type) %>%
  summarise(mean_dur = mean(dur),
            sd_dur = sd(dur))

disambg_m.ffd %>%
  group_by(RECORDING_SESSION_LABEL, type) %>%
  summarise(dur = mean(IA_FIRST_FIXATION_DURATION)) %>%
  group_by(type) %>%
  summarise(mean_dur = mean(dur),
            sd_dur = sd(dur))

disambg_m.gd %>%
  group_by(RECORDING_SESSION_LABEL, type) %>%
  summarise(dur = mean(IA_FIRST_RUN_DWELL_TIME)) %>%
  group_by(type) %>%
  summarise(mean_dur = mean(dur),
            sd_dur = sd(dur))

disambg_m.tt %>%
  group_by(RECORDING_SESSION_LABEL, type) %>%
  summarise(dur = mean(IA_DWELL_TIME)) %>%
  group_by(type) %>%
  summarise(mean_dur = mean(dur),
            sd_dur = sd(dur))

disambg_b.ffd %>%
  group_by(RECORDING_SESSION_LABEL, type) %>%
  summarise(dur = mean(IA_FIRST_FIXATION_DURATION)) %>%
  group_by(type) %>%
  summarise(mean_dur = mean(dur),
            sd_dur = sd(dur))

disambg_b.gd %>%
  group_by(RECORDING_SESSION_LABEL, type) %>%
  summarise(dur = mean(IA_FIRST_RUN_DWELL_TIME)) %>%
  group_by(type) %>%
  summarise(mean_dur = mean(dur),
            sd_dur = sd(dur))

disambg_b.tt %>%
  group_by(RECORDING_SESSION_LABEL, type) %>%
  summarise(dur = mean(IA_DWELL_TIME)) %>%
  group_by(type) %>%
  summarise(mean_dur = mean(dur),
            sd_dur = sd(dur))
#######Построение моделей#########
mod.adjectives_m.ffd <- lmer(log(IA_FIRST_FIXATION_DURATION) ~ type + 
                               (1|RECORDING_SESSION_LABEL) + (1|IA_LABEL), 
                             data = adjectives_m.ffd, REML=FALSE,
                             control = lmerControl(optimizer = "bobyqa"))
summary(mod.adjectives_m.ffd)


mod.adjectives_m.gd <- lmer(log(IA_FIRST_RUN_DWELL_TIME) ~ type + 
                              (1|RECORDING_SESSION_LABEL) + (1|IA_LABEL) , 
                            data = adjectives_m.gd,
                            REML = FALSE,
                            control = lmerControl(optimizer = "bobyqa"))
mod.adjectives_m.tt <- lmer(log(IA_DWELL_TIME) ~ type + 
                              (1|RECORDING_SESSION_LABEL) + (1|IA_LABEL) , 
                            data = adjectives_m.tt,
                            REML = FALSE,
                            control = lmerControl(optimizer = "bobyqa"))
summary(mod.adjectives_m.tt)


adjectives_m$IA_REGRESSION_IN <- as.numeric(adjectives_m$IA_REGRESSION_IN)
mod.adjectives_m.regr <- glmer(IA_REGRESSION_IN ~ type + 
                                 (1|RECORDING_SESSION_LABEL) + (1|IA_LABEL),  
                               data = adjectives_m.regr, family = binomial,
                               control = glmerControl(optimizer = "bobyqa"))
summary(mod.adjectives_m.regr)


mod.adjectives_b.ffd <- lmer(log(IA_FIRST_FIXATION_DURATION) ~ type + 
                               (1|RECORDING_SESSION_LABEL) + (1|IA_LABEL) , 
                             data = adjectives_b.ffd, REML=FALSE,
                             control = lmerControl(optimizer = "bobyqa"))
summary(mod.adjectives_b.ffd)


mod.adjectives_b.gd <- lmer(log(IA_FIRST_RUN_DWELL_TIME) ~ type + 
                              (1|RECORDING_SESSION_LABEL) + (1|IA_LABEL) , 
                            data = adjectives_b.gd,
                            REML = FALSE,
                            control = lmerControl(optimizer = "bobyqa"))
summary(mod.adjectives_b.gd)


adjectives_b$IA_REGRESSION_IN <- as.numeric(adjectives_b$IA_REGRESSION_IN)
mod.adjectives_b.tt <- lmer(log(IA_DWELL_TIME) ~ type + 
                              (1|RECORDING_SESSION_LABEL) + (1|IA_LABEL) , 
                            data = adjectives_b.tt,
                            REML = FALSE,
                            control = lmerControl(optimizer = "bobyqa"))
summary(mod.adjectives_b.tt)


mod.adjectives_b.regr <- glmer(IA_REGRESSION_IN ~ type + 
                                 (1|RECORDING_SESSION_LABEL) + (1|IA_LABEL) , 
                               data = adjectives_b.regr, family = binomial,
                               control = glmerControl(optimizer = "bobyqa"))
summary(mod.adjectives_b.regr)


mod.nouns_m.ffd <- lmer(log(IA_FIRST_FIXATION_DURATION) ~ type + 
                          (1|RECORDING_SESSION_LABEL) + (1|IA_LABEL)   , 
                        data = nouns_m.ffd, REML=FALSE,
                        control = lmerControl(optimizer = "bobyqa"))
summary(mod.nouns_m.ffd)


mod.nouns_m.gd <- lmer(log(IA_FIRST_RUN_DWELL_TIME) ~ type + 
                         (1|RECORDING_SESSION_LABEL) + (1|IA_LABEL)  , 
                       data = nouns_m.gd,
                       REML = FALSE,
                       control = lmerControl(optimizer = "bobyqa"))
summary(mod.nouns_m.gd)


mod.nouns_m.tt <- lmer(log(IA_DWELL_TIME) ~ type + 
                         (1|RECORDING_SESSION_LABEL) + (1|IA_LABEL)   , 
                       data = nouns_m.tt,
                       REML = FALSE,
                       control = lmerControl(optimizer = "bobyqa"))
summary(mod.nouns_m.tt)


nouns_m$IA_REGRESSION_IN <- as.numeric(nouns_m$IA_REGRESSION_IN)
mod.nouns_m.regr <- glmer(IA_REGRESSION_IN ~ type + 
                            (1|RECORDING_SESSION_LABEL) + (1|IA_LABEL), 
                          data = nouns_m.regr, family = binomial,
                          control = glmerControl(optimizer = "bobyqa"))
summary(mod.nouns_m.regr)


mod.nouns_b.ffd <- lmer(log(IA_FIRST_FIXATION_DURATION) ~ type + 
                          (1|RECORDING_SESSION_LABEL) + (1|IA_LABEL)   , 
                        data = nouns_b.ffd, REML=FALSE,
                        control = lmerControl(optimizer = "bobyqa"))
summary(mod.nouns_b.ffd)


mod.nouns_b.gd <- lmer(log(IA_FIRST_RUN_DWELL_TIME) ~ type + 
                         (1|RECORDING_SESSION_LABEL) + (1|IA_LABEL)   , 
                       data = nouns_b.gd,
                       REML = FALSE,
                       control = lmerControl(optimizer = "bobyqa"))
summary(mod.nouns_b.gd)


mod.nouns_b.tt <- lmer(log(IA_DWELL_TIME) ~ type + 
                         (1|RECORDING_SESSION_LABEL) + (1|IA_LABEL)   , 
                       data = nouns_b.tt,
                       REML = FALSE,
                       control = lmerControl(optimizer = "bobyqa"))
summary(mod.nouns_b.tt)


nouns_b$IA_REGRESSION_IN <- as.numeric(nouns_b$IA_REGRESSION_IN)
mod.nouns_b.regr <- glmer(IA_REGRESSION_IN ~ type + 
                            (1|RECORDING_SESSION_LABEL) + (1|IA_LABEL)  , 
                          data = nouns_b.regr, family = binomial,
                          control = glmerControl(optimizer = "bobyqa"))
summary(mod.nouns_b.regr)


mod.disambg_m.ffd <- lmer(log(IA_FIRST_FIXATION_DURATION) ~ type + 
                            (1|RECORDING_SESSION_LABEL) + (1|IA_LABEL)   , 
                          data = disambg_m.ffd, REML=FALSE,
                          control = lmerControl(optimizer = "bobyqa"))
summary(mod.disambg_m.ffd)


mod.disambg_m.gd <- lmer(log(IA_FIRST_RUN_DWELL_TIME) ~ type + 
                           (1|RECORDING_SESSION_LABEL) + (1|IA_LABEL)   , 
                         data = disambg_m.gd,
                         REML = FALSE,
                         control = lmerControl(optimizer = "bobyqa"))
summary(mod.disambg_m.gd)


mod.disambg_m.tt <- lmer(log(IA_DWELL_TIME) ~ type + 
                           (1|RECORDING_SESSION_LABEL) + (1|IA_LABEL)   , 
                         data = disambg_m.tt,
                         REML = FALSE,
                         control = lmerControl(optimizer = "bobyqa"))
summary(mod.disambg_m.tt)


mod.disambg_b.ffd <- lmer(log(IA_FIRST_FIXATION_DURATION) ~ type + 
                            (1|RECORDING_SESSION_LABEL) + (1|IA_LABEL)   , 
                          data = disambg_b.ffd, REML=FALSE,
                          control = lmerControl(optimizer = "bobyqa"))
summary(mod.disambg_b.ffd)


mod.disambg_b.gd <- lmer(log(IA_FIRST_RUN_DWELL_TIME) ~ type + 
                           (1|RECORDING_SESSION_LABEL) + (1|IA_LABEL)   , 
                         data = disambg_b.gd,
                         REML = FALSE,
                         control = lmerControl(optimizer = "bobyqa"))
summary(mod.disambg_b.gd)


mod.disambg_b.tt <- lmer(log(IA_DWELL_TIME) ~ type + 
                           (1|RECORDING_SESSION_LABEL) + (1|IA_LABEL) , 
                         data = disambg_b.tt,
                         REML = FALSE,
                         control = lmerControl(optimizer = "bobyqa"))
summary(mod.disambg_b.tt)

#######Построение таблиц с множественным сравнением#########
library(lme4)
library(sjmisc)
library(sjPlot)
#Буквальное-метафора
tab_model(mod.adjectives_m.ffd, mod.adjectives_m.gd, mod.adjectives_m.tt, mod.adjectives_m.regr,
          mod.nouns_m.ffd, mod.nouns_m.gd, mod.nouns_m.tt, mod.nouns_m.regr,
          mod.disambg_m.ffd, mod.disambg_m.gd, mod.disambg_m.tt, p.adjust="bonferroni",
          string.se = "SE",digits = 3, show.ci = FALSE, show.icc = FALSE,
          show.est = TRUE, show.se = TRUE, 
          show.p = TRUE, dv.labels = c("ADJ Log FFD", "ADJ Log GD", "ADJ Log TT", "ADJ REGR",
                                       "N Log FFD", "N Log GD", "N Log TT", "N REGR",
                                       "D Log FFD", "D Log GD", "D Log TT"))
#Буквальное-бличинг
tab_model(mod.adjectives_b.ffd, mod.adjectives_b.gd, mod.adjectives_b.tt, mod.adjectives_b.regr,
          mod.nouns_b.ffd, mod.nouns_b.gd, mod.nouns_b.tt, mod.nouns_b.regr,
          mod.disambg_b.ffd, mod.disambg_b.gd, mod.disambg_b.tt,
          string.se = "SE",digits = 3, show.ci = FALSE, show.icc = FALSE, p.adjust="bonferroni",
          show.est = TRUE, show.se = TRUE, 
          show.p = TRUE, dv.labels = c("ADJ Log FFD", "ADJ Log GD", "ADJ Log TT", "ADJ REGR",
                                       "N Log FFD", "N Log GD", "N Log TT", "N REGR",
                                       "D Log FFD", "D Log GD", "D Log TT"))

