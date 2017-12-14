library(dplyr)
library(ggplot2)
results <- read.csv("D:/OneDrive/Documents/2017-2018/Sociolinguistics/quest2.csv", sep=",", header = TRUE, encoding = "UTF-8")

summary (results)
head(results)

#посмотрим, есть ли корреляция с полом
results %>%
  filter(Gender=="Женский") %>%
  select(6,7,8)-> fem_df
results %>%
  filter(Gender=="Мужской") %>%
  select(6,7,8)-> mal_df
head(fem_df)
#print(mal_df[,3])
female_ans <- c(fem_df[,1], fem_df[,2], fem_df[,3])
male_ans <- c(mal_df[,1], mal_df[,2], mal_df[,3])
#print(female_ans)
#грубо - 1, нормально - 2, фамильярно - 3
yes <- 0
doubt <- 0
no <- 0
for (i in 1:length(female_ans)){
  if (female_ans[i] == 2) {
    yes <- yes + 1
  } else if(female_ans[i] == 3){
    doubt <- doubt + 1
  } else {no <- no + 1}
}
matr <- c(yes, doubt, no)
print(matr)
yes <- 0
doubt <- 0
no <- 0
for (i in 1:length(male_ans)){
  if (male_ans[i] == 2) {
    yes <- yes + 1
  } else if(male_ans[i] == 3){
    doubt <- doubt + 1
  } else {no <- no + 1}
}
matr <- c(matr, yes, doubt, no)
print(matr)
sex_answers <- matrix(matr,
                      nrow = 3,
                      ncol = 2)
print(sex_answers)
chisq.test(sex_answers)
#Зависимости от пола нет, p-value = 0.2982

#Посмотрим, есть ли корреляция с уровнем образования
results %>%
  filter(Education == "незаконченное среднее (=школьник)" | Education == "среднее" | Education == "среднее специальное") %>%
  select(6,7,8)-> school_df

results %>%
  filter(Education == "высшее" | Education == "неоконченное высшее") %>%
  select(6,7,8)-> high_df
summary(school_df)
summary(high_df)

school_ans <- c(school_df[,1], school_df[,2], school_df[,3])
high_ans <- c(high_df[,1], high_df[,2], high_df[,3])
head (school_ans)
head(school_df)
#грубо - 1, нормально - 2, фамильярно - 3
yes <- 0
doubt <- 0
no <- 0
for (i in 1:length(school_ans)){
  if (school_ans[i] == 2) {
    yes <- yes + 1
  } else if(school_ans[i] == 3){
    doubt <- doubt + 1
  } else {no <- no + 1}
}
matr <- c(yes, doubt, no)

yes <- 0
doubt <- 0
no <- 0
for (i in 1:length(high_ans)){
  if (high_ans[i] == 2) {
    yes <- yes + 1
  } else if(high_ans[i] == 3){
    doubt <- doubt + 1
  } else {no <- no + 1}
}
matr <- c(matr, yes, doubt, no)

education_answers <- matrix(matr,
                            nrow = 3,
                            ncol = 2)
print(education_answers)
chisq.test(education_answers)
#Зависимости от уровня образования нет, p-value = 0.5272

#Посмотрим, есть корреляция с возрастом
results %>%
  filter(Age < 19) %>%
  select(6,7,8)-> young_df
print(young_df)
results %>%
  filter(Age >= 19) %>%
  filter(Age < 26) %>%
  select(6,7,8)-> middle_df
print(middle_df)
results %>%
  filter(Age >= 26) %>%
  select(6,7,8)-> old_df
print(old_df)
young_ans <- c(young_df[,1], young_df[,2], young_df[,3])
middle_ans <- c(middle_df[,1], middle_df[,2], middle_df[,3])
old_ans <- c(old_df[,1], old_df[,2], old_df[,3])

#грубо - 1, нормально - 2, фамильярно - 3
yes <- 0
doubt <- 0
no <- 0
for (i in 1:length(young_ans)){
  if (young_ans[i] == 2) {
    yes <- yes + 1
  } else if(young_ans[i] == 3){
    doubt <- doubt + 1
  } else {no <- no + 1}
}
matr <- c(yes, doubt, no)

yes <- 0
doubt <- 0
no <- 0
for (i in 1:length(middle_ans)){
  if (middle_ans[i] == 2) {
    yes <- yes + 1
  } else if(middle_ans[i] == 3){
    doubt <- doubt + 1
  } else {no <- no + 1}
}
matr <- c(matr, yes, doubt, no)

yes <- 0
doubt <- 0
no <- 0
for (i in 1:length(old_ans)){
  if (old_ans[i] == 2) {
    yes <- yes + 1
  } else if(old_ans[i] == 3){
    doubt <- doubt + 1
  } else {no <- no + 1}
}
matr <- c(matr, yes, doubt, no)

age_answers <- matrix(matr,
                      nrow = 3,
                      ncol = 3)
print(age_answers)
chisq.test(age_answers)
#Зависимость есть
#p-value = 3.132e-05
library(reshape2)
longData <- melt(age_answers)
longData %>%
  ggplot(aes(x = Var2, y = value, fill = as.factor(Var1)))+
  geom_bar(stat = "identity", position = "dodge")+
  labs(title = "Зависимость ответов от возраста респондентов",
       y = "count",
       x = "age",
       fill = "answers")+
  theme_bw()
