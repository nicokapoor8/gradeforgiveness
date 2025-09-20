library(tidyverse)
library(ggplot2)
library(dplyr)
library(tidyr)
library(readr)
library(scales)
library(stringr)
library(plm)
setwd("/Users/nicokapoor/Desktop/R Studio")
gradeforgive <- read_csv("gradeforgive.csv")

#cleaning
gradeforgive <- gradeforgive[-c(1515, 1638, 4501, 4504, 5966), ] %>%
  filter(!is.na(faculty), faculty >= 20) %>%
  mutate(policy = factor(case_when(
      avg == 1 & forgive == 0 ~ "Grade Averaging",
      avg == 0 & forgive == 1 ~ "Grade Forgiveness",
      TRUE                    ~ "Neither"), 
      levels = c("Grade Averaging", "Grade Forgiveness", "Neither")),
    black_grads = gradblack6new / blacks,
    white_grads = gradwhite6new / whites,
    asian_grads = gradasian6new / asian,
    hisp_grads  = gradhisp6new / hispanics,
    public_factor = factor(case_when(
      public == 1 ~ "Public",
      public == 0 ~ "Private",
      TRUE        ~ NA_character_), 
      levels = c("Public", "Private")),
    fac_student_ratio = ifelse(enrolltotal > 0, enrolltotal / faculty, NA_real_)) %>%
  mutate(top_race = c("Black", "White", "Asian", "Hispanic")[
      max.col(cbind(blacks, whites, asian, hispanics), ties.method = "first")],
    top_race = factor(top_race, levels = c("Black", "White", "Asian", "Hispanic")))

#VISUAL1cleaning: create df with avg grad rates for each combo of policy & race
race_policy <- bind_rows(
  gradeforgive %>% transmute(policy, Race = "Black",    rate = black_grads, n = blacks),
  gradeforgive %>% transmute(policy, Race = "White",    rate = white_grads, n = whites),
  gradeforgive %>% transmute(policy, Race = "Asian",    rate = asian_grads, n = asian),
  gradeforgive %>% transmute(policy, Race = "Hispanic", rate = hisp_grads,  n = hispanics)) %>%
  drop_na(policy, rate, n) %>%
  filter(n >= 20) %>% #only counting unis with more than 20 of that student group
  group_by(policy, Race) %>%
  filter(policy != "Neither") %>% #don't want to include neither here, sample size too small
  summarise(avg_rate_pct = mean(rate) * 100, .groups = "drop")
#VISUAL1: avg grad rates for policy/race, one bar per combo of policy/race
ggplot(race_policy, aes(Race, avg_rate_pct, fill = policy)) + 
  geom_col(position = "dodge") +
  geom_text(aes(label = sprintf("%.1f%%", avg_rate_pct)),
            position = position_dodge(width = 0.9),
            angle = 90, hjust = 1.1, vjust = 0.5, color = "black", size = 3)+
  scale_y_continuous(labels = scales::label_number(suffix = "%")) +
  labs(title = "Grad Rates by Race & Policy",
       x = NULL, y = "Average Graduation Rate", fill = "Forgiveness Policy") +
  theme_minimal(base_size = 14) +
  theme(plot.title = element_text(hjust = 0.5))

#VISUAL2cleaning: keep only the columns needed & remove duplicate universities 
visual2_clean <- gradeforgive %>%
  filter(policy %in% c("Grade Averaging", "Grade Forgiveness", "Neither")) %>%
  distinct(unitid, policy, public_factor)
#VISUAL2cleaning: count schools by policy x university_type
policy_uni_count <- visual2_clean %>%
  count(policy, public_factor, name = "count") %>%
  group_by(public_factor) %>%
  mutate(pct_of_type  = round(100 * count / sum(count), 1)) %>%
  ungroup()
#VISUAL2: bar chart visualizing private/public unis vs. policies
ggplot(policy_uni_count, aes(x = public_factor, 
                             y = pct_of_type, 
                             fill = policy)) +
  geom_col(width = 0.6) +
  geom_text(aes(label = paste0(round(pct_of_type,1), "%")),
            position = position_stack(vjust = 0.5), size = 3.5) +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  labs(title = "Policy Distribution by University Type",
       x = "University Type",
       y = "Percentage of Universities",
       fill = "Policy") +
  theme_minimal(base_size = 14) +
  theme(plot.title = element_text(hjust = 0.5))

#VISUAL3: student-faculty ratio vs grad rates by policy graph
gradeforgive %>%
  filter(!is.na(policy), policy != "Neither",
         !is.na(fac_student_ratio), !is.na(grad6per)) %>%
  ggplot(aes(x = fac_student_ratio, y = grad6per, color = policy)) +
  geom_smooth(method = "lm", se = FALSE, size = 1.2) +   
  scale_x_continuous(labels = scales::comma) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(title = "Grad Rate vs. Student-Faculty Ratio",
       x = "Student-Faculty Ratio", y = "Graduation Rate") +
  theme_minimal(base_size = 14) +
  coord_cartesian(ylim = c(0, 1)) +
  theme(plot.title = element_text(hjust = 0.5))

#VISUAL4A: in-state tuition vs grad rates
ggplot(gradeforgive %>% filter(!is.na(policy), policy != "Neither"),
       aes(x = instatetuition, y = grad6per, color = policy)) +
  geom_point(alpha = 0.1, size = 2) +   
  geom_smooth(method = "lm", se = FALSE, size = 2.5, alpha = 2) +  
  scale_y_continuous(labels = scales::percent) +
  scale_x_continuous(labels = scales::dollar_format()) +
  labs(title = "In-State Tuition vs Grad Rate",
       x = "In-State Tuition", y = "Graduation Rate",
       color = "Policy") +
  theme_minimal(base_size = 14) +
  theme(plot.title = element_text(hjust = 0.5)) 

#VISUAL4B: out-of-state tuition vs grad rates
ggplot(gradeforgive %>% filter(!is.na(policy), policy != "Neither"),
       aes(x = outofstatetuition, y = grad6per, color = policy)) +
  geom_point(alpha = 0.1, size = 2) +   
  geom_smooth(method = "lm", se = FALSE, size = 2.5, alpha = 2) +  
  scale_y_continuous(labels = scales::percent,
                     limits = c(0, 1),
                     breaks = seq(0, 1, 0.25)) +
  scale_x_continuous(labels = scales::dollar_format()) +
  labs(title = "Out-of-State Tuition vs Grad Rate",
       x = "Out-of-State Tuition", y = "Graduation Rate") +
  theme_minimal(base_size = 14) +
  theme(plot.title = element_text(hjust = 0.5))

#VISUAL5A: grad rate vs enrollment
ggplot(gradeforgive %>% filter(!is.na(policy), policy != "Neither", enrolltotal >= 100),
      aes(x = enrolltotal, y = grad6per, color = policy)) +
  geom_point(alpha = 0.1, size = 2) +   
  geom_smooth(method = "lm", se = FALSE, size = 2.5, alpha = 2) + 
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_x_continuous(labels = scales::comma) +
  labs(title = "Graduation Rate vs Enrollment",
       x = "Enrollment",
       y = "Graduation Rate",
       color = "Policy") +
  theme_minimal(base_size = 14) +
  theme(plot.title = element_text(hjust = 0.5))

#VISUAL5B: grad rate vs faculty numbers
ggplot(gradeforgive %>% filter(!is.na(policy), policy != "Neither"),
       aes(x = faculty, y = grad6per, color = policy)) +
  geom_point(alpha = 0.1, size = 2) +   
  geom_smooth(method = "lm", se = FALSE, size = 2.5, alpha = 2) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_x_continuous(labels = scales::comma) +
  labs(title = "Graduation Rate vs Faculty",
       x = "Faculty",
       y = "Graduation Rate",
       color = "Policy") +
  theme_minimal(base_size = 14) +
  theme(plot.title = element_text(hjust = 0.5))

#VISUAL6: grad rate vs fed aid numbers
ggplot(gradeforgive %>% filter(!is.na(policy), policy != "Neither"),
       aes(x = fedaid, y = grad6per, color = policy)) +
  geom_point(alpha = 0.1, size = 2) +   
  geom_smooth(method = "lm", se = FALSE, size = 2.5, alpha = 2) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(title = "Grad Rate vs Federal Aid Percentage",
       x = "Federal Aid Percentage",
       y = "Graduation Rate",
       color = "Policy") +
  theme_minimal(base_size = 14) +
  theme(plot.title = element_text(hjust = 0.5))

#calculation: average ratio out-of-state vs in-state
tuition_summary <- gradeforgive %>%
  filter(outofstatetuition != instatetuition,    
         instatetuition > 0, outofstatetuition > 0) %>%  
  mutate(ratio = outofstatetuition / instatetuition) %>%
  summarise(avg_ratio = mean(ratio, na.rm = TRUE))

#calculation: private/public grad rates
public_grad_summary <- gradeforgive %>%
  group_by(public_factor) %>%
  summarise(avg_grad_rate = mean(grad6per, na.rm = TRUE), n = n())

#calculation: private/public student-faculty ratio
public_fac_stu_summary <- gradeforgive %>%
  group_by(public_factor) %>%
  summarise(avg_fac_stu_ratio = mean(fac_student_ratio, na.rm = TRUE), n = n())

#calculation: policy avg grad rates
policy_grad_summary <- gradeforgive %>%
  group_by(policy) %>%
  summarise(avg_grad_rate = mean(grad6per, na.rm = TRUE), n = n())

#calculation: policy and public combo grad rates
policy_public_grad_summary <- gradeforgive %>%
  group_by(policy, public_factor) %>%
  summarise(avg_grad_rate = mean(grad6per, na.rm = TRUE), n = n())

#Regression model: Fit multiple linear regression with the two factors
grad_predict_model <- lm(
  grad6per ~ instatetuition + outofstatetuition + enrolltotal + faculty + fac_student_ratio + fedaid + policy + public_factor + top_race,
  data = gradeforgive)

#Regression model: Get predicted graduation rates in gradeforgive & the difference between the two
gradeforgive$predicted_gradper <- predict(grad_predict_model, newdata = gradeforgive)
gradeforgive <- gradeforgive %>%
  mutate(predict_diff = grad6per - predicted_gradper) 

#statistical importance numbers
summary(grad_predict_model)



