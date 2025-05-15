library(readxl)
library(lme4)
library(lmerTest)
library(ggeffects)
library(emmeans)
library(tidyverse)
library(dplyr)
library(ggplot2)

# Read the data files for both humans and LLMs
human_data <- read_excel("C:/Users/User/Desktop/Thesis/Data/final_human_responses_with_scores.xlsx")
llm_data <- read_excel("C:/Users/User/Desktop/Thesis/Data/final_llm_responses_with_scores_and_cohesion_and_correct IDs_cleared.xlsx")

# Add a Source column which will be used in mixed-effects modelling
human_data$Source <- "Human"
llm_data$Source <- "LLM"

names(human_data)[names(human_data) == "Participant ID"] <- "Participant_ID"
names(llm_data)[names(llm_data) == "LLM_ID"] <- "Participant_ID"

# Align columns
human_data$AvgCohesion <- NA

# head(human_data)
head(llm_data)

human_data$Participant_ID <- as.factor(human_data$Participant_ID)

full_data <- bind_rows(human_data, llm_data)

full_data$ContextLevel <- factor(full_data$Context, levels = c(1, 2, 3), labels= c("Low", "Medium","High"))

names(full_data)[names(full_data) == "Selected word"] <- "Selected_word"
names(full_data)[names(full_data) == "AvgCohesion"] <- "AvgCoherence"

# Convert human Participant_IDs to numeric
full_data$Participant_ID[full_data$Source == "Human"] <- as.factor(full_data$Participant_ID[full_data$Source == "Human"])

# Convert Participant_ID to a factor
full_data$Participant_ID <- as.factor(full_data$Participant_ID)


# To evaluate Hypotheses 1–3 and 5, a series of increasingly complex linear mixed-effects models were fitted

#######################################
# First model
#######################################

model1 <- lmer(InterpretationScore ~ Source + (1|Participant_ID) + (1|Selected_word), data = full_data)
summary(model1)


#######################################
# Second model
#######################################

model2 <- lmer(InterpretationScore ~ Source + ContextLevel + (1|Participant_ID) + (1|Selected_word), data = full_data)
summary(model2)

#######################################
# Third model
#######################################

model3 <- lmer(InterpretationScore ~ Source + ContextLevel + Type + (1|Participant_ID) + (1|Selected_word), data = full_data)
summary(model3)


#######################################
# Fourth model
#######################################

model4 <- lmer(InterpretationScore ~ Source * ContextLevel + Source * Type + ContextLevel * Type + (1|Participant_ID) + (1|Selected_word), data = full_data)
summary(model4)


#######################################
# Fifth model
#######################################

model5 <- lmer(InterpretationScore ~ Source * ContextLevel * Type + (1|Participant_ID) + (1|Selected_word), data = full_data)
summary(model5)


#######################################
# ANOVA between models
#######################################
anova(model1, model2)
anova(model2, model3)
anova(model3, model4)
anova(model4, model5)

#######################################
# Changes in Coefficients Across Models
#######################################
model_list <- list(Model1 = model1, Model2 = model2, Model3 = model3, Model4 = model4) 


library(broom.mixed)

coef_table <- map_dfr(model_list, ~ tidy(.x, effects = "fixed", conf.int = TRUE), .id = "Model")
print(coef_table, n=33)

#######################################
# Check for Collinearity (variance inflation factor)
#######################################
library(car)
vif_model4 <- vif(model4)

# Print the results
print(vif_model4)

#######################################
# Plots for the hypothesise using model 4
#######################################

dir.create("C:/Users/User/Desktop/Thesis/Thesis Coding/figures", showWarnings = FALSE)

# Define a standard size for all your figures
fig_width <- 7  # width in inches
fig_height <- 5  # height in inches
fig_dpi <- 600   # resolution


#########################################################
# Plot for H1
# Compute estimated marginal means
emm1 <- emmeans(model4, ~ Source)
emm_df1 <- as.data.frame(emm1)

# Plot
p1 <- ggplot(emm_df1, aes(x = Source, y = emmean, fill = Source)) +
  geom_col(width = 0.6) +
  geom_errorbar(
    aes(ymin = lower.CL, ymax = upper.CL), width = 0.2) +
    theme_minimal(base_size = 14) +
    scale_fill_manual(values = c("Human" = "#E76F51", "LLM" = "#2A9D8F")) +
  labs(
    #title = "Estimated Interpretation Scores by Source",
    x = "Source",
    y = "Estimated Mean Interpretation Score",
    fill = "Source"
  ) +
  # Add this line to ensure white background (Only useful when downloading automatically)
  theme(panel.background = element_rect(fill = "white", color = NA),
        plot.background = element_rect(fill = "white", color = NA))

#########################################################
# Plot for H2, H3
# Compute estimated marginal means
emm2 <- emmeans(model4, ~ Source * ContextLevel)
emm_df2 <- as.data.frame(emm2)

# Plot
p2 <- ggplot(emm_df2, aes(x = ContextLevel, y = emmean, fill = Source)) +
  geom_col(position = position_dodge(0.8), width = 0.75) +
  geom_errorbar(
    aes(ymin = lower.CL, ymax = upper.CL),
    position = position_dodge(0.8),
    width = 0.2
  ) +
  scale_fill_manual(values = c("Human" = "#E76F51", "LLM" = "#2A9D8F")) +
  theme_minimal(base_size = 14) +
  labs(
    #title = "Estimated Interpretation Scores by Source and Context Level",
    x = "Context Level",
    y = "Estimated Mean Interpretation Score",
    fill = "Source"
  ) +
  theme(panel.background = element_rect(fill = "white", color = NA),
        plot.background = element_rect(fill = "white", color = NA))

#########################################################
# Plot for the combination of H1-H3 and H5
# Compute estimated marginal means
emm4 <- emmeans(model4, ~ Source * ContextLevel | Type)
emm_df3<- as.data.frame(emm4)


# Plot
p3 <- ggplot(emm_df3, aes(x = ContextLevel, y = emmean, fill = Source)) +
  geom_col(position = position_dodge(1), width = 0.9) +
  geom_errorbar(
    aes(ymin = lower.CL, ymax = upper.CL),
    width = 0.2,
    position = position_dodge(1)
  ) +
  scale_fill_manual(values = c("Human" = "#E76F51", "LLM" = "#2A9D8F")) +
  facet_wrap(~ Type) +
  theme_minimal(base_size = 14) +
  coord_cartesian(ylim = c(0.2, NA)) +
  labs(
    #title = "Estimated Interpretation Scores by Source and Context Level"
    x     = "Context Level",
    y     = "Estimated Mean Interpretation Score",
    fill  = "Source"
  ) +
  theme(panel.background = element_rect(fill = "white", color = NA),
        plot.background = element_rect(fill = "white", color = NA))

#########################################################
# Plot only for H5 using Type
# Get estimated marginal means for Source × Type
emm_type_source <- emmeans(model4, ~ Source * Type)
emm_df4 <- as.data.frame(emm_type_source)

# Plot
p4 <- ggplot(emm_df4, aes(x = Type, y = emmean, fill = Source)) +
  geom_col(position = position_dodge(0.8), width = 0.7) +
  geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL),
                width = 0.2,
                position = position_dodge(0.8)) +
  labs(
      #title = "Estimated Interpretation Scores by Source and Word Type",
       x = "Word Type",
       y = "Estimated Mean Interpretation Score") +
  theme_minimal() +
  scale_fill_manual(values = c("Human" = "#E76F51", "LLM" = "#2A9D8F")) +
  # Add this line to ensure white background
  theme(panel.background = element_rect(fill = "white", color = NA),
        plot.background = element_rect(fill = "white", color = NA))



# Save the plots
ggsave(filename = "C:/Users/User/Desktop/Thesis/Thesis Coding/figures/h1_source.png", plot = p1, 
       width = fig_width, height = fig_height, dpi = fig_dpi)
ggsave(filename = "C:/Users/User/Desktop/Thesis/Thesis Coding/figures/h2_context.png", plot = p2, 
       width = fig_width, height = fig_height, dpi = fig_dpi)
ggsave(filename = "C:/Users/User/Desktop/Thesis/Thesis Coding/figures/h5_type.png", plot = p4, 
       width = fig_width, height = fig_height, dpi = fig_dpi)
ggsave(filename = "C:/Users/User/Desktop/Thesis/Thesis Coding/figures/all.png", plot = p3, 
       width = 9, height = fig_height, dpi = fig_dpi)


# show graphs
p1
p2
p3
p4



#######################################
# Coefficients table
#######################################
install.packages("xtable")  # if not already installed
library(xtable)
coef_table_rounded <- coef_table
coef_table_rounded$estimate <- round(coef_table_rounded$estimate, 3)
coef_table_rounded$std.error <- round(coef_table_rounded$std.error, 3)
coef_table_rounded$statistic <- round(coef_table_rounded$statistic, 2)
coef_table_rounded$p.value <- signif(coef_table_rounded$p.value, 2)
coef_table_rounded$conf.low <- round(coef_table_rounded$conf.low, 3)
coef_table_rounded$conf.high <- round(coef_table_rounded$conf.high, 3)

# Generate LaTeX code
xtable_output <- xtable(coef_table_rounded)
print(xtable_output, include.rownames = FALSE)