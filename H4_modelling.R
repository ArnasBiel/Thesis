library(readxl)
library(lme4)
library(lmerTest)
library(ggeffects)
library(emmeans)
library(tidyverse)  # includes purrr, dplyr, ggplot2, etc.

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


llm_data$ContextLevel <- factor(llm_data$Context, levels = c(1, 2, 3), labels= c("Low", "Medium","High"))

names(llm_data)[names(llm_data) == "Selected word"] <- "Selected_word"
names(llm_data)[names(llm_data) == "AvgCohesion"] <- "AvgCoherence"

######################################################
## Data exploration
######################################################

# 1.2 Check distribution of key variables by source
aggregate(Confidence ~ Source, data = full_data, FUN = function(x) c(mean = mean(x), sd = sd(x)))
aggregate(InterpretationScore ~ Source, data = full_data, FUN = function(x) c(mean = mean(x), sd = sd(x)))

# 1.3 Check correlation between confidence and interpretation score by source
correlation_human <- cor.test(human_data$Confidence, human_data$InterpretationScore)
correlation_llm <- cor.test(llm_data$Confidence, llm_data$InterpretationScore)
print(correlation_human)
print(correlation_llm)

# 1.4 Visualize key variables
library(ggplot2)

# Confidence distributions by source
ggplot(full_data, aes(x = Confidence, fill = Source)) +
  geom_density(alpha = 0.5) +
  scale_fill_manual(values = c("Human" = "#E76F51", "LLM" = "#2A9D8F")) +
  labs(title = "Distribution of Confidence Ratings", x = "Confidence", y = "Density") +
  theme_minimal(base_size = 14)

# Interpretation score distributions by source
ggplot(full_data, aes(x = InterpretationScore, fill = Source)) +
  geom_density(alpha = 0.5) +
  scale_fill_manual(values = c("Human" = "#E76F51", "LLM" = "#2A9D8F")) +
  labs(title = "Distribution of Interpretation Scores", x = "Interpretation Score", y = "Density") +
  theme_minimal(base_size = 14)

######################################################
# Hypothesis 4 modelling
######################################################

# Main model
main_model <- lmer(Confidence ~ Source * InterpretationScore + (1 | Participant_ID) + (1 | Selected_word), data = full_data)
summary(main_model)

# 3.1 Compute mean confidence for each group
means <- aggregate(Confidence ~ Source, data = full_data, mean)

# 3.2 Create the main plot with trend lines and raw data
main_plot <- ggplot(full_data, aes(x = InterpretationScore, y = Confidence, color = Source)) +
  geom_point(alpha = 0.2, size = 1.5) +  # raw data points
  geom_smooth(method = "lm", formula = y ~ x, se = TRUE, linewidth = 1.45) +  # fitted trend
  geom_hline(data = means, aes(yintercept = Confidence, color = Source), 
             linetype = "dashed", size = 0.8, show.legend = FALSE) +  # mean confidence lines
  scale_color_manual(values = c("Human" = "#E76F51", "LLM" = "#2A9D8F")) +
  labs(
    #title = "Confidence as a Function of Interpretation Score",
    x = "Interpretation Score",
    y = "Self-Rated Confidence"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    panel.grid.major = element_line(color = "gray90", size = 0.5),
    panel.grid.minor = element_line(color = "gray95", size = 0.25)
  )

main_plot

######################################################
# Confidence vs. AvgCoherence modelling
######################################################


# Model 1: Coherence predicts Interpretation Score
model_coherence <- lmer(InterpretationScore ~ AvgCoherence + (1 | Participant_ID) + (1 | Selected_word : ContextLevel), data = llm_data)
summary(model_coherence)

# Model 2: Confidence predicts Interpretation Score
model_confidence <- lmer(InterpretationScore ~ Confidence + (1 | Participant_ID) + (1 | Selected_word : ContextLevel), data = llm_data)
summary(model_confidence)
# Combined model
interaction_model <- lmer(InterpretationScore ~ Confidence * AvgCoherence + (1 | Participant_ID) + (1 | Selected_word : ContextLevel), data = llm_data)
summary(interaction_model)

ggplot(llm_data, aes(x = Confidence, y = InterpretationScore, color = AvgCoherence)) +
  geom_jitter(alpha = 0.6, width = 0.1, height = 0.1, size = 1.5) +  # Added jitter for clarity
  geom_smooth(method = "lm", se = TRUE) +
  labs(
    #title = "Interaction between Confidence and AvgCoherence in Predicting InterpretationScore",
    x = "LLM Confidence",
    y = "Interpretation Score"
  ) +
  theme_minimal(base_size = 14) +
  scale_color_viridis_c(name = "Avg Coherence", option = "D") +
  theme(
    panel.grid.major = element_line(color = "gray90", size = 0.5),
    panel.grid.minor = element_line(color = "gray95", size = 0.25)
  )


# Graph using Coherence as X axis with ggeffects

pred_data <- ggeffects::ggpredict(
  interaction_model,
  terms = c("AvgCoherence[all]", "Confidence[3:6]", "Participant_ID", "Selected_word:ContextLevel"), #using only the interval 3-6 for Confidence as there is a single data point at level 2 and bellow
  type = "random"
  )

ggplot(pred_data, aes(x = x, y = predicted, color = group, group = group)) +
  geom_line(linewidth = 1.2) +
  labs(
    x = "Average Coherence",
    y = "Predicted Interpretation Score",
    color = "LLM Confidence"
  ) +
  theme_minimal(base_size = 14) +
  scale_color_viridis_d(option = "D") +
  theme(
    panel.grid.major = element_line(color = "gray90", size = 0.5),
    panel.grid.minor = element_line(color = "gray95", size = 0.25),
    legend.position = "top"
  )


# Plot 1: InterpretationScore vs AvgCoherence
ggplot(llm_data, aes(x = AvgCoherence, y = InterpretationScore)) +
  geom_jitter(alpha = 0.4, width = 0.02, height = 0.1, size = 1.5, color = "#2A9D8F") +
  geom_smooth(method = "lm", se = TRUE, color = "#E76F51", linewidth = 1) +
  labs(
    #title = "Interpretation Quality vs Internal Coherence",
    x = "Average Coherence Between LLM Interpretations",
    y = "Interpretation Score"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    panel.grid.major = element_line(color = "gray90", size = 0.5),
    panel.grid.minor = element_line(color = "gray95", size = 0.25)
  )

# Plot 2: InterpretationScore vs Confidence
ggplot(llm_data, aes(x = Confidence, y = InterpretationScore)) +
  geom_jitter(alpha = 0.4, width = 0.1, height = 0.1, size = 1.5, color = "#2A9D8F") +
  geom_smooth(method = "lm", se = TRUE, color = "#E76F51", linewidth = 1) +
  labs(
    #title = "Interpretation Quality vs Self-Rated Confidence",
    x = "LLM Self-Rated Confidence",
    y = "Interpretation Score"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    panel.grid.major = element_line(color = "gray90", size = 0.5),
    panel.grid.minor = element_line(color = "gray95", size = 0.25)
  )

######################################################
# Comparing models
######################################################
AIC(model_coherence)
AIC(model_confidence)
AIC(interaction_model)

BIC(model_coherence)
BIC(model_confidence)
BIC(interaction_model)

anova(model_coherence, interaction_model)
anova(model_confidence, interaction_model)

######################################################
# Interaction between coherence and confidence
######################################################

# Fit the model
h4_llm_model <- lmer(Confidence ~ AvgCoherence + (1 | Participant_ID) + (1 | Selected_word : ContextLevel), data = llm_data)
summary(h4_llm_model)


# Plotting Confidence vs AvgCoherence, showing multiple Confidence values for each AvgCoherence
ggplot(llm_data, aes(x = AvgCoherence, y = Confidence)) +
  geom_jitter(alpha = 0.5, width = 0.02, size = 1.5, color = "blue") +  # jitter to show individual points
  geom_smooth(method = "lm", se = TRUE, color = "red", size = 1) +  # smoothed trend line
  labs(
    #title = "Confidence as a Function of AvgCoherence for LLMs",
    x = "AvgCoherence (Internal Consistency)",
    y = "Confidence"
  ) +
  theme_minimal(base_size = 14) +
  theme(legend.position = "none") + 
  theme(
    panel.grid.major = element_line(color = "gray90", size = 0.5),
    panel.grid.minor = element_line(color = "gray95", size = 0.25)
  )
