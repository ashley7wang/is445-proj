install.packages(c("ggplot2", "tidyverse", "ggeffects"), dependencies=TRUE)
library(ggplot2)
library(tidyverse)
library(ggeffects)

data <- read.csv("/Users/ashley/Downloads/students_mental_health_survey.csv", stringsAsFactors = FALSE)
data$SleepQual_Score <- as.numeric(
  factor(data$Sleep_Quality,
         levels  = c("Poor", "Average", "Good"),
         ordered = TRUE)
)

set.seed(123)
shapiro <- min(nrow(data), 5000)
sampledata <- data[sample(nrow(data), shapiro), ]
norm <- list(
  Sleep_Quality    = shapiro.test(sampledata$SleepQual_Score),
  Stress_Level     = shapiro.test(sampledata$Stress_Level),
  Anxiety_Score    = shapiro.test(sampledata$Anxiety_Score),
  Depression_Score = shapiro.test(sampledata$Depression_Score)
)
for(name in names(norm)) {
  cat("\n", name, round(norm[[name]]$statistic, 3),
      signif(norm[[name]]$p.value, 3), "\n")
}
newdata <- data %>%
  pivot_longer(
    cols      = c(Stress_Level, Anxiety_Score, Depression_Score),
    names_to  = "Indicator",
    values_to = "Score"
  )

ggplot(newdata, aes(x = factor(SleepQual_Score), y = Score, fill = Indicator)) +
  geom_boxplot(position = position_dodge(width = 0.8), outlier.alpha = 0.4) +
  facet_wrap(~ Indicator, nrow = 1, scales = "free_y") +
  scale_fill_brewer(palette = "Set2") +
  scale_x_discrete(labels = c("1\nPoor", "2\nAverage", "3\nGood")) +
  labs(
    title = "Figure 5. Sleep Quality vs Mental Health Indicators",
    x     = "Sleep Quality (1 = Poor, 2 = Average, 3 = Good)",
    y     = "Score (0–5)",
    fill  = "Indicator"
  ) +
  theme_minimal(base_size = 14) +
  theme(plot.title = element_text(face = "bold", hjust = 0.5), legend.position = "none")
  
anxtest <- cor.test(data$SleepQual_Score, data$Anxiety_Score,  method = "spearman")
anxrho <- anxtest$estimate
anxp <- anxtest$p.value
anxmean <- data %>%
  group_by(SleepQual_Score) %>%
  summarise(mean_Anxiety = mean(Anxiety_Score, na.rm = TRUE))
ggplot(anxmean, aes(x = factor(SleepQual_Score), y = mean_Anxiety)) +
  geom_col() +
  annotate(
    "text",
    x     = 2, 
    y     = max(anx_means$mean_Anxiety) * 1.05,
    label = paste0("ρ = ", round(anxrho, 2),
                   "\np = ", signif(anxp, 2)),
    hjust = 0.5
  ) +
  labs(
    title = "Figure 6. Mean Anxiety Score by Sleep Quality",
    x     = "Sleep Quality (1 = Poor, 2 = Average, 3 = Good)",
    y     = "Mean Anxiety Score"
  ) +
  theme_minimal(base_size = 14)

stresstest <- cor.test(data$SleepQual_Score, data$Stress_Level, method = "spearman")
stressrho <- stresstest$estimate
stressp <- stresstest$p.value
stressmeans <- data %>%
  group_by(SleepQual_Score) %>%
  summarise(mean_Stress = mean(Stress_Level, na.rm = TRUE))
ggplot(stressmeans, aes(x = factor(SleepQual_Score), y = mean_Stress)) +
  geom_col() +
  annotate(
    "text",
    x     = 2,
    y     = max(stressmeans $mean_Stress) * 1.05,
    label = paste0("ρ = ", round(stressrho, 2),
                   "\np = ", signif(stressp, 2)),
    hjust = 0.5
  ) +
  labs(
    title = "Figure 7. Mean Stress Level by Sleep Quality",
    x     = "Sleep Quality (1 = Poor, 2 = Average, 3 = Good)",
    y     = "Mean Stress Level"
  ) +
  theme_minimal(base_size = 14)

  
deprtest <- cor.test(data$SleepQual_Score, data$Depression_Score, method = "spearman")
deprrho <- deprtest$estimate
deprp <- deprtest$p.value
deprmeans <- data %>%
  group_by(SleepQual_Score) %>%
  summarise(mean_Depression = mean(Depression_Score, na.rm = TRUE))
ggplot(deprmeans, aes(x = factor(SleepQual_Score), y = mean_Depression)) +
  geom_col() +
  annotate(
    "text",
    x     = 2,
    y     = max(deprmeans$mean_Depression) * 1.05,
    label = paste0("ρ = ", round(deprrho, 2),
                   "\np = ", signif(deprp, 2)),
    hjust = 0.5
  ) +
  labs(
    title = "Figure 8. Mean Depression Score by Sleep Quality",
    x     = "Sleep Quality (1 = Poor, 2 = Average, 3 = Good)",
    y     = "Mean Depression Score"
  ) +
  theme_minimal(base_size = 14)
