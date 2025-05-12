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

anxreg <- cor.test(data$SleepQual_Score, data$Anxiety_Score, method = "spearman")
ggplot(data, aes(x = SleepQual_Score, y = Anxiety_Score)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE, color = "darkgreen") +
  annotate("text",
           x     = 1.5,
           y     = max(data$Anxiety_Score, na.rm=TRUE) * 0.9,
           label = paste0("ρ = ", round(anxreg$estimate, 2),
                          "\np = ", signif(anxreg$p.value, 2)),
           hjust = 0) +
  labs(
    title = "Sleep Quality vs. Anxiety Score",
    x     = "Sleep Quality",
    y     = "Anxiety Score"
  ) +
  theme_minimal()

stressreg <- cor.test(data$SleepQual_Score, data$Stress_Level, method = "spearman")
ggplot(data, aes(x = SleepQual_Score, y = Stress_Level)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE, color = "steelblue") +
  annotate("text",
           x     = 1.5,
           y     = max(data$Stress_Level, na.rm=TRUE) * 0.9,
           label = paste0("ρ = ", round(stressreg$estimate, 2),
                          "\np = ", signif(stressreg$p.value, 2)),
           hjust = 0) +
  labs(
    title = "Figure 6. Sleep Quality vs. Stress Level",
    x     = "Sleep Quality (1 = Poor, 2 = Average, 3 = Good)",
    y     = "Stress Level (0–5)"
  ) +
  theme_minimal()

depressionreg <- cor.test(data$SleepQual_Score, data$Depression_Score, method = "spearman")
ggplot(data, aes(x = SleepQual_Score, y = Depression_Score)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE, color = "firebrick") +
  annotate("text",
           x     = 1.5,
           y     = max(data$Depression_Score, na.rm=TRUE) * 0.9,
           label = paste0("ρ = ", round(depressionreg$estimate, 2),
                          "\np = ", signif(depressionreg$p.value, 2)),
           hjust = 0) +
  labs(
    title = "Figure 8. Sleep Quality vs. Depression Score",
    x     = "Sleep Quality (1 = Poor, 2 = Average, 3 = Good)",
    y     = "Depression Score (0–5)"
  ) +
  theme_minimal()
