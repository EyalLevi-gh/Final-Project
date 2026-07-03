---
title: "ניתוח נתונים פרויקט"
output: html_document
date: "2026-06-18"
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
library(tidyverse)
```

## Main code used in the analyses

```{r}

path <- "C:/Users/eyale/Downloads/Anat & Bella DATA 110226 Trimmed N=588.csv"
data <- read.csv(path)
set.seed(123)


# Emotional Neglect reverse items
data$CTQ5_r  <- 6 - data$CTQ5
data$CTQ7_r  <- 6 - data$CTQ7
data$CTQ13_r <- 6 - data$CTQ13
data$CTQ19_r <- 6 - data$CTQ19
data$CTQ28_r <- 6 - data$CTQ28

# Physical Neglect reverse items
data$CTQ2_r  <- 6 - data$CTQ2
data$CTQ26_r <- 6 - data$CTQ26

# Emotional Abuse
data$Emotional_Abuse <- rowSums(
  data[, c("CTQ3","CTQ8","CTQ14","CTQ18","CTQ25")],
  na.rm = TRUE)

# Physical Abuse
data$Physical_Abuse <- rowSums(
  data[, c("CTQ9","CTQ11","CTQ12","CTQ15","CTQ17")],
  na.rm = TRUE)

# Sexual Abuse
data$Sexual_Abuse <- rowSums(
  data[, c("CTQ20","CTQ21","CTQ23","CTQ24","CTQ27")],
  na.rm = TRUE)

# Emotional Neglect
data$Emotional_Neglect <- rowSums(
  data[, c("CTQ5_r","CTQ7_r","CTQ13_r",
            "CTQ19_r","CTQ28_r")],
  na.rm = TRUE)

# Physical Neglect
data$Physical_Neglect <- rowSums(
  data[, c("CTQ1","CTQ2_r","CTQ4",
            "CTQ6","CTQ26_r")],
  na.rm = TRUE)



# Total score CTQ
data$CTQ_total <-
  data$Emotional_Abuse +
  data$Physical_Abuse +
  data$Sexual_Abuse +
  data$Emotional_Neglect +
  data$Physical_Neglect
# SE reverse items
data$SE3_r  <- 5 - data$SE3
data$SE5_r  <- 5 - data$SE5
data$SE8_r  <- 5 - data$SE8
data$SE9_r  <- 5 - data$SE9
data$SE10_r <- 5 - data$SE10

# SE mean
data$SE_mean <- rowMeans(
  data[, c("SE1","SE2","SE3_r","SE4","SE5_r",
           "SE6","SE7","SE8_r","SE9_r","SE10_r")],
  na.rm = TRUE
)

# SCS reverse items
data$SCS1_r  <- 6 - data$SCS1
data$SCS4_r  <- 6 - data$SCS4
data$SCS8_r  <- 6 - data$SCS8
data$SCS9_r  <- 6 - data$SCS9
data$SCS11_r <- 6 - data$SCS11
data$SCS12_r <- 6 - data$SCS12
# SCS mean
data$SCS_mean <- rowMeans(
  data[, c(
    "SCS1_r",
    "SCS2",
    "SCS3",
    "SCS4_r",
    "SCS5",
    "SCS6",
    "SCS7",
    "SCS8_r",
    "SCS9_r",
    "SCS10",
    "SCS11_r",
    "SCS12_r"
  )],
  na.rm = TRUE
)

# SCC reverse items
data$SCC1_r  <- 6 - data$SCC1
data$SCC2_r  <- 6 - data$SCC2
data$SCC3_r  <- 6 - data$SCC3
data$SCC4_r  <- 6 - data$SCC4
data$SCC5_r  <- 6 - data$SCC5
data$SCC7_r  <- 6 - data$SCC7
data$SCC8_r  <- 6 - data$SCC8
data$SCC9_r  <- 6 - data$SCC9
data$SCC10_r <- 6 - data$SCC10
data$SCC12_r <- 6 - data$SCC12

# SCC mean
data$SCC_mean <- rowMeans(
  data[, c(
    "SCC1_r",
    "SCC2_r",
    "SCC3_r",
    "SCC4_r",
    "SCC5_r",
    "SCC6",
    "SCC7_r",
    "SCC8_r",
    "SCC9_r",
    "SCC10_r",
    "SCC11",
    "SCC12_r"
  )],
  na.rm = TRUE
)

cor_matrix <- cor(
  data[, c("CTQ_total",
           "SE_mean",
           "SCS_mean",
           "SCC_mean")],
  use = "complete.obs"
)

summary(data[,c("CTQ_total","SE_mean","SCS_mean","SCC_mean")])


cor_matrix <- cor(
  data[, c(
    "Emotional_Abuse",
    "Physical_Abuse",
    "Sexual_Abuse",
    "Emotional_Neglect",
    "Physical_Neglect",
    "SE_mean",
    "SCS_mean",
    "SCC_mean"
  )],
  use = "complete.obs"
)

round(cor_matrix, 2)


model_SE <- lm(
  SE_mean ~ CTQ_total + AGE + GENDER + EDUCATION + INCOME + HEALTH,
  data = data
)

model_SCS <- lm(
  SCS_mean ~ CTQ_total + AGE + GENDER + EDUCATION + INCOME + HEALTH,
  data = data
)

model_SCC <- lm(
  SCC_mean ~ CTQ_total + AGE + GENDER + EDUCATION + INCOME + HEALTH,
  data = data
)

summary(model_SE)
summary(model_SCS)
summary(model_SCC)

data$RAS4_r <- 8 - data$RAS4
data$RAS7_r <- 8 - data$RAS7

data$RAS_total <- rowMeans(
  data[, c("RAS1", "RAS2", "RAS3", "RAS4_r","RAS5", "RAS6", "RAS7_r")],
  na.rm = TRUE
)

# CTS subscales
data$CTS_psych <- rowMeans(data[, c("CTS1","CTS2","CTS3","CTS4")], na.rm = TRUE)
data$CTS_sexual <- rowMeans(data[, c("CTS5","CTS6","CTS7","CTS8")], na.rm = TRUE)
data$CTS_physical <- rowMeans(data[, c("CTS9","CTS10","CTS11","CTS12")], na.rm = TRUE)
data$CTS_injury <- rowMeans(data[, c("CTS13","CTS14","CTS15","CTS16")], na.rm = TRUE)
data$CTS_negotiation <- rowMeans(data[, c("CTS17","CTS18","CTS19","CTS20")], na.rm = TRUE)

data$CTS_total <- rowMeans(
  data[, c("CTS_psych", "CTS_sexual", "CTS_physical", "CTS_injury")],
  na.rm = TRUE
)

data$CTS_violence <- 7 - data$CTS_total

model_SE_control <- lm(
  SE_mean ~ CTQ_total  + RAS_total + CTS_violence + AGE + GENDER + EDUCATION + INCOME,
  data = data
)

model_SCS_control <- lm(
  SCS_mean ~ CTQ_total  + RAS_total + CTS_violence + AGE + GENDER + EDUCATION + INCOME,
  data = data
)

model_SCC_control <- lm(
  SCC_mean ~ CTQ_total  + RAS_total + CTS_violence + AGE + GENDER + EDUCATION + INCOME,
  data = data
)

summary(model_SE_control)
summary(model_SCS_control)
summary(model_SCC_control)

cor(data$CTS_total, data$RAS_total, use="complete.obs")




model_SE_final <- model_SE_control
model_SCS_final <- model_SCS_control
model_SCC_final <- model_SCC_control

summary(model_SE_final)
summary(model_SCS_final)
summary(model_SCC_final)



calc_contribution <- function(full_model) {
  full_r2 <- summary(full_model)$r.squared
  predictors <- attr(terms(full_model), "term.labels")
  
  data.frame(
    Predictor = predictors,
    Contribution = sapply(predictors, function(p) {
      reduced_formula <- reformulate(
        predictors[predictors != p],
        response = as.character(formula(full_model)[[2]])
      )
      reduced_model <- lm(reduced_formula, data = model.frame(full_model))
      full_r2 - summary(reduced_model)$r.squared
    })
  )
}

se_contrib <- calc_contribution(model_SE_final) %>%
  mutate(Model = "Self-esteem")

scs_contrib <- calc_contribution(model_SCS_final) %>%
  mutate(Model = "Self-compassion")

scc_contrib <- calc_contribution(model_SCC_final) %>%
  mutate(Model = "Self-concept clarity")

contribution_table <- bind_rows(se_contrib, scs_contrib, scc_contrib) %>%
  mutate(Contribution = round(Contribution, 3))


ggplot(
  contribution_table,
  aes(
    x = Model,
    y = Contribution,
    fill = Predictor
  )
) +
  geom_col(position = "dodge") +
  theme_minimal() +
  labs(
    title = "Unique contribution of predictors across resilience measures",
    x = "Resilience Measure",
    y = "Unique contribution to R²"
  ) +
  scale_fill_brewer(palette = "Set2")

data$CTQ_mean <- rowMeans(
  data[, c(
    "CTQ3","CTQ8","CTQ14","CTQ18","CTQ25",
    "CTQ9","CTQ11","CTQ12","CTQ15","CTQ17",
    "CTQ20","CTQ21","CTQ23","CTQ24","CTQ27",
    "CTQ5_r","CTQ7_r","CTQ13_r","CTQ19_r","CTQ28_r",
    "CTQ1","CTQ2_r","CTQ4","CTQ6","CTQ26_r"
  )],
  na.rm = TRUE
)


plot_data <- data %>%
  select(CTQ_mean, SE_mean, SCS_mean, SCC_mean) %>%
  pivot_longer(
    cols = c(SE_mean, SCS_mean, SCC_mean),
    names_to = "Outcome",
    values_to = "Score"
  ) %>%
  drop_na()

plot_data$Outcome <- recode(
  plot_data$Outcome,
  SE_mean = "Self-esteem",
  SCS_mean = "Self-compassion",
  SCC_mean = "Self-concept clarity"
)

ggplot(plot_data, aes(x = CTQ_mean, y = Score)) +
  geom_point(alpha = 0.35, size = 1.8, color = "grey40") +
  geom_smooth(method = "lm", se = TRUE, color = "#2C7FB8", fill = "#A6CEE3") +
  facet_wrap(~ Outcome, scales = "free_y") +
  theme_minimal(base_size = 14) +
  labs(
    title = "Childhood Trauma and Psychological Resilience",
    x = "Mean Childhood Trauma Score (CTQ)",
    y = "Resilience Score"
  ) +
  theme(
    plot.title = element_text(face = "bold", size = 18),
    strip.text = element_text(face = "bold", size = 13),
    axis.title = element_text(face = "bold")
  )



cv_r2 <- function(model, data, k = 5){

  form <- formula(model)
  outcome <- all.vars(form)[1]

  df <- data %>%
    select(all.vars(form)) %>%
    drop_na() %>%
    mutate(across(where(is.character), ~as.numeric(factor(.x)))) %>%
    mutate(across(where(is.factor), as.numeric)) %>%
    mutate(fold = sample(rep(1:k, length.out = n())))

  map_dbl(1:k, function(i){

    train <- df %>% filter(fold != i) %>% select(-fold)
    test  <- df %>% filter(fold == i) %>% select(-fold)

    fit <- lm(form, data = train)

    pred <- predict(fit, newdata = test)
    y <- test[[outcome]]

    1 - sum((y - pred)^2) /
      sum((y - mean(train[[outcome]]))^2)

  }) %>%
    mean()
}

tibble(
  Model = c("SE", "SCS", "SCC"),
  CV_R2 = c(
    cv_r2(model_SE_final, data),
    cv_r2(model_SCS_final, data),
    cv_r2(model_SCC_final, data)
  )
)


cor_matrix <- cor(
  data[, c("CTQ_total", "SE_mean", "SCS_mean", "SCC_mean")],
  use = "complete.obs"
)

cor_df <- as.data.frame(as.table(round(cor_matrix, 2)))

ggplot(cor_df, aes(x = Var1, y = Var2, fill = Freq)) +
  geom_tile(color = "white") +
  geom_text(aes(label = round(Freq, 2)), size = 5) +
  scale_fill_gradient2(
    low = "#B2182B",     # אדום
    mid = "white",
    high = "#2166AC",    # כחול
    midpoint = 0,
    limits = c(-1, 1),
    name = "r"
  ) +
  theme_minimal(base_size = 16) +
  labs(
    title = "Correlation Heatmap",
    x = "",
    y = ""
  ) +
  theme(
    axis.text.x = element_text(
      size = 14,
      angle = 45,
      hjust = 1
    ),
    axis.text.y = element_text(
      size = 14
    ),
    plot.title = element_text(
      size = 18,
      face = "bold"
    ),
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 12)
  )


# Diagnostic plots for checking linear regression assumptions
par(mfrow = c(2,2))
plot(model_SE_final)

par(mfrow = c(2,2))
plot(model_SCS_final)

par(mfrow = c(2,2))
plot(model_SCC_final)

par(mfrow = c(3,4), 
    mar = c(4,4,2,1))

plot(model_SE_final)
plot(model_SCS_final)
plot(model_SCC_final)

par(mfrow = c(1,1))

```

## This section was used for exploratory analyses to identify potentially useful models.
## The final models reported in the paper are the theory-driven models described above.

```{r, echo=FALSE}




data <- read.csv(path)

# Emotional Neglect reverse items
data$CTQ5_r  <- 6 - data$CTQ5
data$CTQ7_r  <- 6 - data$CTQ7
data$CTQ13_r <- 6 - data$CTQ13
data$CTQ19_r <- 6 - data$CTQ19
data$CTQ28_r <- 6 - data$CTQ28

# Physical Neglect reverse items
data$CTQ2_r  <- 6 - data$CTQ2
data$CTQ26_r <- 6 - data$CTQ26

# Emotional Abuse
data$Emotional_Abuse <- rowSums(
  data[, c("CTQ3","CTQ8","CTQ14","CTQ18","CTQ25")],
  na.rm = TRUE)

# Physical Abuse
data$Physical_Abuse <- rowSums(
  data[, c("CTQ9","CTQ11","CTQ12","CTQ15","CTQ17")],
  na.rm = TRUE)

# Sexual Abuse
data$Sexual_Abuse <- rowSums(
  data[, c("CTQ20","CTQ21","CTQ23","CTQ24","CTQ27")],
  na.rm = TRUE)

# Emotional Neglect
data$Emotional_Neglect <- rowSums(
  data[, c("CTQ5_r","CTQ7_r","CTQ13_r",
            "CTQ19_r","CTQ28_r")],
  na.rm = TRUE)

# Physical Neglect
data$Physical_Neglect <- rowSums(
  data[, c("CTQ1","CTQ2_r","CTQ4",
            "CTQ6","CTQ26_r")],
  na.rm = TRUE)

# Total score CTQ
data$CTQ_total <-
  data$Emotional_Abuse +
  data$Physical_Abuse +
  data$Sexual_Abuse +
  data$Emotional_Neglect +
  data$Physical_Neglect
# SE reverse items
data$SE3_r  <- 5 - data$SE3
data$SE5_r  <- 5 - data$SE5
data$SE8_r  <- 5 - data$SE8
data$SE9_r  <- 5 - data$SE9
data$SE10_r <- 5 - data$SE10

# SE mean
data$SE_mean <- rowMeans(
  data[, c("SE1","SE2","SE3_r","SE4","SE5_r",
           "SE6","SE7","SE8_r","SE9_r","SE10_r")],
  na.rm = TRUE
)

# SCS reverse items
data$SCS1_r  <- 6 - data$SCS1
data$SCS4_r  <- 6 - data$SCS4
data$SCS8_r  <- 6 - data$SCS8
data$SCS9_r  <- 6 - data$SCS9
data$SCS11_r <- 6 - data$SCS11
data$SCS12_r <- 6 - data$SCS12
# SCS mean
data$SCS_mean <- rowMeans(
  data[, c(
    "SCS1_r",
    "SCS2",
    "SCS3",
    "SCS4_r",
    "SCS5",
    "SCS6",
    "SCS7",
    "SCS8_r",
    "SCS9_r",
    "SCS10",
    "SCS11_r",
    "SCS12_r"
  )],
  na.rm = TRUE
)

# SCC reverse items
data$SCC1_r  <- 6 - data$SCC1
data$SCC2_r  <- 6 - data$SCC2
data$SCC3_r  <- 6 - data$SCC3
data$SCC4_r  <- 6 - data$SCC4
data$SCC5_r  <- 6 - data$SCC5
data$SCC7_r  <- 6 - data$SCC7
data$SCC8_r  <- 6 - data$SCC8
data$SCC9_r  <- 6 - data$SCC9
data$SCC10_r <- 6 - data$SCC10
data$SCC12_r <- 6 - data$SCC12

# SCC mean
data$SCC_mean <- rowMeans(
  data[, c(
    "SCC1_r",
    "SCC2_r",
    "SCC3_r",
    "SCC4_r",
    "SCC5_r",
    "SCC6",
    "SCC7_r",
    "SCC8_r",
    "SCC9_r",
    "SCC10_r",
    "SCC11",
    "SCC12_r"
  )],
  na.rm = TRUE
)

cor_matrix <- cor(
  data[, c("CTQ_total",
           "SE_mean",
           "SCS_mean",
           "SCC_mean")],
  use = "complete.obs"
)




summary(data[,c("CTQ_total","SE_mean","SCS_mean","SCC_mean")])




model_SE <- lm(
  SE_mean ~ CTQ_total + AGE + GENDER + EDUCATION + INCOME + HEALTH,
  data = data
)

model_SCS <- lm(
  SCS_mean ~ CTQ_total + AGE + GENDER + EDUCATION + INCOME + HEALTH,
  data = data
)

model_SCC <- lm(
  SCC_mean ~ CTQ_total + AGE + GENDER + EDUCATION + INCOME + HEALTH,
  data = data
)

summary(model_SE)
summary(model_SCS)
summary(model_SCC)

data$RAS4_r <- 8 - data$RAS4
data$RAS7_r <- 8 - data$RAS7

data$RAS_total <- rowMeans(
  data[, c("RAS1", "RAS2", "RAS3", "RAS4_r","RAS5", "RAS6", "RAS7_r")],
  na.rm = TRUE
)

# CTS subscales
data$CTS_psych <- rowMeans(data[, c("CTS1","CTS2","CTS3","CTS4")], na.rm = TRUE)
data$CTS_sexual <- rowMeans(data[, c("CTS5","CTS6","CTS7","CTS8")], na.rm = TRUE)
data$CTS_physical <- rowMeans(data[, c("CTS9","CTS10","CTS11","CTS12")], na.rm = TRUE)
data$CTS_injury <- rowMeans(data[, c("CTS13","CTS14","CTS15","CTS16")], na.rm = TRUE)
data$CTS_negotiation <- rowMeans(data[, c("CTS17","CTS18","CTS19","CTS20")], na.rm = TRUE)

# CTS total violence score - without negotiation
data$CTS_total <- rowMeans(
  data[, c("CTS_psych", "CTS_sexual", "CTS_physical", "CTS_injury")],
  na.rm = TRUE
)

data$CTS_violence <- 7 - data$CTS_total

# Models with demographic controls + questionnaire controls
model_SE_control <- lm(
  SE_mean ~ CTQ_total  + RAS_total + CTS_violence + AGE + GENDER + EDUCATION + INCOME + HEALTH,
  data = data
)

model_SCS_control <- lm(
  SCS_mean ~ CTQ_total  + RAS_total + CTS_violence + AGE + GENDER + EDUCATION + INCOME + HEALTH,
  data = data
)

model_SCC_control <- lm(
  SCC_mean ~ CTQ_total  + RAS_total + CTS_violence + AGE + GENDER + EDUCATION + INCOME + HEALTH,
  data = data
)

summary(model_SE_control)
summary(model_SCS_control)
summary(model_SCC_control)

cor(data$CTS_total, data$RAS_total, use="complete.obs")

summary(data$CTS_total)



# trying more models
data$CTQ10_r <- 6 - data$CTQ10
data$CTQ16_r <- 6 - data$CTQ16
data$CTQ22_r <- 6 - data$CTQ22

model_all_items <- lm(
  SCC_mean ~
    CTQ1 + CTQ2_r + CTQ3 + CTQ4 + CTQ5_r +
    CTQ6 + CTQ7_r + CTQ8 + CTQ9 + CTQ10_r + CTQ16_r + CTQ22_r +
    CTQ11 + CTQ12 + CTQ13_r + CTQ14 + CTQ15 +
    CTQ17 + CTQ18 + CTQ19_r + CTQ20 + CTQ21 +
    CTQ23 + CTQ24 + CTQ25 + CTQ26_r + CTQ27 + CTQ28_r +
    RAS1 + RAS2 + RAS3 + RAS4_r + RAS5 + RAS6 + RAS7_r +
    CTS1 + CTS2 + CTS3 + CTS4 +
    CTS5 + CTS6 + CTS7 + CTS8 +
    CTS9 + CTS10 + CTS11 + CTS12 +
    CTS13 + CTS14 + CTS15 + CTS16 +
    GENDER,
  data = data
)

summary(model_all_items)

model_reduced <- lm(
  SCC_mean ~
    CTQ11 +
    CTQ12 +
    CTQ17 +
    RAS2 +
    RAS4_r +
    RAS7_r +
    CTS6 +
    GENDER,
  data = data
)

summary(model_reduced)

model_reduced_contrib <- calc_contribution(model_reduced) %>%
  mutate(Model = "Reduced model")

contribution_table <- model_reduced_contrib %>%
  mutate(Contribution = round(Contribution, 3))

ggplot(
  contribution_table,
  aes(
    x = Model,
    y = Contribution,
    fill = Predictor
  )
) +
  geom_col(position = "dodge") +
  theme_minimal() +
  labs(
    title = "Unique contribution of predictors",
    x = "",
    y = expression(Delta*R^2)
  ) +
  scale_fill_brewer(palette = "Set2")

data$CTQ_selected_mean <- rowMeans(
  data[, c("CTQ11", "CTQ12", "CTQ17")],
  na.rm = TRUE
)

model_SE_final <- lm(
  SE_mean ~ CTQ_total + RAS_total + CTS_violence + GENDER,
  data = data
)

model_SCS_final <- lm(
  SCS_mean ~ CTQ_total + RAS_total + CTS_violence + GENDER,
  data = data
)

model_SCC_final <- lm(
  SCC_mean ~ CTQ_total + RAS_total + CTS_violence + GENDER,
  data = data
)

summary(model_SE_final)
summary(model_SCS_final)
summary(model_SCC_final)


calc_contribution <- function(full_model) {
  full_r2 <- summary(full_model)$r.squared
  predictors <- attr(terms(full_model), "term.labels")
  
  data.frame(
    Predictor = predictors,
    Contribution = sapply(predictors, function(p) {
      reduced_formula <- reformulate(
        predictors[predictors != p],
        response = as.character(formula(full_model)[[2]])
      )
      reduced_model <- lm(reduced_formula, data = model.frame(full_model))
      full_r2 - summary(reduced_model)$r.squared
    })
  )
}

se_contrib <- calc_contribution(model_SE_final) %>%
  mutate(Model = "Self-esteem")

scs_contrib <- calc_contribution(model_SCS_final) %>%
  mutate(Model = "Self-compassion")

scc_contrib <- calc_contribution(model_SCC_final) %>%
  mutate(Model = "Self-concept clarity")

contribution_table <- bind_rows(se_contrib, scs_contrib, scc_contrib) %>%
  mutate(Contribution = round(Contribution, 3))

ggplot(
  contribution_table,
  aes(x = Model, y = Contribution, fill = Predictor)
) +
  geom_col(position = "dodge") +
  theme_minimal() +
  labs(
    title = "Unique contribution of predictors across resilience measures",
    x = "Resilience Measure",
    y = "Unique contribution to R²",
    fill = "Predictor"
  )


data2 <- data[, 1:106]

data2 <- data2[, !grepl("^SE", names(data2))]
#data2 <- data2[, !grepl("^SCS", names(data2))]
#data2 <- data2[, !grepl("^SCC", names(data2))]


data2$SE_mean <- data$SE_mean


head(data)
data2$CTS_violence <- data$CTS_violence

predictors <- setdiff(names(data2), "SE_mean")

formula_se <- as.formula(
  paste("SE_mean ~", paste(predictors, collapse = " + "))
)

data2_clean <- na.omit(data2)

model_se_all <- lm(formula_se, data = data2_clean)

model_step_both <- step(
  model_se_all,
  direction = "both",
  trace = FALSE
)

summary(model_step_both)


model_se_all_no_step <- lm(
  formula_se,
  data = data2,
  na.action = na.omit
)
summary(model_se_all_no_step)

coef_table <- as.data.frame(
  summary(model_se_all_no_step)$coefficients
)

coef_table$Variable <- rownames(coef_table)

top10 <- coef_table %>%
  dplyr::filter(Variable != "(Intercept)") %>%
  dplyr::arrange(desc(abs(`t value`))) %>%
  dplyr::select(
    Variable,
    Estimate,
    `t value`,
    `Pr(>|t|)`
  ) %>%
  head(10)

summary(model_se_all_no_step)





data2$ras_mean <- data$RAS_total
data2$ras_mean



data2_clean$AGE <- as.numeric(data2_clean$AGE)
unique(data2_clean$AGE)
class(data$AGE)
head(data$AGE, 20)
unique(data$AGE)

data2_clean$AGE_num <- NA




data2_clean$AGE_num[data2_clean$AGE == "18-30"] <- 24
data2_clean$AGE_num[data2_clean$AGE == "31-40"] <- 35.5
data2_clean$AGE_num[data2_clean$AGE == "41-50"] <- 45.5
data2_clean$AGE_num[data2_clean$AGE == "51-60"] <- 55.5
data2_clean$AGE_num[data2_clean$AGE == "61+"]   <- 65

data2_clean$AGE_num <- as.numeric(data2_clean$AGE_num)

table(data2_clean$AGE, useNA = "ifany")
table(data2_clean$AGE_num, useNA = "ifany")



data3 <- read.csv(path)
data3 <- data3[, 1:106]

data3 <- data3[, !grepl("^SE", names(data3))]
data3 <- data3[, !grepl("^SCC", names(data3))]
data3 <- data3[, !grepl("^SCS", names(data3))]

data3$SE_mean <- data$SE_mean


data3[] <- lapply(data3, function(x) {
  if (!is.numeric(x)) {
    as.numeric(factor(x))
  } else {
    x
  }
})



head(data3)

plot(data3$AGE, data3$SE_mean)


predictors <- setdiff(names(data3), "SE_mean")

formula_se <- as.formula(
  paste("SE_mean ~", paste(predictors, collapse = " + "))
)

# הסרת שורות עם NA
data3_clean <- na.omit(data3)

# -------------------------
# Model 1: Full model
# -------------------------
model_full <- lm(formula_se, data = data3_clean)

summary(model_full)$r.squared
summary(model_full)$adj.r.squared

# -------------------------
# Model 2: Stepwise model
# -------------------------
model_step <- step(
  model_full,
  direction = "both",
  trace = FALSE
)

summary(model_step)$r.squared
summary(model_step)$adj.r.squared


data3_fe <- data3_clean

data3_fe$AGE2 <- data3_fe$AGE^2

data3_fe$AGE_GENDER <- data3_fe$AGE * data3_fe$GENDER
data3_fe$AGE_EDUCATION <- data3_fe$AGE * data3_fe$EDUCATION

predictors3 <- setdiff(names(data3_fe), "SE_mean")

formula3 <- as.formula(
  paste("SE_mean ~", paste(predictors3, collapse = " + "))
)

model_fe <- lm(formula3, data = data3_fe)

model_fe_step <- step(
  model_fe,
  direction = "both",
  trace = FALSE
)

summary(model_fe_step)





df <- data3_clean
target <- "SE_mean"

vars <- setdiff(names(df), target)

# נשמור רק משתנים מספריים לפיצ'רים כמו log ו-^2
num_vars <- vars[sapply(df[vars], is.numeric)]

n_models <- 10

results <- data.frame(
  model_id = integer(),
  formula = character(),
  R2 = numeric(),
  Adj_R2 = numeric(),
  stringsAsFactors = FALSE
)

best_model <- NULL
best_adj <- -Inf
best_r2 <- -Inf
best_model_adj <- NULL
best_model_r2 <- NULL

for(i in 1:n_models){

  terms <- c()

  #### משתנים רגילים
  k1 <- sample(2:max(10,length(vars)),1)
  terms <- c(
    terms,
    sample(vars, k1)
  )

  #### ריבועים
  if(length(num_vars) > 0){

    k2 <- sample(
      0:min(3,length(num_vars)),
      1
    )

    if(k2 > 0){

      sq_vars <- sample(
        num_vars,
        k2
      )

      terms <- c(
        terms,
        paste0("I(",sq_vars,"^2)")
      )
    }
  }

  #### לוגים
  positive_vars <- num_vars[
    sapply(
      df[num_vars],
      function(x) all(x > 0, na.rm = TRUE)
    )
  ]

  if(length(positive_vars) > 0){

    k3 <- sample(
      0:min(3,length(positive_vars)),
      1
    )

    if(k3 > 0){

      lg_vars <- sample(
        positive_vars,
        k3
      )

      terms <- c(
        terms,
        paste0("log(",lg_vars,")")
      )
    }
  }

  #### אינטראקציה אחת אקראית
  if(length(vars) >= 2){

    pair <- sample(vars,2)

    terms <- c(
      terms,
      paste0(pair[1],":",pair[2])
    )
  }

  form <- as.formula(
    paste(
      target,
      "~",
      paste(unique(terms),
            collapse = " + ")
    )
  )

  fit <- try(
    lm(form, data=df),
    silent=TRUE
  )

  if(!inherits(fit,"try-error")){

    s <- summary(fit)

    results <- rbind(
      results,
      data.frame(
        model_id=i,
        formula=deparse(form),
        R2=s$r.squared,
        Adj_R2=s$adj.r.squared
      )
    )

    if(s$adj.r.squared > best_adj){
      best_adj <- s$adj.r.squared
      best_model_adj <- fit
    }

    if(s$r.squared > best_r2){
      best_r2 <- s$r.squared
      best_model_r2 <- fit
    }
  }
}


summary(best_model_adj)
summary(best_model_r2)

head(
  results[
    order(-results$Adj_R2),
  ],
  10
)

head(
  results[
    order(-results$R2),
  ],
  10
)
is.null(best_model_adj)
is.null(best_model_r2)




auto_transform <- function(df, target){

  vars <- setdiff(names(df), target)

  results <- data.frame(
    variable = character(),
    best_transform = character(),
    R2 = numeric(),
    Adj_R2 = numeric(),
    stringsAsFactors = FALSE
  )

  for(v in vars){

    x <- df[[v]]

    if(!is.numeric(x))
      next

    candidates <- list(
      original = x,
      square = x^2
    )

    if(all(x > 0, na.rm = TRUE)){
      candidates$log <- log(x)
      candidates$sqrt <- sqrt(x)
    }

    if(all(is.finite(exp(x[x < 20])), na.rm = TRUE)){
      candidates$exp <- exp(x)
    }

    best_r2 <- -Inf
    best_adj <- -Inf
    best_name <- NA

    for(nm in names(candidates)){

      tmp <- data.frame(
        y = df[[target]],
        x = candidates[[nm]]
      )

      tmp <- na.omit(tmp)

      if(nrow(tmp) < 20)
        next

      fit <- try(
        lm(y ~ x, data = tmp),
        silent = TRUE
      )

      if(inherits(fit, "try-error"))
        next

      s <- summary(fit)

      if(s$adj.r.squared > best_adj){
        best_adj <- s$adj.r.squared
        best_r2 <- s$r.squared
        best_name <- nm
      }
    }

    results <- rbind(
      results,
      data.frame(
        variable = v,
        best_transform = best_name,
        R2 = best_r2,
        Adj_R2 = best_adj
      )
    )
  }

  results[order(-results$Adj_R2), ]
}


best_transforms <- auto_transform(
  data3_clean,
  "SE_mean"
)

head(best_transforms, 20)


build_best_features <- function(df, target, transform_table){

  out <- data.frame(
    y = df[[target]]
  )

  for(i in 1:nrow(transform_table)){

    v <- transform_table$variable[i]
    tr <- transform_table$best_transform[i]

    x <- df[[v]]

    newx <- switch(
      tr,
      original = x,
      square = x^2,
      log = log(x),
      sqrt = sqrt(x),
      exp = exp(x)
    )

    out[[paste0(v, "_", tr)]] <- newx
  }

  out
}
df_best <- build_best_features(
  data3_clean,
  "SE_mean",
  best_transforms
)

summary(df_best)

df_best <- na.omit(df_best)

model_best <- lm(y ~ ., data = df_best)

summary(model_best)
summary(model_best)$r.squared
summary(model_best)$adj.r.squared

model_best_step <- step(
  model_best,
  direction = "both",
  trace = FALSE
)
summary(model_best_step)







data2 <- data[, 1:106]

data2 <- data2[, !grepl("^SE", names(data2))]
data2 <- data2[, !grepl("^SCC", names(data2))]
data2 <- data2[, !grepl("^SCS", names(data2))]

data2$SE_mean <- data$SE_mean


head(data)
data2$CTS_violence <- data$CTS_violence

predictors <- setdiff(names(data2), "SE_mean")

formula_se <- as.formula(
  paste("SE_mean ~", paste(predictors, collapse = " + "))
)

data2_clean <- na.omit(data2)

model_se_all <- lm(formula_se, data = data2_clean)

model_step_both <- step(
  model_se_all,
  direction = "both",
  trace = FALSE
)

summary(model_step_both)


model_se_all_no_step <- lm(
  formula_se,
  data = data2,
  na.action = na.omit
)
summary(model_se_all_no_step)




build_models <- function(data, target){

  df <- data[, 1:106]

  df <- df[, !grepl("^SE", names(df))]
  df <- df[, !grepl("^SCC", names(df))]
  df <- df[, !grepl("^SCS", names(df))]

  df[[target]] <- data[[target]]

  if("CTS_violence" %in% names(data)){
    df$CTS_violence <- data$CTS_violence
  }

  predictors <- setdiff(names(df), target)

  form <- as.formula(
    paste(target, "~", paste(predictors, collapse = " + "))
  )

  df_clean <- na.omit(df)

  model_no_step <- lm(form, data = df_clean)

  model_step <- step(
    model_no_step,
    direction = "both",
    trace = FALSE
  )

  list(
    data = df,
    data_clean = df_clean,
    formula = form,
    model_no_step = model_no_step,
    model_step = model_step
  )
}

se_models <- build_models(data, "SE_mean")
data2 <- se_models$data

model_se_all_no_step <- se_models$model_no_step
model_se_step <- se_models$model_step

summary(model_se_all_no_step)
summary(model_se_step)


scs_models <- build_models(data, "SCS_mean")
data4 <- scs_models$data

model_scs_all_no_step <- scs_models$model_no_step
model_scs_step <- scs_models$model_step

summary(model_scs_all_no_step)
summary(model_scs_step)


scc_models <- build_models(data, "SCC_mean")
data5 <- scc_models$data

model_scc_all_no_step <- scc_models$model_no_step
model_scc_step <- scc_models$model_step

summary(model_scc_all_no_step)
summary(model_scc_step)
```



