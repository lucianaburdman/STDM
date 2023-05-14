---
title: "SDTM Project PXL"
author: "Luciana Burdman"
date: "2023-05-14"
output: html_document
---

For this small project, I will first export the SDTM dataset to a CSV file using the write.csv() function with the row.names = FALSE argument to prevent the row names from being included in the output file.

Next, I will fit a Cox proportional hazards model to the data using the coxph() function from the survival package. I will specify the survival object as Surv(TIME, EVENT) and include all other variables as covariates. The model will be stored in cox_model.

After that, I will print a summary of the Cox model using the summary() function.

Then, I will create a Kaplan-Meier plot of survival by treatment arm using the ggsurvplot() function from the survminer package. I will use the survfit() function to compute the survival estimates and specify the formula as Surv(TIME, EVENT) ~ ARM. I will also set conf.int = TRUE to include confidence intervals in the plot.

Finally, I will create a forest plot of hazard ratios by covariate using the ggforest() function from the survminer package. I will pass in the cox_model and the SDTM dataset as data. This will create a plot with the hazard ratios for each covariate, along with their confidence intervals and p-values.

Final report is knitted in a pdf file with the TFLs. 

# Load required packages
```{r}
# Load required packages
library(tidyverse)
library(survival)
library(survminer)
library(knitr)
library(kableExtra)
library(flextable)
library(ggplot2)
library(gridExtra)
```


# Create simulated data and export as STDM dataset

```{r}
# Set seed for reproducibility
set.seed(123)

# Create dataset
n <- 1000
age <- rnorm(n, 50, 10)
sex <- sample(c("M", "F"), n, replace = TRUE)
trt <- sample(c("A", "B"), n, replace = TRUE)
sbp <- rnorm(n, 120, 10)
dbp <- rnorm(n, 80, 5)
chol <- rnorm(n, 200, 30)
time_to_event <- rnorm(n, 12, 4)
event <- ifelse(time_to_event < 15, 1, 0)

data <- tibble(age, sex, trt, sbp, dbp, chol, time_to_event, event)

# Create SDTM dataset
sdtm_data <- data %>%
  mutate(
    STUDYID = "EXAMPLE",
    DOMAIN = "DM",
    USUBJID = paste0("EXAMPLE-", row_number()),
    SEX = factor(sex, levels = c("M", "F"), labels = c("MALE", "FEMALE")),
    ARM = factor(trt, levels = c("A", "B"), labels = c("ARM A", "ARM B")),
    AGE = age,
    SBP = sbp,
    DBP = dbp,
    CHOL = chol,
    TRT = trt,
    TIME = time_to_event,
    EVENT = event
  ) %>%
  select(STUDYID, DOMAIN, USUBJID, SEX, ARM, AGE, SBP, DBP, CHOL, TRT, TIME, EVENT)

# Export SDTM dataset to CSV file
write.csv(sdtm_data, "sdtm_data.csv", row.names = FALSE)

# Load SDTM dataset from CSV file
sdtm_data <- read.csv("sdtm_data.csv", stringsAsFactors = FALSE)

# Print first few rows of the dataset
head(sdtm_data)

```


# Now let's run some analyses

```{r}
# Fit Cox proportional hazards model
cox_model <- coxph(Surv(TIME, EVENT) ~ AGE + SEX + ARM + SBP + DBP + CHOL, data = sdtm_data)

# Print summary of the Cox model
summary(cox_model)

# Plot Kaplan-Meier curves by treatment arm
ggsurvplot(survfit(Surv(TIME, EVENT) ~ ARM, data = as.data.frame(sdtm_data)), conf.int = TRUE)

# Plot forest plot of hazard ratios by covariate
ggforest(cox_model, data = as.data.frame(sdtm_data))

```

# Now I create TFLs for the final report and knit it to a pdf final_report

```{r}
# Table 1: Descriptive statistics by treatment arm
table1 <- sdtm_data %>%
  group_by(ARM) %>%
  summarise(
    n = n(),
    mean_age = round(mean(AGE), 2),
    sd_age = round(sd(AGE), 2),
    mean_sbp = round(mean(SBP), 2),
    sd_sbp = round(sd(SBP), 2),
    mean_dbp = round(mean(DBP), 2),
    sd_dbp = round(sd(DBP), 2),
    mean_chol = round(mean(CHOL), 2),
    sd_chol = round(sd(CHOL), 2),
    event_rate = round(mean(EVENT), 2)
  )

# Convert the table to a flextable object
table1_ft <- flextable(table1) %>%
  set_caption("Descriptive statistics by treatment arm") %>%
  theme_box() %>%
  merge_v(j = "ARM") %>%
  align(align = "center", part = "all") %>%
  width(width = 1, j = "ARM") %>%
  width(width = 0.7, j = c("n", "event_rate")) %>%
  width(width = 0.9, j = c("mean_age", "sd_age", "mean_sbp", "sd_sbp", "mean_dbp", "sd_dbp", "mean_chol", "sd_chol"))


# Figure 1: Kaplan-Meier curves by treatment arm
surv_plot <- ggsurvplot(
  survfit(Surv(TIME, EVENT) ~ ARM, data = sdtm_data),
  conf.int = TRUE,
  ggtheme = theme_bw(),
  xlab = "Time (months)",
  ylab = "Survival probability"
)


# Figure 2: Boxplots of age by treatment arm
age_boxplot <- ggplot(sdtm_data, aes(x = ARM, y = AGE)) +
  geom_boxplot(fill = "gray80", color = "gray40") +
  ggtitle("Age by treatment arm") +
  xlab("Treatment arm") +
  ylab("Age")

# Listing 1: Cox proportional hazards model results
# Create a flextable from the coefficients table
cox_table <- flextable(as.data.frame(summary(cox_model)$coefficients))

# Add a caption to the table
cox_table <- set_caption(cox_table, "Cox proportional hazards model results")

# Format the table
cox_table <- fontsize(cox_table, size = 10)
cox_table <- align(cox_table, align = "center", part = "all")
cox_table <- autofit(cox_table)
cox_table <- set_header_labels(cox_table, hazard_ratio = "Hazard Ratio")

# Add some styling to the table
cox_table <- bg(cox_table, bg = "#e6e6e6", part = "header")
#cox_table <- bg(cox_table, bg = "#f2f2f2", part = "odd")
cox_table <- color(cox_table, color = "black", part = "all")
cox_table <- bold(cox_table, part = "header")

# Create a new PDF file for the final report
pdf("final_report.pdf")

# Print the table and figures
par(mfrow = c(2, 2), mar = c(4, 4, 2, 1), oma = c(0, 0, 2, 0))
print(table1)
print(surv_plot)
print(age_boxplot)
cat(cox_results)

# Close the PDF file
dev.off()


```
Table 1: Descriptive statistics by treatment arm

<img src="https://github.com/lucianaburdman/STDM/blob/d5f98448e4fc50abe3e6b5827b173ac10e761b2d/Table1.jpeg">


Figure 1: Kaplan-Meier curves by treatment arm

<img src="https://github.com/lucianaburdman/STDM/blob/d5f98448e4fc50abe3e6b5827b173ac10e761b2d/Fig1.jpeg">


Figure 2: Boxplots of age by treatment arm

<img src="https://github.com/lucianaburdman/STDM/blob/d5f98448e4fc50abe3e6b5827b173ac10e761b2d/Fig2.jpeg">


Figure 3: Plot forest plot of hazard ratios by covariate

<img src="https://github.com/lucianaburdman/STDM/blob/a773b5b7316d4f0408a75b7a4463669be6c37740/Fig3.jpeg">


Listing 1: Cox proportional hazards model results

<img src="https://github.com/lucianaburdman/STDM/blob/d5f98448e4fc50abe3e6b5827b173ac10e761b2d/4.jpeg">
