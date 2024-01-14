?filter
?~
install.packages("tidyverse")
install.packages("survival")
install.packages("survminer")
install.packages("janitor")
install.packages("tidyquant")
install.packages("patchwork")
install.packages("exact2x2")
install.packages("pwr")

library(survival)
library(survminer)
library(tidyverse)
library(janitor)
library(patchwork)
library(tidyquant)
library(exact2x2)
library(pwr)
library(readxl)

surv.data <- read_excel("Lifespan_C2017.xlsx")
View(surv.data)
str(surv.data)
summary(surv.data)

ff.surv.data <- surv.data %>% 
  filter(sex == "f" & group == "RaAc9" | sex == "f" & group == "Control") 
ff.surv.data
msurv <- Surv(ff.surv.data$age, ff.surv.data$dead)
msurv
mfit <- survfit(msurv ~ ff.surv.data$group)
survdiff(msurv ~ ff.surv.data$group)
plot(mfit, col=c("red", "blue"))

summary(ff.surv.data)
  
  head(ff.surv.data)
table(ff.surv.data$group)
table(ff.surv.data$age)
names(surv.data)
missing <- !complete.cases(surv.data)
surv.data[missing, ]

sfit2 <- survfit(Surv(age, dead) ~ group, data = ff.surv.data)

g1 <- ggsurvplot(
  sfit2,
  conf.int = TRUE,
  data = ff.surv.data
)

g1$plot +
  theme_tq() +
  scale_fill_tq() +
  scale_color_tq() +
  labs(title = "Survival Analysis: Rapamycin + Acarbose vs Control")

g2 <- ggsurvplot(
  sfit2,
  conf.int = TRUE,
  data = ff.surv.data,
  risk.table = TRUE,
  break.x.by = 250,
  pval = TRUE,
)

g2_plot <- g2$plot +
  theme_tq() +
  scale_fill_tq() +
  scale_color_tq() +
  labs(title = "Survival Analysis: Rapamycin + Acarbose vs Control")

g2_table <- g2$table +
  theme_tq() +
  scale_fill_tq() +
  scale_color_tq() +
  theme(panel.grid = element_blank())

g2_plot / g2_table + plot_layout(heights = c(4,1))
data.survplot

ffc.surv.data <- ff.surv.data %>%
  filter(sex == "f" & group == "Control") 
summary(ffc.surv.data$age)
ffr.surv.data <- ff.surv.data %>% 
  filter(sex == "f" & group == "RaAc9") 
summary(ffr.surv.data$age)

g2_plot[["data"]][["surv"]]

fff.surv.data <- ff.surv.data %>% 
  mutate(group = factor(group, levels = c("RaAc9", "Control"))) 

time_point <- quantile(fff.surv.data$age, 0.9)
time_point

fff.surv.data$classification <- ifelse(fff.surv.data$age > time_point, "Alive", "Dead")
fff.surv.data$classification

table_data <- table(fff.surv.data$group, fff.surv.data$classification)

fisher.test(table_data)
levels(table_data$group)

test_result <- boschloo(40, 96, 2, 278)
print(test_result)

boxplot(age ~ group, data = ff.surv.data,
        main = "Survival Times by Group",
        xlab = "Group", ylab = "Survival Time")
points(jitter(as.numeric(ff.surv.data$group)), ff.surv.data$age, pch = 20, col = "blue")

ggplot(ff.surv.data, aes(x = group, y = age)) +
  geom_boxplot(outlier.color = "red") +
  geom_jitter(width = 0.2, alpha = 0.5, color = "blue") +
  labs(title = "Survival Times by Group",
       x = "Group",
       y = "Survival Time")

