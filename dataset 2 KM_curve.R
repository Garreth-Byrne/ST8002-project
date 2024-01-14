library(survival)
library(survminer)
library(tidyverse)
library(janitor)
library(patchwork)
library(tidyquant)
library(exact2x2)
library(pwr)
library(ggridges)

install.packages("e1071")
install.packages("ggridges")
install.packages("nortest")
library(nortest)
install.packages("dunn.test")
library(dunn.test)

surv.data2 <- read_excel("Lifespan_C2016.xlsx")
View(surv.data2)
str(surv.data2)
summary(surv.data2)

fm.surv.data2 <- surv.data2 %>%
  filter(sex == "m" & site == "UT" & group == "17aE2_16m" | sex == "m" & site == "UT" & group == "Control" 
         | sex == "m" & site == "UM" & group == "17aE2_16m" | sex == "m" & site == "UM" & group == "Control" 
         | sex == "m" & site == "TJL" & group == "17aE2_16m" | sex == "m" & site == "TJL" & group == "Control") 

fmc.surv.data2 <- fm.surv.data2 %>% 
  filter(group == "Control")

fmt.surv.data2 <- fm.surv.data2 %>% 
  filter(group == "17aE2_16m")

km.fmt <- survfit(Surv(age, dead) ~ site, data = fmt.surv.data2)

g3 <- ggsurvplot(
  km.fmt,
  conf.int = TRUE,
  data = fmt.surv.data2
)

g3$plot +
  theme_tq() +
  scale_fill_tq() +
  scale_color_tq() +
  labs(title = "Survival Analysis: 17-alpha-estradiol UT vs UM vs TJL")

g4 <- ggsurvplot(
  km.fmt,
  conf.int = TRUE,
  data = fmt.surv.data2,
  risk.table = TRUE,
  break.x.by = 250,
)

g4_plot <- g4$plot +
  theme_tq() +
  scale_fill_tq() +
  scale_color_tq() +
  labs(title = "Survival Analysis: 17-alpha-estradiol UT vs UM vs TJL")

g4_table <- g4$table +
  theme_tq() +
  scale_fill_tq() +
  scale_color_tq() +
  theme(panel.grid = element_blank())

g4_plot / g4_table + plot_layout(heights = c(4,1))


km.fmc <- survfit(Surv(age, dead) ~ site, data = fmc.surv.data2)

g5 <- ggsurvplot(
  km.fmc,
  conf.int = TRUE,
  data = fmc.surv.data2
)

g5$plot +
  theme_tq() +
  scale_fill_tq() +
  scale_color_tq() +
  labs(title = "Survival Analysis: (Control) UT vs UM vs TJL")

g6 <- ggsurvplot(
  km.fmc,
  conf.int = TRUE,
  data = fmc.surv.data2,
  risk.table = TRUE,
  break.x.by = 250,
)

g6_plot <- g6$plot +
  theme_tq() +
  scale_fill_tq() +
  scale_color_tq() +
  labs(title = "Survival Analysis: (Control) UT vs UM vs TJL")

g6_table <- g6$table +
  theme_tq() +
  scale_fill_tq() +
  scale_color_tq() +
  theme(panel.grid = element_blank())

g6_plot / g6_table + plot_layout(heights = c(4,1))

ggplot(fmc.surv.data2, aes(x = site, y = age)) +
  geom_boxplot(outlier.color = "red") +
  geom_jitter(width = 0.2, alpha = 0.5, color = "blue") +
  labs(title = "Survival Times by Site (17-alpha-estradiol)",
       x = "Site",
       y = "Survival Time")

ggplot(fmc.surv.data2, aes(x = site, y = age)) +
  geom_boxplot(outlier.color = "red") +
  geom_jitter(width = 0.2, alpha = 0.5, color = "blue") +
  labs(title = "Survival Times by Site (Control)",
       x = "Group",
       y = "Survival Time")

fmt.results <- fmt.surv.data2 %>%
  group_by(site) %>%
  summarize(
    Mean = mean(age, na.rm = TRUE),
    Median = median(age, na.rm = TRUE),
    Standard_Deviation = sd(age, na.rm = TRUE),
    IQR = IQR(age, na.rm = TRUE),
    Skewness = skewness(age, na.rm = TRUE),
    Kurtosis = kurtosis(age, na.rm = TRUE)
  )
print(fmt.results)


fmc.results <- fmc.surv.data2 %>%
  group_by(site) %>%
  summarize(
    Mean = mean(age, na.rm = TRUE),
    Median = median(age, na.rm = TRUE),
    Standard_Deviation = sd(age, na.rm = TRUE),
    IQR = IQR(age, na.rm = TRUE),
    Skewness = skewness(age, na.rm = TRUE),
    Kurtosis = kurtosis(age, na.rm = TRUE)
  )
print(fmc.results)


ggplot(fmt.surv.data2, aes(sample = age)) +
  stat_qq() +
  stat_qq_line() +
  facet_wrap(~ site, scales = "free") +
  labs(title = "Q-Q Plots for Each Site (17-alpha-estradiol)")

ggplot(fmc.surv.data2, aes(sample = age)) +
  stat_qq() +
  stat_qq_line() +
  facet_wrap(~ site, scales = "free") +
  labs(title = "Q-Q Plots for Each Site (Control)")



ggplot(fmc.surv.data2, aes(age)) +
  geom_density()


ggplot(fmc.surv.data2, aes(age)) +
  geom_density(fill = "lightgray") +
  geom_vline(aes(xintercept = mean(age)), linetype = 2)




mu <- fmc.surv.data2 %>%
  group_by(site) %>%
  summarise(grp.mean = mean(age))

mu2 <- fmt.surv.data2 %>%
  group_by(site) %>%
  summarise(grp.mean = mean(age))

ggplot(fmc.surv.data2, aes(age, color = site)) +
  geom_density(size = 2) +
  geom_vline(aes(xintercept = grp.mean, color = site),
             data = mu, linetype = 2, size = 1) +
  labs(title = "Density plots (Control)") +
  scale_color_viridis_d()

ggplot(fmt.surv.data2, aes(age, color = site)) +
  geom_density(size = 2) +
  geom_vline(aes(xintercept = grp.mean, color = site),
             data = mu, linetype = 2, size = 1) +
  labs(title = "Density plots (17-alpha-estradiol)") +
  scale_color_viridis_d()


ggplot(fmc.surv.data2, aes(x = age, y = site)) +
  geom_density_ridges(aes(fill = site)) +
  scale_fill_manual(values = c("#00AFBB", "#E7B800", "#FC4E07")) +
  geom_vline(aes(xintercept = grp.mean, color = site),
             geom_vline(aes(xintercept = grp.mode, color = site),
                        geom_vline(aes(xintercept = grp.median, color = site),
             data = mu, linetype = 1, size = )))

ggplot(fmt.surv.data2, aes(x = age, y = site)) +
  geom_density_ridges(aes(fill = site)) +
  scale_fill_manual(values = c("#00AFBB", "#E7B800", "#FC4E07")) +
  geom_vline(aes(xintercept = grp.mean, grp.median, grp.mode, color = site),
             data = mu, linetype = 1, size = 1)




# Fit ANOVA model
fmc.anova_model <- aov(age ~ site, data = fmc.surv.data2)

# View results
summary(fmc.anova_model)

fmt.anova_model <- aov(age ~ site, data = fmt.surv.data2)

# View results
summary(fmt.anova_model)

fmt.kruskal_result <- kruskal.test(age ~ site, data = fmt.surv.data2)

fmc.kruskal_result <- kruskal.test(age ~ site, data = fmc.surv.data2)

# Print the result
print(fmt.kruskal_result)

fmc.ad.results <- lapply(split(fmc.surv.data2$age, fmc.surv.data2$site), ad.test)

fmt.ad.results <- lapply(split(fmt.surv.data2$age, fmt.surv.data2$site), ad.test)

dunn_test_results <- dunn.test(fmt.surv.data2$age, fmt.surv.data2$site, method = "bonferroni")

fmc.dunn_test_results <- dunn.test(fmc.surv.data2$age, fmc.surv.data2$site, method = "bonferroni")
