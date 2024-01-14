library(ggplot2)

IQGPA.data <- read.csv("gpa_iq.csv")

str(IQGPA.data)

IQGPA.data$gender_label <- ifelse(IQGPA.data$gender == 1, "Male", "Female")

cor_coeff <- cor(IQGPA.data$iq, IQGPA.data$gpa, use = "complete.obs")
ggplot(IQGPA.data, aes(x = iq, y = gpa, color = gender_label)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  annotate("text", x = Inf, y = Inf, label = paste("R =", round(cor_coeff, 2)), 
           hjust = 1.1, vjust = 1.1, size = 4) +
  labs(title = "Scatter Plot of GPA vs. IQ", x = "IQ", y = "GPA") +
  theme_minimal()

mean_iq_by_gender <- IQGPA.data %>%
  group_by(gender_label) %>%
  summarise(mean_IQ = mean(iq, na.rm = TRUE))

ggplot(IQGPA.data, aes(x = iq, fill = gender_label)) +
  geom_density(alpha = 0.5) +
  geom_vline(data = mean_iq_by_gender, aes(xintercept = mean_IQ, color = gender_label), linetype = "dashed") +
  facet_wrap(~ gender_label) +
  labs(title = "Density Plot of IQ by Gender", x = "IQ", y = "Density")

mean_gpa_by_gender <- IQGPA.data %>%
  group_by(gender_label) %>%
  summarise(mean_GPA = mean(gpa, na.rm = TRUE))

ggplot(IQGPA.data, aes(x = gpa, fill = gender_label)) +
  geom_density(alpha = 0.5) +
  geom_vline(data = mean_gpa_by_gender, aes(xintercept = mean_GPA, color = gender_label), linetype = "dashed") +
  facet_wrap(~ gender_label) +
  labs(title = "Density Plot of GPA by Gender", x = "GPA", y = "Density")




library(lmtest)
regression_model <- lm(gpa ~ iq + gender_label, data = IQGPA.data)
summary(regression_model)
bptest(regression_model)  # Breusch-Pagan test for heteroskedasticity

install.packages("FactoMineR")
library(FactoMineR)
pca_results <- PCA(IQGPA.data[, c("gpa", "iq")], graph = TRUE)
print(pca_results$eig)
print(pca_results$call$centre)
print(pca_results$var)
print(pca_results$var$cor)
print(pca_results$call)


install.packages("GGally")
library(GGally)
IQGPA.data$gender_label <- as.factor(IQGPA.data$gender_label)

ggpairs(IQGPA.data[, c("gpa", "iq", "gender_label")], 
        mapping = ggplot2::aes(colour = gender_label))


library(corrplot)
correlations <- cor(IQGPA.data[, c("gpa", "iq")]) +

corrplot(correlations, method = "number") +
labs(title = "IQ_GPA correlations heatmap") 

library(ggridges)
ggplot(IQGPA.data, aes(x = gpa, y = gender_label, fill = gender_label)) +
  geom_density_ridges()

ggplot(IQGPA.data, aes(x = gender_label, y = gpa, fill = gender_label)) +
  geom_violin() +
  geom_jitter(width = 0.2, alpha = 0.5, color = "black", size = 1) +
  geom_boxplot(width = 0.1) +
  labs(x = "Gender", y = "GPA", title = "Distribution of GPA Scores by Gender", fill = "Gender")

ggplot(IQGPA.data, aes(x = gender_label, y = iq, fill = gender_label)) +
  geom_violin() +
  geom_jitter(width = 0.2, alpha = 0.5, color = "black", size = 1) +
  geom_boxplot(width = 0.1) +
  labs(x = "Gender", y = "IQ", title = "Distribution of IQ Scores by Gender", fill = "Gender")

t_test_gpa <- t.test(gpa ~ gender_label, data = IQGPA.data)
print(t_test_gpa)

t_test_iq <- t.test(iq ~ gender_label, data = IQGPA.data)
print(t_test_iq)

cor_test <- cor.test(IQGPA.data$gpa, IQGPA.data$iq)
print(cor_test)  


model <- lm(gpa ~ iq, data = IQGPA.data)
summary(model)

plot(model$residuals)
plot(model$fitted.values, model$residuals)
abline(h = 0, col = "red")
qqnorm(model$residuals)
qqline(model$residuals)
plot(model$fitted.values, sqrt(abs(model$residuals)))
plot(hatvalues(model))
plot(model, which = 3)
print(plot(model, which = 3))

qqnorm(IQGPA.data$iq, main = "Q-Q Plot for IQ")
qqline(IQGPA.data$iq, col = "black")

qqnorm(IQGPA.data$gpa, main = "Q-Q Plot for GPA")
qqline(IQGPA.data$gpa, col = "black")

library(nortest)

male_gpa_data <- IQGPA.data[IQGPA.data$gender_label == "Male", "gpa"]
male_ad_test <- ad.test(male_gpa_data)
print(male_ad_test)

female_gpa_data <- IQGPA.data[IQGPA.data$gender_label == "Female", "gpa"]
female_ad_test <- ad.test(female_gpa_data)
print(female_ad_test)


male_iq_data <- IQGPA.data[IQGPA.data$gender_label == "Male", "gpa"]
male_iq_test <- ad.test(male_iq_data)
print(male_ad_test)

female_iq_data <- IQGPA.data[IQGPA.data$gender_label == "Female", "iq"]
female_ad_test <- ad.test(female_iq_data)
print(female_ad_test)

qqnorm(male_iq_data, main = "Q-Q Plot for Male IQ")
qqline(male_iq_data, col = "black")

qqnorm(female_iq_data, main = "Q-Q Plot for Female IQ")
qqline(female_iq_data, col = "black")


qqnorm(male_gpa_data, main = "Q-Q Plot for Male GPA")
qqline(male_gpa_data, col = "black")

qqnorm(female_gpa_data, main = "Q-Q Plot for Female GPA")
qqline(female_gpa_data, col = "black")
