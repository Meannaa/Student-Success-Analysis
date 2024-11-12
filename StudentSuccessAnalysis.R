library(readr)
library(ggplot2)
# Question One 
setwd("/Users/MeannaSolomon/Downloads")
uci_student <- read_csv("uci_student.csv")
View(uci_student)
read_csv("student_math.csv")
math <- read_csv("student_math.csv")

library(dplyr)
#Question 1- chi-square goodness of fit test

# Separate into separate groups
men_data <- filter(math, sex == "M")
women_data <- filter(math, sex == "F")

# Sum of number of failures for men and women
failures_men <- sum(men_data$failures >0)
failures_women <- sum(women_data$failures >0)

# Contingency table with observed frequencies
observed_freq <- c(failures_men, failures_women)

# Calculate the expected frequencies
sum_failure <- sum(observed_freq)
expected_freq <- rep(sum_failure / 2, 2)

# Conduct the Chi-Squared Goodness of Fit Test
chisq_test <- chisq.test(observed_freq)
print(chisq_test)

# Question Two 

# Test Normality 
shapiro.test(uci_student$G3.x)
shapiro.test(uci_student$G3.y)

# Histogram for original Math Grades
ggplot(data = uci_student, aes(x = G3.x)) +
  geom_histogram(binwidth = 1, fill = "lightblue", color = "black") +
  labs(title = "Histogram of Original G3 Math Grades",
       x = "G3 Math Grades",
       y = "Frequency") +
  theme_minimal()

# Histogram for original Portuguese Grades
ggplot(data = uci_student, aes(x = G3.y)) +
  geom_histogram(binwidth = 1, fill = "lightgreen", color = "black") +
  labs(title = "Histogram of Original G3 Portuguese Grades",
       x = "G3 Portuguese Grades",
       y = "Frequency") +
  theme_minimal()

# Applying square root transformation
uci_student$G3.x_sqrt <- sqrt(uci_student$G3.x)
uci_student$G3.y_sqrt <- sqrt(uci_student$G3.y)

# Check the transformation by viewing the first few rows of the new columns
head(uci_student$G3.x_sqrt)
head(uci_student$G3.y_sqrt)

# Visualize Transformed Data

# Histogram of sqrt-transformed Math Grades
ggplot(data = uci_student, aes(x = G3.x_sqrt)) +
  geom_histogram(binwidth = 0.5, fill = "lightblue", color = "black") +
  labs(title = "Histogram of sqrt-transformed G3 Math Grades",
       x = "sqrt(Transformed G3 Math Grades)",
       y = "Frequency") +
  theme_minimal()

# Histogram for sqrt-transformed Portuguese Grades
ggplot(data = uci_student, aes(x = G3.y_sqrt)) +
  geom_histogram(binwidth = 0.5, fill = "lightgreen", color = "black") +
  labs(title = "Histogram of Sqrt-Transformed G3 Portuguese Grades",
       x = "Sqrt(Transformed G3 Portuguese Grades)",
       y = "Frequency") +
  theme_minimal()



# Convert the variables to factors
uci_student$Dalc.y <- as.factor(uci_student$Dalc.y)
uci_student$failures.y <- as.factor(uci_student$failures.y)

# Homogeneity of Variances 
library(car)
leveneTest(G3.y ~ Dalc.y * failures.y, data = uci_student)

# Conduct Anova

# Converting numeric variables to factors
uci_student$Dalc.x <- as.factor(uci_student$Dalc.x)
uci_student$failures.x <- as.factor(uci_student$failures.x)

# Rebuild the ANOVA model with these factors

aov_por <- aov(G3.y ~ Dalc.y + failures.y, data=uci_student)
summary(aov_por)

aov_math <- aov(G3.x ~ Dalc.y + failures.y, data=uci_student)
summary(aov_math)
summary(aov_por)

# Conduct TukeyHSD Test 

TukeyHSD(aov_por)

TukeyHSD(aov_math)


# Question 3 

# Correlation between G1 and G2, and G1 and G3 for Math
cor.test(uci_student$G1.x, uci_student$G2.x)
cor.test(uci_student$G1.x, uci_student$G3.x)

# Correlation between G1 and G2, and G1 and G3 for Portuguese
cor.test(uci_student$G1.y, uci_student$G2.y)
cor.test(uci_student$G1.y, uci_student$G3.y)

t.test(uci_student$G1.x, uci_student$G1.y, paired = FALSE, var.equal = FALSE)






