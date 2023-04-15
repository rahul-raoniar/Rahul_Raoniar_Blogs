############################################
# Load libraries
############################################
library(MASS)
library(foreign)
library(tidyverse)
library(margins)

############################################
# Data Preparation
############################################

data <- read_csv("admission.csv")
head(data)

# apply (1/2/3): It has 3 levels namely “unlikely”, “somewhat likely”, and “very likely”, 
       # coded in 1, 2, and 3 respectively.
       # 3 being highest and 1 being lowest.
# gpa: grade point average(gpa)
# pared (0/1): a binary variable that indicates if at 
             # least one parent went to graduate school.
             # 1 indicates at least one parent went to graduate school
# public (0/1): refers to the type of undergraduate institute
             # 1 indicates went to public institute

# Data types
str(data)

data$apply <- ordered(data$apply, levels = c("unlikely", "somewhat likely", "very likely"))
data$pared <- as.factor(data$pared)
data$public <- as.factor(data$public)

levels(data$apply)
levels(data$pared)
levels(data$public)

############################################
# Fitting model
############################################

model <- polr(apply ~ pared + public + gpa, data = data, Hess = TRUE)
summary(model)

############################################
# Model interpretation
############################################

#Equations
#logit(P(Y<=1)) = logit(F_unlikely) = 2.20 - (1.05 * Pared - 0.06 * Public + 0.616 * GPA)
#logit(P(Y<=2)) = logit(F_somewhat_likely) = 4.30 - (1.05 * Pared - 0.06 * Public + 0.616 * GPA)


# Adding P-value
(ctable <- coef(summary(model)))
p <- pnorm(abs(ctable[,"t value"]), lower.tail = FALSE) * 2
(ctable <- cbind(ctable,
                 "p-value" = round(p, digits = 2)))


# Calculating 95% CI

(ci <- confint(model))


# Odd ratios
round(exp(coef(model)), digits = 2)

############################################
# Case study of observation
############################################

# Take the second observation
pared = 1 # one parent went to graduate school
public = 0 # attended a private institution
gpa = 3.21 # gpa secured

F_unlikely = 2.20 - (1.05 * pared - 0.06 * public + 0.616 * gpa)
F_unlikely

F_somewhat = 4.30 - (1.05 * pared - 0.06 * public + 0.616 * gpa)
F_somewhat

# Convert to odds
Odds_F_unlikely = exp(F_unlikely)
Odds_F_unlikely

Odds_F_somewhat = exp(F_somewhat)
Odds_F_somewhat

# Convert to probabilities
F_unlikely = Odds_F_unlikely / (1 + Odds_F_unlikely)
F_unlikely

F_somewhat = Odds_F_somewhat / (1 + Odds_F_somewhat)
F_somewhat

# Get probabilities for individual categories based on the concept of cumulative 
# probabilities

# for unlikely i.e., P(unlikely)
P_unlikely = F_unlikely
P_unlikely

# for somewhat likely i.e., P(somewhat) = P(Y<=2) - P(Y<=1)
P_somewhat_likely = F_somewhat - F_unlikely
P_somewhat_likely

# for somewhat likely i.e., P(very) = 1 - P(Y<=2)
P_very_likely = 1- F_somewhat
P_very_likely


############################################
# Plotting probabilities for the trained data
############################################

data1 <- cbind(data, predict(model, data, type = "probs"))
head(data1)

plotdata <- data1 %>% 
    pivot_longer(
        cols = c("unlikely", "somewhat likely", "very likely"),
        names_to = "Level",
        values_to = "Probability"
    )

head(plotdata)

# Plotting probabilities
ggplot(plotdata, aes(x = gpa, y = Probability, color = Level)) +
    geom_line(size = 0.8) +
    facet_grid(pared ~ public, labeller = "label_both") +
    theme_bw()

ggsave("original_data_probs.png",
       width = 20,
       height = 15,
       units = "cm",
       dpi = 600)


############################################
# Plotting the probabilities for a new dataset
############################################
newdata <- data.frame(
    pared = rep(0:1, 200),
    public = rep(0:1, each = 200),
    gpa = rep(seq(from = 1.9, to = 4, length.out = 100), 4)
)

newdata$pared <- as.factor(newdata$pared)
newdata$public <- as.factor(newdata$public)

newdata <- cbind(newdata, predict(model, newdata, type = "probs"))

head(newdata)


# Wide to long
plotdata1 <- newdata %>% 
    pivot_longer(
        cols = c("unlikely", "somewhat likely", "very likely"),
        names_to = "Level",
        values_to = "Probability"
        )

head(plotdata1)


# Plotting probabilities
ggplot(plotdata1, aes(x = gpa, y = Probability, color = Level)) +
    geom_line() +
    facet_grid(pared ~ public, labeller = "label_both") +
    theme_bw()


ggsave("new_data_probs.png",
       width = 20,
       height = 15,
       units = "cm",
       dpi = 600)

