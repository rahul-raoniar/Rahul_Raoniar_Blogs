library(tidyverse)


##############################
# Loading data

weight_height <- read_csv("weight_height.csv")
head(weight_height)



levels(weight_height$Gender)


library(WVPlots)

plot <- Scatter.hist(weight_height, "Height", "Weight",
            smoothmethod= "none",
            hist_color = "steelblue",
            density_color = "red",
            title="Height vs Weight")


weight = weight_height$Weight
height = weight_height$Height



new_theme <- theme(axis.text=element_text(size=14),
                   axis.title=element_text(size=20,face="bold"), legend.text=element_text(size=22),
                   legend.title = element_text(face = "bold", size = 26), legend.position="top",
                   axis.line = element_line(colour="black", size=1, lineend="square"),
                   strip.text.y = element_text(size = 12, colour = "gray30"))



##### Weight

ggplot(weight_height, aes(weight)) + 
  stat_ecdf(geom = "point")+
  scale_y_continuous(labels = scales::percent) +
  theme_classic() +
  new_theme +
  xlab("Weight") +
  ylab("Percent")


quantile(weight, c(0.5, 0.85))


## Weight ECDF Across Gender

ggplot(weight_height, aes(weight, color = Gender)) + 
  stat_ecdf(geom = "point", size = 3)+
  scale_y_continuous(labels = scales::percent) +
  theme_classic() +
  new_theme +
  xlab("Weight") +
  ylab("Percent")


male_weight = weight_height[weight_height$Gender == "Male", ]
female_weight = weight_height[weight_height$Gender == "Female", ]

quantile(male_weight$Weight, c(0.5, 0.85))
quantile(female_weight$Weight, c(0.5, 0.85))





