library(dplyr)
library(ggplot2)

income_data <- read.csv("income.csv")
income_df <- data.frame(Year = income_data$Years, 
                        High = income_data$High.School,
                        Bachelor = income_data$Bachelor.s, 
                        Master = income_data$Masters)

data <- read.csv("data.csv")

new_df <- data.frame(X0 = data[4:55, "X0"], X = data[4:55, "X"])
new_df$X <- as.numeric(gsub(",", "", new_df$X))
new_df$X0 <- as.numeric(new_df$X0)

ggplot(new_df, aes(x = X0, y = X)) +
  geom_line() +
  scale_x_continuous(breaks = seq(1963, max(new_df$X0), 5), 
                     labels = seq(1963, max(new_df$X0), 5)) +
  scale_y_continuous(breaks = seq(5000, max(new_df$X), 500), 
                     limits = c(5000, max(new_df$X))) +
  labs(x = "Years", y = "Cost ($)", title = "Cost of Tuition over Time")


ggplot() +
  geom_line(data = income_df, aes(x = Year, y = Bachelor, color = "Bachelor's"), size = 1) +
  geom_line(data = income_df, aes(x = Year, y = Master, color = "Master's"), size = 1) +
  geom_line(data = income_df, aes(x = Year, y = High, color = "High School"), size = 1) +
  scale_x_continuous(name = "Year", breaks = seq(1960, 2019, 5)) +
  scale_y_continuous(
    name = "Yearly Income ($)"
  ) +
  labs(color = "") +
  ggtitle("Higher Education Income Over Time") +
  theme_bw()

# found same year for data year and income year (ended up having only 9 rows)
co_new_df <- new_df[ (new_df$X0 %in% income_data$Years), ]
co_new_income_data <- income_df[ (income_df$Year %in% co_new_df$X0), ]

View(co_new_df)

# find difference in bachelor salary and high school salary
bach_high <- co_new_income_data$Bachelor - co_new_income_data$High
print(bach_high)

#combine two column so difference between bachelor and high school value and tution cost is on same df
tuition <- co_new_df$X
combined_df <- cbind(co_new_income_data,bach_high, tuition)
print(combined_df$tuition)

#visualize combined data graph

plot_combined_data <- function(){
  scatter_pop <- ggplot(combined_df, aes(tuition,bach_high)) +
    geom_point()  
  scatter_pop <-scatter_pop +
    stat_smooth(
      method = "lm", 
      formula = y ~ x, 
      geom = "smooth"
    ) + 
    labs(
      title = "Tuition Costs vs Value of Bachelor's Degree in the US",
      x = "Tuition Cost ($)",
      y = "Value of Bachelor's Degree ($)", 
      caption = "portrays the trends of United States education"
    )+ 
    scale_y_continuous(labels = scales::comma)
  
  return(scatter_pop)
}

plot_combined_data()

cor(tuition, bach_high)

