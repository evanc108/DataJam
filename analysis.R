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
  labs(x = "Years", y = "Cost", title = "Line Graph of Cost over Time")


ggplot() +
  geom_line(data = income_df, aes(x = Year, y = Bachelor, color = "Bachelor's"), size = 1) +
  geom_line(data = income_df, aes(x = Year, y = Master, color = "Master's"), size = 1) +
  geom_line(data = income_df, aes(x = Year, y = High, color = "High School"), size = 1) +
  scale_x_continuous(name = "Year", breaks = seq(1960, 2019, 5)) +
  scale_y_continuous(
    name = "Income (thousands of dollars)"
  ) +
  labs(color = "") +
  ggtitle("Higher Education Income Over Time") +
  theme_bw() +
  theme(
    legend.position = "bottom",
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )

