library(quantmod)   # get stock prices; useful stock analysis functions
library(xts)        # working with extensible time series 
library(rvest)      # web scraping
library(tidyverse)  # ggplot2, purrr, dplyr, tidyr, readr, tibble
library(stringr)    # working with strings
library(forcats)    # working with factors
library(lubridate)  # working with dates in tibbles / data frames
library(plotly)     # Interactive plots
library(corrplot)   


from <- "2019-01-01"
till <- "2020-09-01"

getSymbols("CABK.MC",src="yahoo",from=from, till=till)%>%get() # from yahoo 

mean_log_returns <-
  CABK.MC %>%
  Ad() %>%
  dailyReturn(type="log") %>%
  mean()

mean_log_returns

sd_log_returns <-
  CABK.MC %>%
  Ad() %>%
  dailyReturn(type="log") %>%
  sd()

sd_log_returns

# Parameters
N     <- 252 # Number of Stock Price Simulations
M     <- 10  # Number of Monte Carlo Simulations   
mu    <- mean_log_returns
sigma <- sd_log_returns
day <- 1:N
price_init <-CABK.MC[nrow(CABK.MC),6]

price_init

# Simulate prices - Montecarlo - lognormal
set.seed(123)
monte_carlo_mat <- matrix(nrow = N, ncol = M)

monte_carlo_mat[1, ] <- price_init
head(monte_carlo_mat)

for (j in 1:M) {
  for(i in 2:N) {
    monte_carlo_mat[i, j] <- monte_carlo_mat[i - 1, j] * exp(mu-sigma^2/2+sigma*rnorm(1, 0, 1))
      }
}

# Format and organize data frame
price_sim <- cbind(day, monte_carlo_mat) %>%
  timetk::tk_tbl()
price_sim

#nm <- str_c("Sim.", seq(1, M))
#works similarly to paste0 below
nm <- paste0("Sim.", seq(1, M))
nm <- c("Day", nm)

names(price_sim) <- nm
price_sim <- price_sim %>%
  gather(key = "Simulation", value = "Stock.Price", -(Day))

price_sim
# Visualize simulation
price_sim %>%
  ggplot(aes(x = Day, y = Stock.Price, Group = Simulation)) + 
  geom_line(alpha = 0.9) +
  #geom_line(alpha = 0.1) 
      ggtitle(str_c("MA: ", M, 
                " Monte Carlo Simulations for Prices Over ", N, 
                " Trading Days"))

mean(monte_carlo_mat[252,])
sd(monte_carlo_mat[252,])

ls <-mean(monte_carlo_mat[252,])+1.96*sd(monte_carlo_mat[252,])
li <-mean(monte_carlo_mat[252,])-1.96*sd(monte_carlo_mat[252,])

IC<-c(li,ls)

mean(monte_carlo_mat[252,])
IC
