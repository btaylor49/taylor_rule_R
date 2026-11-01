install.packages("quantmod")
install.packages("ggplot2")
install.packages("plotly")
install.packages("forecast")
install.packages("lmtest")

library(quantmod)
library(ggplot2)
library(plotly)
library(forecast)
library(lmtest)

# Download the data
getSymbols("FEDFUNDS", src = "FRED")

# Check first 6 observations
head(FEDFUNDS)

# Download data. GDP deflator, real GDP, potential GDP. 
# Check values
getSymbols("GDPDEF", src = "FRED")
getSymbols("GDPC1", src = "FRED")
getSymbols("GDPPOT", src = "FRED")
head(GDPDEF)
head(GDPC1)
head(GDPPOT)

# Convert FFR into quarterly averages to match data and check
FEDFUNDS_quarterly <- apply.quarterly(FEDFUNDS, FUN = colMeans)
head(FEDFUNDS_quarterly)

# Give series a year and quarter label
index(FEDFUNDS_quarterly) <- as.yearqtr(index(FEDFUNDS_quarterly))
index(GDPDEF) <- as.yearqtr(index(GDPDEF))
index(GDPC1) <- as.yearqtr(index(GDPC1))
index(GDPPOT) <- as.yearqtr(index(GDPPOT))

# Combine the four series. quarters is present in all four
taylor_data <- merge (
  FEDFUNDS_quarterly, GDPDEF, GDPC1, GDPPOT,
  all = FALSE
)

# Create new column titled inflation. Calculate inflation using GDP deflator
# We want GDP from the year prior in the same quarter
# Lag function allows us to shift 4 quarters back with k = 4 to match quarters 
# Subtract 1 for percent growth
taylor_data$inflation <- (taylor_data$GDPDEF / lag(taylor_data$GDPDEF, k = 4) - 1) * 100

# Create new column for output gap 
# Calculate output gap with real gdp and potential gdp
taylor_data$output_gap <- ((taylor_data$GDPC1 - taylor_data$GDPPOT) / taylor_data$GDPPOT) * 100

# Create new data frame for our selected years of study 1987 - 2007 and check values
study_data <- taylor_data["1987/2007"]
head(study_data)
tail(study_data)
nrow(study_data)

# Now we calculate using the Taylor Rule
# Create a new column in study_data df and then calculate optimal interest rate
# Prescribed rate of 2%
# FFR = 2 + inflation + 0.5 (inflation - 2) + 0.5 (output gap)
study_data$taylor_rate <- 2 + study_data$inflation + 0.5 * (study_data$inflation - 2) + 0.5 * study_data$output_gap
head(study_data)

# Add year column in a new df for ggplot
plot_data <- data.frame(
  year = as.Date(index(study_data)),
  coredata(study_data)
)

plot_data$period <- paste0(
  "Q", format(as.yearqtr(plot_data$year), "%q"),
  " ", format(plot_data$year, "%B %Y")
)

# Plot FFR alongside taylor rate, create a tool tip
p <- ggplot(plot_data, aes(x = year, group = 1)) +
   geom_line(aes(
     y = FEDFUNDS, color = "Federal Funds Rate",
     text = paste0(period, "<br>", sprintf("%.2f%%", FEDFUNDS)) 
     )) +
    geom_line(aes(
      y = taylor_rate, color = "Taylor Rule",
      text = paste0(period, "<br>", sprintf("%.2f%%", taylor_rate))
    )) +
    scale_color_manual(
    values = c(
      "Federal Funds Rate" = "red",
      "Taylor Rule" = "blue"
    )
  ) +
  theme(
    legend.position = "none",
    plot.title = ggtext::element_markdown()
  ) +
  labs(
    x = NULL,
    y = NULL,
    title = paste0(
      "<b><span style='color:red'>Federal Funds Rate</span> vs. ",
      "<span style='color:blue'>Taylor Rule</span> (%)</b>",
      "<br><span style='font-size:14px;color:black'>1987 Q1–2007 Q4</span>"
    )
  )

# More aesthetics for tool tip box
# Adding a boarder, changing the background color
interactive_plot <- plotly::ggplotly(p, tooltip = "text")

interactive_plot <- plotly::style(
  interactive_plot,
  hoverlabel = list(
    bgcolor = "white",
    bordercolor = "red",
    font = list(color = "black")
  ),
  traces = 1
)

interactive_plot <- plotly::style(
  interactive_plot,
  hoverlabel = list(
    bgcolor = "white",
    bordercolor = "blue",
    font = list(color = "black")
  ),
  traces = 2
)
interactive_plot <- plotly::layout(
  interactive_plot,
  title = list(
    text = paste0(
      "<b><span style='color:red'>Federal Funds Rate</span> vs. ",
      "<span style='color:blue'>Taylor Rule</span> (%)</b>",
      "<br>1987 Q1–2007 Q4"
    ),
    font = list(size = 20),
    yref = "container",
    y = 0.97,
    yanchor = "top"
  ),
  margin = list(t = 60)
)
interactive_plot

# Taylor rule in the form of a OLS regression
ols_model <- lm(
  FEDFUNDS ~ inflation + output_gap,
  data = as.data.frame(study_data)
)

# Print results
summary(ols_model)

# Plot residuals across time using ggplot
p_residuals <- ggplot(plot_data, aes(x = year, group = 1)) +
  geom_line(aes(
   y = residuals(ols_model), color = "Residuals (percentage points)",
   text = paste0(period, "<br>", sprintf("%.2f percentage points", residuals(ols_model)))
  )) +
  geom_hline(
    yintercept = 0,
    color = "red",
    linetype = "dashed"
  ) +
  scale_color_manual(
    values = c("Residuals (percentage points)" = "black")
  ) +
  labs(
    title = "<b>Residuals Across Time<b>"
  ) +
  theme(
    legend.position = "none",
    plot.title = ggtext::element_markdown()
  )

# Add border and change background color of tooltip
interactive_plot <- plotly::ggplotly(p_residuals, tooltip = "text")

interactive_plot <- plotly::style( 
  interactive_plot,
  hoverlabel = list(
    bgcolor = "white",
    bordercolor = "black",
    font = list(color = "black")
  ),
  traces = 1
)
interactive_plot

# Checking for autocorrelation
acf_residuals <- acf(
  residuals(ols_model),
  lag.max = 12,
  plot = FALSE
)

# Organize results in dataframe
acf_data <- data.frame(
  lag = as.numeric(acf_residuals$lag),
  correlation = as.numeric(acf_residuals$acf)
)

# 95% bounds with no autocorrelation
p_acf <- forecast::autoplot(acf_residuals, ci = 0.95) +
labs(
  title = "<b>Autocorrelation of Residuals<b>",
  x = "Lag (Quarter)",
  y = NULL,
) +
  theme(
    legend.position = "none",
    plot.title = ggtext::element_markdown()
  )
p_acf

# Conduct Breusch Godfrey test
lmtest::bgtest(ols_model, order = 4)

# Due to autocorrelation in the model, we will add lags to the original ols
study_data$FFR_lag1 <- c(
  NA,
  head(as.numeric(study_data$FEDFUNDS), -1)
)

# Estimate our new model
ols_lag <- lm(
  FEDFUNDS ~ inflation + output_gap + FFR_lag1,
  data = as.data.frame(study_data)
)
summary(ols_lag)

# Check for autocorrelation once more
lmtest::bgtest(ols_lag, order = 4)

# We still have autocorrelation despite using a lagged a model





