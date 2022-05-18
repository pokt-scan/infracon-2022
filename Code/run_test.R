library(dplyr)
library(ggplot2)
library(repr)
library(readxl)
library("scales")
library(tidyverse)
library(magrittr)
library(quantmod)
library(tseries)
library(forecast)
library(TSA)
library(Quandl)
library(Quandl)
library(ggplot2)
library(tidyverse)
library(tseries)
library(astsa)
library(lubridate)
library(foreign)
library(magrittr)
library(dplyr)
library(stats)
library(forecast)
library(here)



DATA_PATH = './dataset.csv' # dataset_2022-04-28.csv ; dataset_2022-04-14.csv ; dataset_2022-04-20.csv
TEST_DATA_PATH = './dataset_test.csv'  
OUTPUT_PATH_PLOTS = "."


CHAIN_USE = '0049' # Chain code



COLOR_LINES_0009 = "dodgerblue3"
COLOR_LINES_2_0009 = "blue"
COLOR_LINES_0040 = "chartreuse3"
COLOR_LINES_2_0040 = "green"
COLOR_LINES_0027 = "goldenrod3"
COLOR_LINES_2_0027 = "yellow"
COLOR_LINES_0005 = "tomato3"
COLOR_LINES_2_0005 = "orange"
COLOR_LINES_0021 = "hotpink4"
COLOR_LINES_2_0021 = "pink"
COLOR_LINES_0049 = "magenta2"
COLOR_LINES_2_0049 = "orchid2"


if(CHAIN_USE == '0009') {
  COLOR_LINES = COLOR_LINES_0009
  COLOR_LINES_2 = COLOR_LINES_2_0009
  
} else if( CHAIN_USE == '0040') {
  COLOR_LINES = COLOR_LINES_0040
  COLOR_LINES_2 = COLOR_LINES_2_0040
  
} else if( CHAIN_USE == '0027') {
  COLOR_LINES = COLOR_LINES_0027
  COLOR_LINES_2 = COLOR_LINES_2_0027
  
} else if( CHAIN_USE == '0005') {
  COLOR_LINES = COLOR_LINES_0005
  COLOR_LINES_2 = COLOR_LINES_2_0005
  
} else if( CHAIN_USE == '0021') {
  COLOR_LINES = COLOR_LINES_0021
  COLOR_LINES_2 = COLOR_LINES_2_0021
  
} else if( CHAIN_USE == '0049') {
  COLOR_LINES = COLOR_LINES_0049
  COLOR_LINES_2 = COLOR_LINES_2_0049
  
} else {
  COLOR_LINES = "firebrick3"
  COLOR_LINES_2 = "red"
}

COLOR_LINES_FORECAST = "black"


FORECAST_LEN = 48 # Sessions (4 blocks)
DATA_2_FORECAST_PLOT = 2 # Plots this times more sessions of data than forecast
SAVE_EXTENSION = 'png'
PLOT_WIDTH = 185.166
PLOT_HEIGHT = 114.554
PLOT_DPI = 300
PLOT_TEXT_SIZE = 15
DATE_TICK_DIST = 24*3 # Sessions
N_TOP = 6



# Read Data
datos <- read.csv(DATA_PATH)
datos_test <- read.csv(TEST_DATA_PATH)


# Print dates
print('Train data:')
print(head(datos$date, n=1))
last_date = tail(datos$date, n=1)
print(last_date)
print('Test data:')
print(head(datos_test$date, n=1))
print(tail(datos_test$date, n=1))


# Get chains with most relays 
last_block = print(tail(datos$aligned_height, n=1))
last_n_blocks = last_block-2*24

totals <- datos %>%
  filter(aligned_height>=last_n_blocks) %>%
  group_by(chain) %>%
  mutate(all_relays = sum(all_relays))  %>%
  mutate(mean_nodes = mean(amount_staked_nodes))  %>%
  mutate(mean_apps = mean(amount_staked_apps))  %>%
  slice(1) %>%
  ungroup() %>%
  mutate(prop_all_relays = (all_relays/sum(all_relays)))
totals = totals[order(-totals$prop_all_relays), ]
totals = subset(totals, select = c(chain,amount_staked_nodes,amount_staked_apps,all_relays,prop_all_relays) )
totals$prop_all_relays = totals$prop_all_relays*100.0
print(head(totals, n=N_TOP ))

# Get boxplot of all chains

# Custom number format for plots
formatCustomSci <- function(x) {     
  x_sci <- str_split_fixed(formatC(x, format = "e"), "e", 2)
  alpha <- as.numeric(x_sci[ , 1])
  power <- as.integer(x_sci[ , 2])
  paste(alpha * 1, power - 1L, sep = "e")
}

# Custom color map
group.colors <- c("0009" = COLOR_LINES_0009,
                  "0040" = COLOR_LINES_0040, 
                  "0027" = COLOR_LINES_0027,
                  "0049" = COLOR_LINES_0049, 
                  "0005" = COLOR_LINES_0005, 
                  "0021" = COLOR_LINES_0021)

ggplot(data=datos, mapping=aes(x=chain, y=all_relays, color=chain)) +
  geom_boxplot() + 
  theme(axis.text.x = element_text(angle = 45, vjust=0.65)) +
  scale_color_manual(values=group.colors) + 
  scale_y_continuous(labels=formatCustomSci) +
  xlab("Blockchain Code") + 
  ylab("Relays by Session") +
  theme(text = element_text(size = PLOT_TEXT_SIZE)) 
ggsave(paste0("boxplot.",SAVE_EXTENSION), path = OUTPUT_PATH_PLOTS, width=PLOT_WIDTH, height=PLOT_HEIGHT, dpi=PLOT_DPI, units="mm")


# Filter for selected chain and get Relays by Node by Session (prop_realys)
datos_mean <- datos %>%
  filter(chain==CHAIN_USE) %>%
  mutate(prop_realys = all_relays/amount_staked_nodes)

datos_test_mean <- datos_test %>%
  filter(chain==CHAIN_USE) %>%
  filter(date>last_date) %>%
  mutate(prop_realys = all_relays/amount_staked_nodes)



# Add ts count
datos_mean = datos_mean[order(datos_mean$date), ]
datos_mean["t"] = as.integer(rownames(datos_mean))

# Add ts count and offset using the last value of the train data
tLast = tail(datos_mean, n = 1)$t
datos_test_mean = datos_test_mean[order(datos_test_mean$date), ]
datos_test_mean["t"] = as.integer(rownames(datos_test_mean))+tLast
# keep only the test length
datos_test_mean = head(datos_test_mean, n = FORECAST_LEN)


# Create ticks for tidy dates 
breaks_idx = floor(seq(from=1, to=length(datos_mean$date), by = DATE_TICK_DIST))
breaks = datos_mean$date[breaks_idx]
x_ticks = strsplit(breaks, split = " ") %>% sapply(extract2, 1)


# Plot time series data
ggplot(datos_mean, aes(x = date, y = all_relays)) +
  geom_line(color=COLOR_LINES, group = 1) +
  theme(axis.text.x = element_text(angle = 45, vjust=0.65)) +
  theme(axis.text.y = element_text(angle = 45)) +
  scale_y_continuous(labels=formatCustomSci) +
  scale_x_discrete(breaks = breaks, labels = x_ticks) + 
  xlab("Date") + 
  ylab("Relays by Session") +
  theme(text = element_text(size = PLOT_TEXT_SIZE)) 
ggsave(paste0("relays_",CHAIN_USE,".",SAVE_EXTENSION), path = OUTPUT_PATH_PLOTS, width=PLOT_WIDTH, height=PLOT_HEIGHT, dpi=PLOT_DPI, units="mm")


ggplot(datos_mean, aes(x = date, y = amount_staked_nodes)) +
  geom_line(color=COLOR_LINES, group = 1) + 
  theme(axis.text.x = element_text(angle = 45, vjust=0.65)) +
  theme(axis.text.y = element_text(angle = 45)) +
  scale_y_continuous(labels=formatCustomSci) +
  scale_x_discrete(breaks = breaks, labels = x_ticks) + 
  xlab("Date") + 
  ylab("Staked Nodes") +
  theme(text = element_text(size = PLOT_TEXT_SIZE)) 
ggsave(paste0("nodes_",CHAIN_USE,".",SAVE_EXTENSION), path = OUTPUT_PATH_PLOTS, width=PLOT_WIDTH, height=PLOT_HEIGHT, dpi=PLOT_DPI, units="mm")

ggplot(datos_mean, aes(x = date, y = prop_realys)) +
  geom_line(color=COLOR_LINES, group = 1) + 
  theme(axis.text.x = element_text(angle = 45, vjust=0.65)) +
  theme(axis.text.y = element_text(angle = 45)) +
  scale_x_discrete(breaks = breaks, labels = x_ticks) + 
  xlab("Date") + 
  ylab("Relays by Session, Avg. by Node") +
  theme(text = element_text(size = PLOT_TEXT_SIZE)) 
ggsave(paste0("avg_",CHAIN_USE,".",SAVE_EXTENSION), path = OUTPUT_PATH_PLOTS, width=PLOT_WIDTH, height=PLOT_HEIGHT, dpi=PLOT_DPI, units="mm")


# Create time series for arima model
arima_this.ts=ts(datos_mean$prop_realys,start = c(1,1),frequency = 1)

# Test statinoarity
print(adf.test(arima_this.ts,alternative = "stationary",k=0))

# Calculate best model
modelo=auto.arima(arima_this.ts,d=1,D=1,stepwise = FALSE,approximation = FALSE) # , seasonal=FALSE
print(modelo)

print(coef(modelo))


# Compile model test data
compiled_data <- cbind(as.numeric(time(arima_this.ts)), arima_this.ts, fitted(modelo), rstandard(modelo), resid(modelo))
colnames(compiled_data) <- c("t", "ts", "fitted", "std_residual", "residual")
compiled_data <- data.frame(compiled_data)

# Compute ACF
acz <- acf(compiled_data$residual, plot=F, lag.max=7*24) # A week of lags to plot
ci <- qnorm((1 + 0.95)/2)/sqrt(sum(!is.na(arima_this.ts)))   # Get 95% lines location
acd <- data.frame(lag=acz$lag, acf=acz$acf) # Build df

# Compute PACF
pacz <- pacf(compiled_data$residual, plot=F, lag.max=7*24) # A week of lags to plot
pci <- qnorm((1 + 0.95)/2)/sqrt(sum(!is.na(arima_this.ts)))   # Get 95% lines location
pacd <- data.frame(lag=pacz$lag, acf=pacz$acf) # Build df

# Plot model tests
ggplot(compiled_data, aes(x = t, y  = residual)) +
  geom_line(color=COLOR_LINES) +
  theme(axis.text.x = element_text(angle = 45, vjust=0.65)) +
  scale_x_continuous(breaks = breaks_idx, labels = x_ticks) + 
  xlab("Date") + 
  ylab("Residuals") +
  theme(text = element_text(size = PLOT_TEXT_SIZE)) 
ggsave(paste0("residuals_",CHAIN_USE,".",SAVE_EXTENSION), path = OUTPUT_PATH_PLOTS, width=PLOT_WIDTH, height=PLOT_HEIGHT, dpi=PLOT_DPI, units="mm")

ggplot(compiled_data, aes(sample = residual)) +
  stat_qq(color=COLOR_LINES) + 
  stat_qq_line() +
  xlab("Normal quantiles") + 
  ylab("Residuals quantiles") +
  theme(text = element_text(size = PLOT_TEXT_SIZE)) 
ggsave(paste0("QQ_",CHAIN_USE,".",SAVE_EXTENSION), path = OUTPUT_PATH_PLOTS, width=PLOT_WIDTH, height=PLOT_HEIGHT, dpi=PLOT_DPI, units="mm")

ggplot(compiled_data, aes(x=residual)) + 
  geom_histogram(bins = 32,aes(y = ..density..), fill=COLOR_LINES, color=COLOR_LINES_2) +
  stat_function(fun = dnorm,
                args = list(mean = mean(compiled_data$residual),
                            sd = sd(compiled_data$residual)),
                col = "black",
                size = 1) +
  xlab("Residual") + 
  ylab("Density") +
  theme(text = element_text(size = PLOT_TEXT_SIZE)) 
ggsave(paste0("error_hist_",CHAIN_USE,".",SAVE_EXTENSION), path = OUTPUT_PATH_PLOTS, width=PLOT_WIDTH, height=PLOT_HEIGHT, dpi=PLOT_DPI, units="mm")

ggplot(acd, aes(lag, acf)) +
  geom_hline(aes(yintercept=0)) +
  geom_segment(aes(lag,acf,xend=lag,yend=acf-acf), color=COLOR_LINES) +
  geom_hline(yintercept=c(ci, -ci), linetype="dashed") +
  xlab("Lag") + 
  ylab("ACF") +
  theme(text = element_text(size = PLOT_TEXT_SIZE)) 
ggsave(paste0("ACF_",CHAIN_USE,".",SAVE_EXTENSION), path = OUTPUT_PATH_PLOTS, width=PLOT_WIDTH, height=PLOT_HEIGHT, dpi=PLOT_DPI, units="mm")



ggplot(pacd, aes(lag, acf)) +
  geom_hline(aes(yintercept=0)) +
  geom_segment(aes(lag,acf,xend=lag,yend=acf-acf), color=COLOR_LINES) +
  geom_hline(yintercept=c(pci, -pci), linetype="dashed") +
  xlab("Lag") + 
  ylab("PACF") +
  theme(text = element_text(size = PLOT_TEXT_SIZE)) 
ggsave(paste0("PACF_",CHAIN_USE,".",SAVE_EXTENSION), path = OUTPUT_PATH_PLOTS, width=PLOT_WIDTH, height=PLOT_HEIGHT, dpi=PLOT_DPI, units="mm")



# Predict 
prediccion=forecast(modelo,h=FORECAST_LEN,level = c(95))
prediccion=as.data.frame(prediccion)

# Create dataframe for plot
colnames(prediccion) <- c("forecast", "l95", "h95")
prediccion["t"] = as.integer(rownames(prediccion))
# Add last row for continuity in plot
patch_row = data.frame( tail(datos_mean["prop_realys"],n=1)$prop_realys, tail(datos_mean["prop_realys"],n=1)$prop_realys, tail(datos_mean["prop_realys"],n=1)$prop_realys, tail(datos_mean["t"],n=1)$t)
colnames(patch_row) <- c("forecast", "l95", "h95", "t")
prediccion <- rbind(prediccion, patch_row)


# Get last N samples from dataset to plot
last_rows = tail(datos_mean, n = DATA_2_FORECAST_PLOT*FORECAST_LEN)

# Create indexes for plot dates ticks
last_t = tail(datos_mean["t"],n=1)$t
breaks_idx_tail = floor(seq(from=last_t-nrow(last_rows), to=last_t, by = 24))
breaks_tail = datos_mean$date[breaks_idx_tail]
x_ticks_tail = strsplit(breaks_tail, split = " ") %>% sapply(extract2, 1)

# Plot forecast
ggplot(prediccion, aes(x = t, y = forecast)) +
    geom_line(color=COLOR_LINES_FORECAST,linetype="dashed") +
    geom_line(data=last_rows, aes(x = t, y = prop_realys), color=COLOR_LINES, group = 1) +
  geom_line(data=datos_test_mean, aes(x = t, y = prop_realys), color=COLOR_LINES_2, group = 1) +
  scale_x_continuous(breaks = breaks_idx_tail, labels = x_ticks_tail) + 
  theme(axis.text.x = element_text(angle = 45, vjust=0.65)) +
  geom_ribbon(data=prediccion,
              aes(x=t,ymin=l95,ymax=h95),
              inherit.aes=FALSE,alpha=0.3,color="grey30") +
  xlab("Date") + 
  ylab("Relays by Session, Avg. by Node") +
  theme(text = element_text(size = PLOT_TEXT_SIZE)) 
ggsave(paste0("forecast_",CHAIN_USE,".",SAVE_EXTENSION), path = OUTPUT_PATH_PLOTS, width=PLOT_WIDTH, height=PLOT_HEIGHT, dpi=PLOT_DPI, units="mm")

