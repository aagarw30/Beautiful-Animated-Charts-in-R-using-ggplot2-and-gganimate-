# Recreated chart in R (based on original post by James Eagle in LinkedIn) 
# https://www.linkedin.com/feed/update/urn:li:activity:6919289928434511872/


# Load required libraries
library(ggplot2)
library(dplyr)
library(lubridate)
library(gganimate)
library(scales)
library(stringr)

# Read the data from csv
yield = read.csv(file = "Monthly_US_10-Year Bond Yield Historical Data.csv", header = TRUE)
str(yield)
# Create a data frame with coordiates and labels for the stock fall events
points = data.frame(x=as.Date(c("01-01-1982", "01-10-1987", "01-07-1990", "01-11-1994","01-07-1997", "01-03-2000", "01-06-2006", "01-05-2008", "01-08-2011", "01-01-2014", "01-10-2018", "01-01-2020", "01-05-2022"), format = "%d-%m-%Y"), 
                    # y=c(6.46, 8.79, 7.92, 8.35, 6.00, 4.47, 3.82, 3.30, 2.23, 2.21 , 1.16), 
                    label=c("Monetary Policy",
                            "Black Monday",
                            "Oil price increase",
                            "Orange County Bankruptcy",
                            "Asian Financial Crisis",
                            "Dotcom Bubble",
                            "Housing Bubble",
                            "Fin. Crisis",
                            "Market crash",
                            "Stocks sell-off",
                            "Market Correction",
                            "Pandemic",
                            "@2.74"
                    ))
# Rename column name
yield_new = yield %>% 
  rename("date" = "ï..Date") 

# Date processing
yield_new = yield_new %>% 
  mutate(date = as.Date(date, format = "%d-%m-%Y"))

# Merge the yield data and points dataset to get labels merged with the data
merged = left_join(x = yield_new, y =points , by = c("date" = "x"))


## Plot here 
ggplot(data = merged, 
              mapping = aes(x=date, y=Price)) +
  geom_line(color="#996515") +
  geom_text(aes(label= ifelse(!is.na(label), stringr::str_wrap(label, 5), ""), group=seq_along(date)), hjust=1, vjust=-0.55, color="white", size=2.5) +
  geom_point(aes(size=!is.na(label), group=seq_along(date)), color="white") +
  scale_size_manual(values = c(-1,1)) +
  scale_y_continuous(breaks = seq(1, 15, 1), labels = scales::number_format(accuracy = 0.1)) +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y", expand = c(0,0)) +
  labs(title ="Falling 10-year US Treasury yield dips below 3%" , 
       subtitle = "Monthly yield, % (Jan 1972 - May 2022)", 
       caption = "Data from investing.com | Chart by @abhiag30") +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.line = element_line(color="white", size=0.1),
    plot.caption = element_text(hjust=0, size = 12),
    plot.subtitle = element_text(size=12),
    axis.ticks.y  = element_line(size=0.5, color="white"), 
    axis.ticks.x  = element_line(size=0.5, color="white"),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    plot.title=element_text(face="bold", size=15),
    text=element_text(color="white"), # other texts and labels
    axis.text =element_text(color="white"), # axis ticks
    axis.text.x = element_text(angle = 90, size=10, vjust = 0.5),
    axis.text.y = element_text(size=10),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    plot.background = element_rect(fill="black")) 




