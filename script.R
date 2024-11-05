library(readxl)
library(dplyr)
library(ggplot2)

#The path needs to be specified
sales_data <- read_excel("/Sales Data.xlsx", sheet = "Sheet1")

caribbean_green <- "#00c8a9"

#Add days
# sales_data$TRAN_DAY <- weekdays(as.Date(sales_data$TRAN_DATE))

#Question A
foods_data <- sales_data %>%
  filter(BUSINESS_UNIT_NAME == 'FOODS')

result <- sum(foods_data['TOTAL_SALES'])/(366*24*60*60)

daily_sales <- foods_data %>%
  group_by(TRAN_DATE) %>%
  summarise(total_sales = sum(TOTAL_SALES))

daily_sales <- daily_sales %>%
  arrange(TRAN_DATE) %>%
  mutate(cumulative_sales = cumsum(total_sales))

total_cumulative_sales <- tail(daily_sales$cumulative_sales, 1)

ggplot(daily_sales, aes(x = TRAN_DATE, y = cumulative_sales)) +
  geom_line(color = caribbean_green, size = 1.2) +
  geom_point(color = "darkgreen", size = 2) +
  geom_area(fill = caribbean_green, alpha = 0.3) +
  geom_text(aes(label = paste("Total:", total_cumulative_sales), 
                x = max(TRAN_DATE), 
                y = total_cumulative_sales), 
            vjust = -1, hjust = 1.1, color = "black", size = 4) +
  labs(title = "Cumulative Total Sales Over Time (FOODS Business Unit)", 
       x = "Date", y = "Cumulative Total Sales") +
  theme_minimal(base_size = 14)

#Question B

filter_cats <- sales_data[sales_data$BUSINESS_UNIT_NAME %in% c("CLOTHING","BEAUTY","HOME"),]
total_sales_per_day <- filter_cats %>% group_by(TRAN_DAY) %>% summarise(total_sales = sum(TOTAL_SALES))

max_day <- total_sales_per_day %>%
  filter(total_sales == max(total_sales)) %>%
  pull(TRAN_DAY)

ggplot(total_sales_per_day, aes(x = TRAN_DAY, y = total_sales, fill = (TRAN_DAY == max_day))) +
  geom_bar(stat = "identity", color = "black", width = 0.7) +
  scale_fill_manual(values = c("TRUE" = "darkred", "FALSE" = caribbean_green), guide = "none") +
  labs(title = "Total Sales per Day of the Week (FBH)", x = "Day of the Week", y = "Total Sales") +
  theme_minimal(base_size = 14)

#Question C
total_sales_per_group <- sales_data %>% group_by(GROUP_NAME) %>% summarise(total_items = sum(ITEMS))

max_group <- total_sales_per_group %>%
  filter(total_items == max(total_items)) %>%
  pull(GROUP_NAME)

ggplot(total_sales_per_group, aes(x = reorder(GROUP_NAME, total_items), y = total_items, fill = (GROUP_NAME == max_group))) +
  geom_bar(stat = "identity", color = "black", width = 0.7) +
  scale_fill_manual(values = c("TRUE" = "darkred", "FALSE" = caribbean_green), guide = "none") +
  labs(title = "Total Items Sold per Product Group", x = "Product Group", y = "Total Items Sold") +
  theme_minimal(base_size = 14) +
  coord_flip()

#Question D
last_quarter <- sales_data[sales_data$TRAN_DATE >= '2020-10-01' & sales_data$TRAN_DATE <= '2020-12-31',]
sale_values <- last_quarter %>% group_by(SUBGROUP_NAME) %>% summarise(total_sales = sum(TOTAL_SALES))

max_subgroup <- sale_values %>%
  filter(total_sales == max(total_sales)) %>%
  pull(SUBGROUP_NAME)

ggplot(sale_values, aes(x = reorder(SUBGROUP_NAME, total_sales), y = total_sales, fill = (SUBGROUP_NAME == max_subgroup))) +
  geom_bar(stat = "identity", color = "black", width = 0.7) +
  scale_fill_manual(values = c("TRUE" = "darkred", "FALSE" = caribbean_green), guide = "none") +
  labs(title = "Total Sales per Sub-Group in Q4 2020", x = "Sub-Group", y = "Total Sales") +
  theme_minimal(base_size = 14) +
  coord_flip()

#Question E
time_series_data_1 <- sales_data %>%
  group_by(TRAN_DATE, GROUP_NAME) %>%  
  summarize(total_sales = sum(TOTAL_SALES, na.rm = TRUE)) %>%
  ungroup()

ggplot(time_series_data_1, aes(x = TRAN_DATE, y = total_sales)) +
  geom_line(color = "blue") +
  geom_point(color = "red") +
  labs(title = "Time Plot of Sales by Group", x = "Date", y = "Total Sales") +
  theme_minimal() +
  facet_wrap(~ GROUP_NAME, scales = "free_y")

time_series_data_2 <- sales_data %>%
  group_by(TRAN_DATE, GROUP_NAME) %>%  
  summarize(total_items = sum(ITEMS, na.rm = TRUE)) %>%
  ungroup()

ggplot(time_series_data_2, aes(x = TRAN_DATE, y = total_items)) +
  geom_line(color = "blue") +
  geom_point(color = "red") +
  labs(title = "Time Plot of Items by Group", x = "Date", y = "Total Items") +
  theme_minimal() +
  facet_wrap(~ GROUP_NAME, scales = "free_y")

time_series_data_3 <- sales_data %>%
  group_by(TRAN_DATE, SUBGROUP_NAME) %>%  
  summarize(total_sales = sum(TOTAL_SALES, na.rm = TRUE)) %>%
  ungroup()

ggplot(time_series_data_3, aes(x = TRAN_DATE, y = total_sales)) +
  geom_line(color = "blue") +
  geom_point(color = "red") +
  labs(title = "Time Plot of Sales by Sub Group", x = "Date", y = "Total Sales") +
  theme_minimal() +
  facet_wrap(~ SUBGROUP_NAME, scales = "free_y")

time_series_data_4 <- sales_data %>%
  group_by(TRAN_DATE, SUBGROUP_NAME) %>%  
  summarize(total_items = sum(ITEMS, na.rm = TRUE)) %>%
  ungroup()


ggplot(time_series_data_4, aes(x = TRAN_DATE, y = total_items)) +
  geom_line(color = "blue") +
  geom_point(color = "red") +
  labs(title = "Time Plot of Items by Sub Group", x = "Date", y = "Total Items") +
  theme_minimal() +
  facet_wrap(~ SUBGROUP_NAME, scales = "free_y")

time_series_data_5 <- sales_data %>%
  group_by(TRAN_DATE, BUSINESS_UNIT_NAME) %>%  
  summarize(total_sales = sum(TOTAL_SALES, na.rm = TRUE)) %>%
  ungroup()

ggplot(time_series_data_5, aes(x = TRAN_DATE, y = total_sales)) +
  geom_line(color = "blue") +
  geom_point(color = "red") +
  labs(title = "Time Plot of Sales by BUSINESS UNIT", x = "Date", y = "Total Sales") +
  theme_minimal() +
  facet_wrap(~ BUSINESS_UNIT_NAME, scales = "free_y")

time_series_data_6 <- sales_data %>%
  group_by(TRAN_DATE, BUSINESS_UNIT_NAME) %>%  
  summarize(total_items = sum(ITEMS, na.rm = TRUE)) %>%
  ungroup()

ggplot(time_series_data_6, aes(x = TRAN_DATE, y = total_items)) +
  geom_line(color = "blue") +
  geom_point(color = "red") +
  labs(title = "Time Plot of Items by BUSINESS UNIT", x = "Date", y = "Total Items") +
  theme_minimal() +
  facet_wrap(~ BUSINESS_UNIT_NAME, scales = "free_y")

#add 'months' column
sales_data <- sales_data %>% mutate(month = format(TRAN_DATE, "%Y-%m"))

monthly_summary <- sales_data %>%
  group_by(month) %>%
  summarize(
    total_sales = sum(TOTAL_SALES, na.rm = TRUE),
    total_items = sum(ITEMS, na.rm = TRUE)
  ) %>%
  ungroup()

monthly_summary$month <- as.Date(paste0(monthly_summary$month, "-01"))

ggplot(monthly_summary, aes(x = month, y = total_sales)) +
  geom_line(color = "blue", size = 1) +
  geom_point(color = "red") +
  labs(title = "Total Sales Over Time", x = "Month", y = "Total Sales") +
  theme_minimal()

ggplot(monthly_summary, aes(x = month, y = total_items)) +
  geom_line(color = "darkgreen", size = 1) +
  geom_point(color = "orange") +
  labs(title = "Total Items Sold Over Time", x = "Month", y = "Total Items Sold") +
  theme_minimal()

marker_dates <- as.Date(c("2020-03-05","2020-03-26", "2020-04-30", "2020-05-01", "2020-05-31", 
                          "2020-06-01", "2020-08-17", "2020-08-18", "2020-09-21", 
                          "2020-12-28", "2020-12-29", "2021-02-28"))

ggplot(monthly_summary, aes(x = month, y = total_sales)) +
  geom_line(color = "blue", size = 1) +
  geom_point(color = "red") +
  geom_vline(xintercept = as.numeric(marker_dates), linetype = "dashed", color = "black") +
  labs(title = "Total Sales Over Time", x = "Month", y = "Total Sales") +
  theme_minimal()

ggplot(monthly_summary, aes(x = month, y = total_items)) +
  geom_line(color = "darkgreen", size = 1) +
  geom_point(color = "orange") +
  geom_vline(xintercept = as.numeric(marker_dates), linetype = "dashed", color = "black") +
  labs(title = "Total Items Sold Over Time", x = "Month", y = "Total Items Sold") +
  theme_minimal()

lockdown_interval <- sales_data %>%
  filter(format(TRAN_DATE, "%m") %in% c("03", "04", "05")) %>%  
  mutate(month = format(TRAN_DATE, "%Y-%m")) %>% 
  group_by(month, GROUP_NAME) %>%  
  summarize(total_sales = sum(TOTAL_SALES, na.rm = TRUE)) %>%
  ungroup()

lockdown_interval$month <- as.Date(paste0(lockdown_interval$month, "-01"))

ggplot(lockdown_interval, aes(x = month, y = total_sales)) +
  geom_line(color = "blue") +
  geom_point(color = "red") +
  labs(title = "Monthly Sales by Group (March-May)", x = "Month", y = "Total Sales") +
  theme_minimal() +
  facet_wrap(~ GROUP_NAME, scales = "free_y")

  ggplot(lockdown_interval, aes(x = month, y = total_sales, group = GROUP_NAME, color = GROUP_NAME)) +
  geom_line() +
  geom_point() +
  labs(title = "Monthly Sales by Group (March-May)", x = "Month", y = "Total Sales") +
  theme_minimal()