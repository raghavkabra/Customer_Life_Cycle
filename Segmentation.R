

# COMPUTING RECENCY, FREQUENCY, MONETARY VALUE 


# Load text file into local variable called 'data'
data = read.delim(file = 'C:/Users/raghav/Desktop/MA/purchases.txt', header = FALSE, sep = '\t', dec = '.')

# Add headers and interpret the last column as a date, extract year of purchase
colnames(data) = c('customer_id', 'purchase_amount', 'date_of_purchase')
data$date_of_purchase = as.Date(data$date_of_purchase, "%Y-%m-%d")
data$days_since       = as.numeric(difftime(time1 = "2016-01-01",
                                            time2 = data$date_of_purchase,
                                            units = "days"))

# Display the data after transformation
head(data)
summary(data)

# Compute key marketing indicators using SQL language
library(sqldf)

# Compute recency, frequency, and average purchase amount
customers = sqldf("SELECT customer_id,
                          MIN(days_since) AS 'recency',
                          COUNT(*) AS 'frequency',
                          AVG(purchase_amount) AS 'amount'
                   FROM data GROUP BY 1")

# Explore the data
head(customers)
summary(customers)
hist(customers$recency)
hist(customers$frequency)
hist(customers$amount)
hist(customers$amount, breaks = 100)


# --- PREPARING AND TRANSFORMING DATA ----------------------


# Copy customer data into new data frame
new_data = customers

# Remove customer id as a variable, store it as row names
head(new_data)
row.names(new_data) = new_data$customer_id
new_data$customer_id = NULL
head(new_data)

# Take the log-transform of the amount, and plot
new_data$amount = log(new_data$amount)
hist(new_data$amount)

# Standardize variables
new_data = scale(new_data)
head(new_data)


# --- RUNNING A HIERARCHICAL SEGMENTATION ------------------


# Compute distance metrics on standardized data
# This will likely generate an error on most machines
# d = dist(new_data)

# Take a 10% sample
sample = seq(1, 18417, by = 10)
head(sample)
customers_sample = customers[sample, ]
new_data_sample  = new_data[sample, ]

# Compute distance metrics on standardized data
d = dist(new_data_sample)

# Perform hierarchical clustering on distance metrics
c = hclust(d, method="ward.D2")

# Plot de dendogram
plot(c)

# Cut at 9 segments
members = cutree(c, k = 9)

# Show 30 first customers, frequency table
members[1:30]
table(members)

# Show profile of each segment
aggregate(customers_sample[, 2:4], by = list(members), mean)







# Compute key marketing indicators using SQL language
# Compute recency, frequency, and average purchase amount for the year 2015
customers_2015 = sqldf("SELECT customer_id,
                       MIN(days_since) AS 'recency',
                       MAX(days_since) AS 'first_purchase',
                       COUNT(*) AS 'frequency',
                       AVG(purchase_amount) AS 'amount'
                       FROM data GROUP BY 1")

# Explore the data
head(customers_2015)
summary(customers_2015)
hist(customers_2015$recency)
hist(customers_2015$frequency)
hist(customers_2015$amount)
hist(customers_2015$amount, breaks = 100)
customers_2015$segment = "NA"
customers_2015$segment[which(customers_2015$recency > 365*3)] = "inactive"
customers_2015$segment[which(customers_2015$recency <= 365*3 & customers_2015$recency > 365*2)] = "cold"
customers_2015$segment[which(customers_2015$recency <= 365*2 & customers_2015$recency > 365*1)] = "warm"
customers_2015$segment[which(customers_2015$recency <= 365)] = "active"
customers_2015$segment[which(customers_2015$segment == "warm" & customers_2015$first_purchase <= 365*2)] = "new warm"
customers_2015$segment[which(customers_2015$segment == "warm" & customers_2015$amount < 100)] = "warm low value"
customers_2015$segment[which(customers_2015$segment == "warm" & customers_2015$amount >= 100)] = "warm high value"
customers_2015$segment[which(customers_2015$segment == "active" & customers_2015$first_purchase <= 365)] = "new active"
customers_2015$segment[which(customers_2015$segment == "active" & customers_2015$amount < 100)] = "active low value"
customers_2015$segment[which(customers_2015$segment == "active" & customers_2015$amount >= 100)] = "active high value"
table(customers_2015$segment)
aggregate(x = customers_2015[, 2:5], by = list(customers_2015$segment), mean)

# Re-order factor in a way that makes sense
customers_2015$segment = factor(x = customers_2015$segment, levels = c("inactive", "cold",
                                                                       "warm high value", "warm low value", "new warm",
                                                                       "active high value", "active low value", "new active"))
table(customers_2015$segment)
aggregate(x = customers_2015[, 2:5], by = list(customers_2015$segment), mean)


# Compute key marketing indicators using SQL language for year 2014
# Compute recency, frequency, and average purchase amount
customers_2014 = sqldf("SELECT customer_id,
                       MIN(days_since) - 365 AS 'recency',
                       MAX(days_since) - 365 AS 'first_purchase',
                       COUNT(*) AS 'frequency',
                       AVG(purchase_amount) AS 'amount'
                       FROM data
                       WHERE days_since > 365
                       GROUP BY 1")

# Complete segment solution using which, and exploiting previous test as input
customers_2014$segment = "NA"
customers_2014$segment[which(customers_2014$recency > 365*3)] = "inactive"
customers_2014$segment[which(customers_2014$recency <= 365*3 & customers_2014$recency > 365*2)] = "cold"
customers_2014$segment[which(customers_2014$recency <= 365*2 & customers_2014$recency > 365*1)] = "warm"
customers_2014$segment[which(customers_2014$recency <= 365)] = "active"
customers_2014$segment[which(customers_2014$segment == "warm" & customers_2014$first_purchase <= 365*2)] = "new warm"
customers_2014$segment[which(customers_2014$segment == "warm" & customers_2014$amount < 100)] = "warm low value"
customers_2014$segment[which(customers_2014$segment == "warm" & customers_2014$amount >= 100)] = "warm high value"
customers_2014$segment[which(customers_2014$segment == "active" & customers_2014$first_purchase <= 365)] = "new active"
customers_2014$segment[which(customers_2014$segment == "active" & customers_2014$amount < 100)] = "active low value"
customers_2014$segment[which(customers_2014$segment == "active" & customers_2014$amount >= 100)] = "active high value"

# Re-order factor in a way that makes sense
customers_2014$segment = factor(x = customers_2014$segment, levels = c("inactive", "cold",
                                                                       "warm high value", "warm low value", "new warm",
                                                                       "active high value", "active low value", "new active"))

# Show segmentation results
table(customers_2014$segment)
pie(table(customers_2014$segment), col = rainbow(24))
aggregate(x = customers_2014[, 2:5], by = list(customers_2014$segment), mean)


# --- COMPUTING REVENUE GENERATION PER SEGMENT -------------


# Compute how much revenue is generated by segments
# Notice that people with no revenue in 2015 do NOT appear
revenue_2015 = sqldf("SELECT customer_id, SUM(purchase_amount) AS 'revenue_2015'
                     FROM data
                     WHERE year_of_purchase = 2015
                     GROUP BY 1")
summary(revenue_2015)

# Merge 2015 customers and 2015 revenue
actual = merge(customers_2015, revenue_2015, all.x = TRUE)
actual$revenue_2015[is.na(actual$revenue_2015)] = 0

# Show average revenue per customer and per segment
aggregate(x = actual$revenue_2015, by = list(customers_2015$segment), mean)

# Merge 2014 customers and 2015 revenue 
forward = merge(customers_2014, revenue_2015, all.x = TRUE)
forward$revenue_2015[is.na(forward$revenue_2015)] = 0

# Show average revenue per customer and per segment
r = aggregate(x = forward$revenue_2015, by = list(customers_2014$segment), mean)
print(r)

# Re-order and display results
r = r[order(r$x, decreasing = TRUE), ]
print(r)
barplot(r$x, names.arg = r$Group.1)

