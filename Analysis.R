# Read dataset
data <- read.csv("top 240 restaurants recommanded in los angeles 2.csv", stringsAsFactors = FALSE)

# Detect columns
rating_col <- grep("rating", names(data), ignore.case = TRUE, value = TRUE)[1]
reviews_col <- grep("review", names(data), ignore.case = TRUE, value = TRUE)[1]

cat("Detected rating column:", rating_col, "\n")
cat("Detected reviews column:", reviews_col, "\n")

df <- data[, c(rating_col, reviews_col)]
names(df) <- c("rating", "reviews")

df$rating  <- as.numeric(df$rating)
df$reviews <- as.numeric(df$reviews)

df <- df[complete.cases(df$rating, df$reviews), ]

summary(df)

# Scatterplot
png("scatter_rating_vs_reviews.png", width = 800, height = 600)
plot(df$reviews, df$rating,
     main = "Scatterplot of Rating vs Number of Reviews",
     xlab = "Number of Reviews",
     ylab = "Rating",
     pch = 19)
abline(lm(rating ~ reviews, data = df), lwd = 2)
dev.off()

# Histogram rating
png("hist_rating.png", width = 800, height = 600)
hist(df$rating,
     main = "Histogram of Restaurant Ratings",
     xlab = "Rating",
     breaks = 10)
dev.off()

# Histogram reviews
png("hist_reviews.png", width = 800, height = 600)
hist(df$reviews,
     main = "Histogram of Number of Reviews",
     xlab = "Number of Reviews",
     breaks = 10)
dev.off()

# Correlation test
correlation_result <- cor.test(df$rating, df$reviews, method = "pearson")
print(correlation_result)

# Log file
sink("Rscript.log")
cat("7COM1079 - Correlation analysis log\n")
cat("------------------------------------\n\n")
str(data)
cat("\nSelected columns:\n")
cat("Rating column  :", rating_col, "\n")
cat("Reviews column :", reviews_col, "\n\n")
cat("Summary:\n")
print(summary(df))
cat("\nCorrelation Test Result:\n")
print(correlation_result)
sink()
