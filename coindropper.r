library(ggplot2)

experience <- read.table("c:/scripts/coindropper/experience.txt", header = FALSE, sep = ",")
names(experience) <- c("start_bucket", "end_bucket")

# there are 8 start buckets, and 9 end buckets

# | | | | | | | | |
# so each start_bucket is 9/8 the size of an end bucket

# possibly an unfair assumption, but assume fully symmetric, so we can duplicate the data but mirrored:
experience <- rbind(
  experience, 
  within(experience, {
    start_bucket <- 9 - start_bucket
	end_bucket <- 10 - end_bucket
	 }))

experience[["count"]] <- 1
experience[order(experience[["start_bucket"]]), ]

integer_experience <- subset(experience, start_bucket %in% 1:8)

aggregate(. ~ end_bucket, data = experience, FUN = mean)
aggregate(. ~ end_bucket, data = integer_experience, FUN = mean)
aggregate(. ~ start_bucket, data = experience, FUN = mean)
integer_experience_means <- aggregate(. ~ start_bucket, data = integer_experience, FUN = mean)
integer_experience_length <- aggregate(. ~ start_bucket, data = integer_experience, FUN = length)


unique_experience <- aggregate(count ~ end_bucket + start_bucket, data = experience, FUN = sum)
unique_experience <- unique_experience[order(unique_experience[["start_bucket"]]), ]

ggplot(aes(x = start_bucket, y = end_bucket, size = count), data = unique_experience) + geom_point() + geom_smooth()

ggplot(aes(x = as.factor(start_bucket), y = as.factor(end_bucket), size = count), data = unique_experience) + geom_point()

## calculating deviations:

# given that on the edges they could "bounce" need to make some assumption about this.

# look at buckets 4 and 5 (the middle ones):

ggplot(aes(x = as.factor(end_bucket), y = ..count..), data = experience) + geom_bar(binwidth = 0.25) + scale_x_discrete() + facet_wrap(~ start_bucket)
ggplot(aes(x = as.factor(start_bucket), y = ..count..), data = experience) + geom_bar(binwidth = 0.25) + scale_x_discrete() + facet_wrap(~ end_bucket)
#ggplot(aes(x = as.factor(end_bucket), y = ..count..), data = experience) + geom_bar(binwidth = 0.25) + scale_x_discrete() + facet_wrap(~ start_bucket, scales = "free_y")
#ggplot(aes(x = as.factor(start_bucket), y = ..count..), data = experience) + geom_bar(binwidth = 0.25) + scale_x_discrete() + facet_wrap(~ end_bucket, scales = "free_y")

ggplot(aes(x = start_bucket, y = ..count..), data = subset(experience, end_bucket == 1)) + geom_bar(binwidth = 0.25) + scale_x_discrete()
ggplot(aes(x = start_bucket, y = ..count..), data = subset(experience, end_bucket == 2)) + geom_bar(binwidth = 0.25) + scale_x_discrete()
ggplot(aes(x = start_bucket, y = ..count..), data = subset(experience, end_bucket == 3)) + geom_bar(binwidth = 0.25) + scale_x_discrete()
ggplot(aes(x = start_bucket, y = ..count..), data = subset(experience, end_bucket == 4)) + geom_bar(binwidth = 0.25) + scale_x_discrete()
ggplot(aes(x = start_bucket, y = ..count..), data = subset(experience, end_bucket == 5)) + geom_bar(binwidth = 0.25) + scale_x_discrete()
ggplot(aes(x = start_bucket, y = ..count..), data = subset(experience, end_bucket == 6)) + geom_bar(binwidth = 0.25) + scale_x_discrete()
ggplot(aes(x = start_bucket, y = ..count..), data = subset(experience, end_bucket == 7)) + geom_bar(binwidth = 0.25) + scale_x_discrete()
ggplot(aes(x = start_bucket, y = ..count..), data = subset(experience, end_bucket == 8)) + geom_bar(binwidth = 0.25) + scale_x_discrete()
ggplot(aes(x = start_bucket, y = ..count..), data = subset(experience, end_bucket == 9)) + geom_bar(binwidth = 0.25) + scale_x_discrete()

ggplot(aes(x = as.factor(end_bucket), y = ..count..), data = subset(experience, start_bucket == 1)) + geom_bar(binwidth = 1) + scale_x_discrete()
ggplot(aes(x = as.factor(end_bucket), y = ..count..), data = subset(experience, start_bucket == 2)) + geom_bar(binwidth = 1) + scale_x_discrete()
ggplot(aes(x = as.factor(end_bucket), y = ..count..), data = subset(experience, start_bucket == 3)) + geom_bar(binwidth = 1) + scale_x_discrete()
ggplot(aes(x = as.factor(end_bucket), y = ..count..), data = subset(experience, start_bucket == 4)) + geom_bar(binwidth = 1) + scale_x_discrete()
ggplot(aes(x = as.factor(end_bucket), y = ..count..), data = subset(experience, start_bucket == 5)) + geom_bar(binwidth = 1) + scale_x_discrete()
ggplot(aes(x = as.factor(end_bucket), y = ..count..), data = subset(experience, start_bucket == 6)) + geom_bar(binwidth = 1) + scale_x_discrete()
ggplot(aes(x = as.factor(end_bucket), y = ..count..), data = subset(experience, start_bucket == 7)) + geom_bar(binwidth = 1) + scale_x_discrete()
ggplot(aes(x = as.factor(end_bucket), y = ..count..), data = subset(experience, start_bucket == 8)) + geom_bar(binwidth = 1) + scale_x_discrete()


unique(experience[["end_bucket"]])

aggregate(count ~ start_bucket, data = experience, FUN = length)
