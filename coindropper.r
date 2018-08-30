library(tidyverse)
# there are 8 start buckets, and 9 end buckets

# | | | | | | | | |
# so each start_bucket is 9/8 the size of an end bucket

# recorded data has singl eline with start_bucket and end_bucket observations:
experience_individuals <- read_csv("experience.txt", col_names = c("start_bucket", "end_bucket"))

experience <- experience_individuals %>% 
  group_by(start_bucket, end_bucket) %>% 
  summarise(count = length(end_bucket))

ggplot(experience, aes(x = end_bucket, y = count)) + 
  geom_bar(stat = "identity", aes(fill = factor(end_bucket))) +
  facet_wrap(~ start_bucket) +
  labs(
    title = "End bucket distribution by starting bucket",
    fill = "End bucket",
    x = "End Bucket"
  ) +
  scale_x_continuous(breaks = 1:9)
