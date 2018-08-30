library(tidyverse)
# there are 8 start buckets, and 9 end buckets

# | | | | | | | | |
# so each start_bucket is 9/8 the size of an end bucket

# recorded data has single line with start_bucket and end_bucket observations:
experience_individuals <- read_csv("experience.txt", col_names = c("start_bucket", "end_bucket"))

experience_individuals <- bind_rows(
  experience_individuals, 
  mutate(experience_individuals,
    start_bucket = 9 - start_bucket,
    end_bucket   =  10 - end_bucket))

experience <- experience_individuals %>% 
  group_by(start_bucket, end_bucket) %>% 
  summarise(count = length(end_bucket))

experience_rescaled <- experience %>% 
  group_by(start_bucket) %>% 
  mutate(proportion = count / sum(count)) %>% 
  ungroup()


ggplot(experience, aes(x = end_bucket, y = count)) + 
  geom_bar(stat = "identity", aes(fill = factor(end_bucket))) +
  labs(
    title = "End bucket distribution by starting bucket",
    fill = "End bucket",
    x = "End Bucket"
  ) +
  scale_x_continuous(breaks = 1:9) +
  facet_wrap(~ start_bucket, scales = "free_y")


ggplot(experience_rescaled, aes(x = start_bucket, y = proportion)) + 
  geom_bar(stat = "identity", aes(fill = factor(start_bucket))) +
  labs(
    title = "End bucket distribution by starting bucket",
    fill = "End bucket",
    x = "End Bucket"
  ) +
  scale_x_continuous(breaks = 1:9) +
  facet_wrap(~ end_bucket, scales = "free_y")
