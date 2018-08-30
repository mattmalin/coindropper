library(tidyverse)
# there are 8 start buckets, and 9 end buckets
# | | | | | | | | |
# so each start_bucket is 9/8 the size of an end bucket


# Data loading ------------------------------------------------------------
# recorded data has single line for each start_bucket and end_bucket observations:
experience_individuals <- read_csv("experience.txt", col_names = c("start_bucket", "end_bucket"))

# add 'mirrored' observations under assumption of symmetry:
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


# Visualisations ----------------------------------------------------------

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



# Distribution fitting ----------------------------------------------------

experience %>% 
  group_by(start_bucket) %>% 
  summarise(sum = sum(count)) %>% 
  arrange(desc(sum))

experience %>% 
  filter(start_bucket == 4)

library(fitdistrplus)

sample_points <- rbinom(100, 10, 0.5)

fitdist(
  data = sample_points,
  distr = "binom",
  fix.arg = list(size = 10),
  start = list(prob = 0.4)
  )



fitdist(
  data = sample_points,
  distr = "binom",
  fix.arg = list(size = 10),
  start = list(prob = 0.4)
)


experience %>% 
  ungroup() %>% 
  filter(start_bucket == 4) %>% 
  (function(x) {rep(x$end_bucket, x$count)}) %>% 
  fitdist("binom", fix.arg = list(size = 9), start = list(prob = 0.4))

example_bucket_4 <- experience %>% 
  ungroup() %>% 
  filter(start_bucket == 4) %>% 
  (function(x) {rep(x$end_bucket, x$count)}) %>% 
  fitdist("binom", fix.arg = list(size = 9), start = list(prob = 0.4))

str(example_bucket_4)




library(broom)


