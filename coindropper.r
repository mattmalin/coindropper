library(tidyverse)
library(fitdistrplus)

# there are 8 start buckets, and 9 end buckets
# | | | | | | | | |
# so each start_bucket is 9/8 the size of an end bucket

# Data loading ------------------------------------------------------------
# recorded data has single line for each start_bucket and end_bucket observations:
experience_individuals <- read_csv("experience.txt", col_names = c("start_bucket", "end_bucket"))

# for ease of later distribution assumptions, label end buckets 0 to 8 (rather than 1 to 9),
# and add 'mirrored' observations under assumption of symmetry:
experience <- experience_individuals %>% 
  mutate(
    start_bucket = 9 - start_bucket,
    end_bucket   = 10 - end_bucket) %>%
  bind_rows(experience_individuals) %>% 
  mutate(end_bucket = end_bucket - 1) %>% 
  group_by(start_bucket, end_bucket) %>% 
  summarise(count = length(end_bucket)) %>%
  mutate(proportion = count / sum(count)) %>% 
  ungroup()

# Distribution fitting ----------------------------------------------------

# note that binomial distribution has chance of 0, i.e. with size 10 there can be between 0 and 10
# successes, but here there is always a bucket, i.e. 1-9 rather than 0-9, so offset by 1

distribution_fitting <- experience %>% 
  complete(start_bucket, end_bucket, fill = list(count = 0, proportion = 0)) %>% 
  group_by(start_bucket) %>%
  nest() %>% 
  group_by(start_bucket) %>%
  mutate(
    fitted_distribution = map(data, function(x) {
      fitdist(rep(x$end_bucket, x$count), "binom", fix.arg = list(size = 8), start = list(prob = start_bucket / 9))
    })
  ) %>% 
  mutate(
    estimate = map_dbl(fitted_distribution, function(x) x[["estimate"]][["prob"]])
  )

experience_fitted <- distribution_fitting %>%
  unnest(.preserve = fitted_distribution) %>% 
  mutate(
    estimate_proportion = dbinom(end_bucket, 8, estimate)
  ) 

# Visualisations ----------------------------------------------------------

ggplot(experience, aes(x = end_bucket, y = proportion)) + 
  geom_bar(stat = "identity", aes(fill = factor(end_bucket))) +
  labs(
    title = "End bucket distribution by starting bucket",
    fill = "End bucket",
    x = "End Bucket"
  ) +
  scale_x_continuous(breaks = 1:9) +
  facet_wrap(~ start_bucket)


ggplot(distribution_fitting, aes(x = start_bucket, y = estimate)) +
  geom_point(size = 5)


ggplot(experience_fitted, aes(x = end_bucket, y = proportion)) + 
  geom_bar(stat = "identity", aes(fill = factor(end_bucket))) +
  geom_point(aes(y = estimate_proportion)) +
  geom_line(aes(y = estimate_proportion)) +
  labs(
    title = "End bucket distribution by starting bucket",
    fill = "End bucket",
    x = "End Bucket"
  ) +
  scale_x_continuous(breaks = 1:9) +
  facet_wrap(~ start_bucket, scales = "free_y")


# Potential alternative looking another way: if wanting to land in a target
# bucket, what is the distribution of starting bucket to get there, assuming
# starting buckets all equally weighted (i.e. using proportions to rescale counts):
ggplot(experience, aes(x = start_bucket, y = proportion)) + 
  geom_bar(stat = "identity", aes(fill = factor(start_bucket))) +
  labs(
    title = "End bucket distribution by starting bucket",
    fill = "End bucket",
    x = "End Bucket"
  ) +
  scale_x_continuous(breaks = 1:9) +
  facet_wrap(~ end_bucket, scales = "free_y")

