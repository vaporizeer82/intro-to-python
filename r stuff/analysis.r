library(infer)
library(tidyverse)
population <- tibble(
  hip = c(rep("Infection", 13500), rep("No infection", 436500))
  qt()
)
ggplot(population, aes(x = hip)) + 
  geom_bar() + 
  labs(x = "", y = "Count", 
       title = "Ceramic hip patients who develop infection")
population %>%
  count(hip) %>%
  mutate(p = n/sum(n))
samp1 <- population %>%
  sample_n(350)
samp1 %>% 
  count(hip) %>%
  mutate(p_hat = n/sum(n))
samp2 <- population %>%
  sample_n(350)
samp2 %>% 
  count(hip) %>%
  mutate(p_hat = n/sum(n))
sample_props_small  <- population %>%
  rep_sample_n(size = 100, reps = 10, replace = TRUE) %>%
  count(hip) %>%
  mutate(p_hat = n/sum(n)) %>%
  filter(hip == "Infection")
sample_mean = .014
View(sample_props_small)
standard = sqrt((sample_mean*(1-sample_mean))/350)
print(standard)
how_far = ((.04-sample_mean)/standard)

pnorm(how_far,0,1)
how_far_dos = ((.1-sample_mean)/standard)
1-pnorm(how_far_dos,0,1)
how_far_tres = ((.05-sample_mean)/standard)
pnorm(how_far_dos,0,1)-pnorm(how_far_tres,0,1)
2*(1-pnorm(1.63))
2*(1-pnorm(1.63,29))