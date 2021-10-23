library(tidyverse)

level_events <- read_csv("level_events.csv")
installation_events <- read_csv("installation_events.csv")

results <- left_join(level_events, installation_events, "player_id")
results <- results %>% 
        mutate(age = round((as.numeric(timestamp.x - timestamp.y)/60/60/24))) %>% 
        select(- c(timestamp.y,timestamp.x)) %>% 
        group_by(age) %>% 
        summarise(avg_level_all = mean(level_number)) %>% 
        distinct(age, .keep_all = TRUE)

g <- ggplot(results, aes(x = age)) +
        # geom_line(aes(y = avg_level_active), color = "darkred") +
        geom_line(aes(y = avg_level_all), color = "steelblue")

ggsave("Q1_2.png", plot = g)
