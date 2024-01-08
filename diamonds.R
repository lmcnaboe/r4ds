library(tidyverse)

ggplot(diamonds, aes(x = carat, y = price)) + 
  geom_hex()
ggsave("diamonds.png")
print(diamonds)
write_csv(diamonds, "data/diamonds.csv")