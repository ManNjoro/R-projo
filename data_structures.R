fruits <- c("banana", "apple", "orange", "mango", "lemon")
length(fruits)
sort(fruits)
fruits[5] <- "melon"
fruits
fruits[c(-1)]
repeat_each <- rep(c(1,2,3), each=3)
repeat_times <- rep(c(1,2,3), times=3)
repeat_independent <- rep(c(1,2,3), times = c(5, 2,1))
numbers <- seq(from = 0, to = 100, by = 20)
