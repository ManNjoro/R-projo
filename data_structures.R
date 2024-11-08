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

#List
fruits <- list("apple", "banana", "oranges", "berry")
fruits <- append(fruits,"mango")
fruits[4] <- "strawberry"
length(fruits)
"grape" %in% fruits
music_genre <- 
  factor(c("Jazz", "Rock", "Classic", "Classic", "Pop", "Jazz", "Rock", "Jazz"),
         levels = c("Classic", "Jazz", "Pop", "Rock", "Other"))
levels(music_genre)
music_genre
