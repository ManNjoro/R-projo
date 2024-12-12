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
repeat_each
repeat_times

numbers <- seq(from = 0, to=100, by=20)
numbers
#List
fruits <- list("apple", "banana", "oranges", "berry")
fruits <- append(fruits,"mango")
fruits[1]
fruits[4] <- "strawberry"
length(fruits)
"grape" %in% fruits
music_genre <- factor(c("Jazz", "Rock", "Classic", "Classic", "Pop", "Jazz", "Rock", "Jazz"))
music_genre <- 
  factor(c("Jazz", "Rock", "Classic", "Classic", "Pop", "Jazz", "Rock", "Jazz"),
         levels = c("Classic", "Jazz", "Pop", "Rock", "Other"))
levels(music_genre)
music_genre
length(music_genre)
# Define the vector x
x <- c(7, 12, 9, 15, NA, 8, 14, NA, 2, 9, NA, 8)

# Get the positions of missing values
missing_positions <- which(is.na(x))

# Print the positions
print(missing_positions)
# Count the number of non-missing values
non_missing_count <- sum(!is.na(x))

# Print the count
print(non_missing_count)
# Calculate the mean of non-missing values
mean_value <- mean(x, na.rm = TRUE)

# Replace missing values with the mean
x[is.na(x)] <- mean_value

# Print the updated vector
print(x)

replace_na_with_mean <- function(vec) {
  # Calculate the mean of non-missing values
  mean_value <- mean(vec, na.rm = TRUE)
  
  # Replace missing values with the mean
  vec[is.na(vec)] <- mean_value
  
  # Return the updated vector
  return(vec)
}
res <- replace_na_with_mean(x)
print(res)
tail(df)
