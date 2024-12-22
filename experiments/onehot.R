df <- data.frame(
  id = 1:5,
  checkbox_column = c("1;2", "2;3;5", "1", "4;5", "2;4")
)

checkbox_options <- c("1", "2", "3", "4", "5")

for (option in checkbox_options) {
  df[[paste0("checkbox_", option)]] <- sapply(df$checkbox_column, function(x) {
    option %in% unlist(strsplit(x, ";"))
  }) * 1  # Convert logical to integer (1 for TRUE, 0 for FALSE)
}

print(df)
