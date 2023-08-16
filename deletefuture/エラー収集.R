df <- data.frame(x = 1:10, y = 11:20)

result <- tryCatch({
  base::lm(y ~ x, data = df)
}, error = function(e) {
  e
})

if (inherits(result, "error")) {
  error_message <- as.character(result)
  error_df <- data.frame(Error_Message = error_message)
  write.csv(error_df, file = "/Users/aizawaharuka/Documents/GitHub/名称未設定/deletefuture/エラーの内容.csv", row.names = FALSE)
} else {
  # エラーが発生しなかった場合の処理
}
