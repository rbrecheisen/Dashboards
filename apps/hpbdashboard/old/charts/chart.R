library(R6)


Chart = R6Class(
  
  "Chart",
  
  public = list(
    
    df = NULL,
    tmp_dir = NULL,
    
    initialize = function(df, tmp_dir = NULL) {
      self$tmp_dir = tmp_dir
      self$df = df
    },
    
    show = function() {
      message("Not implemented")
    }
  )
)
