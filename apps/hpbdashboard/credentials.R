library(R6)


CastorApiCredentials <- R6Class("CastorApiCredentials",
  public = list(
    
    client_id_file_name = "castorclientid.txt",
    client_id_file_path = NULL,
    client_id = NULL,
    client_secret_file_name = "castorclientsecret.txt",
    client_secret_file_path = NULL,
    client_secret = NULL,
    
    initialize = function(client_id_file_name = NULL, client_secret_file_name = NULL) {
      if(!is.null(client_id_file_name)) {
        print(paste0("Using client_id_file_name = ", client_id_file_name))
        self$client_id_file_name <- client_id_file_name
      }
      
      self$client_id_file_path <- file.path(path.expand("~"), self$client_id_file_name)
      
      if(!is.null(client_secret_file_name)) {
        print(paste0("Using client_secret_file_name = ", client_secret_file_name))
        self$client_secret_file_name <- client_secret_file_name
      }
      
      self$client_secret_file_path <- file.path(path.expand("~"), self$client_secret_file_name)
    },
    
    load_client_id = function() {
      if(file.exists(self$client_id_file_path)) {
        client_id_file <- file(self$client_id_file_path, "r")
        self$client_id <- readLines(client_id_file)
        close(client_id_file)
        return(self$client_id)
      } else {
        print(paste0(
          "The file '", self$client_id_file_name, "' could not be found. ", 
          "Make sure to store the Castor client ID in a text file called '", self$client_id_file_name, "' ",
          "in the '", path.expand("~"), "' directory."
        ))
        return(NULL)
      }
    },
    
    save_client_id = function(client_id) {
      if(!file.exists(self$client_id_file_path)) {
        client_id_file <- file(self$client_id_file_path, "w")
        writeLines(client_id, client_id_file)
        close(client_id_file)
        return(client_id)
      } else {
        print(paste0("Warning: client ID file ", self$client_id_file_path, " already exists"))
        return(NULL)
      }
    },
    
    load_client_secret = function() {
      if(file.exists(self$client_secret_file_path)) {
        client_secret_file <- file(self$client_secret_file_path, "r")
        self$client_secret <- readLines(client_secret_file)
        close(client_secret_file)
        return(self$client_secret)
      } else {
        print(paste0(
          "The file '", self$client_secret_file_name, "' could not be found. ", 
          "Make sure to store the Castor client secret in a text file called '", self$client_secret_file_name, "' ",
          "in the '", path.expand("~"), "' directory."
        ))
        return(NULL)
      }
    },
    
    save_client_secret = function(client_secret) {
      if(!file.exists(self$client_secret_file_path)) {
        client_secret_file <- file(self$client_secret_file_path, "w")
        writeLines(client_secret, client_secret_file)
        close(client_secret_file)
        return(client_secret)
      } else {
        print(paste0("Warning: client secret file ", self$client_secret_file_path, " already exists"))
        return(NULL)
      }
    }
  )
)


credentials <- CastorApiCredentials$new("id.txt", "secret.txt")
# credentials <- CastorApiCredentials$new()
id <- credentials$load_client_id()
if(is.null(id)) {
  id <- credentials$save_client_id("1234")
  print(paste0("Saved ID: ", id))
}

secret <- credentials$load_client_secret()
if(is.null(secret)) {
  secret <- credentials$save_client_secret("ABCD")
  print(paste0("Saved secret: ", secret))
}
