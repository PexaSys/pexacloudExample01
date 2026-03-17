# Hello, world!
#
# This is an example of a PexaCloud Service
# which prints 'Hello, world!'.
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Install Package:           'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'

# ------------------------------------------------------------------------------------------------
# PexaCloud Service specifications
# Arguments, sent to the R are the input (funcInput) of the raw json sent to PexaClaod. 
# There are 2 pfields that are added by PexaCloud, unless ignoreDefaultInput is true:
# 1. execution_id: a unique identifier for this execution
# 2. callback_url: a URL to report progress and results back to PexaCloud

# Default name of the function to be called is 'gateway'
#  If you want to call a different function, you can specify the name of the function in the request to PexaCloud: 
#  funcName="your_function_name"
# fields that have json value are known as List in R.
# a:{b:1, c:2} is a list with 2 fields: b and c, with values 1 and 2.

# There are 3 ways to provide result to PexaCloud:
# 1. Main function returns results
# 2. Artifacts like images created in the current session, know as ExtraData in PexaCloud.
# 3. Use the callback method to send the results back to PexaCloud

# Callback mechanism:
# When function is working long time, data can be send to the Pexacload by calling the callback function.
# execeution_id is the identifier of the execution and it is used to identify the execution in the PexaCloud.
# When the callback is called, PexaCloud will wait for the response to be returned, and if the response is not returned within the timeout, 
# the execution will be timed out. having func_output is signed for the callbakss are done,
# progress_step and progress_total are used to report the progress of the execution.
# func_output is used to send the results back to PexaCloud and it assume that the function is finished and the results are ready to be returned.

# Best practices:
# Use '...' to capture the arguments of the function. It is highly recommended to use it, even if the function has no arguments.
# Always take care of the the package version, it is required to be sure what version is really running.
# Have a fucntion that gives the information about the package, it can be like a metadata function/; get_my_info()

# ------------------------------------------------------------------------------------------------

#  These are the packages that are needed to be imported from preparing the package for PexaCloud Service
#' @importFrom jsonlite fromJSON toJSON
#' @importFrom httr GET POST
#' @importFrom ggplot2 ggplot
#'


# This is the callback helper function that is used to send the progress and results back to PexaCloud
callback<- function(json_body, callback_url){

  require(jsonlite)
  require(httr)

  # Convert the list to JSON
  # json_body <- toJSON(json_body, auto_unbox = TRUE)

  # Make the POST request, ignoring SSL certificate verification
  response <- POST(
    callback_url,
    # add_headers(.headers = c("Content-Type" = "application/json")),
    body = json_body,
    encode = "json",
    config(ssl_verifypeer = FALSE, ssl_verifyhost = FALSE)
  )

  
}


# This is a simple function that returns 'Hello!', without ..., ignoreDefauktInput must be true
# funcInput: {}
#' @export
hello<-function(...)
{
  return('Hello!')
}

# This is a simple function that returns the information about the package.
# funcInput: {}
#' @export
get_my_info<-function(...)
{
  require(jsonlite)

  return(toJSON(
    list(
      package_name = getPackageName(),
      package_version = as.character(packageVersion("pexacloudExample01")),
      package_dependencies = tools::package_dependencies('pexacloudExample01', recursive = TRUE, which = c('Depends', 'Imports', 'LinkingTo', 'Suggests')),
      package_dependencies_utils = utils::packageDescription("pexacloudExample01")$Imports
    )
  ))
}

# This is the gateway function that is used to be called by PexaCloud.
# It is default function that is called by PexaCloud if no function is specified in the request
# funcInput: {"param1": "value1", "param2": "value2", "param3": "value3"}
#' @export
gateway<-function(...){

  # Capture the list of arguments
  arguments <- list(...)

  # first level of the json 
  # this is an example that user wanted the getway function to call a different function: func_name="model_run"
  param1<-arguments$param1
  param2<-arguments$param2
  param3<-arguments$param3

  # Do the work


  # For example contact these params.
  out<- list(result="success", param1=param1, param2=param2, param3=param3)

  # Preparing to send the result back to PexaCloud
  require(jsonlite)

  return(toJSON(out))
}

# example of choose a function and call it dynamically
# funcInput: {"func_name": "model_run", "iteration": 10, "wait": 1}
#' @export
call_another_function<-function(...){

  arguments <- list(...)

  func_name <- arguments$func_name
  arguments$func_name <- NULL

  out <- do.call(func_name, args = arguments)

  # Preparing to send the result back to PexaCloud
  require(jsonlite)

  # Return the results, this is the result for the main call to the service
  return(toJSON(out))
}

# Example of a function that used callback method to works long
#' @export
long_running_model_run <- function(...) {
  arguments <- list(...)

  execution_id <- arguments$execution_id
  callback_url <- arguments$callback_url
  iteration <- as.integer(arguments$iteration)
  wait <- as.numeric(arguments$wait)

  if (is.null(execution_id)) stop("execution_id is required")
  if (is.null(callback_url)) stop("callback_url is required")
  if (is.na(iteration) || iteration < 1) stop("iteration must be a positive integer")
  if (is.na(wait) || wait < 0) stop("wait must be non-negative")

  for (i in seq_len(iteration)) {
    callback(
      list(
        execution_id = execution_id,
        progress_step = i,
        progress_total = iteration
      ),
      callback_url
    )

    Sys.sleep(wait)
  }

  
  # Send the results back to PexaCloud
  ret <- callback(list(execution_id = execution_id, func_output=list(key1=1,key2=2)), callback_url)

  return(list(
    execution_id = execution_id,
    result = "done"
  ))
}

# Example of a function that creates extra data
# funcInput: {}
#' @export
create_extra_data<-function(...){

  arguments <- list(...)

  # Call another function that creates extra data
  generate_plot()
  return(list(result="success", message="Extra data created"))
}

# Example of a function that is called by the other functions
# It is a simple function that demonstrates the use of the ggplot2 package to create a plot
# It is creates extra data function.
# funcInput: {}
#' @export
generate_plot <- function(...) {

  plot(cars)
  plot(cars$speed)
  plot(cars$dist)

  require(ggplot2)

  # Create a simple dataframe
  df <- data.frame(
    x = 1:10,
    y = rnorm(10)
  )

  # Generate the first plot
  p1 <- ggplot(df, aes(x = x, y = y)) +
    geom_line() +
    ggtitle("Sample Plot 1")
  print(p1)
  #It is needed in Linux
  #dev.off() # It cause no image provided

  # Generate the second plot
  p2 <- ggplot(df, aes(x = x, y = -y)) +
    geom_line() +
    ggtitle("Sample Plot 2")
  print(p2)

  #It is needed in Linux
  dev.off()

  return("Done!")
}

