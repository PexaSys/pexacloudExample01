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

# ------------------------------------------------------------------------------------------------

#  These are the packages that are needed to be imported from preparing the package for PexaCloud Service
#' @importFrom jsonlite fromJSON toJSON
#' @importFrom httr GET POST
#' @importFrom ggplot2 ggplot
#'

# Thiis s a simple function that returns 'Hello!', No input, ignoreDefauktInput must be true
#' @export
hello<-function()
{
  return('Hello!')
}
# If ignoreDefaultInput is not true, the function will works if:
#' @export
hello2<-function(...)
{
  return('Hello ${execution_id}!')
}

# This is a simple function that returns the name of the package, No input, ignoreDefauktInput must be true
#' @export
get_my_name<-function(xecution_id, callback_url)
{
  x<-getPackageName()
  return(x)
}

# This is a simple function that returns the information about the package, No input, ignoreDefauktInput must be true
#' @export
get_my_info<-function(xecution_id, callback_url)
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

# This is the callback helper function that is used to send the progress and results back to PexaCloud
callback<- function(json_body, callback_url){

  require(jsonlite)
  require(httr)

  # Convert the list to JSON
  json_body <- toJSON(json_body, auto_unbox = TRUE)

  # Make the POST request, ignoring SSL certificate verification
  response <- POST(
    callback_url,
    add_headers(.headers = c("Content-Type" = "application/json")),
    body = json_body,
    encode = "json",
    config(ssl_verifypeer = FALSE, ssl_verifyhost = FALSE)
  )

}

# This is the gateway function that is used to be called by PexaCloud to invoke the service function
# It is default function that is called by PexaCloud if no function is specified in the request
#' @export
gateway<-function(...){

  # Capture the list of arguments
  arguments <- list(...)

  # first level of the json 
  # this is an example that user wanted the getway function to call a different function: func_name="model_run"
  func_name<-arguments$func_name
  param1<-arguments$param1
  param2<-arguments$param2
  param3<-arguments$param3

  # An example of calling a function that requested form gateway to run it
  out<-do.call(func_name, args = list(param1, param2, param3))

  # Preparing to send the result back to PexaCloud
  require(jsonlite)

  return(toJSON(out))
}

# Example of a function that is called by the gateway function
# It is a simple function that demonstrates the use of the callback function
model_run<-function(param1, param2, param3){

  # Start the while loop
  for (i in 1:param1) {

    # Report progress
    ret <- callback(list(execution_id = execution_id, progress_step = counter, progress_total=iteration), callback_url)

    # Print the current value of the counter
    print(counter)

    # Increment the counter
    counter <- counter + 1
  }

  # Call another function that is called by the model_run function
  generate_plot()

  # Send the results back to PexaCloud
  ret <- callback(list(execution_id = execution_id, func_output=list(key1=1,key2=2)), callback_url)

  # Return the results to the gateway function, this is the result for the main call to the service
  return(list(result="success", execution_id=execution_id, func_output=list(key1=1,key2=2)))
}

# Example of a function that is called by the model_run function
# It is a simple function that demonstrates the use of the ggplot2 package to create a plot
# It is an extra data function, it is not called by the gateway function, but by the model_run function
#' @export
generate_plot <- function() {

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

