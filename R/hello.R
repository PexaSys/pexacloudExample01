# Hello, world!
#
# This is an example function named 'hello'
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

#' @importFrom jsonlite fromJSON toJSON
#' @importFrom httr GET POST
#' @importFrom ggplot2 ggplot
#'
# library(jsonlite)
# library(httr)

#' @export
hello<-function()
{
  return('Hello!')
}

#' @export
get_my_name<-function()
{
  x<-getPackageName()
  return(x)
}

#' @export
get_my_info<-function()
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

  # Print the status code and response content
  # print(status_code(response))
  # print(content(response, "text", encoding = "UTF-8"))
}

# pexacloudExample01::gateway(func="model_run", func_input=list("iteration"= 4, wait=1), execution_id  ="123456", callback_url  ="https://localhost:9443/servicecallback/")
# pexacloudExample01::gateway(func="generate_plot", func_input=Na, execution_id  ="123456", callback_url  ="https://localhost:9443/servicecallback/")

#' @export
gateway<-function(...){

  # Capture the list of arguments
  arguments <- list(...)

  # # Check if a specific named argument exists
  # if ("func" %in% names(args)) {
  #   func <- args$func
  #   cat("func:", func, "\n")
  # }

  func_name<-arguments$func
  execution_id <-arguments$execution_id
  callback_url <-arguments$callback_url
  func_input <-arguments$func_input

  out<-do.call(func_name, args = list(func_input, execution_id, callback_url))

  require(jsonlite)

  return(toJSON(out))
}


model_run<-function(func_input, execution_id, callback_url){

  iteration <-func_input$iteration
  wait <-func_input$wait

  # Initialize the counter variable
  counter <- 1

  # Start the while loop
  while (counter <= iteration) {

    Sys.sleep(wait)

    # JSON body to send in the POST request
    # json_body <- list(
    #   execution_idValue = execution_id,
    #   counterValue = counter
    # )

    ret <- callback(list(execution_id = execution_id, progress_step = counter, progress_total=iteration), callback_url)

    # Print the current value of the counter
    print(counter)

    # Increment the counter
    counter <- counter + 1
  }

  generate_plot()

  ret <- callback(list(execution_id = execution_id, func_output=list(key1=1,key2=2)), callback_url)

  return(list(result="success", execution_id=execution_id, func_output=list(key1=1,key2=2)))
}

#' @export
generate_plot <- function(func_input=list(), execution_id='', callback_url='') {

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

