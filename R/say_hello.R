#' Say Hello
#'
#' This function returns a simple greeting message.
#' You can customize the greeting
#' by passing a name.
#'
#' @param name A character string with the name of the person to greet.
#'
#' @return A character string with the greeting message.
#' @export
#'
#' @examples
#' say_hello("R-Ladies")

say_hello <- function(name) {
  paste0("Hello, ", name, "!")
}
