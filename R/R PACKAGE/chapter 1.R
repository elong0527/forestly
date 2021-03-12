#' Print "Hello world"
#'
#' This is a simple function that, by default, prints "Hello world". You can
#' customize the text to print (using the \code{to_print} argument) and add
#' an exclamation point (\code{excited = TRUE}).
#'
#' @param to_print A character string giving the text the function will print
#' @param excited Logical value specifying whether to include an exclamation
#'    point after the text
#'
#' @return This function returns a phrase to print, with or without an
#'    exclamation point added. As a side effect, this function also prints out
#'    the phrase.
#'
#' @examples
#' hello_world()
#' hello_world(excited = TRUE)
#' hello_world(to_print = "Hi world")
#'
#' @export
hello_world <- function(to_print = "Hello world", excited = FALSE){
  if(excited) to_print <- paste0(to_print, "!")
  print(to_print)
}

hello_world <- function(to_print = "Hello world", excited = FALSE){
  if(excited) to_print <- paste0(to_print, "!")
  print(to_print)
}
hello_world(excited = TRUE)
hello_world(to_print = "yanping world")
