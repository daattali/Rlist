library(R6)
library(magrittr)
library(ggplot2)

#TODO work with non-names lists (when indices are integers or only some have names)
#TODO format nicer (maybe in a table-like format? or just make the "x elements, y depth" line up?)
# TODO separate functions into other files
# TODO mention in docs that both SE and NSE are supported and how to chain calls
#TODO shiny visualization
#TODO cache print() results so that the lists and print can be used again
#TODO allow going down multiple levels in one call instead of chaining calls
#TODO initialize: check that argument is proper list
# TODO as.list: not sure what the correct behaviour here should be - if the underlying
# list is of class ggplot, should this return an actual list or the ggplot object?
# TODO attributes are not shown (and they themselves are often lists)
# TODO formula should be regarded as list?

Rlist <- R6Class(
  "Rlist",

  public = list(

    orig = NULL,
    tail = NULL,
    loc = c(),

    initialize = function(orig) {
      self$orig <- orig
      self$tail <- self$orig
      invisible(self)
    },

    as.list = function() {
      self$orig
    },

    down = function(arg) {
      arg <- as.list(match.call()[2])[[1]]
      if (is.numeric(arg)) {
        name <- as.numeric(arg)
        if (arg < 1 || arg > length(self$tail)) {
          stop(paste0(name, " is not a valid index in the curent list"), call. = FALSE)
        }
      } else {
        name <- as.character(arg)
        if (!(name %in% names(self$tail))) {
          stop(paste0(name, " is not a valid name in the current list."), call. = FALSE)
        }
      }

      self$loc <- c(self$loc, name)
      self$tail <- self$tail[[name]]
      invisible(self)
    },

    up = function(levels = 1) {
      if (length(self$loc) == 0) {
        stop("Already at root of list", call. = FALSE)
      }
      upto <- max(length(self$loc) - levels, 0)
      self$loc <- self$loc[seq_len(upto)]
      tail <- self$orig
      for (i in seq_along(self$loc)) {
        tail <- tail[[self$loc[[i]]]]
      }
      self$tail <- tail
      invisible(self)
    },

    top = function() {
      self$tail <- self$orig
      self$loc <- c()
      invisible(self)
    },

    atTop = function() {
      length(self$loc) == 0
    },

    print = function(...) {
      cat0("Current location: /",
           paste(self$loc, collapse = "/"), "\n")

      if (is.list(self$tail) && !is.data.frame(self$tail)) {
        depths <- private$depths()
        cat0("List of depth ", max(depths),
             " with ", countNoun(length(self$tail), "element"),
             "\n")
        lists <- c()
        others <- c()
        for(i in seq_along(self$tail)) {
          name <- names(self$tail)[[i]]
          if(is.null(name) || name == "") {
            name <- i
          }
          item <- self$tail[[i]]
          if(is.list(item) && !is.data.frame(item)) {
            name <- paste0(name, " (",
                           "depth ", depths[[name]] - 1, ", ",
                           countNoun(length(item), "element"), ")")
            lists <- c(lists, name)
          } else {
            name <- paste0(name, " (", paste(class(item), collapse = ", "), ")")
            others <- c(others, name)
          }
        }

        if (length(lists) > 0) {
          cat("  Lists:\n")
          lapply(lists, function(l) cat0("    ", l, "\n"))
        }
        if (length(others) > 0) {
          cat("  Other:\n")
          lapply(others, function(l) cat0("    ", l, "\n"))
        }
      } else {
        print(self$tail)
      }

      invisible(self)
    }
  ),

  private = list(
    depths = function(x = self$tail, d = 0, all = TRUE) {
      if(!is.list(x) || length(x) == 0) {
        return(d)
      }
      depths <- lapply(x, private$depths, d + 1, FALSE) %>% unlist
      if (!all) {
        depths %<>% max
      }

      depths
    }
  )
)

countNoun <- function(num, noun) {
  paste0(num, " ", noun, ifelse(num == 1, "", "s"))
}
cat0 <- function(...) {
  cat(sep = "", ...)
}
as.list.Rlist <- function(x, ...) {
  x$as.list()
}



################################## tests #####################

a <- ggplot(mtcars, aes(cyl, mpg))+geom_point()+geom_line()
b <- Rlist$new(a)
#b$down(mapping)
#b$up()
#b$down(data) %>% print
#(b$up())
aa <- Rlist$new(list("dsf","AAA", "g"="v"))


