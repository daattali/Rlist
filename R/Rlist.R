library(R6)

Rlist <- R6Class("Rlist",
                 public = list(
                   orig = NULL,
                   tail = NULL,
                   loc = c(),
                   initialize = function(orig) {
                     self$orig <- orig
                     self$tail <- self$orig
                     invisible(self)
                   },
                   down = function(y) {
                     name <- as.character(as.list(match.call()[2]))
                     if (!(name %in% names(self$tail))) {
                       stop(paste0(name, " is not a valid name in the current list."), call. = FALSE)
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
                       tail <- tail[[self$loc[i]]]
                     }
                     self$tail <- tail
                     invisible(self)
                   },
                   top = function() {
                     self$tail <- self$orig
                     invisible(self)
                   },
                   print = function(...) {
                     cat("Location: ", self$loc,
                         "\norig: ", address(self$orig),
                         "\ntail: ", address(self$tail),
                         "\n", sep = "")

                     if(is.list(self$tail) && !is.data.frame(self$tail)) {
                       cat("List with ", length(self$tail), " items:\n", sep = "")
                       lists <- c()
                       others <- c()
                       for(i in seq_along(self$tail)) {
                         name <- ifelse(is.null(names(self$tail)), i, names(self$tail)[i])
                         if(name == "") name <- paste0("[",i,"]")
                         item <- self$tail[[i]]
                         if(is.list(item) && !is.data.frame(item)) {
                           name <- paste0(name, " (", length(item), " items)")
                           lists <- c(lists, name)
                         } else {
                           name <- paste0(name, " (", paste(class(item), collapse = ", "), ")")
                           others <- c(others, name)
                         }
                       }

                       if (length(lists) > 0) {
                         cat("  Lists:\n")
                         lapply(lists, function(l) cat("    ", l, "\n", sep = ""))
                       }
                       if (length(others) > 0) {
                         cat("  Other:\n")
                         lapply(others, function(l) cat("    ", l, "\n", sep = ""))
                       }
                     } else {
                       print(self$tail)
                     }

                     invisible(self)
                   }
                 )
)
b <- Rlist$new(a)
b$down(mapping)
b$up()
b$down(data) %>% print
