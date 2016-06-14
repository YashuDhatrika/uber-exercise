n.dishes.vec <- c(A=2L,B=3L)
dishes.by.cuisine <- list()
for(cuisine in names(n.dishes.vec)){
  dishes.by.cuisine[[cuisine]] <- paste0(cuisine, 1:(n.dishes.vec[[cuisine]]))
}
dishes.by.cuisine


## A menu can be represented by a list of dishes such as “A3 B11 C4”.
## Write a program that takes the number of cuisines and the number of
## dishes available within each cuisine as input and lists all
## possible menus as output.
all.possible.menus <- function(n.dishes.vec){
  stopifnot(is.integer(n.dishes.vec))
  stopifnot(0 < n.dishes.vec)
  cuisine.letter.vec <- names(n.dishes.vec)
  stopifnot(is.character(cuisine.letter.vec))
  stopifnot(nchar(cuisine.letter.vec)==1)
  stopifnot(grepl("[a-zA-Z]", cuisine.letter.vec))
  dishes.by.cuisine <- list()
  for(cuisine in names(n.dishes.vec)){
    dishes.by.cuisine[[cuisine]] <- paste0(cuisine, 1:(n.dishes.vec[[cuisine]]))
  }
  menu.df <- do.call(expand.grid, dishes.by.cuisine)
  apply(menu.df, 1, paste, collapse=" ")
}
library(testthat)
computed.menus <- all.possible.menus(c(A=2L,B=3L))
expected.menus <- c(
  "A1 B1", "A1 B2", "A1 B3",
  "A2 B1", "A2 B2", "A2 B3")
expect_identical(sort(computed.menus), sort(expected.menus))
expect_error({
  all.possible.menus(c(2L, 3L))
})
expect_error({
  all.possible.menus(c(foo=2L, bar=3L))
})
expect_error({
  all.possible.menus(c(A=2, B=3L))
})
expect_error({
  all.possible.menus(c("0"=2L, B=3L))
})
expect_error({
  all.possible.menus(c(A=-2L, B=3L))
})
expect_error({
  all.possible.menus(c(A=0L, B=3L))
})
(ex.menus <- all.possible.menus(c(A=3L, B=11L, C=4L)))
expect_true("A3 B11 C4" %in% ex.menus)
