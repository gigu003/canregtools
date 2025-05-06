#' Reframe data of class canregs or fbswicds
#'
#' @rdname cr_reframe
#' @param x Object with class of canregs or fbswicds.
#' @param strat Stratification variables used to reframe 'canregs' or
#'        'fbswicds'.
#'
#' @return Reframed canregs or fbswicds.
#' @export
#'
cr_reframe <- function(x, strat = "registry") {
  UseMethod("cr_reframe", x)
}

#' @rdname cr_reframe
#' @method cr_reframe canregs
#' @export
#' 
cr_reframe.canregs <- function(x, strat = "registry") {
  areacodes <- unique(unlist(map(x, pluck("areacode"))))
  new <- map(strat, function(a) {
    pluck(classify_areacode(areacodes), a)
  })
  nnew <- map(new, unique)
  res <-  map(1:length(nnew), function(b){
    rr <-  map(pluck(nnew, b), function(i) {
      loc <- which(pluck(new, b) == i)
      res <- cr_merge(cr_select(x, index = loc))
      res$areacode <- i
      return(res)
    })
    names(rr) <- pluck(nnew, b)
    return(rr)
  })
  res1 <- do.call(c, res)
  if (length(res1) == 1) {
    res1 <- pluck(res1, 1)
  } else {
    res1 <- res1[!duplicated(names(res1))]
    class(res1) <- c("canregs", "list")  
  }
  return(res1)
}

#' @rdname cr_reframe
#' @method cr_reframe fbswicds
#' @export
#' 
cr_reframe.fbswicds <- function(x, strat = "registry") {
  areacodes <- unique(unlist(map(x, pluck("areacode"))))
  new_code <- map(strat, function(a) {
    pluck(classify_areacode(areacodes), a)
  })
  tran_code <- map(new_code, unique)
  res <-  map(1:length(tran_code), function(b){
    rr <-  map(pluck(tran_code, b), function(i) {
      loc <- which(pluck(new_code, b) == i)
      res <- cr_merge(cr_select(x, index = loc))
      res$areacode <- i
      return(res)
    })
    names(rr) <- pluck(tran_code, b)
    return(rr)
  })
  
  output <- do.call(c, res)
  if (length(output) == 1) {
    output <- pluck(output, 1)
  } else {
    class(output) <- c("fbswicds", "list")  
  }
  return(output)
}

full_fbswicd <- function(x) {
  areacode_info <- as.data.frame(classify_areacode(x[["areacode"]]))
  fbswicd <- bind_cols(areacode_info, x[["fbswicd"]])
  sitemorp <- bind_cols(areacode_info, x[["sitemorp"]])
  pop <- bind_cols(areacode_info, x[["pop"]])
  res <- list(areacode = x[["areacode"]],
              fbswicd = fbswicd,
              sitemorp = sitemorp,
              pop = pop)
  class(res) <- c("fbswicd", "list")
  return(res)
}

