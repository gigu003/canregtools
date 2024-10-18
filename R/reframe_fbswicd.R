#' Reframe data of class fbswicds
#'
#' @rdname reframe_fbswicd
#' @param x Object of class fbswicds.
#' @param ... Stratification variables used to reframe.
#'
#' @return Merged fbswicd.
#' @export
#'
reframe_fbswicd <- function(x, ...) {
  UseMethod("reframe_fbswicd", x)
}

#' @rdname reframe_fbswicd
#' @method reframe_fbswicd canregs
#' @param cancer_type Method for classify cancer sites.
#' @export
#' 
reframe_fbswicd.canregs <- function(x, ..., cancer_type = "big"){
  data__ <- count_canreg.canregs(x,
                                 cancer_type = cancer_type,
                                 sep_zero = FALSE)
  res <- reframe_fbswicd.fbswicds(data__, ...)
  return(res)
}

#' @rdname reframe_fbswicd
#' @method reframe_fbswicd fbswicds
#' @export
#'
reframe_fbswicd.fbswicds <- function(x, ...) {
  # 捕获传入的分层变量
  strat_vars <- rlang::quos(...)
  strat_vars <- purrr::map_chr(strat_vars, as_name)
  strat_vars <- rlang::syms(setdiff(strat_vars,
                                    c("year", "sex", "cancer", "agegrp")))
  
  # 默认分层变量，转换为 quosure
  default_vars <- rlang::syms(c("year", "sex", "cancer", "agegrp"))
  
  # 如果没有传入任何分层变量，则使用默认变量
  if (length(strat_vars) == 0) {
    group_vars <- default_vars
  } else {
    # 如果传入了分层变量，将传入的 quosures 列表与默认 quosures 结合
    group_vars <- c(as.list(strat_vars), default_vars)
  }
  
  # 提取并处理每个 fbswicd 对象的数据
  data <- purrr::map(x, full_fbswicd)
  areacode <- purrr::map_chr(data, "areacode")
  
  # 合并多个 fbswicd 对象，并按指定变量进行汇总
  fbswicd <- purrr::reduce(purrr::map(data, "fbswicd"), bind_rows)
  fbswicd <- fbswicd |> 
    group_by(!!!group_vars) |> 
    reframe(across(c("fbs", "sws", "mv", "ub", "sub", "m8000", "dco"),
                   ~ sum(.x, na.rm = TRUE)))
  
  # 合并人口数据 pop 并汇总
  pop <- purrr::reduce(purrr::map(data, "pop"), bind_rows)
  # 对 pop 数据，`cancer`列不存在，所以在去除默认的`cancer`列后进行分组
  pop_group_vars <- group_vars[!sapply(group_vars, as_label) %in% "cancer"]
  pop <- pop |> 
    group_by(!!!pop_group_vars) |> 
    reframe(across(c("rks"), ~ sum(.x, na.rm = TRUE)))
  
  # 合并 sitemorp 数据并汇总
  sitemorp <- purrr::reduce(purrr::map(data, "sitemorp"), bind_rows)
  # 对 sitemorp 数据，`agegrp`列不存在，所以在去除`agegrp`列后进行分组
  sitemorp_group_vars <- group_vars[!sapply(group_vars, as_label) %in% "agegrp"]
  sitemorp <- sitemorp |> 
    group_by(!!!sitemorp_group_vars) |> 
    reframe(site = list(combine_tp(site)),
            morp = list(combine_tp(morp)))
  
  # 形成新的 fbswicd 对象
  res <- list(
    areacode = areacode,
    fbswicd = fbswicd,
    sitemorp = sitemorp,
    pop = pop
  )
  
  class(res) <- c("fbswicd", "list")
  return(res)
}

full_fbswicd <- function(x) {
  areacode_info <- as.data.frame(tidy_areacode(x[["areacode"]]))
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
