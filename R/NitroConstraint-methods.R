#' @rdname tnt_cmd
setMethod("tnt_cmd", "NitroConstraint", function (n) {
  fixed <- paste(which(n@fixed) - 1, collapse = " ")
  floating <- paste(which(n@floating) - 1, collapse = " ")
  if (nchar(floating) > 0) {
    floating <- paste(" (", floating, ")")
  }
  type <- ifelse(n@is_positive, "+", "-")
  return(paste(type, " [ ", fixed, floating, " ]", sep = ""))
})
