test = list(
  name = "q6_2",
  cases = list(
    ottr::TestCase$new(
      name = "q6_2",
      points = 1,
      code = {
        if (is.null(coords)) stop("Replace NULL with your answer.")
        if (length(coords) != 2) stop("`coords` should have 2 values.")
        if (!isTRUE(all.equal(as.numeric(coords), c(samson_lat, samson_lng)))) stop("`coords` should be c(samson_lat, samson_lng).")
      }
    )
  )
)
