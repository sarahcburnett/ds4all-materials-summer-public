test = list(
  name = "q6_3",
  cases = list(
    ottr::TestCase$new(
      name = "q6_3",
      points = 1,
      code = {
        if (is.null(named_coords)) stop("Replace NULL with your answer.")
        if (!all(c("lat", "lng") %in% names(named_coords))) stop("`named_coords` needs elements named lat and lng.")
        if (!isTRUE(all.equal(unname(named_coords[["lat"]]), samson_lat))) stop("`named_coords` lat should be samson_lat.")
        if (!isTRUE(all.equal(unname(named_coords[["lng"]]), samson_lng))) stop("`named_coords` lng should be samson_lng.")
      }
    )
  )
)
