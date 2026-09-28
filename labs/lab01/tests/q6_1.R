test = list(
  name = "q6_1",
  cases = list(
    ottr::TestCase$new(
      name = "q6_1",
      points = 1,
      code = {
        if (is.null(samson_lat) || is.null(samson_lng)) stop("Replace NULL with the coordinates.")
        if (!isTRUE(abs(samson_lat - 38.89936573701641) < 0.01)) stop("Check samson_lat (should be about 38.8994).")
        if (!isTRUE(abs(samson_lng - (-77.04638577421989)) < 0.01)) stop("Check samson_lng (should be about -77.0464).")
      }
    )
  )
)
