mini_fixture <- function() {
  demand <- sf::st_as_sf(
    data.frame(id = c("D1", "D2", "D3"), x = c(0, 1, 2), y = c(0, 0, 0)),
    coords = c("x", "y"), crs = 4326
  )
  candidate <- sf::st_as_sf(
    data.frame(id = c("C1", "C2"), fixed_cost = c(3, 1), capacity = c(1, 3),
               x = c(10, 20), y = c(0, 0)),
    coords = c("x", "y"), crs = 4326
  )
  existing <- sf::st_as_sf(
    data.frame(id = c("E2"), x = c(30), y = c(0)),
    coords = c("x", "y"), crs = 4326
  )
  od_candidates <- data.frame(
    from_id = rep(c("D1", "D2", "D3"), times = 2),
    to_id   = rep(c("C1", "C2"), each = 3),
    distance = c(1, 2, 9,   5, 5, 6)
  )
  od_existing <- data.frame(
    from_id = c("D1", "D2", "D3"),
    to_id   = c("E2", "E2", "E2"),
    distance = c(0.5, 0.5, 0.5)
  )
  list(demand = demand, candidate = candidate, existing = existing,
       od_candidates = od_candidates, od_existing = od_existing)
}

competition_fixture <- function() {
  demand <- sf::st_as_sf(
    data.frame(id = c("D1", "D2", "D3"), x = c(0, 1, 2), y = c(0, 0, 0)),
    coords = c("x", "y"), crs = 4326
  )
  candidate <- sf::st_as_sf(
    data.frame(id = c("C1", "C2"), fixed_cost = c(0, 0), x = c(10, 20), y = c(0, 0)),
    coords = c("x", "y"), crs = 4326
  )
  existing <- sf::st_as_sf(
    data.frame(id = c("E1"), x = c(30), y = c(0)),
    coords = c("x", "y"), crs = 4326
  )
  od_candidates <- data.frame(
    from_id = rep(c("D1", "D2", "D3"), times = 2),
    to_id   = rep(c("C1", "C2"), each = 3),
    distance = c(4, 4, 5,   15, 15, 1)
  )
  od_existing <- data.frame(
    from_id = c("D1", "D2", "D3"),
    to_id   = c("E1", "E1", "E1"),
    distance = c(10, 10, 2)
  )
  list(demand = demand, candidate = candidate, existing = existing,
       od_candidates = od_candidates, od_existing = od_existing)
}
