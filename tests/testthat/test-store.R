test_that("ragnar_store_update/insert", {
  store <- ragnar_store_create(embed = \(x) matrix(nrow = length(x), ncol = 100, stats::runif(100)))
  
  chunks <- data.frame(
    origin = "foo",
    hash = "foo",
    text = "foo"
  )
  ragnar_store_update(store, chunks)

  val <- dbGetQuery(store@.con, "select origin, hash, text from chunks")
  expect_equal(val, chunks)

  # now try to update the store again - without changing the hash
  chunks2 <- data.frame(
    origin = "foo",
    hash = "foo",
    text = "bar"
  )
  ragnar_store_update(store, chunks2)

  # Expect that the text is not updated, because the hash is the same
  val <- dbGetQuery(store@.con, "select origin, hash, text from chunks")
  expect_equal(val, chunks)

  # now try to update the store again - changing the hash
  chunks2 <- data.frame(
    origin = "foo",
    hash = "bar",
    text = "bar"
  )
  ragnar_store_update(store, chunks2)
  
  # Expect that the text is updated
  val <- dbGetQuery(store@.con, "select origin, hash, text from chunks")
  expect_equal(val, chunks2)

  # Finally, try adding a new origin - even with the same hash
  chunks2 <- data.frame(
    origin = "bar",
    hash = "bar",
    text = "bar"
  )
  ragnar_store_update(store, chunks2)

  # Expect the origin is added
  val <- dbGetQuery(store@.con, "select origin, hash, text from chunks")
  expect_equal(nrow(val), 2)  

  # Try adding with insert
  chunks2 <- data.frame(
    origin = "bar",
    hash = "bar",
    text = "bar"
  )
  ragnar_store_insert(store, chunks2)

  # Since we used insert, there's no checking if the hash is the same
  val <- dbGetQuery(store@.con, "select origin, hash, text from chunks")
  expect_equal(nrow(val), 3)  
})

test_that("behavior when no hash/origin are provided", {
  store <- ragnar_store_create(embed = \(x) matrix(nrow = length(x), ncol = 100, stats::runif(100)))
    
  chunks <- data.frame(
    text = "foo"
  )
  # users must explicitly set origin and hash when updating!
  expect_error(
    ragnar_store_update(store, chunks),
    "chunks` must have `origin` and `hash`"
  )
  # they can insert though
  ragnar_store_insert(store, chunks)
  
  val <- dbGetQuery(store@.con, "select origin, hash, text from chunks")
  expect_equal(val, data.frame(origin = NA_character_, hash = rlang::hash("foo"), text = "foo"))

  # if they insert again, even though the text has the same hash, we don't update anything
  ragnar_store_insert(store, chunks)

  # Expect that the text is not updated, because the hash is the same
  val <- dbGetQuery(store@.con, "select origin, hash, text from chunks")
  expect_equal(
    val, 
    rbind(
      data.frame(origin = NA_character_, hash = rlang::hash("foo"), text = "foo"),
      data.frame(origin = NA_character_, hash = rlang::hash("foo"), text = "foo")
    )
  )
})