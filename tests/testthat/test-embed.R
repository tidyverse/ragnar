factory_args <- list(
  embed_ollama = list(model = "nomic-embed-text"),
  embed_openai = list(model = "text-embedding-3-large"),
  embed_azure_openai = list(model = "text-embedding-3-large"),
  embed_databricks = list(model = "databricks-gte-large-en"),
  embed_google_gemini = list(model = "text-embedding-004"),
  embed_google_vertex = list(
    model = "text-embedding-005",
    location = "us-central1",
    project_id = "test-project"
  ),
  embed_bedrock = list(
    model = "amazon.titan-embed-text-v2:0",
    profile = NULL,
    api_args = list(dimensions = 256L)
  )
)

for (name in names(factory_args)) {
  test_that(paste(name, "factories preserve configured arguments"), {
    args <- factory_args[[name]]
    factory <- getExportedValue("ragnar", name)
    embed <- do.call(factory, args)
    embed_null <- do.call(factory, c(list(x = NULL), args))

    local_mocked_bindings(
      !!!setNames(list(function(x, ...) c(list(x = x), list(...))), name),
      .package = "ragnar"
    )

    expected <- c(list(x = "hello world"), args)
    expect_identical(embed("hello world"), expected)
    expect_identical(embed_null("hello world"), expected)
    expect_identical(
      unserialize(serialize(embed, NULL))("hello world"),
      expected
    )
    expect_identical(
      unserialize(serialize(embed_null, NULL))("hello world"),
      expected
    )
  })
}
