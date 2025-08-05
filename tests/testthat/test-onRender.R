test_that("onRender works as expected", {
  x <- htmlwidgets::createWidget(
    name = "testWidget",
    x = list(),
    width = 100,
    height = 100
  )

  expect_equal(
    onRender(x, jsCode = NULL),
    x
  )

  with_jsHooks <- x

  with_jsHooks$jsHooks[["render"]] <- c(
    with_jsHooks$jsHooks[["render"]],
    list(list(
      code = "function(el, x, data) { console.log('test'); }",
      data = NULL
    ))
  )

  expect_equal(
    onRender(
      x,
      jsCode = "function(el, x, data) { console.log('test'); }"
    ),
    with_jsHooks
  )

  with_jsHooks <- x

  with_jsHooks$jsHooks[["render"]] <- c(
    with_jsHooks$jsHooks[["render"]],
    list(list(
      code = "function(el, x, data) { console.log('test'); }\nfunction(el, x, data) { console.log('test'); }",
      data = NULL
    ))
  )

  expect_equal(
    onRender(
      x,
      jsCode = c(
        "function(el, x, data) { console.log('test'); }",
        "function(el, x, data) { console.log('test'); }"
      )
    ),
    with_jsHooks
  )
})
