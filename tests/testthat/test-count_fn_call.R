e1 <- quote(View %<% x)
e2 <- quote(x %>% View())
e3 <- quote(df %>% filter(x == 1) %>% View)

# No need to worry for native pipe operations! Try this:
# e4 <- quote(x |> View)

f1 <- TreeHarp(e1, TRUE)
f2 <- TreeHarp(e2, TRUE)
f3 <- TreeHarp(e3, TRUE)

test_that("count_fn_call: pipe fixes", {
  expect_equal(count_fn_call(f3, pattern="^[vV]iew"), 1L)
  expect_equal(count_fn_call(f1, pattern="^[vV]iew"), 1L)
  expect_equal(count_fn_call(f2, pattern="^[vV]iew"), 1L)
})
