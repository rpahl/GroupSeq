# verify_arg

# Interface
expect_error(verify_arg(NULL, type = NULL), "is.character[(]type[)] is not TRUE")
expect_error(verify_arg(NULL, len = 1.5), "is.integer[(]len[)] is not TRUE")

# Function
expect_error(verify_arg(1), "1 is not of type character")
expect_true(verify_arg(1, "numeric"))
expect_error(verify_arg(1:2, "numeric"), "1:2 is not of type numeric")
expect_error(verify_arg(1:2, type = c("integer", "numeric")),
             "length[(]1:2[)] == 1 is not true")
expect_true(verify_arg(rnorm(2), type = "numeric", len = 2L))
expect_error(verify_arg(1, "integer"), "1 is not of type integer")
expect_true(verify_arg(1L, "integer"))
expect_error(verify_arg(NULL), "NULL is not of type character")
expect_error(verify_arg(1, type = "foo"), "1 is not of type foo")

