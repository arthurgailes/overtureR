# Convert a tbl_sql object to a overture_call object

This function adds the overture_call class to a tbl_sql object. It is
primarily used internally#' by the open_curtain() function but can also
be used directly on tbl_sql \#' objects representing Overture Maps data.

## Usage

``` r
as_overture(x, type, theme = get_theme_from_type(type))
```

## Arguments

- x:

  A tbl_sql object representing an Overture Maps dataset.

- type:

  A string specifying the type of overture dataset to read. Setting to
  "\*" or `NULL` will read all types for a given theme.

- theme:

  Inferred from type by default. Must be set if type is "\*" or NULL

## Value

A tbl_sql object with the additional class overture_call and attributes
overture_type and overture_theme.

## Details

The function adds the overture_call class as the first class of the
object

## Examples

``` r
if (FALSE) { # interactive()
# The open_curtain() function already uses as_overture() internally,
# but you can also use it directly:
conn <- stage_conn()
division <- open_curtain("division", tablename = "test")

class(division)

# views
division2 <- tbl(conn, "test")
division2 <- as_overture(division2)

exit_stage(conn)
}
```
