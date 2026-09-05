# Convert a tbl_sql object to an overture_call object

Adds the `overture_call` class to a `tbl_sql` object.
[`open_curtain()`](https://arthurgailes.github.io/overtureR/reference/open_curtain.md)
does this for you; call it directly on a lazy table of Overture data you
built yourself, so that
[`collect()`](https://dplyr.tidyverse.org/reference/compute.html)
returns `sf` and
[`record_overture()`](https://arthurgailes.github.io/overtureR/reference/record_overture.md)
knows the type and theme of the data.

## Usage

``` r
as_overture(x, type, theme = get_theme_from_type(type), release = NULL)
```

## Arguments

- x:

  A tbl_sql object representing an Overture Maps dataset.

- type:

  A string specifying the type of overture dataset to read. Setting to
  "\*" or `NULL` will read all types for a given theme. See
  [`overture_types()`](https://arthurgailes.github.io/overtureR/reference/overture_types.md)
  for the valid values.

- theme:

  Inferred from type by default. Must be set if type is "\*" or `NULL`.

- release:

  The Overture release the data came from, such as `"2026-08-19.0"`, or
  `NULL` if unknown.

## Value

A tbl_sql object with the additional class `overture_call` and an
`overture_playbill` attribute: a list with `type`, `theme` and
`release`.

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
division2 <- as_overture(division2, "division")

strike_stage(conn)
}
```
