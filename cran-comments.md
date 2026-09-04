## Patch update 0.2.6

`open_curtain()`'s default `base_url` no longer hardcodes a specific
Overture release; it now discovers the latest release dynamically
(new exported `latest_overture_release()`), so future Overture releases
no longer require a package update. Version bumped from 0.2.4 to 0.2.6
to catch up with 0.2.5, which was previously submitted without a
corresponding version-controlled release.

## R CMD check results

0 errors | 0 warnings | 0 notes
