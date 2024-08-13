# overtureR 0.2.0

* Add support for downloading Overture Maps data via `record_overture` and 
`snapshot_overture`. These functions return a lazy 'overture_call' dataframe
linked to the new local dataset.
  
* The second parameter to `open_curtain` has changed from 'bbox' to 
'spatial_filter', which allows both bounding boxes (named vector or class 
'bbox'), 'sf' objects, or another `dbplyr` dataframe (e.g. a different 
`overtureR` dataset/table. In the latter two cases, the data will first be 
filtered by the bounding box of `spatial_filter`, then geographically if 
necessary. Filtering is currently by intersection.

* `open_curtain` parameter 'bbox' is deprecated, and will likely be removed in a
future release.

* Class 'overture_call' has been added to facilitate `collect` calls directly as
`sf` objects.

* `collect_sf` is now deprecated, use `collect`.

# overtureR 0.1.0

* Initial CRAN submission
