# grout dev


* New function 'tile_spec()' to generate tile specification from input
 extent and dimension at a given zoom.  New function `tile_zoom()` return
 the natural maximum zoom value. 
 
* Removed all spatial input, just use dimension, extent. 

* Original `grout()` function is effectively removed, and renamed to internal .groutfrom(). 

* Added extent (xmin,xmax, ymin, ymax ) to `tile_index()`. 

* Removed use of raster for core parts, package not yet clear. We use {vaster} for
basic extent and cell logic. No plotting atm. 

o BREAKING CHANGE

* `tiles()` function replaced by `grout()`

# grout 0.0.2.9001

* New function `tile_index()` to generate offsets table, for reading/writing with GDAL. 

* Fixed missing projection metadata in `$tileraster`. 

