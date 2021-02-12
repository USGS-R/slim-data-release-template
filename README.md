# slim-data-release-template


## code

Need to have non-CRAN packages`dssecrets`, `meddle`, and `scipiper` installed, among other packages. 
Need to have CRAN package `sbtools` installed

- Create a data release or create a child item on sciencebase to experiment on
- Add "write" permissions on the release item for `cidamanager` (this is the `dssecrets` service account)
- Change the files and the functions in `src/` to what you need
- Edit data release information in `in_text/text_data_release.yml` to fit your data release and your file names and contents
- modify the sciencebase indentifier to your parent data release identifier (should be a string that is something like "5faaac68d34eb413d5df1f22")
- run `scmake()`
- validate your `out_xml/fgdc_metadata.xml` file with the [validator tool](https://mrdata.usgs.gov/validation/)
- fix any validation errors (usually this requires filling in metadata information in the `in_text/text_data_release.yml` and perhaps looking a the [metadata template](https://raw.githubusercontent.com/USGS-R/meddle/master/inst/extdata/FGDC_template.mustache))
- win

## remake.yml details

This slim template is designed to keep everything in a single remake yaml. So all data munging, manipulation, and file writing happens there, in addition to the sciencebase uploads.


This is the single push to sciencebase, it does the xml (metadata) and data at the same time. Because the upload step uses an internal task table, you can specify all files that should be pushed to the same sbid at one time. Again, because the upload step uses an internal task table, data files aren't replaced everytime you fix a metadata typo or add information to a metadata field. The result of the sciencebase push step is a file with timestamps for when each file got pushed that can be checked into GitHub. Having the file with timestamps in GitHub will clearly show when updates were made and will render nicely without having to build the object target locally.
```yaml
targets:
  all:
    depends:
      - log/sb_posted_files.csv
    
```

Read in (`st_read`) and manipulate data here. `extract_feature()` is from the `meddle` package and builds a list of structured spatial information for use in metadata files.

```yaml
  sf_spatial_data:
    command: st_read('example_data/example_shapefile/example_shapefile.shp')
    depends:
      - example_data/example_shapefile/example_shapefile.dbf
      - example_data/example_shapefile/example_shapefile.prj
      - example_data/example_shapefile/example_shapefile.shx
  
  spatial_metadata:
    command: extract_feature(sf_spatial_data)
```
write final metadata files as you want them to appear in the data release. 
```yaml
  out_data/cars.csv:
    command: file.copy(from = "example_data/example_cars.csv", 
      to = target_name, overwrite = TRUE)
  
  out_data/spatial.zip:
    command: sf_to_zip(zip_filename = target_name, 
      sf_object = sf_spatial_data, layer_name = I('spatial_data'))
  
  out_xml/fgdc_metadata.xml:
    command: render(filename = target_name,
      "in_text/text_data_release.yml",
      spatial_metadata)
```

Push the files to sciencebase using the `sb_replace_files` function from `src/sb_utils.R` of this repo template. If you are uploading many files at once using `sb_replace_files` (either in a file hash or just multiple files passed in through `...`), it is recommended to use a task table to do so. Internal task table methods are enabled by default. If you do not want to use an internal task table, set the `use_task_table = FALSE` when using `sb_replace_files`. You must also specify the file(s) where the `sb_replace_files` functions exist using the argument, `sources`. You can specify multiple files and a file hash file in one call to `sb_replace_files`. Currently, each `sb_replace_files` function can only push to one sbid. 

```yaml
  log/sb_posted_files.csv:
    command: sb_replace_files(filename = target_name, 
      sb_id = I('5faaac68d34eb413d5df1f22'),
      "out_data/spatial.zip",
      "out_data/cars.csv",
      "out_xml/fgdc_metadata.xml",
      sources = "src/sb_utils.R")
```


