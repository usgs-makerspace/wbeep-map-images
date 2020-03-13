# wbeep-map-images
Create local PNG maps of specific days using model output data.

WARNING: This is not currently hooked up to a shared cache. May add S3 instead of GD as a next step.

# Building the images

This code is used to build images using data from the WBEEP oNHM project. Currently, it is set up to rebuild the storage images locally (labs.waterdata.usgs.gov/estimated-availability). It doesn't currently combine them into a GIF since that is a step we've been doing outside of R in Photoshop. 

To build images:

1. Change the dates in `1_fetch.yml` under the target `date_range`. 
2. Run the following to execute the full pipeline.

```r
scmake(remake_file = '1_fetch.yml')
scmake(remake_file = '2_process.yml')
scmake('6_visualize/log/wbeep_storage_maps.ind', remake_file = '6_visualize.yml') # build all images at once
```

3. Find the images in `6_visualize/out/`.
