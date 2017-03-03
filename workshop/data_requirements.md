# Data requirements for potential example data

The following are required to run "feasibility simulations" on a given data set.

Coordinates should already be suitably projected, using metric units and be consistent across the data set.

## Study area/strata

Bounding "box" around the area under investigation, i.e., the "top level" stratum. Any sub-strata that are used for estimation should also be included.

Format: GIS shapefile. Spreadsheet or R `data.frame` with vertices of the strata/study area with appropriate labels and guide to nesting.

## Tracklines

Location of realised effort (not the survey design, but the effort as carried out). Pre-segmented transect lines, with centroids or start-stop coordinates. Segments may be defined as either

1. sections of transect that are approximately square, being as wide and long as twice the truncation (i.e., one truncation distance wide each side of the centreline and twice the truncation distance long), or,
2. sections of transect that have, say, 30 mins of on effort time in them.

Format: GIS shapefile, spreadsheet or saved R `data.frame` with appropriate columns: location, unique label, length in metres and length in seconds; one row per segment..

## "Weather" covariate

At least a 2 level factor indicating "good" or "bad" weather, could be more complicated and be something like Beaufort sea state. This must be available for every segment of the effort (if conditions were recorded on change throughout the survey, this should be enough).

Format: GIS data (attached to shapefile), additional column in spreadsheet or column in R `data.frame` above. Guide to the encoding of this covariate.

## Location of observations

The location of each observation, at least to the level of the segment (e.g., "observation 2 was in segment 2011-A-2").

Group size will be ignored, so it is not necessary to include this information.

Format: shapefile, spreadsheet or `data.frame`, location columns plus label to associate with the tracklines above.


## Fitted detection function

Details of a detection function fitted to the distances with the aforementioned weather covariate included.

Format: Parameter estimates for the detection function, details of functional form (half-normal, hazard-rate, adjustment terms etc), truncation distance.



