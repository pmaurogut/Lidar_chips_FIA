# Scripts to create directories to download lidar chips

These two scripts generate the folder structure to use for the download of the lidar chips. I will put these folders in one one drive shared folder so they can be downloaded.

-   BBOX

-   INTERSECTED_INDEXES

-   STATES

Downloaded data will go to the STATES folder. BBOX contains the 2Km x 2Km squares around each public location. INTERSECTED_INDEXES is empty for now.

Inside the STATES folder there is a folder with the state number for each state. Inside each state's folder there are three sub-folders as well.

-   BBOX

-   INTERSECTED_INDEXES

-   STATES

    -   1 (For state 1)

        -   BBOX

        -   INTERSECTED_INDEX

        -   PLOTS_LIDAR

    -   2 (For state 2)

        -   BBOX

        -   INTERSECTED_INDEX

        -   PLOTS_LIDAR

The states folders have all the information that is necessary to download one state

BBOX has the 2 x 2 Km bounding boxes for the state and INTERSECTED_INDEXES contain data in this case. There is one geopackage for the intersection of the bounding-boxes with the ENTWINEPLUS index plus another one for the intersection of the bounding boxes for with the index from NOAA. The intersection with NOAA is important for the west. For central US and eastern US the ENTWINE has almost everything that is in NOAA.

In the PLOTS_LIDAR folder there is one folder named STATECD_UNITCD_COUNTYCD_PLOT for each XY location. These folders can contain lidar data from zero, one or several lidar collections. All plots in the state have a folder, this can be useful as we add more indexes so the folders are all created at the beginning. But maybe we can only create the folders for those plots intersecting lidar acquisitions.

For a given intersection lidar collection x plot location we have three subfolders

-   1 (For state 1)

    -   BBOX

    -   INTERSECTED_INDEX

    -   PLOTS_LIDAR

        -   1_1\_1_1

            -   lidar_project_1

                -   PC (to download the Point cloud)

                -   DTM (to store the DTM

                -   Metrics (To store the lidar metrics)

            -   lidar_project_3

                -   PC (to download the Point cloud)

                -   DTM (to store the DTM

                -   Metrics (To store the lidar metrics)

        -   1_1\_1_2

            -   lidar_project_1

                -   PC (to download the Point cloud)

                -   DTM (to store the DTM

                -   Metrics (To store the lidar metrics)

The geopackages with the intersects store the path of each intersection lidar_acquisition x XY plot location. There are still some things to do. the intersections are not bounding boxes. The idea is to substitute the geometry of the intersection for a 2 km by 2 Km bounding box so they intersections have the geometry that will be used to clip the lidar. But this is not critical this is somthing that USGSlidar package can do.

There is a function that is not used yet called add_intersect_fields. This is a good place to implement some of the suggestions from Demetrios. It is not used.
