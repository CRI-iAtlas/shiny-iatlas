# iAtlas Data Structures

<span style="color: red; font: normal 1.4rem/1 arial">**Please Note:** This file is a work in progress. Please do not reference this file until this warning has been removed.</span>

When importing data into iAtlas, it is very imprtant that the following conventions are followed. Doing so will get the new data into the iAtlas database and make it available for the app.

## File Format

  - feather files

    All data should come into the iAtlas application in the form of feather files. Feather files allow for fast reading and help ensure structural integrity.

## Data Locations

All data (feather files) should be located in the `data` folder.

Within the `data` folder, data files should be segregated as follows:

  - Patients & samples

  - Genes

  - Features

  - Tags

  - Results

  - Relationships