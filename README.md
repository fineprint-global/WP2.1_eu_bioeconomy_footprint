# EU Bioeconomy Footprint 



### Calculations
To calculate the cropland footprint of the EU non-food bioeconomy, run the script [footprints.R](./footprints.R). Note that before running the script, you must download the EXIOBASE MRIO database, freely available from www.exiobase.eu. Files containing information on land embodied in non-food uses of agricultural commodities were generated with IIASA's [LANDFLOW model](http://www.iiasa.ac.at/web/home/research/researchPrograms/water/Landflow.html) and are located in the [input](./input) folder.



### Maps and figures
To produce the maps, run the script [maps.R](./maps.R). The mosaic chart is generated running the script [mosaic.R](./mosaic.R). The required crop production maps are downloaded automatically from [http://spam05.harvestchoice.org/v3r2/tiff/](http://spam05.harvestchoice.org/v3r2/tiff/) when running the code. Please note, that this script is independent of the footprints script, as the necessary results from the footprint calculations are provided in the [input](./input) folder. Thus, all maps and figures can be reproduced without previously running the footprint calculations.



### License
This code is distributed under the GNU General Public License Version 3.

