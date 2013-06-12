#unitMongoRecord
Developmental code to parse electronic data dump from test lot and add to mongoDB database.

To use, execute ./addUnits \<rawDataFile\> \<lotIndicator\> \<numberOfUnitsToRead\> \<fluid\> \<group\> \<liveWrite?\>.
Alternatively, the data can be manually entered by executing ./addUnitsManual \<fluid\> \<group\> \<liveWrite?\>.

##Requirements
1. The rawDataFile should be in the form output from the test lot data.  Line endings must be MANUALLY converted to UNIX. (For example, open in gedit, save with different line ending setting.)
2. The lotIndicator should be the SN prefix, e.g. for SN 8061-2-90, lotIndicator is 8061-2
3. Number of units to be read from input file.  Exception thrown if this exceeds number of units in rawDataFile
4. Fluid used for filling
5. Group identifier given to that batch of units
6. Livewrite? 1 if data should be written to production database as specified in C code and both addUnits scripts.  Otherwise, data written to test database as specified in addUnits scripts.