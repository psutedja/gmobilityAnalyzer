# gmobilityAnalyzer
Simple R project to read Google Mobility data

The script first read the supplied Google mobility data, and format it into a data frame.

There are several main function
retrieveByRegion = read the mobility data into data frame, filtered by provided country_region and sub_region_1

plotAndSave = run the retrieveByRegion function based on the provided country_region and sub_region_1, and create line plot of each sub_region_1 vs national value, and save it in the ./data folder, based on the sub_regin_1 name.

Added command to run plotAndSave on all Indonesia regions
