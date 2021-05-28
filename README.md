### {midsprint}

This package provides coaches with a low barrier of entry into athlete profiling and positional tracking data. This file provides you with a simple walkthrough that allows you to model athlete: 

* Speed-Acceleration 
* Speed and acceleration over time from game data 
* Speed and acceleration over time from Combine data

### Current Updates

* Major changes: some functions are not updated from version 0.1.1 and are not included in this update. This is a temporary change and will be rectified shortly. Functions omitted do not affect modeling and reporting aspects of the package, and were omitted because of inconsistency in how they are called upon. They are functions that are (presumably) not often called like `time_to_position_compare()` which calculates how long a players will take to reach a given distance. 

* Version 0.2.0 simplifies the functionality of the package, allowing you to create simple coach's reports in minutes. This is accomplished by relying on the following packages: `ggplot2`, `patchwork`, and `glue`. 

* Not much has changed in terms of workflow. However, functions within the workflow have been updated to provide greater personalization. For example, game_data() initially required the athlete's speed and acceleration values. Now, you can set the deault speed units (initially set as m/s) and can include the athlete's name. This allows the functions to return more accurate player values.

* It is encouraged that you include speed metrics because the functions on the back-end assume all values are in metric. If your default speeds and acceleration are not in metric, you can type in either "yd/s", "ft/s", "mi/h", or "km/h". There is some leeway if your spelling is different, i.e. "km/h" can be typed in as "kilometers/hour" but this is not advised.

Please note: speed and acceleration must be in the same units. The functions cannot convert speed and acceleration values separately. To do so is unfeasible and would disrupt the workflow.

* Finally, the updated vignette (walthrough) is more compact. The "must-knows" are set at the beginning of each section with more details proceeding them for those interested.

### Installing and Loading midsprint

To install midsprint, copy and paste the following code into your 'Console'.

```
devtools::install_github("aaronzpearson/midsprint")
```
To load the package, copy and pase the following into your 'Console'.

```
library(midsprint)
```

### {midsprint} Functionality

Please see "Welcome to {midsprint}" for a simple walkthrough.

Please see "sample_report.pdf" for an example of what you can create in minutes.

Note: when using {midsprint} in your published work, please run `citation(midsprint)` for formatting.

### Contact

To reach out to report an error or to discuss this package and its functionality further, please email Aaron at `aaronzpearson@outlook.com`.
