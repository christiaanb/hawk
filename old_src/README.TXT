Visual Hawk Installation Instructions
- Byron on Jan 14, 1999
- Thomas on Sep 24, 1999

* Install Hugs 98 and HaskellScript.

* If hawk lives in a different place then C:\ change the four
  registry files in the visual_hawk directory (WARNING: updating
  the registry can destroy your system, make a backup before continuing).

* Double click on the four registry files to install them in the 
  registry.

* Install the Hawk distribution (be sure to set HAWKROOT environment
  variable appropriately in Control-panel/System/Environment)
  
* Set the environment variable VISUAL_HAWKROOT to the location of
  this directory.

* When opening a model for the first time it will fail to find the
  templates.  The template files its looking for are in 
  %VISUAL_HAWKROOT/lib  

* The models are in %VISUAL_HAWKROOT/models, try the two icons on the
  toolbar to view the specification or to execute it. Double clicking
  on a wire after/during simulation shows the value/clock information.

* Try the LawDemo.vsd model, and choose update transformation from the
  menu. By selecting more or less component you change which rules
  match. Select any of the laws to have it applied to the current
  specification.

* Enjoy



