## ImageJ shell length measurments


For each image:

1. file -> open to open image

2. set measurement from reference ruler in image

	a. click line icon and draw line (e.g. 1/4 inch)
	b. then go to menu bar analyze -> set scale
		- change known distance to 0.25 
		- change unit of length to inch
		- click ok 

3. click line icon from toolbar and draw straight line from shell hinge to opposite side of the shell (a perpendicular line, as if you were cutting the animal in half). Zoom in to be sure you are measuring only shell and not mantle (meat). 

4. then go to menu bar -> Analyze -> measure
	- this create a pop-up window where all your measurements will be stored. You will see your first measurement there. leave this window open.

5. Now you can measure all the animals, each time drawing a line across the shell from the shell hinge to the opposite side, go to menu bar -> analyze -> measure.

6. Once you've made all your measurements, click the 'x' in the measurements pop-up window and you will be prompted to save the data. **Click save!** to save the data as a CSV. Save it as the image name.

7. copy data over to google sheet (https://docs.google.com/spreadsheets/d/1Y75kqmWWuAink48U4vHjgaEWLgbKEyJRrspbAxzW3jc/edit#gid=1208042429). Create a separate tab for each image. 

8. Create a column for length(cm) and calculate length in cm for each measurement.


### Notes:
- only count animals that look alive. There is lots of debris, shrivled up dead animals. Look for plump siphons (brown part) and rounder looking animals. 

- Determine a way to keep track of animals you've measured as to not measure the same animal twice. Maybe it's possible to draw quadrants in the photo? Or number the animals?

	- To make photo into quadrants: 
		a. After opening the image, go to menu bar Image -> Stacks -> Tools -> Montage to Stack
		b. Set Columns to 2
		c. Set Rows to 2
		d. Click ok
		e. Now, the image is split into quadrants -- tip: use the scroll of mouse to go through each of the four quadrants more easily.
	
	- To label each animal by number: 
		a. Go to menu Analyze -> Set Measurements
		b. Select Add to overlay checkbox
		c. Click ok
		d. Now, once an animal is measured, a number will pop up where the measurement was placed, which corresponds with the measurement information for that specific animal. 
 