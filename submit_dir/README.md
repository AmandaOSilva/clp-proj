
# PLR Project
# Boxes on SHelves (BOSH) 

## Requirements (http://people.brunel.ac.uk/~mastjjb/jeb/orlib/boxinfo.html)

```
For each product in turn:
   product family, quantity to be shelved, length, width, height

All orientations of each product are allowed and no product is allowed 
to overhang a shelf.

The two test problems have different bay files. 
These files are baytp1 and baytp2 and the format of these files is:

For each bay in turn:
   width, height, depth, available height
    1200   2400    650     3000
Bays must be shelved used in same sequence as given in each file.

Each bay can have a number of possible shelves.
These are given in the file shelves
The format of this file is:

For each shelf in turn:
    shelf number, thickness, position, top gap, left gap, inter gap, right gap
	37	   40   	1800   	  15	   10 	    10 	       10 
```

## "Paper" approach 
```
| ?- consult('src/bosh_paper').
```
## "Cumulative" approach (can't be loaded together) 
```
| ?- consult('src/bosh_paper_cumulative').
```

## Tests
### shelve all families (6000 products, 67 families), one by one: 
```
| ?- go.
``` 

###  shelve a single family (1 =< N =< 67): 
```
| ?- go(N). 
```

### run and visualize: first you need to export results, then run python script 
 
```
| ?- go_export(N).

# if needed: "pip install tk matplotlib"
python src/visualizer.py
 ```