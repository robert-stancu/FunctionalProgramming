FP - A1 - Robert Stancu - 07/05/2021

Common.hs

The solutions are based on some simple mathematical/visual observations:
-vflip = reversing the lines (1st line = nth line etc)
-hflip = reversing the columns , so we map the reverse function to each line (so that 1st elem becomes last etc)
-rot90l = we obtain it by first transposing the matrix and then reverse it
-rot90r = same idea but applied column-wise, so we map the reverse to each line	
-rot180 = just compose 2 rot90

Greyscale.hs
-invert = we have to apply 255-x where x is the pixel so we use 2 maps 
-crop = the most tricky one
	-first make sure to have only required lines so we drop a-1 lines from the image, 
		then take c of the remaining where a = line coordinate of top right corner and c = height of rectangle
	-then, in order to crop the columns we map each line as following:
		we take b elements from the line and then we drop (b - d) elements ,
		where b = column coordinate of top right corner and d = width of rectangle
		for example if corner is on (_,7) and width = 5 we want to drop first 2 elements of each line

-brightness = add the parameter value to each pixel so we use 2 maps 
-mask = we use 2 zipWith functions in order to traverse both matrices in the same time (the mask and the image)
	if the value on the mask is false then we set the pixel to 0 (black) else we keep the original pixel


Color.hs
-invert: same as greyscale, just that we apply 255- on every element of the tuple 255-a, 255-b, 255-c
-crop: identical to greyscale (could be a common function :) )
-swapColors: 2 maps, permute in anonymous function
-brightness: same as greyscale, just add the value to each element of the tuple
-mask: same as greyscale, set the whole tuple to (0,0,0) if False
-toGreyScale (toPGM) : map each tuple to the average of the values in the tuple (we use 2 maps)
