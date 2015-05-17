// CREATE AND FILL NUMBER ARRAY WITH NON-UNIQUE RANDOM NUMBERS
var myNumArray = create_non_unique_random_array(16,5,20);

function random_vector(num_elements,min,max) {
    var nums = new Array;
    for (var element=0; element < num_elements; element++) {
        nums[element] = random_number(min,max);
    }

    return (nums);
}

// suppose we have integers 1 to 100
// store densities for 3 distributions:
   //  (x-5)/100 ~ Beta(7,7) is symmetric, close to normal.
   //  (x-5)/100 ~ Beta(3,6) is right skewed
   //  (x-5)/100 ~ Beta(6,3) is left skewed
// plot and fill the overlapping polygons in D3 or svg.

// user selects one. The other 2 disappear.

// User selects sample size.
// draw the original sample with replacement.
  // they pop up in some nice way.
  // now the distributions go away leaving perhaps the mean.

/// from:  http://bl.ocks.org/mbostock/3883195




  
