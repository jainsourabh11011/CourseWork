##Five functions I wished I had known five years ago

Like just about every other grad statistics student, my first 2-3 years of education in R programming was a bit disjoint. 
I’d pick up skills and learn new functions from random courses and sources. This post features five functions that I wish I’d 
known soon after I started programming in R but didn’t learn until 2 or more years in.

identical(x, y). Check if two R objects x and y are exactly identical.

txtProgressBar, setTxtProgressBar. Set the parameters of a progress bar using txtProgressBar, then print the progress bar in each 
iteration of a loop using setTxtProgressBar. Pretty much any for loop that takes more than five seconds to run would benefit 
from the use of these functions.

package.skeleton. Generate all the skeleton files for a package using the objects in an R session. I’ve taken to creating a 
package for most projects I work on to organize data and functions. If you aren’t familiar with package building, start 
by viewing this video guide.

by(data, INDICES, FUN, …). Apply a function across subsets of data, where the subsets are defined by the INDICES argument. 
The by function returns an object that summarizes each particular subset of the data according to the indices used.

traceback. If there’s an error returned from a function, use the traceback function to trace the last command run that caused the error.

###identical(x, y)

Check whether x and y are identical objects.
◾x – Any R object.
◾y – Any R object.


Example. Below we show that we can recreate two identical sets of pseudorandom numbers — exactly — by setting the seed. We 
verify that the two sets are identical using the identical function.
```{r}
set.seed(5)
x <- rnorm(3)
x
set.seed(5)
y <- rnorm(3)
y

identical(x, y)

z <- c(-0.8408555, 1.3843593, -1.2554919)
> identical(x, z)
```

The objects x and y are not just really similar; they are identical. If we investigated z, we would find that x and z are 
different due to rounding errors.

Tip. The identical function may be applied to complex R objects, including lists and functions

###txtProgressBar, setTxtProgressBar

Create a progress bar for a loop using the txtProgressBar function, then use the setTxtProgressBar command within the 
loop to print the progress bar, which updates as the loop progresses.

txtProgressBar(min, max, style=1)
◾min – The minimum value in the loop.
◾max – The maximum value in the loop.
◾style – A number representing the style, where 3 is my favorite.

setTxtProgressBar(pb, value)
◾pb – A progress bar object that was output from txtProgressBar.
◾value – The current iteration value.



Example. The example below creates a loop of 100 values and uses Sys.sleep to pause for a 20ms during each loop. The progress bar 
is shown full, though when viewed live it progresses across the console.

```{r}
#=====> Example <=====#
SEQ  <- seq(1,100)
pb   <- txtProgressBar(1, 100, style=3)
TIME <- Sys.time()
for(i in SEQ){
     Sys.sleep(0.02)
     setTxtProgressBar(pb, i)
}
Sys.time() - TIME


#=====> Tip <=====#
SEQ  <- seq(1,100000)
TIME <- Sys.time()
for(i in SEQ){
     Sys.sleep(0.00002)
}
Sys.time() - TIME

pb   <- txtProgressBar(1, 100000, style=3)
TIME <- Sys.time()
for(i in SEQ){
     Sys.sleep(0.00002)
     setTxtProgressBar(pb, i)
}
Sys.time() - TIME

pb   <- txtProgressBar(1, 100000, style=3)
TIME <- Sys.time()
for(i in SEQ){
     Sys.sleep(0.00002)
     if(i %% 1000 == 0){
          setTxtProgressBar(pb, i)
     }
}
Sys.time() - TIME
```

Tip. If each iteration in the loop is much under a millisecond, then setting the progress bar in each iteration can notably 
slow things down. The three blocks of code below provide another example. The loop in the first block is a "baseline" for how 
quickly the loop can run. The second block shows a loop that has been greatly slowed by setting the progress bar in every iteration. 
The final block uses an if statement with the modulo command %% to update the progress bar only once every 1000 iterations, 
which speeds up the code again.


###package.skeleton

package.skeleton(name, list, environment = .GlobalEnv, path=".")

Initialize the files for an R package. First load in all the objects to be included in the package into the current R session, 
then run the package.skeleton command. See the Building Packages tab on this blog for videos about building R packages.

* ◾name – A character string for the name of the package.  
* ◾list – If only some objects in the current session should be included in the package, list them here in a character vector.  
* ◾environment – The environment where the objects are looked for. (Most users should leave this argument alone.)  
* ◾path – By default, the package will be saved in the current working directory. Change the path argument to specify a different 
place to save the package files.


Example. In the sample code below, four objects have been preloaded into the current R session. Then the package.skeleton function 
was used to initialize the package. This command creates a package folder in the current working directory that includes 
files for all of the objects in the current R session.

Tip. Several additional options are available in the package.skeleton function, including list to specify only a subset of 
objects in the current session for the package.

###by(data, INDICES, FUN, …)

Apply a function across subsets of data, where the subsets are defined by the INDICES argument. The by function returns a list, where each list item represents the results for a particular subset of the data.

* ◾data – The full set of data, e.g. a vector or data.frame.
* ◾INDICES – A vector describing how the data subsets are to be constructed (e.g. a factor vector). Multiple columns in a data 
frame may also be given, where each combination of the variables defines a new subset of data.
* ◾FUN – A function to apply to each subset of data.
* ◾… – Any additional arguments to pass to FUN.

Example. The textbooks data from the openintro package is loaded. Then we examine a summary of the price differences between new 
textbook prices at the UCLA bookstore and on Amazon for two subsets: courses where one book is required and courses where more 
than one book is required.

```{r}

library(openintro)
data(textbooks)
by(textbooks$diff, textbooks$more, summary)
```

###traceback()

traceback provides a trail of functions to track where and why an error occurred. For instance, if the error was deeply nested, 
then traceback is sort of like a road map showing where the error occurred.

Example. Below the getReturns function from the stockPortfolio package is used to retrieve stock performance data for both 
Research in Motion (RIMM), Apple (AAPL), and a third stock that does not exist. The traceback function is used here to 
query the source of the error, and it is nested within several layers of functions. Seeing that there was a problem with a 
call to a URL (read.delim(URL, …)) is a hint that there may be trouble with a stock that I chose.

As a developer, the traceback function can be powerfully paired with the debug function. First identify the function call 
that is causing trouble, then use debug to go through the function, one line at a time, until arriving at that line of code, 
then investigate the arguments (e.g. URL) to identify what went wrong.

```{r}
# install.packages("stockPortfolio")
library(stockPortfolio)

gr2 <- getReturns(c("RIMM", "AAPL", "oweifjwoej"))

traceback()
```

