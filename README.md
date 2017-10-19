# blastxml: Extract data from Blast XML files

### R requirements

[R (>= 3.2)](https://www.r-project.org/) 

[xml2](https://cran.r-project.org/package=xml2)

[magrittr](https://cran.r-project.org/package=magrittr)

[tibble](https://cran.r-project.org/package=tibble)

### Installation

```R
library(devtools)
install_github("BigelowLab/blastxml")

### or into the the system-wide library
with_lib(.Library, install_github("BigelowLab/blastxml"))
```

### Example

Retrieve basic info about the file contents.

```

xmlfile <- system.file("extdata", "blastn.xml.gz", package = "blastxml")
info <- blastxml_info(xmlfile)

```

A straight dump of data into a character matrix or a tibble.

```

x <- blastxml_dump(xmlfile)

```

You can save to a delimited text file.

```

x <- blastxml_dump(xmlfile, out_file = 'dump.csv', sep = ",", row.names = FALSE)

```

You can reduce the number of fields to extract.

```
# get a copy of the default listing of fields
bxd <- BXD(app = 'blastn')
fields <- get_BXD_names()

# trim off the last 3 fields
len <- length(fields)
fields <- fields[-c((len-3):len)]

# update your listing
bxd <- set_BXD_names(fields, bxd = bxd)

# read and compare to the first example above
x_trimmed<- blastxml_dump(xmlfile, bxd = bxd)
```