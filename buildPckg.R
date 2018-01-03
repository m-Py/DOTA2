 
pckDIR <- "~/git/DOTA2/"
myRlib <- "/home/martin/R/x86_64-pc-linux-gnu-library/3.4/"
#myRlib <- "/home/martin/R/i686-pc-linux-gnu-library/3.4/" # 32bit

library("roxyPackage")
roxy.package(
   pck.source.dir=pckDIR,
   pck.version="0.0.02",
   R.libs=myRlib,
   repo.root="~/R/repo/DOTA2",
   pck.description=data.frame(
   Package="DOTA2",
   Type="Package",
   Title="Discrete option test analysis",
   Author="Martin Papenberg <martin.papenberg@hhu.de>",
   Maintainer="Martin Papenberg <martin.papenberg@hhu.de>",
   Depends="R (>= 3.0.0)",
   Description="Analyze and simulate responses in sequential DOMC tests",
   License="GPL-3",
   Encoding="UTF-8",
   LazyLoad="yes",
   stringsAsFactors=FALSE,
   Imports="jsonlite, plyr")
)

# devtools::check()

