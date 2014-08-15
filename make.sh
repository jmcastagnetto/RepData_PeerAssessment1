#! /bin/sh
rm figure/*.png
R CMD BATCH make_report.R /dev/null
ls -l PA1*
