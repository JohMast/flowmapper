## R CMD check results

0 errors | 0 warnings | 1 note

* This is a resubmission.

The following changes were made:

- Replaced direct console output with a warning (R/add_flowmap.r)

- Changed references to the ggplot2 package in DESCRIPTION from:
 ggplots
to :
 'ggplot2' plots

Concerning the note:
Found the following (possibly) invalid URLs:
  URL: https://www.bfs.admin.ch/bfs/de/home/statistiken/bevoelkerung/migration-integration/binnenwanderung.assetdetail.3222163.html
    From: man/CH_migration_data.Rd
    Status: Error
    Message: Operation timed out after 60011 milliseconds with 0 bytes received

I tested the link in several browsers and from several IPs. I am not sure why the check is not able to access it. As a reference, I would like to keep it in the documentation. If you have any suggestions on how to proceed, please let me know.
