# Purpose
This repository is for storing files related to the investigation of mystery radio signals on 868MHz which is written up in this [Blog post](https://simonjonsson87.github.io/blog/2024/Investigating-mystery-signals-on-868MHz/) 

# Summary of contents 

|Location | Conents |
|---------|---------|
| data/initial_data | These are files with decoded signal data in csv format produced by [rtl_433](https://github.com/merbanan/rtl_433). This data capture was done manually on a ubuntu laptop.|
| data/RPi4_data | Similar to above. However, these files also include capture on 433MHz, and the collection was done on a Raspberry Pi. |
| auto-capture.sh | The script used to collect data on the Raspberry Pi |
| MysteryAnalysis_initial.R | This is the R script used for analysing the files in data/initial_data |
| MysteryAnalysis_RPi4.r | This is the R script use for analysing the files in data/RPi4_data |
| messages.html | A file containing an html table generated by MysteryAnalysis_initial.R |

