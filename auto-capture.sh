#!/bin/bash
location="Servershelf"
duration="5m"
directory="/home/pi/MysterySignals/data"


while true
do

	currentdate=$(date +"%Y-%m-%dT%H%M")
	sudo rtl_433 -f 868M -X 'n=mystery,m=FSK_PCM,s=500,l=500,r=10000,g=7000' -M level -F csv:"$directory/capture_RPi4_868_${location}_${currentdate}.csv" -T $duration

	currentdate=$(date +"%Y-%m-%dT%H%M")
	sudo rtl_433 -f 433M -M level -F csv:"$directory/capture_RPi4_433_${location}_${currentdate}.csv" -T $duration
	
done
