#!/bin/sh

./buffer ass1e.pls f > ass1e.out

./buffer ass1.pls f > ass1fi.out

./buffer ass1.pls a > ass1ai.out

./buffer ass1.pls m > ass1mi.out

if [[$((md5sum ../MPTFAssignment1_F16/ass1e.out)) = $((md5sum ass1e.out))]]; then 
	echo "Empty test successful.\n\n"
else
	echo "Empty test unsuccessful. \n\n"
fi

if [[$(md5sum ../MPTFAssignment1_F16/ass1fi.out) = $(md5sum ass1fi.out)]]; then 
	echo "Fixed test successful.\n\n"
else
	echo "Fixed test unsuccessful. \n\n"
fi

if [[$(md5sum ../MPTFAssignment1_F16/ass1ai.out) = $(md5sum ass1ai.out)]]; then 
	echo "Additive test successful.\n\n"
else
	echo "Additive test unsuccessful. \n\n"
fi

if [[$(md5sum ../MPTFAssignment1_F16/ass1mi.out) = $(md5sum ass1mi.out)]]; then 
	echo "Multiplicative test successful.\n\n"
else
	echo "Multiplicative test unsuccessful. \n\n"
fi