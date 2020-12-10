#!/bin/bash

basepath="/nafs/narr/canderson/new_pipeline_test_runs/out"

dirlist=($(ls $basepath | grep -E "^e|^k|^s" | grep -v ^sub))

for id in ${dirlist[@]};
do
	echo $id
	cp $basepath/${id}/T1w/${id}/stats/rh.aparc.stats /ifshome/bwade/NARSAD/Aim_1/data/FS_Output_112119/${id}_rh.aparc.stats
	cp $basepath/${id}/T1w/${id}/stats/lh.aparc.stats /ifshome/bwade/NARSAD/Aim_1/data/FS_Output_112119/${id}_lh.aparc.stats
	cp $basepath/${id}/T1w/${id}/stats/aseg.stats /ifshome/bwade/NARSAD/Aim_1/data/FS_Output_112119/${id}_aseg.stats
done
