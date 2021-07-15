#! /usr/bin/env bash

#qsub -N OA-tests \
#	-n \
#	-d /home/tcm0036/OA_2021_AU/scripts/ \
#	-q gen28 \
#	-W group_list=jro0014_lab \
#	-W x=FLAGS:ADVRES:jro0014_s28 \
#	-l nodes=1:ppn=1,mem=120gb,walltime=48:00:00 <<<"
	
	# Load conda environment with R
	source ~/mambaforge/etc/profile.d/conda.sh # Or path to where your conda is
	conda activate R_env

	Rscript analyze_OAdata_testsonly.R
#	"
