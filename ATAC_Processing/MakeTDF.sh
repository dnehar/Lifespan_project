#!/bin/bash
#SBATCH --nodes=1
#SBATCH --ntasks-per-node=1
#SBATCH --cpus-per-task=1
#SBATCH --time=24:00:00
#SBATCH --mem-per-cpu=128G
#SBATCH --job-name=MakeTDF

if [ -n $SLURM_JOB_ID ] ; then
  SCRIPTDIR=$(scontrol show job $SLURM_JOBID | awk -F= '/Command=/{print $2}' | cut -f1 -d" ")
else
  SCRIPTDIR=$(realpath $0)
fi
SCRIPTDIR=$(dirname "$SCRIPTDIR")

BAMFILE=$1
GENOME=hg38

BAMDIR=$(dirname ${BAMFILE})
BAMNAME=$(basename ${BAMFILE})

TAGDIR=${BAMDIR}/${BAMNAME}_TAG
mkdir -p ${TAGDIR}

BEDGRAPH=${BAMDIR}/${BAMNAME}.bedGraph
TDF=${BAMDIR}/${BAMNAME}.tdf

module load singularity

singularity exec -B / ${SCRIPTDIR}/MakeTDFImage.sif /HOMER/bin/makeTagDirectory ${TAGDIR} ${BAMFILE} -genome ${GENOME} -single -fragLength 150

singularity exec -B / ${SCRIPTDIR}/MakeTDFImage.sif /HOMER/bin/makeUCSCfile ${TAGDIR} -fragLength 150 -o ${BEDGRAPH}

gunzip ${BEDGRAPH}.gz

singularity exec -B / ${SCRIPTDIR}/MakeTDFImage.sif /IGV/IGV_2.12.0/igvtools toTDF ${BEDGRAPH} ${TDF} ${GENOME}

