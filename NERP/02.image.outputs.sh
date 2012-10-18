#drafted by Jeremy VanDerWal ( jjvanderwal@gmail.com ... www.jjvanderwal.com )
#GNU General Public License .. feel free to use / distribute ... no warranties

################################################################################

#!/bin/bash
source /etc/profile.d/modules.sh

#define the script file
SCRIPT=/home1/31/jc165798/SCRIPTS/sdmcode/SDM/AWT/future.pcmdi/10.script2run.R 

#define the base directory
BASEDIR=/data/jc165798/AWT.future.sdm/models/

#create a tmp pbs directory, move to it and empty it
tmppbs=/data/jc165798/tmppbs33
mkdir $tmppbs
cd $tmppbs
if [ $? -eq 0 ] ; then rm -f * ; fi ; #remove contents if correctly goes to directory

#cycle through each of the species, create the job and submit them
for TDIR in `find $BASEDIR -type d -maxdepth 1 -mindepth 1` ; 
do
        ### get the species 
        SPP=`basename $TDIR` 
        echo $SPP
        ### write out the shell script to be run
        echo '#!/bin/bash' > ${SPP}.sh
        echo 'source /etc/profile.d/modules.sh' >> ${SPP}.sh
        echo R CMD BATCH \'--args spp=\"${SPP}\" \' $SCRIPT ${tmppbs}/${SPP}.Rout >> ${SPP}.sh
        #submit the job
        qsub -m n -l nodes=1:ppn=1:V20Z ${SPP}.sh
        sleep 1 
done