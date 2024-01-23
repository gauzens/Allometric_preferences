
linecount=$(wc -l < parameters.txt)
output="/home/gauzens/allometric_prefences_model/results-$(date +%s).csv"
sbatch -a 1-$linecount single_job_min.sh /home/gauzens/allometric_preferences_model/parameters.txt
