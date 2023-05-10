#!/usr/bin/bash

# systemctl start cronie.service
# # check status with
# ps -ef | grep cron | grep -v grep
#
# # then use 'tty' to get dev number, 'crontab -e' and:
# */10 * * * * /bin/bash /<path>/<to>/cronscript.bash > /dev/pts/1

NUM_R_PROC=$(ps aux | grep exec/R | grep -v grep | wc -l)
NUM_BG=$(ps aux | grep exec/R | grep -v grep | grep -v script.R | wc -l)
NUM_FG=$(ps aux | grep exec/R | grep -v grep | grep script.R | wc -l)
if [ $NUM_R_PROC -eq 0 ]; then
    echo "no R processes; starting script ..."
    Rscript /<path>/<to>/UrbanAnalyst/uta-engine/script.R
else
    if [ $NUM_R_PROC -eq 1 ]; then
        echo "There is ${NUM_R_PROC} R procsses (${NUM_BG} in background) running"
    else
        echo "There are ${NUM_R_PROC} R procsses (${NUM_BG} in background) running"
    fi
    if [ $NUM_BG -eq $NUM_R_PROC ] || [ $NUM_FG -gt 1 ]; then
        echo "Looks like something went wrong; re-starting ..."
        ps aux | grep exec/R | grep -v grep | grep "?" | awk '{print $2}' | xargs kill
        sleep 10s
        Rscript /<path>/<to>/UrbanAnalyst/uta-engine/script.R
    fi
fi
