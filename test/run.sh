#!/bin/sh
#
# Copyright (c) 2015-2018 Vasilios E. Raptis <polyana.software@gmail.com>
#
# Permission is hereby granted, free of charge, to any person obtaining a copy
# of this software and associated documentation files (the "Software"), to deal
# in the Software without restriction, including without limitation the rights
# to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
# copies of the Software, and to permit persons to whom the Software is
# furnished to do so, subject to the following conditions:
#
# The above copyright notice and this permission notice shall be included in
# all copies or substantial portions of the Software.
#
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
# THE SOFTWARE.
#
#-----------------------------------------------------------------------
#
if [ -z $1 ] || [ $1 = "h" ] || [ $1 = "-h" ] 
then
    echo "\v\tUsage"
    echo "\v\tAssuming that an executable (polyana) is placed in the test directory, then:\n"
    echo "\n\t*\t./run md \n\t\t to run a set of clean MD simulations from the beginning"
    echo "\n\t*\t./run polyana \n\t\t to run polyana in each directory"
    echo "\n\t*\t./run md polyana \n\t\t\tor\n\t\t./run polyana md "
    echo "\t\t to run a clean MD and then polyana"
    echo "\n\t*\t./run \n\t\t\tor \n\t\t./run h \n\t\t\tor \n\t\t./run -h \n\t\tto print this help screen"
    echo "\v\tOnce MD has been completed you can run polyana as many times as you want"
    echo "\tYou can also run polyana individually in each separate test directory\v"
    exit 0
fi

# Variables; modify DLPOLY if need be
ROOT=$PWD
EXE=$ROOT/polyana
DLPOLY="DLPOLY.X"

sleep 1
echo "\v\v"
echo "\tSix test cases are about to be analysed"
if [ "$1" = "md" ] || [ "$2" = "md" ] 
then
    echo "\tEach DL_POLY run will last no more than about 5 minutes"
fi
echo "\v\tEach polyana run will last only a few seconds"
echo "\tA progress bar will be showing the elapsed time"
echo "\tIn it, each # will denote approx. 1 s, each #-block will be about 10 s"
echo "\v\tWait a few seconds to continue\v\v\v\v\v"
sleep 8

# Next four lines are from http://ccollins.wordpress.com/2009/07/20/howto-pause-a-shell-script/
# OLDCONFIG=`stty -g`
# stty -icanon -echo min 1 time 0
# dd count=1 2>/dev/null
# stty $OLDCONFIG   
# 
# echo "\v\tPress Enter to continue\v\v"

for testdir in `ls -d test*`
do
    cd $testdir
    echo "\v\tTEST CASE: "`basename "$PWD"`
    sleep 1
    if [ "$1" = "md" ] || [ "$2" = "md" ] 
    then
        rm -f HISTORY polyana.out OUTPUT POP RDF RDFDAT REVCON REVIVE \
              STATIS ZDEN ZDNDAT TABLE STDOUT
        echo "\tDL_POLY running..."
        "$DLPOLY" &
        sleep 1
        
        t=0
        m=0
        printf "\n\t"
        mystatus=`lsof "$PWD/OUTPUT"`
        while [ "$mystatus" != "" ]  
        do
            if [ "$t" -eq 10 ] 
            then
                t=0
                printf " "
                m=`expr $m + 1`
                if [ "$m" -eq 6 ]
                then
                    m=0
                    printf "\n\t"
                fi
            fi
            printf "#"
            sleep 1
            t=`expr $t + 1`
            mystatus=`lsof "$PWD/OUTPUT"`
        done
        printf "\n"
        sleep 1
        echo "\v\tMD run completed"
        echo "\v\tReady to run polyana"
        sleep 1
    fi
    if [ "$1" = "polyana" ] || [ "$2" = "polyana" ] 
    then
        if [ ! -e $EXE ]
        then
            echo "\v\tNo executable (polyana) found!\n\tPlace it in the test directory and retry\v"
            exit 0
        fi
        if [ ! -e FIELD ] || [ ! -e HISTORY ] 
        then
            echo "\v\t polyana in " $testdir ": no FIELD or HISTORY found!"
        fi
        if [ ! -e CONTROL ]
        then
            echo "\v\tpolyana in " $testdir ": no CONTROL file found!"
            echo "\tCalculation will be carried out without directives"
        fi
        rm -f polyana.out RDF POP TABLE STDOUT
    	$EXE > polyana.out &
        echo "\tpolyana running..."
        sleep 1
         
        t=0
        m=0
        printf "\n\t"
        mystatus=`lsof "$PWD/polyana.out"`
        while [ "$mystatus" != "" ]  
        do
            if [ "$t" -eq 10 ] 
            then
                t=0
                printf " "
                m=`expr $m + 1`
                if [ "$m" -eq 6 ]
                then
                    m=0
                    printf "\n\t"
                fi
            fi
            printf "#"
            sleep 1
            t=`expr $t + 1`
            mystatus=`lsof "$PWD/OUTPUT"`
        done
        printf "\n"
        sleep 1
        echo "\v\tPolyana run is over"
        echo "\tOutput is in file RDF.\n\tOutput messages are in file polyana.out\v"
    fi
    cd ..
    sleep 1
done
echo "\tAll tests are over. Thank you for trying polyana!\v"

