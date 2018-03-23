#!/usr/bin/env bash

source_dir=`dirname $(realpath "${BASH_SOURCE[0]}")`
#export source_dir

cd $source_dir

./gopher -r "${source_dir}/res/" $@
