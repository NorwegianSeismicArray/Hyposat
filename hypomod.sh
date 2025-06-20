#!/usr/bin/env bash

SCRIPT=$(readlink -f "$0" )
SCRIPTPATH=$(dirname "$SCRIPT" )

export HYPOSAT_DATA=${SCRIPTATH}/data

${SCRIPTPATH}/bin/hypomod $@
