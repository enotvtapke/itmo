#!/bin/bash

`./tracker.sh mem1.bash report1.log` &
`./tracker.sh mem2.bash report2.log` &
`./mem1.bash` &
`./mem2.bash` &
