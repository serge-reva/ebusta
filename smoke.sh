#!/bin/bash
make run
sleep 3
make smoke-test
EXIT_CODE=$?
make stop
exit $EXIT_CODE
