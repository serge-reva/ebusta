#!/bin/bash
nc -z localhost 50054 && echo "Processor: [UP]" || echo "Processor: [DOWN]"
