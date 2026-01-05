#!/bin/bash
nc -z localhost 50053 && echo "Processor: [UP]" || echo "Processor: [DOWN]"
