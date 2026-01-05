#!/bin/bash
nc -z localhost 50053 && echo "Processor: [OK]" || echo "Processor: [FAIL]"
