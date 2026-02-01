#!/bin/bash
nc -z localhost 50054 && echo "Processor: [OK]" || echo "Processor: [FAIL]"
