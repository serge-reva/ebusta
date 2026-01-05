#!/bin/bash
nc -z localhost 50052 && echo "Converter: [OK]" || echo "Converter: [FAIL]"
