#!/bin/bash
nc -z localhost 50052 && echo "Converter: [UP]" || echo "Converter: [DOWN]"
