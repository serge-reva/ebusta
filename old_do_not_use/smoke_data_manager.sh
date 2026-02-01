#!/bin/bash
nc -z localhost 50051 && echo "Data-Manager: [OK]" || echo "Data-Manager: [FAIL]"
