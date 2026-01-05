#!/bin/bash
nc -z localhost 50051 && echo "Data-Manager: [UP]" || echo "Data-Manager: [DOWN]"
