#!/bin/bash
curl -sI "http://localhost:50080/input?msg=ping" | grep -q "200 OK" && echo "Web-Adapter: [OK]" || echo "Web-Adapter: [FAIL]"
