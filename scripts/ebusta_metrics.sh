#!/usr/bin/env bash
set -euo pipefail

WEB_URL="${WEB_URL:-http://localhost:50080/input?msg=author:King}"
METRICS_URL="${METRICS_URL:-http://localhost:50090/metrics}"

usage() {
  cat <<USAGE
Usage: $0 <cmd>

Commands:
  head           - show first lines of metrics endpoint
  check          - verify listener on 50090 and HTTP 200 on /metrics
  inc [N]        - send N requests via web-adapter and show counter delta
Env overrides:
  WEB_URL=...        (default: $WEB_URL)
  METRICS_URL=...    (default: $METRICS_URL)
USAGE
}

get_counter() {
  curl -fsS "$METRICS_URL" \
  | awk '$1=="orchestrator_requests_total"{print $2; found=1} END{ if(!found) exit 2 }'
}

cmd="${1:-}"
case "$cmd" in
  head)
    curl -fsS "$METRICS_URL" | head -n 30
    ;;
  check)
    ss -ltnp | grep -E '(:50090)\b' || true
    code="$(curl -s -o /dev/null -w '%{http_code}' "$METRICS_URL" || true)"
    if [[ "$code" == "200" ]]; then
      echo "OK: /metrics HTTP 200"
    else
      echo "FAIL: /metrics HTTP $code"
      exit 1
    fi
    ;;
  inc)
    n="${2:-1}"
    before="$(get_counter || echo "")"
    if [[ -z "${before}" ]]; then
      echo "FAIL: cannot read orchestrator_requests_total from $METRICS_URL"
      exit 1
    fi

    for _ in $(seq 1 "$n"); do
      curl -fsS "$WEB_URL" >/dev/null || true
    done

    after="$(get_counter)"
    echo "before=$before after=$after"
    ;;
  *)
    usage
    exit 2
    ;;
esac
