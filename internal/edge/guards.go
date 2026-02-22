package edge

import (
	"encoding/json"
	"fmt"
	"strings"
)

func ValidateLineLength(input string, maxChars int) error {
	if maxChars <= 0 {
		return nil
	}
	if len(strings.TrimSpace(input)) > maxChars {
		return fmt.Errorf("input too long: %d > %d", len(strings.TrimSpace(input)), maxChars)
	}
	return nil
}

func ValidateJSONPayload(body []byte, maxBodyBytes int64, maxJSONDepth int) error {
	if maxBodyBytes > 0 && int64(len(body)) > maxBodyBytes {
		return fmt.Errorf("body too large: %d > %d", len(body), maxBodyBytes)
	}

	var v interface{}
	if err := json.Unmarshal(body, &v); err != nil {
		return fmt.Errorf("invalid JSON: %w", err)
	}

	if maxJSONDepth > 0 {
		depth := JSONDepth(v)
		if depth > maxJSONDepth {
			return fmt.Errorf("JSON too deep: %d > %d", depth, maxJSONDepth)
		}
	}

	return nil
}

func JSONDepth(v interface{}) int {
	maxDepth := 1
	switch val := v.(type) {
	case map[string]interface{}:
		for _, child := range val {
			if d := JSONDepth(child) + 1; d > maxDepth {
				maxDepth = d
			}
		}
	case []interface{}:
		for _, item := range val {
			if d := JSONDepth(item) + 1; d > maxDepth {
				maxDepth = d
			}
		}
	}
	return maxDepth
}
