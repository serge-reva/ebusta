package edge

import "testing"

func TestValidateLineLength(t *testing.T) {
	if err := ValidateLineLength("abc", 3); err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	if err := ValidateLineLength("abcd", 3); err == nil {
		t.Fatalf("expected line length error")
	}
}

func TestValidateJSONPayload(t *testing.T) {
	ok := []byte(`{"a":[1,2,3]}`)
	if err := ValidateJSONPayload(ok, 100, 4); err != nil {
		t.Fatalf("unexpected error: %v", err)
	}

	oversize := []byte(`{"x":"1234567890"}`)
	if err := ValidateJSONPayload(oversize, 5, 4); err == nil {
		t.Fatalf("expected oversize error")
	}

	deep := []byte(`{"a":{"b":{"c":{"d":{"e":1}}}}}`)
	if err := ValidateJSONPayload(deep, 1000, 4); err == nil {
		t.Fatalf("expected depth error")
	}

	invalid := []byte(`{"a":`)
	if err := ValidateJSONPayload(invalid, 100, 4); err == nil {
		t.Fatalf("expected invalid json error")
	}
}

func TestValidateTokenAndSHA1(t *testing.T) {
	if !ValidateSHA1("2fb481cc13771f6485091893858808e51a7718ff") {
		t.Fatalf("valid sha1 should pass")
	}
	if ValidateSHA1("XYZ") {
		t.Fatalf("invalid sha1 should fail")
	}
	if !ValidateToken("abcdefghijklmnopqrstuvwxyzABCDE_1234567890") {
		t.Fatalf("valid token should pass")
	}
	if ValidateToken("bad token!") {
		t.Fatalf("invalid token should fail")
	}
}
