package gateway

import "testing"

func TestParseSearchDiagnostics(t *testing.T) {
	tests := []struct {
		in        string
		wantExec  string
		wantMatch string
	}{
		{in: "ok;exec=TEMPLATE;match=broad", wantExec: "TEMPLATE", wantMatch: "broad"},
		{in: "ok;match=exact;exec=DSL", wantExec: "DSL", wantMatch: "exact"},
		{in: "ok", wantExec: "", wantMatch: ""},
		{in: "", wantExec: "", wantMatch: ""},
	}
	for _, tt := range tests {
		t.Run(tt.in, func(t *testing.T) {
			gotExec, gotMatch := parseSearchDiagnostics(tt.in)
			if gotExec != tt.wantExec || gotMatch != tt.wantMatch {
				t.Fatalf("parseSearchDiagnostics(%q) = (%q,%q), want (%q,%q)",
					tt.in, gotExec, gotMatch, tt.wantExec, tt.wantMatch)
			}
		})
	}
}
