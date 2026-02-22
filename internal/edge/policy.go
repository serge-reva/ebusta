package edge

type ActionPolicy struct {
	PerMinute int
	Burst     int
}

type Policy struct {
	Source        string
	MaxLineLength int
	MaxBodyBytes  int64
	MaxJSONDepth  int
	Actions       map[string]ActionPolicy
}

func DefaultPolicy(source string) Policy {
	return Policy{
		Source:        source,
		MaxLineLength: 512,
		MaxBodyBytes:  1 << 20,
		MaxJSONDepth:  10,
		Actions:       map[string]ActionPolicy{},
	}
}

func MergePolicy(base, override Policy) Policy {
	out := base
	if override.Source != "" {
		out.Source = override.Source
	}
	if override.MaxLineLength > 0 {
		out.MaxLineLength = override.MaxLineLength
	}
	if override.MaxBodyBytes > 0 {
		out.MaxBodyBytes = override.MaxBodyBytes
	}
	if override.MaxJSONDepth > 0 {
		out.MaxJSONDepth = override.MaxJSONDepth
	}
	if out.Actions == nil {
		out.Actions = map[string]ActionPolicy{}
	}
	for k, v := range override.Actions {
		out.Actions[k] = v
	}
	return out
}
