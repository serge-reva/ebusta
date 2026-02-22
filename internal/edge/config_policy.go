package edge

import "ebusta/internal/config"

func PolicyFromConfig(cfg *config.Config, source string) Policy {
	base := DefaultPolicy(source)

	edgeCfg := cfg.Edge
	if edgeCfg.Default.MaxLineLength > 0 {
		base.MaxLineLength = edgeCfg.Default.MaxLineLength
	}
	if edgeCfg.Default.MaxBodyBytes > 0 {
		base.MaxBodyBytes = edgeCfg.Default.MaxBodyBytes
	}
	if edgeCfg.Default.MaxJSONDepth > 0 {
		base.MaxJSONDepth = edgeCfg.Default.MaxJSONDepth
	}
	if len(edgeCfg.Default.Actions) > 0 {
		if base.Actions == nil {
			base.Actions = map[string]ActionPolicy{}
		}
		for action, ac := range edgeCfg.Default.Actions {
			base.Actions[action] = ActionPolicy{
				PerMinute: ac.PerMinute,
				Burst:     ac.Burst,
			}
		}
	}

	if src, ok := edgeCfg.Sources[source]; ok {
		override := Policy{
			Source:        source,
			MaxLineLength: src.MaxLineLength,
			MaxBodyBytes:  src.MaxBodyBytes,
			MaxJSONDepth:  src.MaxJSONDepth,
			Actions:       map[string]ActionPolicy{},
		}
		for action, ac := range src.Actions {
			override.Actions[action] = ActionPolicy{
				PerMinute: ac.PerMinute,
				Burst:     ac.Burst,
			}
		}
		base = MergePolicy(base, override)
	}

	return base
}
