package edge

import "regexp"

var (
	sha1Regex  = regexp.MustCompile(`^[a-f0-9]{40}$`)
	tokenRegex = regexp.MustCompile(`^[a-zA-Z0-9_-]{32,64}$`)
)

func ValidateSHA1(sha1 string) bool {
	return sha1Regex.MatchString(sha1)
}

func ValidateToken(token string) bool {
	return tokenRegex.MatchString(token)
}
