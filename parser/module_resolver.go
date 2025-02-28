package parser

import (
	"fmt"
	"os"
	"path/filepath"
	"strings"
)

func PreprocessImports(code string, baseDir string) (string, error) {
	lines := strings.Split(code, "\n")
	var result []string

	for _, line := range lines {
		trimmed := strings.TrimSpace(line)
		if strings.HasPrefix(trimmed, "import") {
			// Extract filename from import statement and convert to lowercase
			filename := strings.ToLower(strings.Trim(strings.TrimPrefix(trimmed, "import"), " \";"))

			// Ensure filename ends with .cool
			if !strings.HasSuffix(filename, ".cool") {
				filename += ".cool"
			}

			// Read the imported file
			importPath := filepath.Join(baseDir, filename)
			content, err := os.ReadFile(importPath)
			if err != nil {
				return "", fmt.Errorf("failed to import %s: %v", filename, err)
			}

			// Remove the module line if it's the first non-empty line
			contentLines := strings.Split(string(content), "\n")
			for i, contentLine := range contentLines {
				if strings.TrimSpace(contentLine) != "" {
					if strings.HasPrefix(strings.TrimSpace(contentLine), "module") {
						contentLines = contentLines[i+1:]
					}
					break
				}
			}

			// Add imported content without module line
			result = append(result, strings.Join(contentLines, "\n"))
		} else {
			// Keep non-import lines as they are
			result = append(result, line)
		}
	}

	return strings.Join(result, "\n"), nil
}
