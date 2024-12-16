package tree_sitter_yolo_test

import (
	"testing"

	tree_sitter "github.com/tree-sitter/go-tree-sitter"
	tree_sitter_yolo "github.com/tree-sitter/tree-sitter-yolo/bindings/go"
)

func TestCanLoadGrammar(t *testing.T) {
	language := tree_sitter.NewLanguage(tree_sitter_yolo.Language())
	if language == nil {
		t.Errorf("Error loading Yolo grammar")
	}
}
