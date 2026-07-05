{ pkgs, ... }:
{
  programs.emacs = {
      enable = true;
      package = pkgs.emacs-pgtk;
      extraPackages = epkgs: with epkgs; [
        mbsync
        mu4e
        vterm
        (treesit-grammars.with-grammars (grammars: with grammars; [
            tree-sitter-bash
            tree-sitter-clojure
            tree-sitter-comment
            tree-sitter-cpp
            tree-sitter-css
            tree-sitter-dockerfile
            tree-sitter-elisp
            tree-sitter-fish
            tree-sitter-go
            tree-sitter-graphql
            tree-sitter-hcl
            tree-sitter-html
            tree-sitter-janet-simple
            tree-sitter-javascript
            tree-sitter-jsdoc
            tree-sitter-json
            tree-sitter-json5
            tree-sitter-latex
            tree-sitter-lua
            tree-sitter-make
            tree-sitter-markdown
            tree-sitter-markdown-inline
            tree-sitter-nix
            tree-sitter-prisma
            tree-sitter-rust
            tree-sitter-scss
            tree-sitter-sql
            tree-sitter-svelte
            tree-sitter-toml
            tree-sitter-tsx
            tree-sitter-typescript
            tree-sitter-vim
            tree-sitter-yaml
        ]))
      ];
  };
}
