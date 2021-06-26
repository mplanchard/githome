
(specifications->manifest
 '(;; essential guix system packages
   ;; "emacs-pgtk-native-comp"  ;; from flatwhatson/guix-channel
   "glibc-utf8-locales"
   "fontconfig"
   "gs-fonts"
   "font-dejavu"
   "info-reader"  ;; ensure info gets installed for packages
   "man-db"  ;; ensure man gets installed for packages
   "nss-certs"

   ;; my own stuff
   ;; "gcc-toolchain"
   "cmake"
   "curl"
   ;; "direnv"
   "fd"
   "font-fira-code"
   "font-awesome"
   "font-hack"
   "font-mononoki"
   "git"
   "gnome-tweaks"
   "guile"
   "gst-plugins-bad"
   "gst-plugins-ugly"
   "htop"
   "isync" ;; mail
   "jq"
   "libtool"
   "llvm"

   "make"

   "mu" ;; mail

   "neovim"
   ;; "nix"
   "node"

   "openssl"
   "pandoc"

   "shellcheck"
   "sqlite"
   "stow" ;; stowception
   "sushi"

   "texlive"
   "tidy"
   "tk"

   "wget"
   ))
