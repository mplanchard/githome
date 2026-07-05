{ pkgs, ... }:
{
  programs = {
    bat.enable = true;
    direnv = {
      enable = true;
      enableBashIntegration = true;
      enableZshIntegration = true;
      # seems to be auto-enabled for fish if direnv is enabled
      # enableFishIntegration = true;
      nix-direnv.enable = true;
    };
    eza.enable = true;

    starship = {
      enable = true;
      # Enable once bash is configured by home manager
      enableBashIntegration = true;
      enableFishIntegration = true;
      enableTransience = true;
      settings = {
        kubernetes.disabled = false;
        # get rid of annoying extra space
        nix_shell.format = "via [$symbol$state($name)]($style) ";
        nix_shell.symbol = "❄️ ";
        nix_shell.impure_msg = "";
        nix_shell.pure_msg = "pure ";
      };
    };

    bash = {
      enable = true;
      profileExtra = ''
        if [[ -f "$HOME/.nix-profile/etc/profile.d/nix.sh" ]]; then
           source "$HOME/.nix-profile/etc/profile.d/nix.sh"
        fi
      '';
      initExtra = ''
        # Bind ctrl-backspace / ctrl-H to backward-kill word
        bind \cH backward-kill-word

        # HACK: for some reason this, unlike hte other sessionVariables, is getting
        # overridden in my shells. Set it manually.
        export EDITOR="emacsclient"
        # Setting this env var to the empty string makes it so that emacs
        # will start a daemon if none is running when trying to execute emacsclient
        export ALTERNATE_EDITOR=
        vterm_printf(){
          if [ -n "$TMUX" ] && ([ "''${TERM%%-*}" = "tmux" ] || [ "''${TERM%%-*}" = "screen" ] ); then
              # Tell tmux to pass the escape sequences through
              printf "\ePtmux;\e\e]%s\007\e\\" "$1"
          elif [ "''${TERM%%-*}" = "screen" ]; then
              # GNU screen (screen, screen-256color, screen-256color-bce)
              printf "\eP\e]%s\007\e\\" "$1"
          else
              printf "\e]%s\e\\" "$1"
          fi
        }
        vterm_prompt_end(){
            vterm_printf "51;A$(whoami)@$(hostname):$(pwd)"
        }
        starship_precmd_user_func="vterm_prompt_end"

        alias rdoc='xdg-open $(dirname $(which rustc))/../share/doc/rust/html/std/index.html'

        PWS=$(gpg -q --for-your-eyes-only --no-tty -d ~/.authinfo.gpg || echo "fail")
        export GITLAB_TOKEN=$(echo "$PWS" | awk '/machine gitlab\.com\/api login mplanchard/ { print $NF }')
        export GITHUB_TOKEN=$(echo "$PWS" | awk '/machine api\.github\.com login mplanchard\^forge/ { print $NF }')
        export ANTHROPIC_API_KEY=$(echo "$PWS" | awk '/machine api\.anthropic\.com/ { print $NF }')
        export CACHIX_AUTH_TOKEN="$(echo "$PWS" | awk '/machine cachix\.org login mplanchard/ { print $NF }')"
        set_profile() {
            export AWS_PROFILE="$1"
        }
      '';
      shellAliases = {
        iam = "set_profile";
      };
    };

    fish = {
      enable = true;
      functions = { };
      shellAliases = {
        iam = "set_profile";
      };
      shellInit = ''
        if test -f ~/.authinfo.gpg
          set PWS "$(gpg -q --for-your-eyes-only --no-tty -d ~/.authinfo.gpg || echo "fail")"
          set -gx GITLAB_TOKEN "$(echo "$PWS" | awk '/machine gitlab\.com\/api login mplanchard/ { print $NF }')"
          set -gx GITHUB_TOKEN "$(echo "$PWS" | awk '/machine api\.github\.com login mplanchard\^forge/ { print $NF }')"
          set -gx ANTHROPIC_API_KEY "$(echo "$PWS" | awk '/machine api\.anthropic\.com/ { print $NF }')"
          set -gx CACHIX_AUTH_TOKEN "$(echo "$PWS" | awk '/machine cachix\.org/ { print $NF }')"
        end
        set -gx EDITOR "emacsclient"
        # Setting this env var to the empty string makes it so that emacs
        # will start a daemon if none is running when trying to execute emacsclient
        set -gx ALTERNATE_EDITOR ""

        function set_aws_profile
          set -gx AWS_PROFILE $argv[1]
        end

        if test 'vterm' = "$INSIDE_EMACS"
          source "$EMACS_VTERM_PATH/etc/emacs-vterm.fish"
        end

        function vterm_finish --on-event fish_prompt
          if test 'vterm' = "$INSIDE_EMACS"; and test -n "$STARSHIP_SESSION_KEY"
            vterm_prompt_end
          end
        end

        abbr iam set_aws_profile
      '';
    };
  };
}
