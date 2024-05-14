{ hmConfig, pkgs, unstable, ... }:

with pkgs.lib.debug;
# Email configuration.
hmConfig // {

  programs = hmConfig . programs or {} // {
    mbsync = {
      enable = true;
    };

    # mail stuff
    mu.enable = true;
  };

  # relative to ~
  accounts.email.maildirBasePath = ".mail";
  accounts.email.accounts.gmail = {
    address = "msplanchard@gmail.com";
    flavor = "gmail.com";
    maildir.path = "gmail";
    passwordCommand = "gpg2 -q --for-your-eyes-only --no-tty -d ~/.authinfo.gpg | awk '/machine imap.gmail.com login msplanchard@gmail.com password/ {print $NF}'";
    primary = true;
    realName = "Matthew Planchard";
    mbsync = {
      enable = true;
      create = "both";
      expunge = "both";
      patterns = [
        "*"
        "![Gmail]*"
        "[Gmail]/Drafts"
        "[Gmail]/Sent Mail"
        "[Gmail]/Starred"
        "[Gmail]/All Mail"
      ];
      # local = {
      #   SubFolders = "Verbatim";
      # };
    };
  };

  accounts.email.accounts.spectrust = {
    address = "matthew@spec-trust.com";
    flavor = "gmail.com";
    maildir.path = "spectrust";
    passwordCommand = "gpg2 -q --for-your-eyes-only --no-tty -d ~/.authinfo.gpg | awk '/machine imap.gmail.com login matthew@spec-trust.com password/ {print $NF}'";
    realName = "Matthew Planchard";
    mbsync = {
      enable = true;
      create = "both";
      expunge = "both";
      patterns = [
        "*"
        "![Gmail]/Important"
      ];
    };
  };

  # accounts.email.accounts.protonmail = {
  #   address = "inbox@mplanchard.com";
  #   maildir.path = "protonmail";
  #   passwordCommand = "gpg2 -q --for-your-eyes-only --no-tty -d ~/.authinfo.gpg | awk '/machine 127.0.0.1 login inbox@mplanchard.com password/ {print $NF}'";
  #   realName = "Matthew Planchard";
  #   mbsync = {
  #     enable = true;
  #     create = "both";
  #     expunge = "both";
  #   };
  # };
} // pkgs.lib.optionalAttrs pkgs.stdenv.isLinux {
  services = hmConfig . services or {} // {
    mbsync = {
      enable = true;
      postExec = "${pkgs.mu}/bin/mu index";
    };
  };
}
