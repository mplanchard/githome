{ pkgs, unstable, ... }:

with pkgs.lib.debug;
# Email configuration.
{
  programs = {
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
        "[Gmail]/Trash"
        # ignore for initial sync, b/c too big
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
      # patterns = [
      #   "*"
      #   "![Gmail]/Important"
      # ];
    };
  };

  services = {
    mbsync = {
      enable = true;
      postExec = "${pkgs.mu}/bin/mu index";
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
}
