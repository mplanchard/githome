# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, lib, pkgs, ... }:

{
  imports =
    [ # Include the results of the hardware scan.
      ./hardware-configuration.nix
    ];

  # for ssd optimization
  boot.kernel.sysctl = {
    "vm.swappiness" = lib.mkDefault 1;
  };
  services.fstrim.enable = lib.mkDefault true;

  # Use the systemd-boot EFI boot loader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  boot.initrd.luks.devices.luksroot = {
    device = "/dev/disk/by-uuid/78384446-6fc9-40d1-aa31-f53338ef18f3";
    preLVM = true;
    allowDiscards = true;
  };

  # install man pages for libraries and stuff
  documentation.dev.enable = true;
  # ensure apropos is populated
  documentation.man.generateCaches = true;

  environment.loginShellInit = ''
    if [ -e $HOME/.profile ]
    then
      . $HOME/.profile
    fi
  '';

  # graphics optimization
  hardware.opengl.enable = true;
  hardware.opengl.driSupport = true;
  hardware.opengl.extraPackages = with pkgs; [
    vaapiIntel
    vaapiVdpau
    libvdpau-va-gl
    intel-media-driver
  ];

  hardware.nvidia.powerManagement.enable = true;

  networking.networkmanager.enable = true;
  networking.hostName = "mp-st-nix"; # Define your hostname.
  # networking.wireless.enable = true;  # Enables wireless support via wpa_supplicant.

  # Set your time zone.
  time.timeZone = "America/Chicago";

  # The global useDHCP flag is deprecated, therefore explicitly set to false here.
  # Per-interface useDHCP will be mandatory in the future, so this generated config
  # replicates the default behaviour.
  networking.useDHCP = false;
  networking.interfaces.enp82s0u2u1u2.useDHCP = true;
  networking.interfaces.wlp0s20f3.useDHCP = true;

  nixpkgs.config.allowUnfree = true;
  nix = {
    package = pkgs.nixUnstable;
    extraOptions = ''
      experimental-features = nix-command flakes
      keep-outputs = true
      keep-derivations = true
    '';
  };

  # Configure network proxy if necessary
  # networking.proxy.default = "http://user:password@proxy:port/";
  # networking.proxy.noProxy = "127.0.0.1,localhost,internal.domain";

  # Select internationalisation properties.
  # i18n.defaultLocale = "en_US.UTF-8";
  # console = {
  #   font = "Lat2-Terminus16";
  #   keyMap = "us";
  # };

  # unlock wallet on login
  security.pam.services.kwallet = {
    name = "kwallet";
    enableKwallet = true;
  };

  # Enable the X11 windowing system.
  services.xserver.enable = true;

  # services.xserver.videoDrivers = [ "nvidia" ];


  # Enable the Plasma 5 Desktop Environment.
  # services.xserver.displayManager.sddm.enable = true;
  services.xserver.desktopManager.plasma5.enable = true;
  nixpkgs.config.firefox.enablePlasmaBrowserIntegration = true;

  # services.xserver.desktopManager.gnome.enable = true;
  # environment.gnome.excludePackages = [
  #   pkgs.gnome.cheese
  #   pkgs.gnome-photos
  #   pkgs.gnome.gnome-music
  #   pkgs.gnome.gnome-terminal
  #   pkgs.gnome.gedit
  #   pkgs.epiphany
  #   pkgs.evince
  #   pkgs.gnome.totem
  #   pkgs.gnome.tali
  #   pkgs.gnome.iagno
  #   pkgs.gnome.hitori
  #   pkgs.gnome.atomix
  #   pkgs.gnome-tour
  # ];
  
  # Configure keymap in X11
  # services.xserver.layout = "us";
  # services.xserver.xkbOptions = "eurosign:e";

  # Enable CUPS to print documents.
  services.printing.enable = true;

  # Enable sound.
  # sound.enable = true;
  # hardware.pulseaudio.enable = true;
  security.rtkit.enable = true;
  services.pipewire = {
    enable = true;
    alsa.enable = true;
    alsa.support32Bit = true;
    pulse.enable = true;
    # jack.enable = true
  };

  # Enable touchpad support (enabled default in most desktopManager).
  services.xserver.libinput.enable = true;
  services.xserver.xkbOptions = "ctrl:nocaps,altwin:swap_lalt_lwin,compose:ralt";
  services.xserver.libinput.touchpad = {
   naturalScrolling = true;
  };

  # Define a user account. Don't forget to set a password with ‘passwd’.
  users.users.matthew = {
    isNormalUser = true;
    extraGroups = [ "wheel" "networkmanager" ]; # Enable ‘sudo’ for the user.
  };

  # List packages installed in system profile. To search, run:
  # $ nix search wget
  environment.systemPackages = with pkgs; [
    git
    man-pages
    man-pages-posix
    vim # Do not forget to add an editor to edit configuration.nix! The Nano editor is also installed by default.
    wget
    firefox
  ];

  # Some programs need SUID wrappers, can be configured further or are
  # started in user sessions.
  # programs.mtr.enable = true;
  programs.gnupg.agent = {
    enable = true;
    enableSSHSupport = true;
  };

  # potentially for screensharing from sway
  # xdg.portal.wlr.enable = true;

  # List services that you want to enable:

  # Enable the OpenSSH daemon.
  # services.openssh.enable = true;

  # Open ports in the firewall.
  # networking.firewall.allowedTCPPorts = [ ... ];
  # networking.firewall.allowedUDPPorts = [ ... ];
  # Or disable the firewall altogether.
  
  # For KDE Connect
  networking.firewall.allowedTCPPortRanges = [
    {from=1714; to=1764;}
  ];
  networking.firewall.allowedUDPPortRanges = [
    {from=1714; to=1764;}
  ];
  networking.firewall.enable = true;

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "21.11"; # Did you read the comment?

}

