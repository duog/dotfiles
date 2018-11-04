
# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, ... }:

{
  # nixpkgs.config.allowUnfree = true;
  imports = let thisMachine = "/etc/nixos/this-machine"; in
    [ # Include the results of the hardware scan.
      "${thisMachine}/hardware-configuration.nix"
      "${thisMachine}/configuration.nix"
      ./multi-glibc-locale-paths.nix
    ];

  networking.networkmanager.enable = true;

  # Select internationalisation properties.
  i18n = {
    consoleFont = "Lat2-Terminus16";
    consoleKeyMap = "us";
    defaultLocale = "en_NZ.UTF-8";
  };

  # Set your time zone.
  time.timeZone = "Pacific/Auckland";

  system.autoUpgrade = {
    enable = true;
    channel = https://nixos.org/channels/nixos-18.09;
  };

  # List packages installed in system profile. To search, run:
  # $ nix search wget
  environment.systemPackages = with pkgs; [
      wget vim git unzip htop whois nix-prefetch-git nox cachix cacert
  ];

  environment.sessionVariables = {
    EDITOR = "vim";
    VISUAL = "vim";
  };

  # Some programs need SUID wrappers, can be configured further or are
  # started in user sessions.
  programs.zsh = {
    enable = true;
    enableCompletion = true;
    # TODO I want this on by default, but off for my user. How much does it cost to leave it on?
    # enableGlobalCompinit = false;
  };
  programs.bash.enableCompletion = true;
  services.locate.enable = true;
  virtualisation.virtualbox.host.enable = true;

# todo these is only for desktops
  hardware.opengl.driSupport32Bit = true;
  hardware.pulseaudio.support32Bit = true;
  services.upower.enable = true;
  systemd.services.upower.enable = true;
  # Enable the X11 windowing system.
  services.xserver.enable = true;
  services.xserver.layout = "us";
  services.xserver.xkbOptions = "caps:swapescape";
  sound.enable = true;
  hardware.pulseaudio.enable = true;

  # Enable the KDE Desktop Environment.
  services.xserver.displayManager.sddm.enable = true;
  services.xserver.desktopManager.plasma5.enable = true;
  services.xserver.windowManager.xmonad.enable = true;

  # Enable touchpad support.
  services.xserver.libinput.enable = true;

  # Enable CUPS to print documents.
  services.printing.enable = true;
  # List services that you want to enable:

  # Enable the OpenSSH daemon.
  services.openssh = {
    enable = true;
    allowSFTP = true;
    permitRootLogin = "no";
    openFirewall = true;
    passwordAuthentication = false;
  };

  # Open ports in the firewall.
  # networking.firewall.allowedTCPPorts = [ ... ];
  # networking.firewall.allowedUDPPorts = [ ... ];
  # Or disable the firewall altogether.
  # networking.firewall.enable = false;



  # Enable sound.
  # Define a user account. Don't forget to set a password with ‘passwd’.
  users.extraUsers.doug = {
    isNormalUser = true;
    extraGroups = [ "wheel" "networkmanager" ];
    initialHashedPassword = "";
    createHome = true;
    shell = "${pkgs.zsh}/bin/zsh";
  }; 

  fonts = {
    fonts = with pkgs; [ nerdfonts noto-fonts source-code-pro source-sans-pro source-serif-pro ];
    enableDefaultFonts = true;
    fontconfig = {
      enable = true;
      defaultFonts = {
        monospace = pkgs.lib.mkBefore [ "SauceCodePro Nerd Font"];
        serif = pkgs.lib.mkBefore [ "Source Serif Pro"];
        sansSerif = pkgs.lib.mkBefore [ "Source Sans Pro"];
      };
      ultimate.enable = true;
      penultimate.enable = false;

      # for now, I think would work though
      includeUserConf = false;
    };
  };

  users.extraUsers.root.initialHashedPassword = "";

  nix = {
    binaryCaches = [
      "https://cache.nixos.org/"
      "https://hie-nix.cachix.org"
    ];
    binaryCachePublicKeys = [
      "hie-nix.cachix.org-1:EjBSHzF6VmDnzqlldGXbi0RM3HdjfTU3yDRi9Pd0jTY="
    ];
    trustedUsers = [ "root" "doug" ];
  };


  # This value determines the NixOS release with which your system is to be
  # compatible, in order to avoid breaking some software such as database
  # servers. You should change this only after NixOS release notes say you
  # should.

  system.stateVersion = "18.03"; # Did you read the comment?
# nix = {
#   binaryCaches = [
#     "https://hie-nix.cachix.org"
#   ];
#   binaryCachePublicKeys = [
#     "hie-nix.cachix.org-1:EjBSHzF6VmDnzqlldGXbi0RM3HdjfTU3yDRi9Pd0jTY="
#   ];
#   trustedUsers = [ "root" "doug" ];
# };

}
