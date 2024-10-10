# Edit this configuration file to define what should be installed on
# your system. Help is available in the configuration.nix(5) man page, on
# https://search.nixos.org/options and in the NixOS manual (`nixos-help`).

{ config, lib, pkgs, ... }:

{
  imports =
    [ # Include the results of the hardware scan.
      ./hardware-configuration.nix
    ];
 boot = {
    loader = {
      systemd-boot.enable = true;
      efi.canTouchEfiVariables = true;
    };

    initrd.availableKernelModules = [
      "xhci_pci"
      "ehci_pci"
      "ahci"
      "usb_storage"
      "sd_mod"
      "rtsx_pci_sdmmc"
    ];

    initrd.luks.devices = lib.mkForce {
      "root" = {
        device = "/dev/vg/root";
        preLVM = false;
      };
    };

    cleanTmpDir = true;
  };

  fileSystems = lib.mkForce {
    "/boot" = {
      device = "/dev/disk/by-uuid/ABBC-45A3";
      fsType = "vfat";
    };
    "/" = lib.mkForce {
      device = "/dev/mapper/root";
      fsType = "ext4";
    };
  };
  
  swapDevices = [
    {
      device = "/dev/vg/swap";
    }
  ];

  networking = {
    hostName = "krad";
    hostId = "d73b179a";
    enableIPv6 = true;
    networkmanager.enable = true;
  };
  
  environment = {
    systemPackages = with pkgs; [ zsh ];
  };

  time.timeZone = "Asia/Manila";

  security.sudo = {
   enable = true;
   configFile = ''
     Defaults env_reset
     root ALL = (ALL:ALL) ALL
     %wheel ALL = (ALL) SETENV: NOPASSWD: ALL
   '';
 };
 
 # allow unfree sources
 nixpkgs.config.allowUnfree = true;
 
 # nix command and flakes
 nix.settings.experimental-features = [ "nix-command" "flakes" ]; 
  
 # PulseAudio
 sound.enable = true;
 hardware.pulseaudio.enable = true;
 hardware.pulseaudio.extraConfig = "load-module module-combine-sink";

 # VB enable
 virtualisation.virtualbox.host.enable = true;
 users.extraGroups.vboxusers.members = [ "user-with-access-to-virtualbox" ];
	  
 nix = {
   maxJobs = lib.mkDefault 4;
   gc.automatic = false;
   trustedBinaryCaches = [ "http://hydra.nixos.org" "http://hydra.cryp.to" "http://cache.nixos.org" ];
   useSandbox = true;
 };

 programs = {
   ssh = {
     startAgent = true;
   };
 };

 hardware = {
  bluetooth.enable = true;
 };

  services = {
    xserver = {
      autorun = true;
      defaultDepth = 24;
      enable = true;
      displayManager.lightdm.enable = true;
      desktopManager.xfce.enable = true;
      videoDrivers = [ "intel" ];
   };

 
 openssh = {
   enable = true;
   ports = [ 2222 ];
   passwordAuthentication = false;
   permitRootLogin = "no";
 };

  ntp = {
    enable = true;
    servers = [ "asia.pool.ntp.org" "0.pool.ntp.org" "1.pool.ntp.org" "2.pool.ntp.org" ];
   };
 };

  users = {
    extraUsers.nycto = {
      isNormalUser = true;
      uid = 1000;
      extraGroups = [ "vboxusers" "audio" "wheel" "networkmanager" "docker" ];
    };
    defaultUserShell = "/run/current-system/sw/bin/zsh";
  };
  
  system.stateVersion = "24.05";
}
