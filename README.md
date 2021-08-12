# Lift Tools Framework

## Nix Setup

This project uses a [Nix Flake](https://nixos.wiki/wiki/Flakes) to provide a consistent development and packaging environment. If you wish to use that tooling, follow the below steps:

1. Install Nix or NixOS: Follow the instructions found on [their website](https://nixos.org/download.html)
2. Configure Flakes on your system: Follow the instructions for either NixOS or Nix on [their wiki](https://nixos.wiki/wiki/Flakes#Installing_flakes)
3. Set up the `haskell.nix` cache in order to avoid compiling the entire Haskell toolchain: [their instructions](https://input-output-hk.github.io/haskell.nix/tutorials/getting-started-flakes/) work for Nix and the following snippet worked for NixOS for my machine
```nix
  nix = {
    package = pkgs.nixUnstable;
    extraOptions = ''
      experimental-features = nix-command flakes
    '';
    binaryCaches = [ "https://hydra.iohk.io"];
    trustedBinaryCaches = [ "https://hydra.iohk.io" ];
    binaryCachePublicKeys = [ "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ=" ];
  };
```

### Trying out the project

1. `nix run github:amy-keibler/lift-tools-framework#lift-tools-framework:exe:lift-tools-framework-example` will run the example found under [`./example/Main.hs`](./example/Main.hs)
2. `git clone git@github.com:amy-keibler/lift-tools-framework.git && cd lift-tools-framework && nix shell` will set up an environment that is ready to run `cabal` commands such as `cabal test`
   * My workflow involves spawing `emacs` from inside this shell so that it has access to a compatible `haskell-langauge-server` with all of the project's dependencies
   * Additionally, I use [`direnv`](https://direnv.net/) with the [Nix Flakes script](https://nixos.wiki/wiki/Flakes#Direnv_integration) so that my flakes are activated when I `cd` into their project folder
