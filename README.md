# Lift Tools Framework

[![Unit Tests](https://github.com/amy-keibler/lift-tools-framework/actions/workflows/tests.yml/badge.svg)](https://github.com/amy-keibler/lift-tools-framework/actions/workflows/tests.yml)

## Purpose

This project exists to enable development of custom tool integrations for [Sonatype Lift](https://lift.sonatype.com). Lift enables you to create custom tools to tailor Lift's analysis to your particular project's needs.

This project facilitates that development by providing a framework for common patterns of tool so you can avoid repetitive boilerplate and focus on what makes your tool different. The framework is responsible for the command-line interface and Lift-compatible JSON output. You implement a `ProjectContext -> ToolApplication` function, which provides a way for the application to check if the tool should run (useful if you are using the same tool in several repositories) and a template for how it should run.

The current templates that we support are `RunProcess` and `RunPerFile`. The first template will run a command-line process given arguments and an environment and then hands the process output to your code to extract `ToolResult`s.

The second template will use a regular expression to filter files to include as relevant. The file name and contents of these files are passed to your code for you to extract `ToolResult`s.

The goal of this project is to cover as many common use-cases as possible to enable users to easily pull in the existing and future tools of the Haskell ecosystem for their purposes.

## Examples

We include examples of real, complete tools that demonstrate the value of using our framework rather than writing the entire application yourself.

### Run Process Template Example

The `example-run-process` example demonstrates using this framework to define a custom tool that runs [`tslint`](https://github.com/palantir/tslint) when there is a `tsconfig.json` file inside the repository and transforms the JSON output into `ToolResult`s that can be understood by Lift.

### Run Per-File Template Example

The `example-run-per-file` example demonstrates using this framework to define a custom tool that runs [`hadolint`](https://github.com/hadolint/hadolint) as a Haskell library to parse any `Dockerfile`s in the repository for errors. The purpose of this example is to demonstrate the ease of integrating Haskell's robust collection of parsers into Lift without needing to worry about `IO`, filesystem concerns, the expected output format, or any other tangential details.

**Note: `hadolint`, and therefore this _specific_ example is licensed under the GPL. Please be aware those license obligations are different from this library's MIT + Apache 2.0 licensing. This is unlikely to impact you if you are using this example as a custom Lift tool, but distributing it to others requires making the source available to them.**

## Compiling and Using a Custom Tool

Lift tools are run in an Ubuntu 20.04 docker image. To ensure that your binary will run inside that container, a [`build-example-binaries.sh`](./build-example-binaries.sh) script and a [`Dockerfile`](./Dockerfile) are provided as examples. The script builds the `Dockerfile`, which compiles the `cabal` project to produce the tool binaries. The script then copies the binaries to the host filesystem so they can be uploaded to the internet.

Once the files are available on the internet, make a `.lift.toml` file with the following in the repository that you want to use the tool.

```toml
customTools = ["https://your.custom/tool/download/url"]
```

[reference](https://help.sonatype.com/lift/configuration-reference#ConfigurationReference-apis-for-custom-tools)

 Lift will fetch the tool from this URL, so ensure it does not require any authentication credentials or use a `setup` script in the `.lift.toml` file to download the tool.

## Development Nix Setup

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

1. `nix run github:amy-keibler/lift-tools-framework#lift-tools-framework:exe:example-run-process` will run an example found under [`./example/RunProcessMain.hs`](./example/RunProcessMain.hs)
2. `git clone git@github.com:amy-keibler/lift-tools-framework.git && cd lift-tools-framework && nix shell` will set up an environment that is ready to run `cabal` commands such as `cabal test`
   * My workflow involves spawing `emacs` from inside this shell so that it has access to a compatible `haskell-langauge-server` with all of the project's dependencies
   * Additionally, I use [`direnv`](https://direnv.net/) with the [Nix Flakes script](https://nixos.wiki/wiki/Flakes#Direnv_integration) so that my flakes are activated when I `cd` into their project folder

## Disclaimer

Although I am employed by Sonatype, this code is implemented using [publicly available documentation](https://help.sonatype.com/lift) and is not supported by Sonatype. Please file any issues encountered against this repository.
