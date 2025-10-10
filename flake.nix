{
  description = "Zig 0.15.1 overlay flake";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";

    zig-overlay.url = "github:mitchellh/zig-overlay";
    zig-overlay.inputs.nixpkgs.follows = "nixpkgs";

    zls-overlay.url = "github:zigtools/zls/0.15.0";
    zls-overlay.inputs.nixpkgs.follows = "nixpkgs";
  };

  outputs = { nixpkgs, zig-overlay, zls-overlay, ... }:
    let
      system = "x86_64-linux";
      pkgs = nixpkgs.legacyPackages.${system};
      zig = zig-overlay.packages.${system}."0.15.1";
      zls = zls-overlay.packages.${system}.default;
    in {
      formatter.${system} = pkgs.alejandra;
      packages.${system} = {
        default = zig;
        zls = zls;
        zig = zig;
      };
      devShells.${system}.default = pkgs.mkShell {
        buildInputs = with pkgs; [
          zig
          zls
          bashInteractive
          vulkan-tools
          vulkan-loader
          # Add all required X11 development libraries for GLFW
          xorg.libX11
          xorg.libXrandr
          xorg.libXinerama
          xorg.libXcursor
          xorg.libXi
          libxkbcommon
          # wine for testing windows builds
          wineWowPackages.stable
          # renderdoc for debugging
          renderdoc
        ];

        shellHook = ''
          echo "Zig version: ${zig.version}"
          echo "ZLS version: ${zls.version}"
          echo "$(qrenderdoc --version | head -n 1)"

          export PROMPT_NAME='dev:zig@${zig.version}';

          # Prepend the host system's GPU driver libraries
          export LD_LIBRARY_PATH="/run/opengl-driver/lib"

          # Prepend the library paths from our Nix packages
          export LD_LIBRARY_PATH="${
            pkgs.lib.makeLibraryPath (with pkgs; [
              vulkan-loader
              xorg.libX11
              xorg.libXrandr
            ])
          }:$LD_LIBRARY_PATH"
        '';
      };
    };
}