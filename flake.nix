{
  description = "Zig master overlay flake";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";

    zig-overlay.url = "github:mitchellh/zig-overlay";
    zig-overlay.inputs.nixpkgs.follows = "nixpkgs";

    zls-overlay.url = "github:zigtools/zls";
    zls-overlay.inputs.nixpkgs.follows = "nixpkgs";
  };

  outputs = { nixpkgs, zig-overlay, zls-overlay, ... }:
    let
      system = "x86_64-linux";
      pkgs = nixpkgs.legacyPackages.${system};
      zig = zig-overlay.packages.${system}.master;
      zls = zls-overlay.packages.${system}.default;
    in {
      formatter.${system} = pkgs.alejandra;
      packages.${system} = {
        default = zig;
        zls = zls;
        zig = zig;
      };
      devShells.${system}.default = pkgs.mkShell {
        nativeBuildInputs = with pkgs; [
          pkg-config
        ];
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
          # gtk for dialogs
          gtk3
          glib
          # wine for testing windows builds
          wineWowPackages.stable
          # for debugging
          renderdoc
          valgrind
        ];

        shellHook = ''
          echo "Zig version: ${zig.version}"
          echo "ZLS version: ${zls.version}"
          echo "$(qrenderdoc --version | head -n 1)"

          export PROMPT_NAME='dev:zig@${zig.version}';

          # Prepend the host system's GPU driver libraries
          export LD_LIBRARY_PATH="/run/opengl-driver/lib"

          export CFLAGS="$(pkg-config --cflags gtk+-3.0 glib-2.0) $CFLAGS"

          # Prepend the library paths from our Nix packages
          export LD_LIBRARY_PATH="${
            pkgs.lib.makeLibraryPath (with pkgs; [
              vulkan-loader
              xorg.libX11
              xorg.libXrandr
              gtk3
              glib
            ])
          }:$LD_LIBRARY_PATH"
        '';
      };
    };
}