//! An example of the new UI wrapper over Ui.

const std = @import("std");
const log = std.log.scoped(.main);
const builtin = @import("builtin");

const wgpu = @import("wgpu");
const stbi = @import("stbi");
const glfw = @import("glfw");

pub const debug = @import("debug.zig");
pub const Batch2D = @import("Batch2D.zig");
pub const AssetCache = @import("AssetCache.zig");
pub const InputState = @import("InputState.zig");
pub const Ui = @import("Ui.zig");

test {
    log.debug("semantic analysis for main.zig", .{});
    std.testing.refAllDecls(@This());
}

pub const std_options = std.Options{
    .log_level = .info,
};

const MSAA_SAMPLE_COUNT: u32 = 4;

// --- Global Application State ---

// WGPU and Window Management
const Demo = struct {
    instance: wgpu.Instance = null,
    surface: wgpu.Surface = null,
    adapter: wgpu.Adapter = null,
    device: wgpu.Device = null,
    config: wgpu.SurfaceConfiguration = .{},
    renderer: *Batch2D = undefined,
    msaa_texture: wgpu.Texture = null,
    msaa_view: wgpu.TextureView = null,
};

// Asset Identifiers
var syntax_image_id: AssetCache.ImageId = undefined;
var check_image1_id: AssetCache.ImageId = undefined;
var check_image2_id: AssetCache.ImageId = undefined;
var check_image3_id: AssetCache.ImageId = undefined;
var check_image4_id: AssetCache.ImageId = undefined;
var check_image5_id: AssetCache.ImageId = undefined;
var zig_logo_image_id: AssetCache.ImageId = undefined;
var wgpu_logo_image_id: AssetCache.ImageId = undefined;

var FONT_ID_BODY: AssetCache.FontId = undefined;
var FONT_ID_TITLE: AssetCache.FontId = undefined;

// UI State
var window_height: i32 = 0;
var window_width: i32 = 0;
var mobile_screen: bool = false;
var focused_element_id: ?u32 = null;

// --- Clay UI Color Constants ---
const COLOR_LIGHT = Ui.Color.init(244, 235, 230, 255);
const COLOR_LIGHT_HOVER = Ui.Color.init(224, 215, 210, 255);
const COLOR_BUTTON_HOVER = Ui.Color.init(238, 227, 225, 255);
const COLOR_BROWN = Ui.Color.init(61, 26, 5, 255);
const COLOR_RED = Ui.Color.init(168, 66, 28, 255);
const COLOR_RED_HOVER = Ui.Color.init(148, 46, 8, 255);
const COLOR_ORANGE = Ui.Color.init(225, 138, 50, 255);
const COLOR_BLUE = Ui.Color.init(111, 173, 162, 255);
const COLOR_TEAL = Ui.Color.init(111, 173, 162, 255);
const COLOR_BLUE_DARK = Ui.Color.init(2, 32, 82, 255);
const COLOR_NONE = Ui.Color.init(0, 0, 0, 255);
const COLOR_WHITE = Ui.Color.init(255, 255, 255, 255);

// Colors for top stripe
const COLORS_TOP_BORDER = [_]Ui.Color{
    .init(240, 213, 137, 255),
    .init(236, 189, 80, 255),
    .init(225, 138, 50, 255),
    .init(223, 110, 44, 255),
    .init(168, 66, 28, 255),
};

const COLOR_BLOB_BORDER_1 = Ui.Color.init(168, 66, 28, 255);
const COLOR_BLOB_BORDER_2 = Ui.Color.init(203, 100, 44, 255);
const COLOR_BLOB_BORDER_3 = Ui.Color.init(225, 138, 50, 255);
const COLOR_BLOB_BORDER_4 = Ui.Color.init(236, 159, 70, 255);
const COLOR_BLOB_BORDER_5 = Ui.Color.init(240, 189, 100, 255);

const border_data = Ui.BorderData{
    .width = .{ .top = 2, .bottom = 2, .left = 2, .right = 2 },
    .color = COLOR_RED,
};

fn landingPageBlob(ui: *Ui, index: u32, font_size: u16, font_id: AssetCache.FontId, color: Ui.Color, image_size: f32, width: f32, text: []const u8, image: AssetCache.ImageId) !void {
    try ui.openElement(.{
        .id = .indexed("HeroBlob", index),
        .layout = .{
            .sizing = .{ .w = .growMinMax(.{ .max = width }) },
            .padding = .all(16),
            .child_gap = 16,
            .child_alignment = .{ .y = .center },
        },
        .border = .{ .width = .all(2), .color = color },
        .corner_radius = .all(10),
    });
    defer ui.closeElement();

    try ui.elem(.{
        .id = .indexed("CheckImage", index),
        .layout = .{ .sizing = .{ .w = .fixed(image_size) } },
        .aspect_ratio = 1,
        .image = image,
    });

    try ui.text(text, .{ .font_size = font_size, .font_id = font_id, .color = color });
}

fn recreatedBlob(ui: *Ui) !void {
    const font_size = 30;
    const font_id = FONT_ID_BODY;
    const image_size = 64;
    const width = 510;
    const text = "The official Clay website recreated with zgpu";

    try ui.openElement(.{
        .id = .fromSlice("RecreatedBlob"),
        .layout = .{
            .sizing = .{ .w = .growMinMax(.{ .max = width }) },
            .padding = .all(16),
            .child_gap = 16,
            .child_alignment = .{ .y = .center },
        },
        .border = .{ .width = .all(2), .color = COLOR_BLOB_BORDER_5 },
        .corner_radius = .all(10),
    });
    defer ui.closeElement();

    try ui.elem(.{
        .id = .fromSlice("ZigLogo"),
        .layout = .{ .sizing = .{ .w = .fixed(image_size) } },
        .aspect_ratio = 1,
        .image = zig_logo_image_id,
    });

    try ui.text(text, .{
        .font_size = font_size,
        .font_id = font_id,
        .color = COLOR_BLOB_BORDER_5,
        .alignment = .center,
    });

    try ui.elem(.{
        .id = .fromSlice("WGPULogo"),
        .layout = .{ .sizing = .{ .w = .fixed(image_size) } },
        .aspect_ratio = 1,
        .image = wgpu_logo_image_id,
    });
}

fn landingPageDesktop(ui: *Ui) !void {
    try ui.openElement(.{
        .id = .fromSlice("LandingPage1Desktop"),
        .layout = .{
            .sizing = .{
                .w = .grow,
                .h = .fitMinMax(.{ .min = @floatFromInt(window_height - 70) }),
            },
            .child_alignment = .{ .y = .center },
            .padding = .{ .left = 50, .right = 50 },
        },
    });
    defer ui.closeElement();

    {
        try ui.openElement(.{
            .id = .fromSlice("LandingPage1"),
            .layout = .{
                .sizing = .{ .w = .grow, .h = .grow },
                .direction = .top_to_bottom,
                .child_alignment = .{ .x = .center },
                .padding = .all(32),
                .child_gap = 32,
            },
            .border = .{ .width = .{ .left = 2, .right = 2 }, .color = COLOR_RED },
        });
        defer ui.closeElement();

        try recreatedBlob(ui);

        {
            try ui.openElement(.{ .id = .fromSlice("ClayPresentation"), .layout = .{
                .sizing = .grow,
                .child_alignment = .{ .y = .center },
                .child_gap = 16,
            } });
            defer ui.closeElement();

            {
                try ui.openElement(.{
                    .id = .fromSlice("LeftText"),
                    .layout = .{
                        .sizing = .{ .w = .percent(0.55) },
                        .direction = .top_to_bottom,
                        .child_gap = 8,
                    },
                });
                defer ui.closeElement();

                try ui.text(
                    "Clay is a flex-box style UI auto layout library in C, with declarative syntax and microsecond performance.",
                    .{ .font_size = 56, .font_id = FONT_ID_TITLE, .color = COLOR_RED },
                );
                try ui.elem(.{
                    .layout = .{
                        .sizing = .{ .w = .grow, .h = .fixed(32) },
                    },
                });
                try ui.text(
                    "Clay is laying out this window right now!",
                    .{ .font_size = 36, .font_id = FONT_ID_BODY, .color = COLOR_ORANGE },
                );
            }

            {
                try ui.openElement(.{
                    .id = .fromSlice("HeroImageOuter"),
                    .layout = .{
                        .sizing = .{ .w = .percent(0.45) },
                        .direction = .top_to_bottom,
                        .child_alignment = .{ .x = .center },
                        .child_gap = 16,
                    },
                });
                defer ui.closeElement();

                try landingPageBlob(
                    ui,
                    1,
                    30,
                    FONT_ID_BODY,
                    COLOR_BLOB_BORDER_5,
                    32,
                    480,
                    "High performance",
                    check_image5_id,
                );

                try landingPageBlob(
                    ui,
                    2,
                    30,
                    FONT_ID_BODY,
                    COLOR_BLOB_BORDER_4,
                    32,
                    480,
                    "Flexbox-style responsive layout",
                    check_image4_id,
                );

                try landingPageBlob(
                    ui,
                    3,
                    30,
                    FONT_ID_BODY,
                    COLOR_BLOB_BORDER_3,
                    32,
                    480,
                    "Declarative syntax",
                    check_image3_id,
                );

                try landingPageBlob(
                    ui,
                    4,
                    30,
                    FONT_ID_BODY,
                    COLOR_BLOB_BORDER_2,
                    32,
                    480,
                    "Single .h file for C/C++",
                    check_image2_id,
                );

                try landingPageBlob(
                    ui,
                    5,
                    30,
                    FONT_ID_BODY,
                    COLOR_BLOB_BORDER_1,
                    32,
                    480,
                    "Compile to 15kb .wasm",
                    check_image1_id,
                );
            }
        }
    }
}

fn landingPageMobile(ui: *Ui) !void {
    try ui.openElement(.{
        .id = .fromSlice("LandingPage1Mobile"),
        .layout = .{
            .sizing = .{
                .w = .grow,
                .h = .fitMinMax(.{ .min = @floatFromInt(window_height - 70) }),
            },
            .direction = .top_to_bottom,
            .child_alignment = .center,
            .padding = .{ .left = 16, .right = 16, .top = 32, .bottom = 32 },
            .child_gap = 16,
        },
    });
    defer ui.closeElement();

    try recreatedBlob(ui);

    {
        try ui.openElement(.{
            .id = .fromSlice("LeftText"),
            .layout = .{
                .sizing = .{ .w = .grow },
                .direction = .top_to_bottom,
                .child_gap = 8,
            },
        });
        defer ui.closeElement();

        try ui.text(
            "Clay is a flex-box style UI auto layout library in C, with declarative syntax and microsecond performance.",
            .{ .font_size = 56, .font_id = FONT_ID_TITLE, .color = COLOR_RED },
        );
        try ui.elem(.{ .layout = .{
            .sizing = .{
                .w = .grow,
                .h = .fixed(32),
            },
        } });
        try ui.text(
            "Clay is laying out this window right now!",
            .{ .font_size = 36, .font_id = FONT_ID_BODY, .color = COLOR_ORANGE },
        );
    }

    {
        try ui.openElement(.{ .id = .fromSlice("HeroImageOuter"), .layout = .{
            .sizing = .{ .w = .grow },
            .direction = .top_to_bottom,
            .child_alignment = .{ .x = .center },
            .child_gap = 16,
        } });
        defer ui.closeElement();

        try landingPageBlob(
            ui,
            1,
            30,
            FONT_ID_BODY,
            COLOR_BLOB_BORDER_5,
            32,
            480,
            "High performance",
            check_image5_id,
        );

        try landingPageBlob(
            ui,
            2,
            30,
            FONT_ID_BODY,
            COLOR_BLOB_BORDER_4,
            32,
            480,
            "Flexbox-style responsive layout",
            check_image4_id,
        );

        try landingPageBlob(
            ui,
            3,
            30,
            FONT_ID_BODY,
            COLOR_BLOB_BORDER_3,
            32,
            480,
            "Declarative syntax",
            check_image3_id,
        );

        try landingPageBlob(
            ui,
            4,
            30,
            FONT_ID_BODY,
            COLOR_BLOB_BORDER_2,
            32,
            480,
            "Single .h file for C/C++",
            check_image2_id,
        );

        try landingPageBlob(
            ui,
            5,
            30,
            FONT_ID_BODY,
            COLOR_BLOB_BORDER_1,
            32,
            480,
            "Compile to 15kb .wasm",
            check_image1_id,
        );
    }
}

fn featureBlocks(ui: *Ui, width_sizing: Ui.SizingAxis, outer_padding: u16) !void {
    const text_config = Ui.TextElementConfig{ .font_size = 24, .font_id = FONT_ID_BODY, .color = COLOR_RED };

    {
        try ui.openElement(.{
            .id = .fromSlice("HFileBoxOuter"),
            .layout = .{
                .direction = .top_to_bottom,
                .sizing = .{ .w = width_sizing },
                .child_alignment = .{ .y = .center },
                .padding = .{ .left = outer_padding, .right = outer_padding, .top = 32, .bottom = 32 },
                .child_gap = 8,
            },
        });
        defer ui.closeElement();

        {
            try ui.openElement(.{
                .id = .fromSlice("HFileIncludeOuter"),
                .layout = .{ .padding = .{ .left = 8, .right = 8, .top = 4, .bottom = 4 } },
                .background_color = COLOR_RED,
                .corner_radius = .all(8),
            });
            defer ui.closeElement();

            try ui.text(
                "#include ui.h",
                .{ .font_size = 24, .font_id = FONT_ID_BODY, .color = COLOR_LIGHT },
            );
        }

        try ui.text("~2000 lines of C99.", text_config);
        try ui.text("Zero dependencies, including no C standard library", text_config);
    }

    {
        try ui.openElement(.{
            .id = .fromSlice("BringYourOwnRendererOuter"),
            .layout = .{
                .direction = .top_to_bottom,
                .sizing = .{ .w = width_sizing },
                .child_alignment = .{ .y = .center },
                .padding = .{ .left = outer_padding, .right = outer_padding, .top = 32, .bottom = 32 },
                .child_gap = 8,
            },
        });
        defer ui.closeElement();

        try ui.text(
            "Renderer agnostic.",
            .{ .font_size = 24, .font_id = FONT_ID_BODY, .color = COLOR_ORANGE },
        );
        try ui.text("Layout with clay, then render with Raylib, WebGL Canvas or even as HTML.", text_config);
        try ui.text("Flexible output for easy compositing in your custom engine or environment.", text_config);
    }
}

fn featureBlocksDesktop(ui: *Ui) !void {
    try ui.openElement(.{
        .id = .fromSlice("FeatureBlocksOuter"),
        .layout = .{
            .sizing = .{ .w = .grow },
            .child_alignment = .{ .y = .center },
        },
        .border = .{ .width = .{ .between_children = 2 }, .color = COLOR_RED },
    });
    defer ui.closeElement();

    try featureBlocks(ui, .percent(0.5), 50);
}

fn featureBlocksMobile(ui: *Ui) !void {
    try ui.openElement(.{
        .id = .fromSlice("FeatureBlocksOuter"),
        .layout = .{
            .sizing = .{ .w = .grow },
            .direction = .top_to_bottom,
        },
        .border = .{ .width = .{ .between_children = 2 }, .color = COLOR_RED },
    });
    defer ui.closeElement();

    try featureBlocks(ui, .grow, 16);
}

fn declarativeSyntaxPage(ui: *Ui, title_text_config: Ui.TextElementConfig, width_sizing: Ui.SizingAxis) !void {
    {
        try ui.openElement(.{ .id = .fromSlice("SyntaxPageLeftText"), .layout = .{
            .sizing = .{ .w = width_sizing },
            .direction = .top_to_bottom,
            .child_gap = 8,
        } });
        defer ui.closeElement();

        try ui.text("Declarative Syntax", title_text_config);
        try ui.elem(.{ .layout = .{ .sizing = .{ .w = .growMinMax(.{ .max = 16 }) } } });

        const text_conf = Ui.TextElementConfig{
            .font_size = 28,
            .font_id = FONT_ID_BODY,
            .color = COLOR_RED,
        };
        try ui.text("Flexible and readable declarative syntax with nested UI element hierarchies.", text_conf);
        try ui.text("Mix elements with standard C code like loops, conditionals and functions.", text_conf);
        try ui.text("Create your own library of re-usable components from UI primitives like text, images and rectangles.", text_conf);
    }

    {
        try ui.openElement(.{ .id = .fromSlice("SyntaxPageRightImageOuter"), .layout = .{
            .sizing = .{ .w = width_sizing },
            .child_alignment = .{ .x = .center },
        } });
        defer ui.closeElement();

        try ui.elem(.{
            .id = .fromSlice("SyntaxPageRightImage"),
            .layout = .{ .sizing = .{ .w = .growMinMax(.{ .max = 568 }) } },
            .aspect_ratio = 1194 / 1136,
            .image = syntax_image_id,
        });
    }
}

fn declarativeSyntaxPageDesktop(ui: *Ui) !void {
    try ui.openElement(.{
        .id = .fromSlice("SyntaxPageDesktop"),
        .layout = .{
            .sizing = .{
                .w = .grow,
                .h = .fitMinMax(.{ .min = @floatFromInt(window_height - 50) }),
            },
            .child_alignment = .{ .y = .center },
            .padding = .{ .left = 50, .right = 50 },
        },
    });
    defer ui.closeElement();

    {
        try ui.openElement(.{
            .id = .fromSlice("SyntaxPage"),
            .layout = .{
                .sizing = .{ .w = .grow, .h = .grow },
                .child_alignment = .{ .y = .center },
                .padding = .all(32),
                .child_gap = 32,
            },
            .border = .{
                .width = .{ .left = 2, .right = 2 },
                .color = COLOR_RED,
            },
        });
        defer ui.closeElement();

        try declarativeSyntaxPage(
            ui,
            .{ .font_size = 52, .font_id = FONT_ID_TITLE, .color = COLOR_RED },
            .percent(0.5),
        );
    }
}

fn declarativeSyntaxPageMobile(ui: *Ui) !void {
    try ui.openElement(.{
        .id = .fromSlice("SyntaxPageMobile"),
        .layout = .{
            .direction = .top_to_bottom,
            .sizing = .{
                .w = .grow,
                .h = .fitMinMax(.{ .min = @floatFromInt(window_height - 50) }),
            },
            .child_alignment = .center,
            .padding = .{ .left = 16, .right = 16, .top = 32, .bottom = 32 },
            .child_gap = 16,
        },
    });
    defer ui.closeElement();

    try declarativeSyntaxPage(
        ui,
        .{ .font_size = 48, .font_id = FONT_ID_TITLE, .color = COLOR_RED },
        .grow,
    );
}

const LOREM_IPSUM_TEXT = "Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua.";

fn highPerformancePage(ui: *Ui, lerp_value: f32, title_text_tonfig: Ui.TextElementConfig, width_sizing: Ui.SizingAxis) !void {
    {
        try ui.openElement(.{
            .id = .fromSlice("PerformanceLeftText"),
            .layout = .{
                .sizing = .{ .w = width_sizing },
                .direction = .top_to_bottom,
                .child_gap = 8,
            },
        });
        defer ui.closeElement();

        try ui.text("High Performance", title_text_tonfig);
        try ui.elem(.{ .layout = .{
            .sizing = .{ .w = .growMinMax(.{ .max = 16 }) },
        } });
        try ui.text(
            "Fast enough to recompute your entire UI every frame.",
            .{ .font_size = 28, .font_id = FONT_ID_BODY, .color = COLOR_LIGHT },
        );
        try ui.text(
            "Small memory footprint (3.5mb default) with static allocation & reuse. No malloc / free.",
            .{ .font_size = 28, .font_id = FONT_ID_BODY, .color = COLOR_LIGHT },
        );
        try ui.text(
            "Simplify animations and reactive UI design by avoiding the standard performance hacks.",
            .{ .font_size = 28, .font_id = FONT_ID_BODY, .color = COLOR_LIGHT },
        );
    }

    {
        try ui.openElement(.{
            .id = .fromSlice("PerformanceRightImageOuter"),
            .layout = .{
                .sizing = .{ .w = width_sizing },
                .child_alignment = .{ .x = .center },
            },
        });
        defer ui.closeElement();

        {
            try ui.openElement(.{
                .id = .fromSlice("PerformanceRightBorder"),
                .layout = .{
                    .sizing = .{ .w = .grow, .h = .fixed(400) },
                },
                .border = .{ .width = .all(2), .color = COLOR_LIGHT },
            });
            defer ui.closeElement();

            {
                try ui.openElement(.{
                    .id = .fromSlice("AnimationDemoContainerLeft"),
                    .layout = .{
                        .sizing = .{ .w = .percent(0.35 + 0.3 * lerp_value), .h = .grow },
                        .child_alignment = .{ .y = .center },
                        .padding = .all(16),
                    },
                    .background_color = COLOR_RED.lerp(COLOR_ORANGE, lerp_value),
                });
                defer ui.closeElement();

                try ui.text(LOREM_IPSUM_TEXT, .{
                    .font_size = 16,
                    .font_id = FONT_ID_BODY,
                    .color = COLOR_LIGHT,
                });
            }

            {
                try ui.openElement(.{
                    .id = .fromSlice("AnimationDemoContainerRight"),
                    .layout = .{
                        .sizing = .{ .w = .grow, .h = .grow },
                        .child_alignment = .{ .y = .center },
                        .padding = .all(16),
                    },
                    .background_color = COLOR_ORANGE.lerp(COLOR_RED, lerp_value),
                });
                defer ui.closeElement();

                try ui.text(LOREM_IPSUM_TEXT, .{
                    .font_size = 16,
                    .font_id = FONT_ID_BODY,
                    .color = COLOR_LIGHT,
                });
            }
        }
    }
}

fn highPerformancePageDesktop(ui: *Ui, lerp_value: f32) !void {
    try ui.openElement(.{
        .id = .fromSlice("PerformanceDesktop"),
        .layout = .{
            .sizing = .{ .w = .grow, .h = .fitMinMax(.{ .min = @floatFromInt(window_height - 50) }) },
            .child_alignment = .{ .y = .center },
            .padding = .{ .left = 82, .right = 82, .top = 32, .bottom = 32 },
            .child_gap = 64,
        },
        .background_color = COLOR_RED,
    });
    defer ui.closeElement();

    try highPerformancePage(
        ui,
        lerp_value,
        .{ .font_size = 52, .font_id = FONT_ID_TITLE, .color = COLOR_LIGHT },
        .percent(0.5),
    );
}

fn highPerformancePageMobile(ui: *Ui, lerp_value: f32) !void {
    try ui.openElement(.{
        .id = .fromSlice("PerformanceMobile"),
        .layout = .{
            .direction = .top_to_bottom,
            .sizing = .{
                .w = .grow,
                .h = .fitMinMax(.{ .min = @floatFromInt(window_height - 50) }),
            },
            .child_alignment = .center,
            .padding = .{ .left = 16, .right = 16, .top = 32, .bottom = 32 },
            .child_gap = 32,
        },
        .background_color = COLOR_RED,
    });
    defer ui.closeElement();

    try highPerformancePage(
        ui,
        lerp_value,
        .{ .font_size = 48, .font_id = FONT_ID_TITLE, .color = COLOR_LIGHT },
        .grow,
    );
}

fn rendererButtonActive(ui: *Ui, text: []const u8) !void {
    try ui.openElement(.{
        .layout = .{ .sizing = .{ .w = .fixed(300) }, .padding = .all(16) },
        .background_color = COLOR_RED,
        .corner_radius = .all(10),
    });
    defer ui.closeElement();

    try ui.text(text, .{ .font_size = 28, .font_id = FONT_ID_BODY, .color = COLOR_LIGHT });
}

fn rendererButtonInactive(ui: *Ui, index: u32, text: []const u8) !void {
    try ui.openElement(.{
        .border = .all(.{ 2, COLOR_RED }, 10),
    });
    defer ui.closeElement();

    {
        try ui.openElement(.{
            .id = .fromSlice("RendererButtonInactiveInner", index),
            .layout = .{
                .sizing = .{ .w = .fixed(300) },
                .padding = .all(16),
            },
            .background_color = COLOR_LIGHT,
            .corner_radius = .all(10),
        });
        defer ui.closeElement();

        try ui.text(text, .{ .font_size = 28, .font_id = FONT_ID_BODY, .color = COLOR_RED });
    }
}

fn rendererPage(ui: *Ui, title_text_config: Ui.TextElementConfig, _: Ui.SizingAxis) !void {
    try ui.openElement(.{ .id = .fromSlice("RendererLeftText"), .layout = .{ .direction = .top_to_bottom, .child_gap = 8 } });
    defer ui.closeElement();

    try ui.text("Renderer & Platform Agnostic", title_text_config);
    try ui.elem(.{
        .layout = .{
            .sizing = .{ .w = .growMinMax(.{ .max = 16 }) },
        },
    });
    try ui.text(
        "Clay outputs a sorted array of primitive render commands, such as RECTANGLE, TEXT or IMAGE.",
        .{ .font_size = 28, .font_id = FONT_ID_BODY, .color = COLOR_RED },
    );
    try ui.text(
        "Write your own renderer in a few hundred lines of code, or use the provided examples for Raylib, WebGL canvas and more.",
        .{ .font_size = 28, .font_id = FONT_ID_BODY, .color = COLOR_RED },
    );
    try ui.text(
        "There's even an HTML renderer - you're looking at it right now!",
        .{ .font_size = 28, .font_id = FONT_ID_BODY, .color = COLOR_RED },
    );
}

fn rendererPageDesktop(ui: *Ui) !void {
    try ui.openElement(.{
        .id = .fromSlice("RendererPageDesktop"),
        .layout = .{
            .sizing = .{ .w = .grow, .h = .fitMinMax(.{ .min = @floatFromInt(window_height - 50) }) },
            .child_alignment = .{ .y = .center },
            .padding = .{ .left = 50, .right = 50 },
        },
    });
    defer ui.closeElement();

    {
        try ui.openElement(.{
            .id = .fromSlice("RendererPage"),
            .layout = .{
                .sizing = .grow,
                .child_alignment = .{ .y = .center },
                .padding = .all(32),
                .child_gap = 32,
            },
            .border = .{ .width = .{ .left = 2, .right = 2 }, .color = COLOR_RED },
        });
        defer ui.closeElement();

        try rendererPage(
            ui,
            .{ .font_size = 52, .font_id = FONT_ID_TITLE, .color = COLOR_RED },
            .percent(0.5),
        );
    }
}

fn rendererPageMobile(ui: *Ui) !void {
    try ui.openElement(.{
        .id = .fromSlice("RendererMobile"),
        .layout = .{
            .direction = .top_to_bottom,
            .sizing = .{
                .w = .grow,
                .h = .fitMinMax(.{ .min = @floatFromInt(window_height - 50) }),
            },
            .child_alignment = .center,
            .padding = .{ .left = 16, .right = 16, .top = 32, .bottom = 32 },
            .child_gap = 32,
        },
        .background_color = COLOR_LIGHT,
    });
    defer ui.closeElement();

    try rendererPage(
        ui,
        .{ .font_size = 52, .font_id = FONT_ID_TITLE, .color = COLOR_RED },
        .grow,
    );
}

fn createLayout(ui: *Ui, lerp_value: f32) !void {
    try ui.openElement(.{
        .id = .fromSlice("OuterContainer"),
        .layout = .{ .sizing = .grow, .direction = .top_to_bottom },
        .background_color = COLOR_LIGHT,
    });
    defer ui.closeElement();

    {
        try ui.openElement(.{
            .id = .fromSlice("Header"),
            .layout = .{
                .sizing = .{ .h = .fixed(50), .w = .grow },
                .child_alignment = .{ .y = .center },
                .padding = .{ .left = 32, .right = 32 },
                .child_gap = 24,
            },
            .state = .scrollable,
        });
        defer ui.closeElement();

        {
            try ui.openElement(.{ .id = .fromSlice("Stuff") });
            defer ui.closeElement();

            for (0..5) |i| {
                const id = Ui.ElementId.indexed("Focusable", @intCast(i));

                try ui.beginElement(id);
                defer ui.closeElement();

                try ui.configureElement(.{
                    .image = if (i % 2 == 0) wgpu_logo_image_id else zig_logo_image_id,
                    .layout = .{ .sizing = .{ .w = .fixed(32), .h = .fixed(32) } },
                    .background_color = if (ui.hovered()) COLOR_WHITE else if (ui.focused()) COLOR_ORANGE else COLOR_BLUE_DARK,
                    .state = .flags(.{ .focus = true }),
                });
            }
        }

        try ui.text(
            "Clay",
            .{
                .font_id = FONT_ID_BODY,
                .font_size = 24,
                .color = if (ui.focusedId() != null) COLOR_ORANGE else COLOR_BLUE_DARK,
            },
        );

        try ui.elem(.{ .layout = .{ .sizing = .{ .w = .grow } } });

        if (!mobile_screen) {
            {
                try ui.openElement(.{
                    .id = .fromSlice("LinkExamplesInner"),
                    .background_color = .{},
                });
                defer ui.closeElement();

                try ui.text("Examples", .{
                    .font_id = FONT_ID_BODY,
                    .font_size = 24,
                    .color = .init(61, 26, 5, 255),
                });
            }

            {
                try ui.openElement(.{
                    .id = .fromSlice("LinkDocsOuter"),
                    .background_color = .{},
                });
                defer ui.closeElement();

                try ui.text("Docs", .{
                    .font_id = FONT_ID_BODY,
                    .font_size = 24,
                    .color = .init(61, 26, 5, 255),
                });
            }
        }

        {
            try ui.beginElement(.fromSlice("LinkGithubOuter"));
            defer ui.closeElement();

            try ui.configureElement(.{
                .layout = .{
                    .padding = .{ .left = 32, .right = 32, .top = 6, .bottom = 6 },
                },
                .border = .{ .width = .all(2), .color = COLOR_RED },
                .corner_radius = .all(10),
                .background_color = if (ui.hovered()) COLOR_LIGHT_HOVER else COLOR_LIGHT,
            });

            try ui.text("Github", .{
                .font_id = FONT_ID_BODY,
                .font_size = 24,
                .color = .init(61, 26, 5, 255),
            });
        }
    }

    for (COLORS_TOP_BORDER, 0..) |color, i| {
        try ui.elem(.{
            .id = .indexed("TopBorder", @intCast(i)),
            .layout = .{
                .sizing = .{ .h = .fixed(4), .w = .grow },
            },
            .background_color = color,
        });
    }

    {
        try ui.beginElement(.fromSrc(@src()));
        defer ui.closeElement();

        try ui.configureElement(.{
            .clip = .{
                .vertical = true,
                .child_offset = ui.scrollOffset(),
            },
            .layout = .{
                .sizing = .grow,
                .direction = .top_to_bottom,
            },
            .background_color = COLOR_LIGHT,
            .border = .{ .width = .{ .between_children = 2 }, .color = COLOR_RED },
        });

        if (!mobile_screen) {
            try landingPageDesktop(ui);
            try featureBlocksDesktop(ui);
            try declarativeSyntaxPageDesktop(ui);
            try highPerformancePageDesktop(ui, lerp_value);
            try rendererPageDesktop(ui);
        } else {
            try landingPageMobile(ui);
            try featureBlocksMobile(ui);
            try declarativeSyntaxPageMobile(ui);
            try highPerformancePageMobile(ui, lerp_value);
            try rendererPageMobile(ui);
        }
    }
}

pub fn main() !void {
    var debug_timer = try std.time.Timer.start();
    var timer = try std.time.Timer.start();
    var last_frame_time = timer.read();

    const gpa = std.heap.page_allocator;

    if (comptime builtin.os.tag != .windows) {
        glfw.initHint(.{ .platform = .x11 });
    } else {
        glfw.initHint(.{ .platform = .win32 });
    }

    try glfw.init();
    defer glfw.deinit();

    stbi.init(gpa);
    defer stbi.deinit();

    var demo = Demo{};

    const instance_extras = wgpu.InstanceExtras{
        .chain = .{ .s_type = .instance_extras },
        .backends = switch (builtin.os.tag) {
            .windows => if (glfw.isRunningInWine()) wgpu.InstanceBackend.vulkanBackend else wgpu.InstanceBackend.dx12Backend,
            else => wgpu.InstanceBackend.vulkanBackend,
        },
    };
    demo.instance = wgpu.createInstance(&wgpu.InstanceDescriptor{ .next_in_chain = @ptrCast(&instance_extras) });
    std.debug.assert(demo.instance != null);
    defer wgpu.instanceRelease(demo.instance);

    glfw.windowHint(.{ .client_api = .none });
    const window = try glfw.createWindow(1000, 1000, "zgpu example", null, null);
    defer glfw.destroyWindow(window);
    glfw.setWindowUserPointer(window, &demo);

    // --- GLFW Callbacks ---
    _ = glfw.setFramebufferSizeCallback(window, &struct {
        fn handle_glfw_framebuffer_size(w: *glfw.Window, width: i32, height: i32) callconv(.c) void {
            if (width <= 0 and height <= 0) return;
            const d: *Demo = @ptrCast(@alignCast(glfw.getWindowUserPointer(w) orelse return));
            if (d.surface == null) return;
            d.config.width = @intCast(width);
            d.config.height = @intCast(height);
            wgpu.surfaceConfigure(d.surface, &d.config);
            createOrResizeMsaaTexture(d);
        }
    }.handle_glfw_framebuffer_size);

    // --- WGPU Surface, Adapter, Device Setup ---
    if (comptime builtin.os.tag != .windows) {
        const x11_display = glfw.getX11Display();
        const x11_window = glfw.getX11Window(window);
        var xlib_source = wgpu.SurfaceSourceXlibWindow{ .chain = .{ .s_type = .surface_source_xlib_window }, .display = x11_display, .window = x11_window };
        demo.surface = wgpu.instanceCreateSurface(demo.instance, &wgpu.SurfaceDescriptor{ .next_in_chain = @ptrCast(&xlib_source) });
    } else {
        const win32_hwnd = glfw.getWin32Window(window);
        const win32_hinstance = glfw.getWin32ModuleHandle();
        var win32_source = wgpu.SurfaceSourceWindowsHWND{ .chain = .{ .s_type = .surface_source_windows_hwnd }, .hwnd = win32_hwnd, .hinstance = win32_hinstance };
        demo.surface = wgpu.instanceCreateSurface(demo.instance, &wgpu.SurfaceDescriptor{ .next_in_chain = @ptrCast(&win32_source) });
    }
    std.debug.assert(demo.surface != null);
    defer wgpu.surfaceRelease(demo.surface);

    _ = wgpu.instanceRequestAdapter(demo.instance, &wgpu.RequestAdapterOptions{ .compatible_surface = demo.surface }, .{ .callback = &struct {
        fn handle_request_adapter(status: wgpu.RequestAdapterStatus, adapter: wgpu.Adapter, msg: wgpu.StringView, ud1: ?*anyopaque, _: ?*anyopaque) callconv(.c) void {
            if (status == .success) {
                const d: *Demo = @ptrCast(@alignCast(ud1.?));
                d.adapter = adapter;
            } else {
                log.err("request_adapter failed: {s}", .{msg.toSlice()});
            }
        }
    }.handle_request_adapter, .userdata1 = &demo });
    while (demo.adapter == null) wgpu.instanceProcessEvents(demo.instance);
    defer wgpu.adapterRelease(demo.adapter);

    _ = wgpu.adapterRequestDevice(demo.adapter, null, .{ .callback = &struct {
        fn handle_request_device(status: wgpu.RequestDeviceStatus, device: wgpu.Device, msg: wgpu.StringView, ud1: ?*anyopaque, _: ?*anyopaque) callconv(.c) void {
            if (status == .success) {
                const d: *Demo = @ptrCast(@alignCast(ud1.?));
                d.device = device;
            } else {
                log.err("request_device failed: {s}", .{msg.toSlice()});
            }
        }
    }.handle_request_device, .userdata1 = &demo });
    while (demo.device == null) wgpu.instanceProcessEvents(demo.instance);
    defer {
        if (demo.msaa_view != null) wgpu.textureViewRelease(demo.msaa_view);
        if (demo.msaa_texture != null) wgpu.textureRelease(demo.msaa_texture);
        wgpu.deviceRelease(demo.device);
    }

    const queue = wgpu.deviceGetQueue(demo.device);
    defer wgpu.queueRelease(queue);
    var surface_capabilities: wgpu.SurfaceCapabilities = undefined;
    _ = wgpu.surfaceGetCapabilities(demo.surface, demo.adapter, &surface_capabilities);
    defer wgpu.surfaceCapabilitiesFreeMembers(surface_capabilities);

    const surface_format = surface_capabilities.formats.?[0];
    demo.config = .{
        .device = demo.device,
        .usage = .renderAttachmentUsage,
        .format = surface_format,
        .present_mode = .immediate,
        .alpha_mode = surface_capabilities.alpha_modes.?[0],
    };

    {
        glfw.getFramebufferSize(window, &window_width, &window_height);
        demo.config.width = @intCast(window_width);
        demo.config.height = @intCast(window_height);
    }
    wgpu.surfaceConfigure(demo.surface, &demo.config);
    createOrResizeMsaaTexture(&demo);

    // --- Application and UI Library Initialization ---
    var arena_state = std.heap.ArenaAllocator.init(gpa);
    defer arena_state.deinit();

    // Init Asset Cache
    var asset_cache = AssetCache.init(gpa);
    defer asset_cache.deinit();

    // Init Batch Renderer
    const provider_ctx = Batch2D.ProviderContext{
        .provider = AssetCache.dataProvider,
        .frame_allocator = arena_state.allocator(),
        .user_context = &asset_cache,
    };
    demo.renderer = try Batch2D.init(gpa, demo.device, queue, surface_format, provider_ctx, MSAA_SAMPLE_COUNT);
    defer demo.renderer.deinit();

    var inputs = InputState{};
    inputs.listenAllGlfw(window);

    // Init Ui
    var ui = try Ui.init(gpa, demo.renderer, &asset_cache, &inputs);
    defer ui.deinit();

    // --- Load Assets ---
    FONT_ID_BODY = try asset_cache.loadFont("assets/fonts/Quicksand-Semibold.ttf");
    FONT_ID_TITLE = try asset_cache.loadFont("assets/fonts/Calistoga-Regular.ttf");

    syntax_image_id = try asset_cache.loadImage("assets/images/clay/declarative.png", true);
    check_image1_id = try asset_cache.loadImage("assets/images/clay/check_1.png", true);
    check_image2_id = try asset_cache.loadImage("assets/images/clay/check_2.png", true);
    check_image3_id = try asset_cache.loadImage("assets/images/clay/check_3.png", true);
    check_image4_id = try asset_cache.loadImage("assets/images/clay/check_4.png", true);
    check_image5_id = try asset_cache.loadImage("assets/images/clay/check_5.png", true);
    zig_logo_image_id = try asset_cache.loadImage("assets/images/zig-mark.png", true);
    wgpu_logo_image_id = try asset_cache.loadImage("assets/images/wgpu-logo.png", true);

    try demo.renderer.preAtlasAllImages(&asset_cache);

    var animation_lerp_value: f32 = -1.0;
    var debug_mode_enabled = false;

    const startup_time = debug.start(&debug_timer);
    log.info("startup_time={d}ms", .{startup_time});

    // --- Main Loop ---
    main_loop: while (!glfw.windowShouldClose(window)) {
        glfw.pollEvents();
        _ = arena_state.reset(.free_all);

        // --- Calculate Delta Time ---
        const current_time = timer.read();
        const delta_time_ns = current_time - last_frame_time;
        last_frame_time = current_time;
        const delta_time_f32 = @as(f32, @floatFromInt(delta_time_ns)) / std.time.ns_per_s;

        // --- Update Application State ---
        animation_lerp_value += delta_time_f32;
        if (animation_lerp_value > 1.0) {
            animation_lerp_value -= 2.0;
        }

        // --- Handle Input ---
        if (glfw.getKey(window, .d) == .press) {
            debug_mode_enabled = !debug_mode_enabled;
            ui.setDebugMode(debug_mode_enabled);
        }

        if (glfw.getKey(window, .a) == .press and glfw.getKey(window, .left_alt).isDown()) {
            try demo.renderer.atlas.debugWriteAllAtlasesToPng("debug_atlas");
            log.info("finished writing debug_atlas_*.png", .{});
        }

        // --- Update Clay UI ---
        glfw.getFramebufferSize(window, &window_width, &window_height);
        mobile_screen = window_width < 750;

        inputs.collectAllGlfw(window);

        // Generate the UI layout and cache rendering commands
        {
            ui.beginLayout(
                .{ .x = @floatFromInt(window_width), .y = @floatFromInt(window_height) },
                delta_time_f32,
            );

            try createLayout(ui, if (animation_lerp_value < 0) animation_lerp_value + 1 else 1 - animation_lerp_value);

            const events = try ui.endLayout();

            for (events) |event| {
                switch (event.data) {
                    .hover_begin => |hover_begin_data| {
                        log.info("hover_begin id={any} loc={f}", .{ event.element_id, hover_begin_data.mouse_position });
                    },
                    .hover_end => |hover_end_data| {
                        log.info("hover_end id={any} loc={f}", .{ event.element_id, hover_end_data.mouse_position });
                    },
                    .hovering => |hovering_data| {
                        _ = hovering_data; // too noisey rn
                        // log.info("hovering id={any} loc={f}", .{ event.element_id, hovering_data.mouse_position });
                    },
                    .mouse_down => |mouse_down_data| {
                        log.info("mouse_down id={any} loc={f}", .{ event.element_id, mouse_down_data.mouse_position });
                    },
                    .mouse_up => |mouse_up_data| {
                        log.info("mouse_up id={any} loc={f}", .{ event.element_id, mouse_up_data.mouse_position });
                    },
                    .clicked => |clicked_data| {
                        log.info("clicked id={any} loc={f}", .{ event.element_id, clicked_data.mouse_position });
                    },
                    .focus_gained => {
                        log.info("focus_gained id={any}", .{event.element_id});

                        focused_element_id = event.element_id.id;
                    },
                    .focus_lost => {
                        log.info("focus_lost id={any}", .{event.element_id});

                        focused_element_id = null;
                    },
                    .scroll => |scroll_data| {
                        log.info("scroll id={any} delta={f}", .{ event.element_id, scroll_data.delta });
                    },
                }
            }
        }

        // --- WGPU Frame Rendering ---
        var surface_texture: wgpu.SurfaceTexture = undefined;
        wgpu.surfaceGetCurrentTexture(demo.surface, &surface_texture);
        switch (surface_texture.status) {
            .success_optimal, .success_suboptimal => {},
            .timeout, .outdated, .lost => {
                if (surface_texture.texture != null) wgpu.textureRelease(surface_texture.texture);
                glfw.getFramebufferSize(window, &window_width, &window_height);
                if (window_width != 0 and window_height != 0) {
                    demo.config.width = @intCast(window_width);
                    demo.config.height = @intCast(window_height);
                    wgpu.surfaceConfigure(demo.surface, &demo.config);
                    createOrResizeMsaaTexture(&demo);
                }
                continue :main_loop;
            },
            else => std.debug.panic("get_current_texture status={any}", .{surface_texture.status}),
        }
        std.debug.assert(surface_texture.texture != null);
        defer wgpu.textureRelease(surface_texture.texture);

        const frame_view = wgpu.textureCreateView(surface_texture.texture, null);
        std.debug.assert(frame_view != null);
        defer wgpu.textureViewRelease(frame_view);

        // --- Queue all draws to screen buffer ---
        {
            const proj = ortho(0, @floatFromInt(demo.config.width), @floatFromInt(demo.config.height), 0, -1, 1);
            demo.renderer.beginFrame(proj, demo.config.width, demo.config.height);

            try ui.render();

            try debug.drawFpsChart(demo.renderer, .{});

            try demo.renderer.endFrame();
        }

        // --- WGPU Command Submission ---
        const encoder = wgpu.deviceCreateCommandEncoder(demo.device, &.{ .label = .fromSlice("main_encoder") });
        defer wgpu.commandEncoderRelease(encoder);

        const render_target_view = if (demo.msaa_view != null) demo.msaa_view else frame_view;
        const resolve_target_view = if (demo.msaa_view != null) frame_view else null;
        const clear_color = wgpu.Color{ .r = 0.1, .g = 0.1, .b = 0.1, .a = 1.0 };

        const render_pass = wgpu.commandEncoderBeginRenderPass(encoder, &wgpu.RenderPassDescriptor{
            .color_attachment_count = 1,
            .color_attachments = &[_]wgpu.RenderPassColorAttachment{.{
                .view = render_target_view,
                .resolve_target = resolve_target_view,
                .load_op = .clear,
                .store_op = .store,
                .clear_value = clear_color,
            }},
        });

        try demo.renderer.render(render_pass);

        wgpu.renderPassEncoderEnd(render_pass);
        wgpu.renderPassEncoderRelease(render_pass);

        const cmd = wgpu.commandEncoderFinish(encoder, null);
        defer wgpu.commandBufferRelease(cmd);

        wgpu.queueSubmit(queue, 1, &.{cmd});

        _ = wgpu.surfacePresent(demo.surface);

        debug.lap();
    }
}

/// (Re)creates or resizes the MSAA texture used for rendering, based on the current swap chain size.
fn createOrResizeMsaaTexture(d: *Demo) void {
    if (d.msaa_view != null) wgpu.textureViewRelease(d.msaa_view);
    if (d.msaa_texture != null) wgpu.textureRelease(d.msaa_texture);

    if (MSAA_SAMPLE_COUNT <= 1) {
        d.msaa_texture = null;
        d.msaa_view = null;
        return;
    }

    const msaa_descriptor = wgpu.TextureDescriptor{
        .label = .fromSlice("msaa_texture"),
        .size = .{ .width = d.config.width, .height = d.config.height, .depth_or_array_layers = 1 },
        .mip_level_count = 1,
        .sample_count = MSAA_SAMPLE_COUNT,
        .dimension = .@"2d",
        .format = d.config.format,
        .usage = wgpu.TextureUsage{ .render_attachment = true },
    };

    d.msaa_texture = wgpu.deviceCreateTexture(d.device, &msaa_descriptor);
    std.debug.assert(d.msaa_texture != null);
    d.msaa_view = wgpu.textureCreateView(d.msaa_texture, null);
    std.debug.assert(d.msaa_view != null);
}

/// orthographic projection matrix helper
fn ortho(left: f32, right: f32, bottom: f32, top: f32, near: f32, far: f32) [16]f32 {
    var mat: [16]f32 = std.mem.zeroes([16]f32);
    mat[0] = 2.0 / (right - left);
    mat[5] = 2.0 / (top - bottom);
    mat[10] = -2.0 / (far - near);
    mat[12] = -(right + left) / (right - left);
    mat[13] = -(top + bottom) / (top - bottom);
    mat[14] = -(far + near) / (far - near);
    mat[15] = 1.0;
    return mat;
}
