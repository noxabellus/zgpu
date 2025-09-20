//! An example of the Clay UI library running on a custom WGPU backend.

const std = @import("std");
const log = std.log.scoped(.main);
const builtin = @import("builtin");

const wgpu = @import("wgpu");
const stbi = @import("stbi");
const glfw = @import("glfw");
const clay = @import("clay");

const Batch2D = @import("Batch2D.zig");
const AssetCache = @import("AssetCache.zig");
const ClayBackend = @import("ClayBackend.zig");

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

// Input Handling
var mouse_scroll_delta = Batch2D.Vec2{ .x = 0, .y = 0 };

// --- Clay UI Color Constants ---
const COLOR_LIGHT = clay.Color{ 244, 235, 230, 255 };
const COLOR_LIGHT_HOVER = clay.Color{ 224, 215, 210, 255 };
const COLOR_BUTTON_HOVER = clay.Color{ 238, 227, 225, 255 };
const COLOR_BROWN = clay.Color{ 61, 26, 5, 255 };
const COLOR_RED = clay.Color{ 168, 66, 28, 255 };
const COLOR_RED_HOVER = clay.Color{ 148, 46, 8, 255 };
const COLOR_ORANGE = clay.Color{ 225, 138, 50, 255 };
const COLOR_BLUE = clay.Color{ 111, 173, 162, 255 };
const COLOR_TEAL = clay.Color{ 111, 173, 162, 255 };
const COLOR_BLUE_DARK = clay.Color{ 2, 32, 82, 255 };
const COLOR_NONE = clay.Color{ 0, 0, 0, 255 };

// Colors for top stripe
const COLORS_TOP_BORDER = [_]clay.Color{
    .{ 240, 213, 137, 255 },
    .{ 236, 189, 80, 255 },
    .{ 225, 138, 50, 255 },
    .{ 223, 110, 44, 255 },
    .{ 168, 66, 28, 255 },
};

const COLOR_BLOB_BORDER_1 = clay.Color{ 168, 66, 28, 255 };
const COLOR_BLOB_BORDER_2 = clay.Color{ 203, 100, 44, 255 };
const COLOR_BLOB_BORDER_3 = clay.Color{ 225, 138, 50, 255 };
const COLOR_BLOB_BORDER_4 = clay.Color{ 236, 159, 70, 255 };
const COLOR_BLOB_BORDER_5 = clay.Color{ 240, 189, 100, 255 };

const border_data = clay.BorderData{
    .width = .{ .top = 2, .bottom = 2, .left = 2, .right = 2 },
    .color = COLOR_RED,
};

fn landingPageBlob(index: u32, font_size: u16, font_id: u16, color: clay.Color, image_size: f32, width: f32, text: []const u8, image: AssetCache.ImageId) void {
    clay.UI()(.{
        .id = .IDI("HeroBlob", index),
        .layout = .{ .sizing = .{ .w = .growMinMax(.{ .max = width }) }, .padding = .all(16), .child_gap = 16, .child_alignment = .{ .y = .center } },
        .border = .{ .width = .outside(2), .color = color },
        .corner_radius = .all(10),
    })({
        clay.UI()(.{ .id = .IDI("CheckImage", index), .layout = .{ .sizing = .{ .w = .fixed(image_size) } }, .aspect_ratio = .{ .aspect_ratio = 1 }, .image = ClayBackend.image(image) })({});
        clay.text(text, .{ .font_size = font_size, .font_id = font_id, .color = color });
    });
}

fn recreatedBlob() void {
    const index = 0;
    const font_size = 30;
    const font_id = FONT_ID_BODY;
    const image_size = 64;
    const width = 510;
    const text = "The official Clay website recreated with zgpu";
    clay.UI()(.{
        .id = .IDI("HeroBlob", index),
        .layout = .{ .sizing = .{ .w = .growMinMax(.{ .max = width }) }, .padding = .all(16), .child_gap = 16, .child_alignment = .{ .y = .center } },
        .border = .{ .width = .outside(2), .color = COLOR_BLOB_BORDER_5 },
        .corner_radius = .all(10),
    })({
        clay.UI()(.{ .id = .IDI("CheckImage", index), .layout = .{ .sizing = .{ .w = .fixed(image_size) } }, .aspect_ratio = .{ .aspect_ratio = 1 }, .image = ClayBackend.image(zig_logo_image_id) })({});
        clay.text(text, .{
            .font_size = font_size,
            .font_id = font_id,
            .color = COLOR_BLOB_BORDER_5,
            .alignment = .center,
        });
        clay.UI()(.{ .id = .IDI("CheckImage", index), .layout = .{ .sizing = .{ .w = .fixed(image_size) } }, .aspect_ratio = .{ .aspect_ratio = 1 }, .image = ClayBackend.image(wgpu_logo_image_id) })({});
    });
}

fn landingPageDesktop() void {
    clay.UI()(.{
        .id = .ID("LandingPage1Desktop"),
        .layout = .{ .sizing = .{ .w = .grow, .h = .fitMinMax(.{ .min = @floatFromInt(window_height - 70) }) }, .child_alignment = .{ .y = .center }, .padding = .{ .left = 50, .right = 50 } },
    })({
        clay.UI()(.{
            .id = .ID("LandingPage1"),
            .layout = .{ .sizing = .{ .w = .grow, .h = .grow }, .direction = .top_to_bottom, .child_alignment = .{ .x = .center }, .padding = .all(32), .child_gap = 32 },
            .border = .{ .width = .{ .left = 2, .right = 2 }, .color = COLOR_RED },
        })({
            recreatedBlob();
            clay.UI()(.{ .id = .ID("ClayPresentation"), .layout = .{ .sizing = .grow, .child_alignment = .{ .y = .center }, .child_gap = 16 } })({
                clay.UI()(.{
                    .id = .ID("LeftText"),
                    .layout = .{ .sizing = .{ .w = .percent(0.55) }, .direction = .top_to_bottom, .child_gap = 8 },
                })({
                    clay.text("Clay is a flex-box style UI auto layout library in C, with declarative syntax and microsecond performance.", .{ .font_size = 56, .font_id = FONT_ID_TITLE, .color = COLOR_RED });
                    clay.UI()(.{ .layout = .{ .sizing = .{ .w = .grow, .h = .fixed(32) } } })({});
                    clay.text("Clay is laying out this window right now!", .{ .font_size = 36, .font_id = FONT_ID_BODY, .color = COLOR_ORANGE });
                });

                clay.UI()(.{
                    .id = .ID("HeroImageOuter"),
                    .layout = .{ .sizing = .{ .w = .percent(0.45) }, .direction = .top_to_bottom, .child_alignment = .{ .x = .center }, .child_gap = 16 },
                })({
                    landingPageBlob(1, 30, FONT_ID_BODY, COLOR_BLOB_BORDER_5, 32, 480, "High performance", check_image5_id);
                    landingPageBlob(2, 30, FONT_ID_BODY, COLOR_BLOB_BORDER_4, 32, 480, "Flexbox-style responsive layout", check_image4_id);
                    landingPageBlob(3, 30, FONT_ID_BODY, COLOR_BLOB_BORDER_3, 32, 480, "Declarative syntax", check_image3_id);
                    landingPageBlob(4, 30, FONT_ID_BODY, COLOR_BLOB_BORDER_2, 32, 480, "Single .h file for C/C++", check_image2_id);
                    landingPageBlob(5, 30, FONT_ID_BODY, COLOR_BLOB_BORDER_1, 32, 480, "Compile to 15kb .wasm", check_image1_id);
                });
            });
        });
    });
}

fn landingPageMobile() void {
    clay.UI()(.{
        .id = .ID("LandingPage1Mobile"),
        .layout = .{
            .sizing = .{ .w = .grow, .h = .fitMinMax(.{ .min = @floatFromInt(window_height - 70) }) },
            .direction = .top_to_bottom,
            .child_alignment = .center,
            .padding = .{ .left = 16, .right = 16, .top = 32, .bottom = 32 },
            .child_gap = 16,
        },
    })({
        recreatedBlob();
        clay.UI()(.{
            .id = .ID("LeftText"),
            .layout = .{ .sizing = .{ .w = .grow }, .direction = .top_to_bottom, .child_gap = 8 },
        })({
            clay.text("Clay is a flex-box style UI auto layout library in C, with declarative syntax and microsecond performance.", .{ .font_size = 56, .font_id = FONT_ID_TITLE, .color = COLOR_RED });
            clay.UI()(.{ .layout = .{ .sizing = .{ .w = .grow, .h = .fixed(32) } } })({});
            clay.text("Clay is laying out this window right now!", .{ .font_size = 36, .font_id = FONT_ID_BODY, .color = COLOR_ORANGE });
        });

        clay.UI()(.{
            .id = .ID("HeroImageOuter"),
            .layout = .{ .sizing = .{ .w = .grow }, .direction = .top_to_bottom, .child_alignment = .{ .x = .center }, .child_gap = 16 },
        })({
            landingPageBlob(1, 30, FONT_ID_BODY, COLOR_BLOB_BORDER_5, 32, 480, "High performance", check_image5_id);
            landingPageBlob(2, 30, FONT_ID_BODY, COLOR_BLOB_BORDER_4, 32, 480, "Flexbox-style responsive layout", check_image4_id);
            landingPageBlob(3, 30, FONT_ID_BODY, COLOR_BLOB_BORDER_3, 32, 480, "Declarative syntax", check_image3_id);
            landingPageBlob(4, 30, FONT_ID_BODY, COLOR_BLOB_BORDER_2, 32, 480, "Single .h file for C/C++", check_image2_id);
            landingPageBlob(5, 30, FONT_ID_BODY, COLOR_BLOB_BORDER_1, 32, 480, "Compile to 15kb .wasm", check_image1_id);
        });
    });
}

fn featureBlocks(width_sizing: clay.SizingAxis, outer_padding: u16) void {
    const text_config = clay.TextElementConfig{ .font_size = 24, .font_id = FONT_ID_BODY, .color = COLOR_RED };
    clay.UI()(.{
        .id = .ID("HFileBoxOuter"),
        .layout = .{
            .direction = .top_to_bottom,
            .sizing = .{ .w = width_sizing },
            .child_alignment = .{ .y = .center },
            .padding = .{ .left = outer_padding, .right = outer_padding, .top = 32, .bottom = 32 },
            .child_gap = 8,
        },
    })({
        clay.UI()(.{
            .id = .ID("HFileIncludeOuter"),
            .layout = .{ .padding = .{ .left = 8, .right = 8, .top = 4, .bottom = 4 } },
            .background_color = COLOR_RED,
            .corner_radius = .all(8),
        })({
            clay.text("#include clay.h", .{ .font_size = 24, .font_id = FONT_ID_BODY, .color = COLOR_LIGHT });
        });
        clay.text("~2000 lines of C99.", text_config);
        clay.text("Zero dependencies, including no C standard library", text_config);
    });
    clay.UI()(.{
        .id = .ID("BringYourOwnRendererOuter"),
        .layout = .{
            .direction = .top_to_bottom,
            .sizing = .{ .w = width_sizing },
            .child_alignment = .{ .y = .center },
            .padding = .{ .left = outer_padding, .right = outer_padding, .top = 32, .bottom = 32 },
            .child_gap = 8,
        },
    })({
        clay.text("Renderer agnostic.", .{ .font_size = 24, .font_id = FONT_ID_BODY, .color = COLOR_ORANGE });
        clay.text("Layout with clay, then render with Raylib, WebGL Canvas or even as HTML.", text_config);
        clay.text("Flexible output for easy compositing in your custom engine or environment.", text_config);
    });
}

fn featureBlocksDesktop() void {
    clay.UI()(.{
        .id = .ID("FeatureBlocksOuter"),
        .layout = .{ .sizing = .{ .w = .grow }, .child_alignment = .{ .y = .center } },
        .border = .{ .width = .{ .between_children = 2 }, .color = COLOR_RED },
    })({
        featureBlocks(.percent(0.5), 50);
    });
}

fn featureBlocksMobile() void {
    clay.UI()(.{
        .id = .ID("FeatureBlocksOuter"),
        .layout = .{ .sizing = .{ .w = .grow }, .direction = .top_to_bottom },
        .border = .{ .width = .{ .between_children = 2 }, .color = COLOR_RED },
    })({
        featureBlocks(.grow, 16);
    });
}

fn declarativeSyntaxPage(title_text_config: clay.TextElementConfig, width_sizing: clay.SizingAxis) void {
    clay.UI()(.{ .id = .ID("SyntaxPageLeftText"), .layout = .{ .sizing = .{ .w = width_sizing }, .direction = .top_to_bottom, .child_gap = 8 } })({
        clay.text("Declarative Syntax", title_text_config);
        clay.UI()(.{ .layout = .{ .sizing = .{ .w = .growMinMax(.{ .max = 16 }) } } })({});
        const text_conf = clay.TextElementConfig{ .font_size = 28, .font_id = FONT_ID_BODY, .color = COLOR_RED };
        clay.text("Flexible and readable declarative syntax with nested UI element hierarchies.", text_conf);
        clay.text("Mix elements with standard C code like loops, conditionals and functions.", text_conf);
        clay.text("Create your own library of re-usable components from UI primitives like text, images and rectangles.", text_conf);
    });
    clay.UI()(.{ .id = .ID("SyntaxPageRightImageOuter"), .layout = .{ .sizing = .{ .w = width_sizing }, .child_alignment = .{ .x = .center } } })({
        clay.UI()(.{
            .id = .ID("SyntaxPageRightImage"),
            .layout = .{ .sizing = .{ .w = .growMinMax(.{ .max = 568 }) } },
            .aspect_ratio = .{ .aspect_ratio = 1194 / 1136 },
            .image = ClayBackend.image(syntax_image_id),
        })({});
    });
}

fn declarativeSyntaxPageDesktop() void {
    clay.UI()(.{
        .id = .ID("SyntaxPageDesktop"),
        .layout = .{ .sizing = .{ .w = .grow, .h = .fitMinMax(.{ .min = @floatFromInt(window_height - 50) }) }, .child_alignment = .{ .y = .center }, .padding = .{ .left = 50, .right = 50 } },
    })({
        clay.UI()(.{
            .id = .ID("SyntaxPage"),
            .layout = .{ .sizing = .{ .w = .grow, .h = .grow }, .child_alignment = .{ .y = .center }, .padding = .all(32), .child_gap = 32 },
            .border = .{ .width = .{ .left = 2, .right = 2 }, .color = COLOR_RED },
        })({
            declarativeSyntaxPage(.{ .font_size = 52, .font_id = FONT_ID_TITLE, .color = COLOR_RED }, .percent(0.5));
        });
    });
}

fn declarativeSyntaxPageMobile() void {
    clay.UI()(.{
        .id = .ID("SyntaxPageMobile"),
        .layout = .{
            .direction = .top_to_bottom,
            .sizing = .{ .w = .grow, .h = .fitMinMax(.{ .min = @floatFromInt(window_height - 50) }) },
            .child_alignment = .center,
            .padding = .{ .left = 16, .right = 16, .top = 32, .bottom = 32 },
            .child_gap = 16,
        },
    })({
        declarativeSyntaxPage(.{ .font_size = 48, .font_id = FONT_ID_TITLE, .color = COLOR_RED }, .grow);
    });
}

fn colorLerp(a: clay.Color, b: clay.Color, amount: f32) clay.Color {
    return clay.Color{ a[0] + (b[0] - a[0]) * amount, a[1] + (b[1] - a[1]) * amount, a[2] + (b[2] - a[2]) * amount, a[3] + (b[3] - a[3]) * amount };
}

const LOREM_IPSUM_TEXT = "Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua.";

fn highPerformancePage(lerp_value: f32, title_text_tonfig: clay.TextElementConfig, width_sizing: clay.SizingAxis) void {
    clay.UI()(.{ .id = .ID("PerformanceLeftText"), .layout = .{ .sizing = .{ .w = width_sizing }, .direction = .top_to_bottom, .child_gap = 8 } })({
        clay.text("High Performance", title_text_tonfig);
        clay.UI()(.{ .layout = .{ .sizing = .{ .w = .growMinMax(.{ .max = 16 }) } } })({});
        clay.text("Fast enough to recompute your entire UI every frame.", .{ .font_size = 28, .font_id = FONT_ID_BODY, .color = COLOR_LIGHT });
        clay.text("Small memory footprint (3.5mb default) with static allocation & reuse. No malloc / free.", .{ .font_size = 28, .font_id = FONT_ID_BODY, .color = COLOR_LIGHT });
        clay.text("Simplify animations and reactive UI design by avoiding the standard performance hacks.", .{ .font_size = 28, .font_id = FONT_ID_BODY, .color = COLOR_LIGHT });
    });
    clay.UI()(.{ .id = .ID("PerformanceRightImageOuter"), .layout = .{ .sizing = .{ .w = width_sizing }, .child_alignment = .{ .x = .center } } })({
        clay.UI()(.{
            .id = .ID("PerformanceRightBorder"),
            .layout = .{ .sizing = .{ .w = .grow, .h = .fixed(400) } },
            .border = .{ .width = .all(2), .color = COLOR_LIGHT },
        })({
            clay.UI()(.{
                .id = .ID("AnimationDemoContainerLeft"),
                .layout = .{ .sizing = .{ .w = .percent(0.35 + 0.3 * lerp_value), .h = .grow }, .child_alignment = .{ .y = .center }, .padding = .all(16) },
                .background_color = colorLerp(COLOR_RED, COLOR_ORANGE, lerp_value),
            })({
                clay.text(LOREM_IPSUM_TEXT, .{ .font_size = 16, .font_id = FONT_ID_BODY, .color = COLOR_LIGHT });
            });

            clay.UI()(.{
                .id = .ID("AnimationDemoContainerRight"),
                .layout = .{ .sizing = .{ .w = .grow, .h = .grow }, .child_alignment = .{ .y = .center }, .padding = .all(16) },
                .background_color = colorLerp(COLOR_ORANGE, COLOR_RED, lerp_value),
            })({
                clay.text(LOREM_IPSUM_TEXT, .{ .font_size = 16, .font_id = FONT_ID_BODY, .color = COLOR_LIGHT });
            });
        });
    });
}

fn highPerformancePageDesktop(lerp_value: f32) void {
    clay.UI()(.{
        .id = .ID("PerformanceDesktop"),
        .layout = .{
            .sizing = .{ .w = .grow, .h = .fitMinMax(.{ .min = @floatFromInt(window_height - 50) }) },
            .child_alignment = .{ .y = .center },
            .padding = .{ .left = 82, .right = 82, .top = 32, .bottom = 32 },
            .child_gap = 64,
        },
        .background_color = COLOR_RED,
    })({
        highPerformancePage(lerp_value, .{ .font_size = 52, .font_id = FONT_ID_TITLE, .color = COLOR_LIGHT }, .percent(0.5));
    });
}

fn highPerformancePageMobile(lerp_value: f32) void {
    clay.UI()(.{
        .id = .ID("PerformanceMobile"),
        .layout = .{
            .direction = .top_to_bottom,
            .sizing = .{ .w = .grow, .h = .fitMinMax(.{ .min = @floatFromInt(window_height - 50) }) },
            .child_alignment = .center,
            .padding = .{ .left = 16, .right = 16, .top = 32, .bottom = 32 },
            .child_gap = 32,
        },
        .background_color = COLOR_RED,
    })({
        highPerformancePage(lerp_value, .{ .font_size = 48, .font_id = FONT_ID_TITLE, .color = COLOR_LIGHT }, .grow);
    });
}

fn rendererButtonActive(text: []const u8) void {
    clay.UI()(.{
        .layout = .{ .sizing = .{ .w = .fixed(300) }, .padding = .all(16) },
        .background_color = COLOR_RED,
        .corner_radius = .all(10),
    })({
        clay.text(text, .{ .font_size = 28, .font_id = FONT_ID_BODY, .color = COLOR_LIGHT });
    });
}

fn rendererButtonInactive(index: u32, text: []const u8) void {
    clay.UI()(.{ .layout = .{}, .border = .outside(.{ 2, COLOR_RED }, 10) })({
        clay.UI()(.{
            .id = .ID("RendererButtonInactiveInner", index),
            .layout = .{ .sizing = .{ .w = .fixed(300) }, .padding = .all(16) },
            .background_color = COLOR_LIGHT,
            .corner_radius = .all(10),
        })({
            clay.text(text, .{ .font_size = 28, .font_id = FONT_ID_BODY, .color = COLOR_RED });
        });
    });
}

fn rendererPage(title_text_config: clay.TextElementConfig, _: clay.SizingAxis) void {
    clay.UI()(.{ .id = .ID("RendererLeftText"), .layout = .{ .direction = .top_to_bottom, .child_gap = 8 } })({
        clay.text("Renderer & Platform Agnostic", title_text_config);
        clay.UI()(.{ .layout = .{ .sizing = .{ .w = .growMinMax(.{ .max = 16 }) } } })({});
        clay.text("Clay outputs a sorted array of primitive render commands, such as RECTANGLE, TEXT or IMAGE.", .{ .font_size = 28, .font_id = FONT_ID_BODY, .color = COLOR_RED });
        clay.text("Write your own renderer in a few hundred lines of code, or use the provided examples for Raylib, WebGL canvas and more.", .{ .font_size = 28, .font_id = FONT_ID_BODY, .color = COLOR_RED });
        clay.text("There's even an HTML renderer - you're looking at it right now!", .{ .font_size = 28, .font_id = FONT_ID_BODY, .color = COLOR_RED });
    });
}

fn rendererPageDesktop() void {
    clay.UI()(.{
        .id = .ID("RendererPageDesktop"),
        .layout = .{ .sizing = .{ .w = .grow, .h = .fitMinMax(.{ .min = @floatFromInt(window_height - 50) }) }, .child_alignment = .{ .y = .center }, .padding = .{ .left = 50, .right = 50 } },
    })({
        clay.UI()(.{
            .id = .ID("RendererPage"),
            .layout = .{ .sizing = .grow, .child_alignment = .{ .y = .center }, .padding = .all(32), .child_gap = 32 },
            .border = .{ .width = .{ .left = 2, .right = 2 }, .color = COLOR_RED },
        })({
            rendererPage(.{ .font_size = 52, .font_id = FONT_ID_TITLE, .color = COLOR_RED }, .percent(0.5));
        });
    });
}

fn rendererPageMobile() void {
    clay.UI()(.{
        .id = .ID("RendererMobile"),
        .layout = .{
            .direction = .top_to_bottom,
            .sizing = .{ .w = .grow, .h = .fitMinMax(.{ .min = @floatFromInt(window_height - 50) }) },
            .child_alignment = .center,
            .padding = .{ .left = 16, .right = 16, .top = 32, .bottom = 32 },
            .child_gap = 32,
        },
        .background_color = COLOR_LIGHT,
    })({
        rendererPage(.{ .font_size = 52, .font_id = FONT_ID_TITLE, .color = COLOR_RED }, .grow);
    });
}

fn createLayout(lerp_value: f32) []clay.RenderCommand {
    clay.beginLayout();
    clay.UI()(.{
        .id = .ID("OuterContainer"),
        .layout = .{ .sizing = .grow, .direction = .top_to_bottom },
        .background_color = COLOR_LIGHT,
    })({
        clay.UI()(.{
            .id = .ID("Header"),
            .layout = .{ .sizing = .{ .h = .fixed(50), .w = .grow }, .child_alignment = .{ .y = .center }, .padding = .{ .left = 32, .right = 32 }, .child_gap = 24 },
        })({
            clay.text("Clay", .{ .font_id = FONT_ID_BODY, .font_size = 24, .color = .{ 61, 26, 5, 255 } });
            clay.UI()(.{ .layout = .{ .sizing = .{ .w = .grow } } })({});

            if (!mobile_screen) {
                clay.UI()(.{ .id = .ID("LinkExamplesInner"), .layout = .{}, .background_color = .{ 0, 0, 0, 0 } })({
                    clay.text("Examples", .{ .font_id = FONT_ID_BODY, .font_size = 24, .color = .{ 61, 26, 5, 255 } });
                });
                clay.UI()(.{ .id = .ID("LinkDocsOuter"), .layout = .{}, .background_color = .{ 0, 0, 0, 0 } })({
                    clay.text("Docs", .{ .font_id = FONT_ID_BODY, .font_size = 24, .color = .{ 61, 26, 5, 255 } });
                });
            }

            clay.UI()(.{
                .layout = .{ .padding = .{ .left = 32, .right = 32, .top = 6, .bottom = 6 } },
                .border = .{ .width = .all(2), .color = COLOR_RED },
                .corner_radius = .all(10),
                .background_color = if (clay.hovered()) COLOR_LIGHT_HOVER else COLOR_LIGHT,
            })({
                clay.text("Github", .{ .font_id = FONT_ID_BODY, .font_size = 24, .color = .{ 61, 26, 5, 255 } });
            });
        });
        for (COLORS_TOP_BORDER, 0..) |color, i| {
            clay.UI()(.{
                .id = .IDI("TopBorder", @intCast(i)),
                .layout = .{ .sizing = .{ .h = .fixed(4), .w = .grow } },
                .background_color = color,
            })({});
        }

        clay.UI()(.{
            .id = .fromSrc(@src()),
            .clip = .{ .vertical = true, .child_offset = clay.getScrollOffset() },
            .layout = .{ .sizing = .grow, .direction = .top_to_bottom },
            .background_color = COLOR_LIGHT,
            .border = .{ .width = .{ .between_children = 2 }, .color = COLOR_RED },
        })({
            if (!mobile_screen) {
                landingPageDesktop();
                featureBlocksDesktop();
                declarativeSyntaxPageDesktop();
                highPerformancePageDesktop(lerp_value);
                rendererPageDesktop();
            } else {
                landingPageMobile();
                featureBlocksMobile();
                declarativeSyntaxPageMobile();
                highPerformancePageMobile(lerp_value);
                rendererPageMobile();
            }
        });
    });
    return clay.endLayout();
}

pub fn main() !void {
    var timer = try std.time.Timer.start();
    var last_frame_time = timer.read();

    const gpa = std.heap.page_allocator;

    // --- Core Initialization (GLFW, WGPU, STBI) ---
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

    const instance_extras = wgpu.InstanceExtras{ .chain = .{ .s_type = .instance_extras }, .backends = switch (builtin.os.tag) {
        .windows => if (glfw.isRunningInWine()) wgpu.InstanceBackend.vulkanBackend else wgpu.InstanceBackend.dx12Backend,
        else => wgpu.InstanceBackend.vulkanBackend,
    } };
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

    _ = glfw.setScrollCallback(window, &struct {
        fn handle_scroll(_: *glfw.Window, xoffset: f64, yoffset: f64) callconv(.c) void {
            // Invert y-axis to match common UI scrolling behavior
            mouse_scroll_delta.x += @as(f32, @floatCast(xoffset)) * 6.0;
            mouse_scroll_delta.y += @as(f32, @floatCast(yoffset)) * 6.0;
        }
    }.handle_scroll);

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
        glfw.getWindowSize(window, &window_width, &window_height);
        demo.config.width = @intCast(window_width);
        demo.config.height = @intCast(window_height);
    }
    wgpu.surfaceConfigure(demo.surface, &demo.config);
    createOrResizeMsaaTexture(&demo);

    // --- Application and UI Library Initialization ---
    var arena_state = std.heap.ArenaAllocator.init(gpa);
    defer arena_state.deinit();

    var asset_cache = AssetCache.init(gpa);
    defer asset_cache.deinit();

    // Init Clay
    const min_memory_size = clay.minMemorySize();
    const clay_memory = try gpa.alloc(u8, min_memory_size);
    defer gpa.free(clay_memory);
    const clay_arena = clay.createArenaWithCapacityAndMemory(clay_memory);
    const ui_context = clay.initialize(clay_arena, .{ .w = @floatFromInt(window_width), .h = @floatFromInt(window_height) }, .{
        // no error handling for now
    });
    _ = ui_context;

    // Init Batch Renderer
    const provider_ctx = Batch2D.ProviderContext{
        .provider = AssetCache.dataProvider,
        .frame_allocator = arena_state.allocator(),
        .user_context = &asset_cache,
    };
    demo.renderer = try Batch2D.init(gpa, demo.device, queue, surface_format, provider_ctx, MSAA_SAMPLE_COUNT);
    defer demo.renderer.deinit();

    // Init Clay Backend Bridge
    var clay_backend = try ClayBackend.init(gpa, demo.renderer, &asset_cache);
    defer clay_backend.deinit(gpa);

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

    var animation_lerp_value: f32 = -1.0;
    var debug_mode_enabled = false;

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
            clay.setDebugModeEnabled(debug_mode_enabled);
        }

        const alt_key_state = glfw.getKey(window, .left_alt);
        if (glfw.getKey(window, .a) == .press and (alt_key_state == .press or alt_key_state == .repeat)) {
            try demo.renderer.atlas.debugWriteAllAtlasesToPng("debug_atlas");
            log.info("finished writing debug_atlas_*.png", .{});
        }

        // --- Update Clay UI ---
        glfw.getWindowSize(window, &window_width, &window_height);
        // TODO: Account for debug view width if clay exposes it
        mobile_screen = window_width < 750;

        var cursor_x: f64 = 0;
        var cursor_y: f64 = 0;
        glfw.getCursorPos(window, &cursor_x, &cursor_y);

        clay.setPointerState(.{
            .x = @floatCast(cursor_x),
            .y = @floatCast(cursor_y),
        }, glfw.getMouseButton(window, .left) == .press);

        // Clay's scroll update function may require delta time.
        clay.updateScrollContainers(false, .{ .x = mouse_scroll_delta.x, .y = mouse_scroll_delta.y }, delta_time_f32);
        mouse_scroll_delta = .{ .x = 0, .y = 0 }; // Reset after consumption

        clay.setLayoutDimensions(.{ .w = @floatFromInt(window_width), .h = @floatFromInt(window_height) });

        // Generate the UI layout and get rendering commands
        const render_commands = createLayout(if (animation_lerp_value < 0) animation_lerp_value + 1 else 1 - animation_lerp_value);

        // --- WGPU Frame Rendering ---
        var surface_texture: wgpu.SurfaceTexture = undefined;
        wgpu.surfaceGetCurrentTexture(demo.surface, &surface_texture);
        switch (surface_texture.status) {
            .success_optimal, .success_suboptimal => {},
            .timeout, .outdated, .lost => {
                if (surface_texture.texture != null) wgpu.textureRelease(surface_texture.texture);
                glfw.getWindowSize(window, &window_width, &window_height);
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

        const proj = ortho(0, @floatFromInt(demo.config.width), @floatFromInt(demo.config.height), 0, -1, 1);
        demo.renderer.beginFrame(proj, demo.config.width, demo.config.height);

        // Render the UI using the ClayBackend
        try clay_backend.render(render_commands);

        try demo.renderer.endFrame();

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
