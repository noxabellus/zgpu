const std = @import("std");
const log = std.log.scoped(.stbtt);

test {
    log.debug("semantic analysis for stbtt.zig", .{});
    std.testing.refAllDecls(@This());
}

pub const Buffer = extern struct {
    data: [*]u8 = undefined,
    cursor: i32 = 0,
    size: i32 = 0,
};

pub const BakedChar = extern struct {
    x0: u16 = 0,
    y0: u16 = 0,
    x1: u16 = 0,
    y1: u16 = 0,
    xoff: f32 = 0.0,
    yoff: f32 = 0.0,
    xadvance: f32 = 0.0,
};

pub const AlignedQuad = extern struct {
    x0: f32 = 0.0,
    y0: f32 = 0.0,
    s0: f32 = 0.0,
    t0: f32 = 0.0,
    x1: f32 = 0.0,
    y1: f32 = 0.0,
    s1: f32 = 0.0,
    t1: f32 = 0.0,
};

pub const PackedChar = extern struct {
    x0: u16 = 0,
    y0: u16 = 0,
    x1: u16 = 0,
    y1: u16 = 0,
    xoff: f32 = 0.0,
    yoff: f32 = 0.0,
    xadvance: f32 = 0.0,
    xoff2: f32 = 0.0,
    yoff2: f32 = 0.0,
};

pub const PackContext = extern struct {
    user_allocator_context: ?*anyopaque = null,
    pack_info: ?*anyopaque = null,
    width: i32 = 0,
    height: i32 = 0,
    stride_in_bytes: i32 = 0,
    padding: i32 = 0,
    skip_missing: i32 = 0,
    h_oversample: u32 = 0,
    v_oversample: u32 = 0,
    pixels: [*]u8 = undefined,
    nodes: ?*anyopaque = null,
};

pub const FontInfo = extern struct {
    userdata: ?*anyopaque = null,
    data: [*]u8 = undefined,
    fontstart: i32 = 0,
    numGlyphs: i32 = 0,
    loca: i32 = 0,
    head: i32 = 0,
    glyf: i32 = 0,
    hhea: i32 = 0,
    hmtx: i32 = 0,
    kern: i32 = 0,
    gpos: i32 = 0,
    svg: i32 = 0,
    index_map: i32 = 0,
    indexToLocFormat: i32 = 0,
    cff: Buffer = .{},
    charstrings: Buffer = .{},
    gsubrs: Buffer = .{},
    subrs: Buffer = .{},
    fontdicts: Buffer = .{},
    fdselect: Buffer = .{},
};

pub const Rect = extern struct {
    id: i32 = 0,
    w: i32 = 0,
    h: i32 = 0,
    x: i32 = 0,
    y: i32 = 0,
    was_packed: BigBool = .False,
};

pub const PackRange = extern struct {
    font_size: f32 = 0.0,
    first_unicode_codepoint_in_range: i32 = 0,
    array_of_unicode_codepoints: [*]const i32 = undefined,
    num_chars: i32 = 0,
    chardata_for_range: [*]PackedChar = undefined,
    h_oversample: u8 = 0,
    v_oversample: u8 = 0,
};

pub const KerningEntry = extern struct {
    glyph1: i32 = 0,
    glyph2: i32 = 0,
    advance: i32 = 0,
};

pub const vmove: i32 = 1;
pub const vline: i32 = 2;
pub const vcurve: i32 = 3;
pub const vcubic: i32 = 4;

pub const VertexType = i16;

pub const Vertex = extern struct {
    x: VertexType = 0,
    y: VertexType = 0,
    cx: VertexType = 0,
    cy: VertexType = 0,
    cx1: VertexType = 0,
    cy1: VertexType = 0,
    type: u8 = 0,
    padding: u8 = 0,
};

pub const Bitmap = extern struct {
    w: i32 = 0,
    h: i32 = 0,
    stride: i32 = 0,
    pixels: [*]u8 = undefined,
};

pub const PLATFORM_ID_UNICODE: i32 = 0;
pub const PLATFORM_ID_MAC: i32 = 1;
pub const PLATFORM_ID_ISO: i32 = 2;
pub const PLATFORM_ID_MICROSOFT: i32 = 3;

pub const UNICODE_EID_UNICODE_1_0: i32 = 0;
pub const UNICODE_EID_UNICODE_1_1: i32 = 1;
pub const UNICODE_EID_ISO_10646: i32 = 2;
pub const UNICODE_EID_UNICODE_2_0_BMP: i32 = 3;
pub const UNICODE_EID_UNICODE_2_0_FULL: i32 = 4;

pub const MS_EID_SYMBOL: i32 = 0;
pub const MS_EID_UNICODE_BMP: i32 = 1;
pub const MS_EID_SHIFTJIS: i32 = 2;
pub const MS_EID_UNICODE_FULL: i32 = 10;

pub const MAC_EID_ROMAN: i32 = 0;
pub const MAC_EID_ARABIC: i32 = 4;
pub const MAC_EID_JAPANESE: i32 = 1;
pub const MAC_EID_HEBREW: i32 = 5;
pub const MAC_EID_CHINESE_TRAD: i32 = 2;
pub const MAC_EID_GREEK: i32 = 6;
pub const MAC_EID_KOREAN: i32 = 3;
pub const MAC_EID_RUSSIAN: i32 = 7;

pub const MS_LANG_ENGLISH: i32 = 0x0409;
pub const MS_LANG_ITALIAN: i32 = 0x0410;
pub const MS_LANG_CHINESE: i32 = 0x0804;
pub const MS_LANG_JAPANESE: i32 = 0x0411;
pub const MS_LANG_DUTCH: i32 = 0x0413;
pub const MS_LANG_KOREAN: i32 = 0x0412;
pub const MS_LANG_FRENCH: i32 = 0x040c;
pub const MS_LANG_RUSSIAN: i32 = 0x0419;
pub const MS_LANG_GERMAN: i32 = 0x0407;
pub const MS_LANG_SPANISH: i32 = 0x0409; // Note: Same as English in header
pub const MS_LANG_HEBREW: i32 = 0x040d;
pub const MS_LANG_SWEDISH: i32 = 0x041D;

pub const MAC_LANG_ENGLISH: i32 = 0;
pub const MAC_LANG_JAPANESE: i32 = 11;
pub const MAC_LANG_ARABIC: i32 = 12;
pub const MAC_LANG_KOREAN: i32 = 23;
pub const MAC_LANG_DUTCH: i32 = 4;
pub const MAC_LANG_RUSSIAN: i32 = 32;
pub const MAC_LANG_FRENCH: i32 = 1;
pub const MAC_LANG_SPANISH: i32 = 6;
pub const MAC_LANG_GERMAN: i32 = 2;
pub const MAC_LANG_SWEDISH: i32 = 5;
pub const MAC_LANG_HEBREW: i32 = 10;
pub const MAC_LANG_CHINESE_SIMPLIFIED: i32 = 33;
pub const MAC_LANG_ITALIAN: i32 = 3;
pub const MAC_LANG_CHINESE_TRAD: i32 = 19;

pub const MACSTYLE_DONTCARE = @as(i32, 0);
pub const MACSTYLE_BOLD = @as(i32, 1);
pub const MACSTYLE_ITALIC = @as(i32, 2);
pub const MACSTYLE_UNDERSCORE = @as(i32, 4);
pub const MACSTYLE_NONE = @as(i32, 8);

pub const BigBool = enum(i32) {
    False,
    _,

    const True: BigBool = @enumFromInt(1);

    pub fn from(x: bool) BigBool {
        return if (x) .True else .False;
    }

    pub fn to(self: BigBool) bool {
        return switch (self) {
            .False => false,
            else => true,
        };
    }
};

pub const bakeFontBitmap = @extern(*const fn (data: [*]const u8, offset: i32, pixel_height: f32, pixels: [*]u8, pw: i32, ph: i32, first_char: i32, num_chars: i32, chardata: [*]BakedChar) callconv(.c) i32, .{ .name = "stbtt_BakeFontBitmap" });
pub const getBakedQuad = @extern(*const fn (chardata: [*]const BakedChar, pw: i32, ph: i32, char_index: i32, xpos: *f32, ypos: *f32, q: *AlignedQuad, opengl_fillrule: i32) callconv(.c) void, .{ .name = "stbtt_GetBakedQuad" });
pub const getScaledFontVMetrics = @extern(*const fn (fontdata: [*]const u8, index: i32, size: f32, ascent: ?*f32, descent: ?*f32, lineGap: ?*f32) callconv(.c) void, .{ .name = "stbtt_GetScaledFontVMetrics" });
pub const packBegin = @extern(*const fn (spc: *PackContext, pixels: [*]u8, width: i32, height: i32, stride_in_bytes: i32, padding: i32, alloc_context: ?*anyopaque) callconv(.c) i32, .{ .name = "stbtt_PackBegin" });
pub const packEnd = @extern(*const fn (spc: *PackContext) callconv(.c) void, .{ .name = "stbtt_PackEnd" });
pub const packFontRange = @extern(*const fn (spc: *PackContext, fontdata: [*]const u8, font_index: i32, font_size: f32, first_unicode_char_in_range: i32, num_chars_in_range: i32, chardata_for_range: [*]PackedChar) callconv(.c) i32, .{ .name = "stbtt_PackFontRange" });
pub const packFontRanges = @extern(*const fn (spc: *PackContext, fontdata: [*]const u8, font_index: i32, ranges: [*]const PackRange, num_ranges: i32) callconv(.c) i32, .{ .name = "stbtt_PackFontRanges" });
pub const packSetOversampling = @extern(*const fn (spc: *PackContext, h_oversample: u32, v_oversample: u32) callconv(.c) void, .{ .name = "stbtt_PackSetOversampling" });
pub const packSetSkipMissingCodepoints = @extern(*const fn (spc: *PackContext, skip: i32) callconv(.c) void, .{ .name = "stbtt_PackSetSkipMissingCodepoints" });
pub const getPackedQuad = @extern(*const fn (chardata: [*]const PackedChar, pw: i32, ph: i32, char_index: i32, xpos: *f32, ypos: *f32, q: *AlignedQuad, align_to_integer: i32) callconv(.c) void, .{ .name = "stbtt_GetPackedQuad" });
pub const packFontRangesGatherRects = @extern(*const fn (spc: *PackContext, info: *const FontInfo, ranges: [*]PackRange, num_ranges: i32, rects: [*]Rect) callconv(.c) i32, .{ .name = "stbtt_PackFontRangesGatherRects" });
pub const packFontRangesPackRects = @extern(*const fn (spc: *PackContext, rects: [*]Rect, num_rects: i32) callconv(.c) void, .{ .name = "stbtt_PackFontRangesPackRects" });
pub const packFontRangesRenderIntoRects = @extern(*const fn (spc: *PackContext, info: *const FontInfo, ranges: [*]const PackRange, num_ranges: i32, rects: [*]Rect) callconv(.c) i32, .{ .name = "stbtt_PackFontRangesRenderIntoRects" });
pub const getNumberOfFonts = @extern(*const fn (data: [*]const u8) callconv(.c) i32, .{ .name = "stbtt_GetNumberOfFonts" });
pub const getFontOffsetForIndex = @extern(*const fn (data: [*]const u8, index: i32) callconv(.c) i32, .{ .name = "stbtt_GetFontOffsetForIndex" });
pub const initFont = @extern(*const fn (info: *FontInfo, data: [*]const u8, offset: i32) callconv(.c) i32, .{ .name = "stbtt_InitFont" });
pub const findGlyphIndex = @extern(*const fn (info: *const FontInfo, unicode_codepoint: i32) callconv(.c) i32, .{ .name = "stbtt_FindGlyphIndex" });
pub const scaleForPixelHeight = @extern(*const fn (info: *const FontInfo, pixels: f32) callconv(.c) f32, .{ .name = "stbtt_ScaleForPixelHeight" });
pub const scaleForMappingEmToPixels = @extern(*const fn (info: *const FontInfo, pixels: f32) callconv(.c) f32, .{ .name = "stbtt_ScaleForMappingEmToPixels" });
pub const getFontVMetrics = @extern(*const fn (info: *const FontInfo, ascent: ?*i32, descent: ?*i32, lineGap: ?*i32) callconv(.c) void, .{ .name = "stbtt_GetFontVMetrics" });
pub const getFontVMetricsOS2 = @extern(*const fn (info: *const FontInfo, typoAscent: ?*i32, typoDescent: ?*i32, typoLineGap: ?*i32) callconv(.c) i32, .{ .name = "stbtt_GetFontVMetricsOS2" });
pub const getFontBoundingBox = @extern(*const fn (info: *const FontInfo, x0: *i32, y0: *i32, x1: *i32, y1: ?*i32) callconv(.c) void, .{ .name = "stbtt_GetFontBoundingBox" });
pub const getCodepointHMetrics = @extern(*const fn (info: *const FontInfo, codepoint: i32, advanceWidth: ?*i32, leftSideBearing: ?*i32) callconv(.c) void, .{ .name = "stbtt_GetCodepointHMetrics" });
pub const getCodepointKernAdvance = @extern(*const fn (info: *const FontInfo, ch1: i32, ch2: i32) callconv(.c) i32, .{ .name = "stbtt_GetCodepointKernAdvance" });
pub const getCodepointBox = @extern(*const fn (info: *const FontInfo, codepoint: i32, x0: ?*i32, y0: ?*i32, x1: ?*i32, y1: ?*i32) callconv(.c) i32, .{ .name = "stbtt_GetCodepointBox" });
pub const getGlyphHMetrics = @extern(*const fn (info: *const FontInfo, glyph_index: i32, advanceWidth: ?*i32, leftSideBearing: ?*i32) callconv(.c) void, .{ .name = "stbtt_GetGlyphHMetrics" });
pub const getGlyphKernAdvance = @extern(*const fn (info: *const FontInfo, glyph1: i32, glyph2: i32) callconv(.c) i32, .{ .name = "stbtt_GetGlyphKernAdvance" });
pub const getGlyphBox = @extern(*const fn (info: *const FontInfo, glyph_index: i32, x0: ?*i32, y0: ?*i32, x1: ?*i32, y1: ?*i32) callconv(.c) i32, .{ .name = "stbtt_GetGlyphBox" });
pub const getKerningTableLength = @extern(*const fn (info: *const FontInfo) callconv(.c) i32, .{ .name = "stbtt_GetKerningTableLength" });
pub const getKerningTable = @extern(*const fn (info: *const FontInfo, table: [*]KerningEntry, table_length: i32) callconv(.c) i32, .{ .name = "stbtt_GetKerningTable" });
pub const isGlyphEmpty = @extern(*const fn (info: *const FontInfo, glyph_index: i32) callconv(.c) i32, .{ .name = "stbtt_IsGlyphEmpty" });
pub const getCodepointShape = @extern(*const fn (info: *const FontInfo, unicode_codepoint: i32, vertices: *[*]Vertex) callconv(.c) i32, .{ .name = "stbtt_GetCodepointShape" });
pub const getGlyphShape = @extern(*const fn (info: *const FontInfo, glyph_index: i32, vertices: *[*]Vertex) callconv(.c) i32, .{ .name = "stbtt_GetGlyphShape" });
pub const freeShape = @extern(*const fn (info: *const FontInfo, vertices: [*]Vertex) callconv(.c) void, .{ .name = "stbtt_FreeShape" });
pub const findSVGDoc = @extern(*const fn (info: *const FontInfo, gl: i32) callconv(.c) [*]const u8, .{ .name = "stbtt_FindSVGDoc" });
pub const getCodepointSVG = @extern(*const fn (info: *const FontInfo, unicode_codepoint: i32, svg: *[*]const u8) callconv(.c) i32, .{ .name = "stbtt_GetCodepointSVG" });
pub const getGlyphSVG = @extern(*const fn (info: *const FontInfo, gl: i32, svg: *[*]const u8) callconv(.c) i32, .{ .name = "stbtt_GetGlyphSVG" });
pub const freeBitmap = @extern(*const fn (bitmap: [*]u8, userdata: ?*anyopaque) callconv(.c) void, .{ .name = "stbtt_FreeBitmap" });
pub const getCodepointBitmap = @extern(*const fn (info: *const FontInfo, scale_x: f32, scale_y: f32, codepoint: i32, width: ?*i32, height: ?*i32, xoff: ?*i32, yoff: ?*i32) callconv(.c) ?[*]u8, .{ .name = "stbtt_GetCodepointBitmap" });
pub const getCodepointBitmapSubpixel = @extern(*const fn (info: *const FontInfo, scale_x: f32, scale_y: f32, shift_x: f32, shift_y: f32, codepoint: i32, width: ?*i32, height: ?*i32, xoff: ?*i32, yoff: ?*i32) callconv(.c) ?[*]u8, .{ .name = "stbtt_GetCodepointBitmapSubpixel" });
pub const makeCodepointBitmap = @extern(*const fn (info: *const FontInfo, output: [*]u8, out_w: i32, out_h: i32, out_stride: i32, scale_x: f32, scale_y: f32, codepoint: i32) callconv(.c) void, .{ .name = "stbtt_MakeCodepointBitmap" });
pub const makeCodepointBitmapSubpixel = @extern(*const fn (info: *const FontInfo, output: [*]u8, out_w: i32, out_h: i32, out_stride: i32, scale_x: f32, scale_y: f32, shift_x: f32, shift_y: f32, codepoint: i32) callconv(.c) void, .{ .name = "stbtt_MakeCodepointBitmapSubpixel" });
pub const makeCodepointBitmapSubpixelPrefilter = @extern(*const fn (info: *const FontInfo, output: [*]u8, out_w: i32, out_h: i32, out_stride: i32, scale_x: f32, scale_y: f32, shift_x: f32, shift_y: f32, oversample_x: i32, oversample_y: i32, sub_x: ?*f32, sub_y: ?*f32, codepoint: i32) callconv(.c) void, .{ .name = "stbtt_MakeCodepointBitmapSubpixelPrefilter" });
pub const getCodepointBitmapBox = @extern(*const fn (font: *const FontInfo, codepoint: i32, scale_x: f32, scale_y: f32, ix0: ?*i32, iy0: ?*i32, ix1: ?*i32, iy1: ?*i32) callconv(.c) void, .{ .name = "stbtt_GetCodepointBitmapBox" });
pub const getCodepointBitmapBoxSubpixel = @extern(*const fn (font: *const FontInfo, codepoint: i32, scale_x: f32, scale_y: f32, shift_x: f32, shift_y: f32, ix0: ?*i32, iy0: ?*i32, ix1: ?*i32, iy1: ?*i32) callconv(.c) void, .{ .name = "stbtt_GetCodepointBitmapBoxSubpixel" });
pub const getGlyphBitmap = @extern(*const fn (info: *const FontInfo, scale_x: f32, scale_y: f32, glyph: i32, width: ?*i32, height: ?*i32, xoff: ?*i32, yoff: ?*i32) callconv(.c) [*]u8, .{ .name = "stbtt_GetGlyphBitmap" });
pub const getGlyphBitmapSubpixel = @extern(*const fn (info: *const FontInfo, scale_x: f32, scale_y: f32, shift_x: f32, shift_y: f32, glyph: i32, width: ?*i32, height: ?*i32, xoff: ?*i32, yoff: ?*i32) callconv(.c) [*]u8, .{ .name = "stbtt_GetGlyphBitmapSubpixel" });
pub const makeGlyphBitmap = @extern(*const fn (info: *const FontInfo, output: [*]u8, out_w: i32, out_h: i32, out_stride: i32, scale_x: f32, scale_y: f32, glyph: i32) callconv(.c) void, .{ .name = "stbtt_MakeGlyphBitmap" });
pub const makeGlyphBitmapSubpixel = @extern(*const fn (info: *const FontInfo, output: [*]u8, out_w: i32, out_h: i32, out_stride: i32, scale_x: f32, scale_y: f32, shift_x: f32, shift_y: f32, glyph: i32) callconv(.c) void, .{ .name = "stbtt_MakeGlyphBitmapSubpixel" });
pub const makeGlyphBitmapSubpixelPrefilter = @extern(*const fn (info: *const FontInfo, output: [*]u8, out_w: i32, out_h: i32, out_stride: i32, scale_x: f32, scale_y: f32, shift_x: f32, shift_y: f32, oversample_x: i32, oversample_y: i32, sub_x: ?*f32, sub_y: ?*f32, glyph: i32) callconv(.c) void, .{ .name = "stbtt_MakeGlyphBitmapSubpixelPrefilter" });
pub const getGlyphBitmapBox = @extern(*const fn (font: *const FontInfo, glyph: i32, scale_x: f32, scale_y: f32, ix0: ?*i32, iy0: ?*i32, ix1: ?*i32, iy1: ?*i32) callconv(.c) void, .{ .name = "stbtt_GetGlyphBitmapBox" });
pub const getGlyphBitmapBoxSubpixel = @extern(*const fn (font: *const FontInfo, glyph: i32, scale_x: f32, scale_y: f32, shift_x: f32, shift_y: f32, ix0: ?*i32, iy0: ?*i32, ix1: ?*i32, iy1: ?*i32) callconv(.c) void, .{ .name = "stbtt_GetGlyphBitmapBoxSubpixel" });
pub const rasterize = @extern(*const fn (result: *Bitmap, flatness_in_pixels: f32, vertices: [*]const Vertex, num_verts: i32, scale_x: f32, scale_y: f32, shift_x: f32, shift_y: f32, x_off: i32, y_off: i32, invert: i32, userdata: ?*anyopaque) callconv(.c) void, .{ .name = "stbtt_Rasterize" });
pub const freeSDF = @extern(*const fn (bitmap: [*]u8, userdata: ?*anyopaque) callconv(.c) void, .{ .name = "stbtt_FreeSDF" });
pub const getGlyphSDF = @extern(*const fn (info: *const FontInfo, scale: f32, glyph: i32, padding: i32, onedge_value: u8, pixel_dist_scale: f32, width: ?*i32, height: ?*i32, xoff: ?*i32, yoff: ?*i32) callconv(.c) ?[*]u8, .{ .name = "stbtt_GetGlyphSDF" });
pub const getCodepointSDF = @extern(*const fn (info: *const FontInfo, scale: f32, codepoint: i32, padding: i32, onedge_value: u8, pixel_dist_scale: f32, width: ?*i32, height: ?*i32, xoff: ?*i32, yoff: ?*i32) callconv(.c) ?[*]u8, .{ .name = "stbtt_GetCodepointSDF" });
pub const findMatchingFont = @extern(*const fn (fontdata: [*]const u8, name: [*:0]const u8, flags: i32) callconv(.c) i32, .{ .name = "stbtt_FindMatchingFont" });
pub const compareUTF8toUTF16_bigendian = @extern(*const fn (s1: [*]const u8, len1: i32, s2: [*]const u8, len2: i32) callconv(.c) i32, .{ .name = "stbtt_CompareUTF8toUTF16_bigendian" });
pub const getFontNameString = @extern(*const fn (font: *const FontInfo, length: *i32, platformID: i32, encodingID: i32, languageID: i32, nameID: i32) callconv(.c) ?[*]const u8, .{ .name = "stbtt_GetFontNameString" });
