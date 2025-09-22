#define CLAY_IMPLEMENTATION
#include "clay.h"

// Result of Clay_GetCharacterOffset, which returns the pixel offset of a character within a text element.
typedef struct Clay_CharacterOffset {
    // The x,y offset of the character relative to the top-left corner of the text element's content area.
    Clay_Vector2 offset;
    // The height of the line the character is on. Useful for rendering a caret.
    float lineHeight;
    // Indicates whether a text element was found with the provided ID and the index was valid.
    bool found;
} Clay_CharacterOffset;

// Result of Clay_GetCharacterIndexAtOffset, which returns the character index at a given pixel offset within a text element.
typedef struct Clay_CharacterIndexResult {
    // The zero-based index of the character in the original string that is closest to the provided offset.
    uint32_t index;
    // Indicates whether a text element was found and an index could be determined.
    bool found;
} Clay_CharacterIndexResult;

// Calculates the offset of a character within a child text element, relative to the PARENT's top-left corner.
// 
// This is the a method for finding character positions for text created with CLAY_TEXT.
// Wrap your CLAY_TEXT element in a CLAY({}) container with a unique ID, and pass that parent's ID to this function.
// The returned offset correctly accounts for the parent's padding and alignment of the child text element.
// 
// - parentElementId is the ID of the parent container element which must contain exactly one child: the target text element.
// - characterIndex is the zero-based index of the character within the original string.
// - returns a Clay_CharacterOffset struct containing the offset relative to the parent's top-left corner, the line height, and a 'found' flag.
CLAY_DLL_EXPORT Clay_CharacterOffset Clay_GetCharacterOffset(Clay_ElementId elementId, uint32_t characterIndex);

// Calculates the character index within a text element that is closest to a given x,y offset.
// 
// This function is the inverse of Clay_GetCharacterOffset and is essential for mouse interaction,
// such as clicking to position a text caret. It correctly handles word wrapping and text alignment.
// 
// - parentElementId is the ID of the parent container element which must contain exactly one child: the target text element.
// - offset is the x,y offset from the top-left corner of the parent container element's content area.
// - returns a Clay_CharacterIndexResult struct containing the closest character index and a 'found' flag.
CLAY_DLL_EXPORT Clay_CharacterIndexResult Clay_GetCharacterIndexAtOffset(Clay_ElementId parentElementId, Clay_Vector2 offset);

Clay_CharacterOffset Clay__GetCharacterOffsetInChildText(uint32_t elementId, uint32_t characterIndex) {
    Clay_Context* context = Clay_GetCurrentContext();
    Clay_CharacterOffset notFoundResult = {0};

    // 1. Find the element and validate that it's a text element
    Clay_LayoutElementHashMapItem* elementItem = Clay__GetHashMapItem(elementId);
    if (!elementItem || elementItem == &Clay_LayoutElementHashMapItem_DEFAULT) {
        return notFoundResult;
    }
    Clay_LayoutElement* layoutElement = elementItem->layoutElement;
    Clay_ElementConfigUnion textConfigUnion = Clay__FindElementConfigWithType(layoutElement, CLAY__ELEMENT_CONFIG_TYPE_TEXT);
    if (!textConfigUnion.textElementConfig) {
        return notFoundResult;
    }
    Clay__TextElementData* textData = layoutElement->childrenOrTextContent.textElementData;
    Clay_TextElementConfig* textConfig = textConfigUnion.textElementConfig;
    if (characterIndex < 0 || characterIndex > textData->text.length) {
        return notFoundResult;
    }

    // Handle the specific case where the requested index is for a caret positioned
    // immediately after a trailing newline character.
    if (characterIndex == textData->text.length &&
        textData->text.length > 0 &&
        textData->text.chars[textData->text.length - 1] == '\n')
    {
        float lineHeight = textConfig->lineHeight > 0 ? (float)textConfig->lineHeight : textData->preferredDimensions.height;
        float xOffset = 0.0f; // Caret is at the start of the new line.
        float yOffset = (float)textData->wrappedLines.length * lineHeight; // Position it on the line *after* the last visible one.

        // Apply alignment for the new, empty line.
        float parentContentWidth = layoutElement->dimensions.width - (layoutElement->layoutConfig->padding.left + layoutElement->layoutConfig->padding.right);
        if (textConfig->textAlignment == CLAY_TEXT_ALIGN_CENTER) {
            // On a new line, the content width is 0, so center is half the parent's content width.
            xOffset += parentContentWidth / 2.0f;
        } else if (textConfig->textAlignment == CLAY_TEXT_ALIGN_RIGHT) {
            // On a new line, right-align places the caret at the far right.
            xOffset += parentContentWidth;
        }

        return (Clay_CharacterOffset){ .offset = { xOffset, yOffset }, .lineHeight = lineHeight, .found = true };
    }

    // Find the correct line using the "true" length of each line segment, accounting for consumed characters.
    int32_t cumulativeChars = 0;
    Clay__WrappedTextLine* targetLine = NULL;
    int32_t targetLineIndex = -1;

    for (int32_t i = 0; i < textData->wrappedLines.length; ++i) {
        Clay__WrappedTextLine* currentLine = Clay__WrappedTextLineArraySlice_Get(&textData->wrappedLines, i);

        int32_t trueLineSegmentLength;
        if (i < textData->wrappedLines.length - 1) {
            Clay__WrappedTextLine* nextLine = Clay__WrappedTextLineArraySlice_Get(&textData->wrappedLines, i + 1);
            trueLineSegmentLength = (int32_t)(nextLine->line.chars - currentLine->line.chars);
        } else {
            const char* textEnd = textData->text.chars + textData->text.length;
            trueLineSegmentLength = (int32_t)(textEnd - currentLine->line.chars);
        }

        if ((characterIndex == textData->text.length && i == textData->wrappedLines.length - 1) ||
            (characterIndex < cumulativeChars + trueLineSegmentLength)) {
            targetLine = currentLine;
            targetLineIndex = i;
            break;
        }

        cumulativeChars += trueLineSegmentLength;
    }

    if (!targetLine) {
        return notFoundResult;
    }

    // 3. Calculate the X-Offset using the word cache
    float xOffset = 0.0f;
    int32_t indexInLine = characterIndex - cumulativeChars;

    Clay__MeasureTextCacheItem* cachedMeasurement = Clay__MeasureTextCached(&textData->text, textConfig);
    int32_t currentWordCacheIndex = cachedMeasurement->measuredWordsStartIndex;

    while(currentWordCacheIndex != -1) {
        Clay__MeasuredWord* word = Clay__MeasuredWordArray_Get(&context->measuredWords, currentWordCacheIndex);
        if (word->startOffset >= (targetLine->line.chars - textData->text.chars)) {
            break;
        }
        currentWordCacheIndex = word->next;
    }

    while(currentWordCacheIndex != -1 && indexInLine > 0) {
        Clay__MeasuredWord* word = Clay__MeasuredWordArray_Get(&context->measuredWords, currentWordCacheIndex);
        if (word->startOffset >= (targetLine->line.chars - textData->text.chars) + targetLine->line.length && word->length > 0) {
             break;
        }

        int32_t lengthToConsider = word->length;
        if (indexInLine >= lengthToConsider) {
            xOffset += word->width;
            indexInLine -= lengthToConsider;
        } else {
            Clay_StringSlice partialWordSlice = { .length = indexInLine, .chars = &textData->text.chars[word->startOffset], .baseChars = textData->text.chars };
            Clay_Dimensions partialDim = Clay__MeasureText(partialWordSlice, textConfig, context->measureTextUserData);
            xOffset += partialDim.width;
            indexInLine = 0;
            break;
        }
        currentWordCacheIndex = word->next;
    }

    // 4. Calculate Y-Offset and apply alignment
    float lineHeight = textConfig->lineHeight > 0 ? (float)textConfig->lineHeight : textData->preferredDimensions.height;
    float yOffset = (float)targetLineIndex * lineHeight;
    float alignmentOffset = 0.0f;
    float parentContentWidth = layoutElement->dimensions.width - (layoutElement->layoutConfig->padding.left + layoutElement->layoutConfig->padding.right);
    if (textConfig->textAlignment == CLAY_TEXT_ALIGN_CENTER) {
        alignmentOffset = (parentContentWidth - targetLine->dimensions.width) / 2.0f;
    } else if (textConfig->textAlignment == CLAY_TEXT_ALIGN_RIGHT) {
        alignmentOffset = parentContentWidth - targetLine->dimensions.width;
    }
    xOffset += alignmentOffset;

    // 5. Return the final result
    return (Clay_CharacterOffset){ .offset = { xOffset, yOffset }, .lineHeight = lineHeight, .found = true };
}

Clay_CharacterIndexResult Clay__GetCharacterIndexAtOffsetInChildText(Clay_ElementId elementId, Clay_Vector2 offset) {
    Clay_Context* context = Clay_GetCurrentContext();
    Clay_CharacterIndexResult notFoundResult = { -1, false };

    // 1. Find the element and validate it's a text element
    Clay_LayoutElementHashMapItem* elementItem = Clay__GetHashMapItem(elementId.id);
    if (!elementItem || elementItem == &Clay_LayoutElementHashMapItem_DEFAULT) return notFoundResult;

    Clay_LayoutElement* layoutElement = elementItem->layoutElement;
    Clay_ElementConfigUnion textConfigUnion = Clay__FindElementConfigWithType(layoutElement, CLAY__ELEMENT_CONFIG_TYPE_TEXT);
    if (!textConfigUnion.textElementConfig) return notFoundResult;

    Clay__TextElementData* textData = layoutElement->childrenOrTextContent.textElementData;
    Clay_TextElementConfig* textConfig = textConfigUnion.textElementConfig;

    // 2. Determine the target line from the Y-Offset
    float lineHeight = textConfig->lineHeight > 0 ? (float)textConfig->lineHeight : textData->preferredDimensions.height;
    if (lineHeight <= 0) return notFoundResult;

    // First, handle clicks that are clearly below all rendered content.
    float totalContentHeight = (float)textData->wrappedLines.length * lineHeight;
    if (offset.y >= totalContentHeight) {
        return (Clay_CharacterIndexResult){ .index = textData->text.length, .found = true };
    }

    int32_t targetLineIndex = (int32_t)(offset.y / lineHeight);
    targetLineIndex = CLAY__MAX(0, CLAY__MIN(targetLineIndex, textData->wrappedLines.length - 1));
    Clay__WrappedTextLine* targetLine = Clay__WrappedTextLineArraySlice_Get(&textData->wrappedLines, targetLineIndex);

    // 3. Adjust the X-Offset to be relative to the line's start, accounting for alignment
    float parentContentWidth = layoutElement->dimensions.width - (layoutElement->layoutConfig->padding.left + layoutElement->layoutConfig->padding.right);
    float alignmentOffset = 0.0f;
    if (textConfig->textAlignment == CLAY_TEXT_ALIGN_CENTER) {
        alignmentOffset = (parentContentWidth - targetLine->dimensions.width) / 2.0f;
    } else if (textConfig->textAlignment == CLAY_TEXT_ALIGN_RIGHT) {
        alignmentOffset = parentContentWidth - targetLine->dimensions.width;
    }
    float xRelativeToLine = offset.x - alignmentOffset;

    // If the click is horizontally beyond the rendered content of the current line,
    // we snap the index to the end of that line's true segment in the original string.
    if (xRelativeToLine > targetLine->dimensions.width) {
        int32_t finalIndex;

        // For any line that is NOT the last one, the "end" is the newline character.
        // Its index is one less than the index of the start of the next line.
        if (targetLineIndex < textData->wrappedLines.length - 1) {
            Clay__WrappedTextLine* nextLine = Clay__WrappedTextLineArraySlice_Get(&textData->wrappedLines, targetLineIndex + 1);
            int32_t nextLineStartIndex = (int32_t)(nextLine->line.chars - textData->text.chars);
            finalIndex = nextLineStartIndex - 1; // This is the index of the newline character
        } else {
            // This is the last line. If it ends with a newline, and the click is
            // within the bounds of this line, snap to before the newline. Otherwise,
            // the click is effectively "off the end" of the text.
            if (textData->text.length > 0 && textData->text.chars[textData->text.length - 1] == '\n') {
                finalIndex = textData->text.length - 1;
            } else {
                finalIndex = textData->text.length;
            }
        }
        return (Clay_CharacterIndexResult){ .index = finalIndex, .found = true };
    }

    // 4. "Walk" the line word by word to find the target character (for clicks *within* the line)
    int32_t indexInLine = 0;
    float cumulativeWidth = 0.0f;

    Clay__MeasureTextCacheItem* cachedMeasurement = Clay__MeasureTextCached(&textData->text, textConfig);
    int32_t currentWordCacheIndex = cachedMeasurement->measuredWordsStartIndex;

    // Find the first word belonging to our target line
    while (currentWordCacheIndex != -1) {
        Clay__MeasuredWord* word = Clay__MeasuredWordArray_Get(&context->measuredWords, currentWordCacheIndex);
        if (word->startOffset >= (targetLine->line.chars - textData->text.chars)) break;
        currentWordCacheIndex = word->next;
    }

    // Iterate through the words on this line
    while (currentWordCacheIndex != -1) {
        Clay__MeasuredWord* word = Clay__MeasuredWordArray_Get(&context->measuredWords, currentWordCacheIndex);
        if (word->startOffset >= (targetLine->line.chars - textData->text.chars) + targetLine->line.length) break;

        if (xRelativeToLine <= cumulativeWidth + word->width) {
            float wordStartWidth = cumulativeWidth;
            for (int32_t i = 1; i <= word->length; ++i) {
                Clay_StringSlice partialWord = { i, &textData->text.chars[word->startOffset], textData->text.chars };
                Clay_Dimensions partialDims = Clay__MeasureText(partialWord, textConfig, context->measureTextUserData);
                float charWidth = (cumulativeWidth + partialDims.width) - wordStartWidth;
                float charMidpoint = wordStartWidth + charWidth / 2.0f;

                if (xRelativeToLine < charMidpoint) {
                    goto found_index_in_line;
                }
                wordStartWidth = cumulativeWidth + partialDims.width;
                indexInLine++;
            }
            goto found_index_in_line;
        }

        cumulativeWidth += word->width;
        indexInLine += word->length;
        currentWordCacheIndex = word->next;
    }

found_index_in_line:;
    indexInLine = CLAY__MIN(indexInLine, targetLine->line.length);

    // 5. Convert the line-relative index to an index in the original string
    int32_t baseIndex = (int32_t)(targetLine->line.chars - textData->text.chars);
    int32_t finalIndex = baseIndex + indexInLine;

    return (Clay_CharacterIndexResult){ .index = finalIndex, .found = true };
}

CLAY_WASM_EXPORT("Clay_GetCharacterOffset")
Clay_CharacterOffset Clay_GetCharacterOffset(Clay_ElementId parentElementId, uint32_t characterIndex) {
    Clay_Context* context = Clay_GetCurrentContext();
    Clay_CharacterOffset notFoundResult = {0};

    // 1. Find the parent element
    Clay_LayoutElementHashMapItem* parentItem = Clay__GetHashMapItem(parentElementId.id);
    if (!parentItem || parentItem == &Clay_LayoutElementHashMapItem_DEFAULT) {
        return notFoundResult;
    }

    // 2. Validate it has exactly one child
    Clay_LayoutElement* parentLayout = parentItem->layoutElement;
    if (parentLayout->childrenOrTextContent.children.length != 1) {
        return notFoundResult;
    }

    // 3. Get the child element and its generated ID
    int32_t childElementIndex = parentLayout->childrenOrTextContent.children.elements[0];
    Clay_LayoutElement* childLayout = Clay_LayoutElementArray_Get(&context->layoutElements, childElementIndex);
    Clay_ElementId childGeneratedId = { .id = childLayout->id };
    
    // Get the child's hash map item to access its bounding box
    Clay_LayoutElementHashMapItem* childItem = Clay__GetHashMapItem(childGeneratedId.id);
    if (!childItem || childItem == &Clay_LayoutElementHashMapItem_DEFAULT) {
        return notFoundResult;
    }

    // 4. Get the character's offset relative to the CHILD's content area
    Clay_CharacterOffset charOffsetResult = Clay__GetCharacterOffsetInChildText(childGeneratedId.id, characterIndex);
    if (!charOffsetResult.found) {
        return notFoundResult;
    }

    // 5. Calculate the child's offset relative to the parent's top-left corner.
    // The boundingBox contains the absolute screen coordinates, so subtracting them gives the relative offset.
    float childRelativeX = childItem->boundingBox.x - parentItem->boundingBox.x;
    float childRelativeY = childItem->boundingBox.y - parentItem->boundingBox.y;
    
    // The character offset is relative to the child's CONTENT area, which is already offset from its
    // bounding box by its own padding. We need to add that padding back in.
    childRelativeX += childLayout->layoutConfig->padding.left;
    childRelativeY += childLayout->layoutConfig->padding.top;

    // Add the child's relative offset to the character's offset to get the final
    // offset relative to the PARENT's top-left corner.
    charOffsetResult.offset.x += childRelativeX;
    charOffsetResult.offset.y += childRelativeY;

    return charOffsetResult;
}

CLAY_WASM_EXPORT("Clay_GetCharacterIndexAtOffset")
Clay_CharacterIndexResult Clay_GetCharacterIndexAtOffset(Clay_ElementId parentElementId, Clay_Vector2 offsetInParent) {
    Clay_Context* context = Clay_GetCurrentContext();
    Clay_CharacterIndexResult notFoundResult = { -1, false };

    // Find parent and child items
    Clay_LayoutElementHashMapItem* parentItem = Clay__GetHashMapItem(parentElementId.id);
    if (!parentItem || parentItem == &Clay_LayoutElementHashMapItem_DEFAULT) return notFoundResult;
    if (parentItem->layoutElement->childrenOrTextContent.children.length != 1) return notFoundResult;
    
    int32_t childIdx = parentItem->layoutElement->childrenOrTextContent.children.elements[0];
    Clay_LayoutElement* childLayout = Clay_LayoutElementArray_Get(&context->layoutElements, childIdx);
    Clay_LayoutElementHashMapItem* childItem = Clay__GetHashMapItem(childLayout->id);
    if (!childItem || childItem == &Clay_LayoutElementHashMapItem_DEFAULT) return notFoundResult;

    // Transform the parent-relative offset to a child-relative offset
    float childRelativeX = childItem->boundingBox.x - parentItem->boundingBox.x;
    float childRelativeY = childItem->boundingBox.y - parentItem->boundingBox.y;

    Clay_Vector2 offsetInChild = {
        .x = offsetInParent.x - childRelativeX,
        .y = offsetInParent.y - childRelativeY
    };

    // Call the core function with the transformed coordinates
    return Clay__GetCharacterIndexAtOffsetInChildText(CLAY__INIT(Clay_ElementId){ .id = childLayout->id }, offsetInChild);
}