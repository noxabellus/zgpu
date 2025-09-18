#include <stdlib.h>

void* (*stbi_MallocPtr)(size_t size) = NULL;
void* (*stbi_ReallocPtr)(void* ptr, size_t size) = NULL;
void (*stbi_FreePtr)(void* ptr) = NULL;

#define STBI_MALLOC(size) stbi_MallocPtr(size)
#define STBI_REALLOC(ptr, size) stbi_ReallocPtr(ptr, size)
#define STBI_FREE(ptr) stbi_FreePtr(ptr)

#define STB_IMAGE_IMPLEMENTATION
#include "stb_image.h"

void* (*stbi_rMallocPtr)(size_t size, void* context) = NULL;
void (*stbi_rFreePtr)(void* ptr, void* context) = NULL;

#define STBIR_MALLOC(size, context) stbi_rMallocPtr(size, context)
#define STBIR_FREE(ptr, context) stbi_rFreePtr(ptr, context)

#define STB_IMAGE_RESIZE_IMPLEMENTATION
#include "stb_image_resize2.h"

void* (*stbi_wMallocPtr)(size_t size) = NULL;
void* (*stbi_wReallocPtr)(void* ptr, size_t size) = NULL;
void (*stbi_wFreePtr)(void* ptr) = NULL;

#define STBIW_MALLOC(size) stbi_wMallocPtr(size)
#define STBIW_REALLOC(ptr, size) stbi_wReallocPtr(ptr, size)
#define STBIW_FREE(ptr) stbi_wFreePtr(ptr)

#define STB_IMAGE_WRITE_IMPLEMENTATION
#include "stb_image_write.h"