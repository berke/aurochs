/* base_types.h
 *
 * Base types
 *
 * Copyright(C) 2007 Exalead SA
 */

#ifndef BASE_TYPES_H
#define BASE_TYPES_H

#ifdef AUROCHS_PLUGGABLE_INCLUDE_DIRECTIVE
#include "aurochs_pluggable_include.h"
#endif

typedef enum { false = 0, true = 1 } bool;

#if 1
#include <sys/types.h>
#include <inttypes.h>

typedef uint8_t u8;
typedef uint16_t u16;
typedef uint32_t u32;
typedef uint64_t u64;

typedef int8_t s8;
typedef int16_t s16;
typedef int32_t s32;
typedef int64_t s64;
#else
typedef unsigned char u8;
typedef unsigned short u16;
typedef unsigned short u32;
typedef unsigned long u64;

typedef signed char s8;
typedef signed short s16;
typedef signed short s32;
typedef signed long s64;
#endif

#endif

#ifndef EXPORT
#define EXPORT
#endif
