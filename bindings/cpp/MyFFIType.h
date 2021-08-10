#ifndef MyFFIType_H
#define MyFFIType_H
#include <stdio.h>
#include <stdint.h>
#include <stddef.h>
#include <stdbool.h>
#include "diplomat_runtime.h"

#ifdef __cplusplus
extern "C" {
#endif

typedef struct MyFFIType {
    int32_t a;
} MyFFIType;

MyFFIType MyFFIType_new();
void MyFFIType_destroy(MyFFIType* self);

#ifdef __cplusplus
}
#endif
#endif
