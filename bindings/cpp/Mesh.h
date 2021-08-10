#ifndef Mesh_H
#define Mesh_H
#include <stdio.h>
#include <stdint.h>
#include <stddef.h>
#include <stdbool.h>
#include "diplomat_runtime.h"

#ifdef __cplusplus
extern "C" {
#endif

typedef struct Mesh Mesh;

Mesh* Mesh_new();
void Mesh_destroy(Mesh* self);

#ifdef __cplusplus
}
#endif
#endif
