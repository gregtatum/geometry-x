#ifndef Mesh_HPP
#define Mesh_HPP
#include <stdint.h>
#include <stddef.h>
#include <stdbool.h>
#include <algorithm>
#include <memory>
#include <optional>
#include <variant>
#include "diplomat_runtime.hpp"

namespace capi {
#include "Mesh.h"
}

class Mesh;

struct MeshDeleter {
  void operator()(capi::Mesh* l) const noexcept {
    capi::Mesh_destroy(l);
  }
};
class Mesh {
 public:
  static Mesh new_();
  inline const capi::Mesh* AsFFI() const { return this->inner.get(); }
  inline capi::Mesh* AsFFIMut() { return this->inner.get(); }
  inline Mesh(capi::Mesh* i) : inner(i) {}
 private:
  std::unique_ptr<capi::Mesh, MeshDeleter> inner;
};


inline Mesh Mesh::new_() {
  return Mesh(capi::Mesh_new());
}
#endif
