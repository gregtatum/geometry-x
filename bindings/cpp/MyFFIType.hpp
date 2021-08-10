#ifndef MyFFIType_HPP
#define MyFFIType_HPP
#include <stdint.h>
#include <stddef.h>
#include <stdbool.h>
#include <algorithm>
#include <memory>
#include <optional>
#include <variant>
#include "diplomat_runtime.hpp"

namespace capi {
#include "MyFFIType.h"
}

struct MyFFIType;

struct MyFFITypeDeleter {
  void operator()(capi::MyFFIType* l) const noexcept {
    capi::MyFFIType_destroy(l);
  }
};
struct MyFFIType {
 public:
  int32_t a;
  static MyFFIType new_();
};


inline MyFFIType MyFFIType::new_() {
  capi::MyFFIType diplomat_raw_struct_out_value = capi::MyFFIType_new();
  return MyFFIType{ .a = std::move(diplomat_raw_struct_out_value.a) };
}
#endif
