#ifndef IMCL_BINDINGS_HPP
#define IMCL_BINDINGS_HPP

#include <ecl/ecl.h>

void imcl_define_bindings();

cl_object imcl_intern_and_export(const char *name);

#endif // IMCL_BINDINGS_HPP
