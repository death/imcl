# Taken from
# https://gist.github.com/Seranth/f172c479fd643bafebfa3ce2a9d65153
#
#  Try to find Embeddable-Common Lisp
#
#  ECL_FOUND - System has ECL
#  ECL_INCLUDE_DIRS - The ECL include directories
#  ECL_LIBRARIES - The libraries needed to use ECL
#  ECL_DEFINITIONS - Compiler switches required for using ECL

set(ECL_DEFINITIONS ${PC_LIBXML_CFLAGS_OTHER})

find_path(ECL_INCLUDE_DIR ecl/ecl.h
          HINTS ${PC_LIBXML_INCLUDEDIR} ${PC_LIBXML_INCLUDE_DIRS}
          PATH_SUFFIXES libecl )

find_library(ECL_LIBRARY NAMES ecl
             HINTS ${PC_ECL_LIBDIR} ${PC_ECL_LIBRARY_DIRS} )

include(FindPackageHandleStandardArgs)
# handle the QUIETLY and REQUIRED arguments and set ECL_FOUND to TRUE
# if all listed variables are TRUE
find_package_handle_standard_args(ECL  DEFAULT_MSG
                                  ECL_LIBRARY ECL_INCLUDE_DIR)

mark_as_advanced(ECL_INCLUDE_DIR ECL_LIBRARY )

set(ECL_LIBRARIES ${ECL_LIBRARY} )
set(ECL_INCLUDE_DIRS ${ECL_INCLUDE_DIR} )
