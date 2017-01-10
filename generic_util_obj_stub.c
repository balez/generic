#define CAML_NAME_SPACE
#include <caml/mlvalues.h>
#include <caml/custom.h> /* Custom_ops_val */
#include <caml/alloc.h> /* caml_copy_string */

CAMLprim value caml_custom_identifier (value v) {
  if (Is_block(v) && Tag_val(v) == Custom_tag)
    return caml_copy_string (Custom_ops_val(v)->identifier);
  else return caml_copy_string("");
}
