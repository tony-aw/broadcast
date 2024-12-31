#include <R.h>
#include <Rdefines.h>
#include <R_ext/Error.h>


SEXP C_check_conf_dim ( SEXP x, SEXP y ) {


    const int *px = INTEGER_RO(x);
    const int *py = INTEGER_RO(y);
    int n = Rf_length(x);
    for(int i = 0; i < n; ++i) {
      if(px[i] != 1 && py[i] != 1) {
        if(px[i] != py[i]) {
           return ScalarLogical(0);
        }
      }
    }
    return ScalarLogical(1);



  warning("your C program does not return anything!");
  return R_NilValue;
}