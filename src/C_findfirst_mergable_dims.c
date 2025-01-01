#include <R.h>
#include <Rdefines.h>
#include <R_ext/Error.h>



SEXP C_findfirst_mergable_dims(
  SEXP Xbroadcastable, SEXP Ybroadcastable
) {
  int n = Rf_length(Xbroadcastable);
  
  const int *pXbroadcastable = INTEGER_RO(Xbroadcastable);
  const int *pYbroadcastable = INTEGER_RO(Ybroadcastable);
  
  SEXP startend = PROTECT(Rf_allocVector(INTSXP, 2));
  int *pstartend = INTEGER(startend);
  pstartend[0] = 0;
  pstartend[1] = 0;
  int found = 0;
  int idx;
  
  for(int i = 1; i < n; ++i) {
    idx = i + 1;
    if(found == 0 && pXbroadcastable[i] == pXbroadcastable[i-1] && pYbroadcastable[i] == pYbroadcastable[i -1]) {
      pstartend[0] = idx - 1;
      found = 1;
    }
    if((found > 0) && pXbroadcastable[i] == pXbroadcastable[i-1] && pYbroadcastable[i] == pYbroadcastable[i -1]) {
      pstartend[1] = idx;
    }
    if(found > 0) {
      if(pXbroadcastable[i] != pXbroadcastable[i-1] || pYbroadcastable[i] != pYbroadcastable[i -1]) {
        UNPROTECT(1);
        return(startend);
      }
    }
  }
  
  UNPROTECT(1);
  return(startend);
}
