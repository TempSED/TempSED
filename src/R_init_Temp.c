#ifndef R_R_H
# include <R.h>
#endif

#ifndef R_EXT_DYNLOAD_H_
# include <R_ext/Rdynload.h>
#endif

#include <Rinternals.h>
#include <stdlib.h> // for NULL

/* register native routines ------------------------------------------------ */


/* NO .C calls */
/* NO .Call calls */
 
/* .Fortran calls */
void F77_NAME(inittemp1d)   (void (* setparms)(int *, double *));
void F77_NAME(forctemp1d)   (void (* setforcs)(int *, double *));
void F77_NAME(modtemp1d)    (int *, double *, double *, double *, double *, int *);
void F77_NAME(backrad)      (double *, double *, double *, double *, double *, 
                             double *, int *,    double *);
void F77_NAME(sensibleheat) (double *, double *, double *, double *, double *, 
                             double *, double *);
void F77_NAME(latentheat)   (double *, double *, double *, double *, double *, double *,
                             double *, double *, double *);
void F77_NAME(airproperties)(double *, double *, double *, double *, double *, double *);
void F77_NAME(heatflux)     (double *, double *, double *, double *, double *, double *, 
                             double *, double *, double *, double *, double *, 
                             int *,    double *, double *, double *, double *);
void F77_NAME(calcbulk)     (int *,    double *, double *, double *, double *, double *,                 
                             double *, double *, double *, double *, double *, double *);
void F77_NAME(calcbulkdensity)(int *,  double *, double *, double *, double *);
void F77_NAME(calcbulktc)    (int *,   double *, double *, double *, double *);
void F77_NAME(calcbulkcp)    (int *,   double *, double *, double *, double *, 
                             double *, double *);
void F77_NAME(calcbulktd)   (int *,    double *, double *, double *, double *, double *,
                             double *, double *, double *);
void F77_NAME(calclhwater)  (int *, double *, double *, double *);
void F77_NAME(calctcwater)  (int *, int *, double *, double *, double *, double*);
void F77_NAME(calccpwater)  (int *, double *, double *, double *);
void F77_NAME(calcrhowater) (int *, double *, double *, double *);
void F77_NAME(calctdwater)  (int *, int *, double *, double *, double *, double *, 
                             double*, double *);

R_FortranMethodDef FEntries[] = {
    {"inittemp1d",     (DL_FUNC) &F77_SUB(inittemp1d),      1},
    {"forctemp1d",     (DL_FUNC) &F77_SUB(forctemp1d),      1},
    {"modtemp1d",      (DL_FUNC) &F77_SUB(modtemp1d),       6},
    {"backrad",        (DL_FUNC) &F77_SUB(backrad),         8},
    {"sensibleheat",   (DL_FUNC) &F77_SUB(sensibleheat),    7},
    {"latentheat",     (DL_FUNC) &F77_SUB(latentheat),      9},
    {"airproperties",  (DL_FUNC) &F77_SUB(airproperties),   6},
    {"heatflux",       (DL_FUNC) &F77_SUB(heatflux),       16},
    {"calcbulk",       (DL_FUNC) &F77_SUB(calcbulk),       12},
    {"calcbulkdensity",(DL_FUNC) &F77_SUB(calcbulkdensity), 5},   
    {"calcbulktc",     (DL_FUNC) &F77_SUB(calcbulktc),      5},
    {"calcbulkcp",     (DL_FUNC) &F77_SUB(calcbulkcp),      7},
    {"calcbulktd",     (DL_FUNC) &F77_SUB(calcbulktd),      9},
    {"calctdwater",    (DL_FUNC) &F77_SUB(calctdwater),     8},
    {"calclhwater",    (DL_FUNC) &F77_SUB(calclhwater),     4},
    {"calccpwater",    (DL_FUNC) &F77_SUB(calccpwater),     4},
    {"calcrhowater",   (DL_FUNC) &F77_SUB(calcrhowater),    4},
    {"calctcwater",    (DL_FUNC) &F77_SUB(calctcwater),     6},
    {NULL, NULL, 0}
};

/* Initialization ---------------------------------------------------------- */
void R_init_TempSED(DllInfo *dll) {

  R_registerRoutines(dll, NULL, NULL, FEntries, NULL);

  // the following line protects against accidentally finding entry points

  R_useDynamicSymbols(dll, FALSE); // disable dynamic searching
}
