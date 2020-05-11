#ifndef cascade_H
#define cascade_H

#include<R.h>
#include<Rinternals.h>
#include<math.h>

/*
########################################################################################################################
   Algorithm


   Parameters

    TODO
 */

void enumerateCascades(  int*    permutation,
                         int*    selection,
                         int*    numClass,
                         int*    numMod,
                         int*    numSol,
                         int*    permSize,
                         int*    current,
                         int*    solutionID,
                         int*    storage,
                         int*    skip);

void enumerateSubcascades( 
                 //int*  permutation,
                 //int*    selection,
                 int*    numClass,
                 int*    numMod,
                 int*    numSol,
                 int*    permSize,
                 int*    dropSize,
                 int*    current,
                 int*    solutionID,
                 int*    storage,
                 int*    skip);


void cascades(   double* fC,
                double* sC,
                int*    permutation,
                int*	selection,
                int*	numClass,
                int*    numMod,
                int*    numSol,
                int*    permSize,
                int*    current,
                int*    solutionID,
                int*    storage,
                int*    skip);


void subcascades( double* fC,
                  double* sC,
                  //int*  permutation,
                  //int*	selection,
                  int*	numClass,
                  int*    numMod,
                  int*    numSol,
                  int*    permSize,
                  int*    dropSize,
                  int*    current,
                  int*    solutionID,
                  int*    storage,
                  int*    skip);



#endif

