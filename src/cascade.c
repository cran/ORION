#include "cascade.h"




// Tested alle Permutationen einer bestimmten Auswahl
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
                int*    skip
)
{
    int i;
    int j;
    int* sel = selection;
    int* perm = permutation;
    int* stor;
    int cl1;
    int cl2;
    int classifierIndex;
    int check;

    if(*current < *permSize)
    {
        // check current classifier, sensitivity and specificities
        check = 1;
        if(*current >1)
        {
            //print("Start checks \n");
            cl1 = permutation[(*current)-2];
            cl2 = permutation[(*current)-1];
            classifierIndex = cl1*(*numClass)+cl2;
            
            // check sensitivity of classifier
            if(fC[classifierIndex] == 0)
            {
                check = 0;
            }
            
            // check specificity of classifier
            if(sC[classifierIndex+ cl2*(*numMod)] == 0)
            {
                check = 0;
            }
            
            // check other classes
            for(j=0; j < *numClass; j++)
            {
                if(selection[j]==0)
                {
                    if(sC[classifierIndex+ j*(*numMod)] == 0)
                    {
                        check = 0;
                        break;
                    }
                }
            }
                
        
        }
        if(check ==1)
        {
            for(i=0; i< *numClass; i++)
            {
                if(*sel==0){
                    (*sel)++;
                    permutation[*current] = i;
                    (*current)++;
                    cascades(    fC,
                                sC,
                                permutation,
                                selection,
                                numClass,
                                numMod,
                                numSol,
                                permSize,
                                current,
                                solutionID,
                                storage,
                                skip);
                    (*current)--;
                    (*sel)--;
                }
            
                sel++;
                perm++;
            }
        }
    }else{
        if(*solutionID < *numSol)
        {
            stor = &storage[(*solutionID)*(*permSize)];
            for(i=0; i< (*permSize); i++)
            {
                    stor[i]=permutation[i];
            }
            (*solutionID)++;
        }else{
            (*skip)++;
        }
    }
    
}





// Tested alle Permutationen einer bestimmten Länge

void subcascades(double* fC,
               double* sC,
               int*    numClass,
               int*    numMod,
               int*    numSol,
               int*    permSize,
               int*    dropSize,
               int*    current,
               int*    solutionID,
               int*    storage,
               int*    skip
)
{
    int i;
    int j;
    
    int* counter;
    int* vec;
    int* permutation;
    counter     = (int*) calloc ( (*dropSize),sizeof(int));
    vec         = (int*) calloc ( (*numClass),sizeof(int));
    permutation = (int*) calloc ( (*permSize),sizeof(int));
    
    for(i=0; i < *dropSize; i++)
    {
        counter[i] = *dropSize-i-1;
    }
    
    for(i=0; i < *dropSize; i++)
    {
        vec[counter[i]] = 1;
    }
    
    *current = 0;
    for(i=0; i < *permSize; i++)
    {
        permutation[i] = 0;
    }
    
    cascades(    fC,
                sC,
                permutation,
                vec,
                numClass,
                numMod,
                numSol,
                permSize,
                current,
                solutionID,
                storage,
                skip);
    
    for(i=0; i < *dropSize; i++)
    {
        vec[counter[i]] = 0;
    }
  
    while(counter[(*dropSize)-1] < *numClass-i)
    {
        i=-1;
        for(j=0; j < *dropSize; j++)
        {
            if(counter[j] < *numClass-j-1)
            {
                i=j;
                break;
            }
        }
        
        if(i==-1)
        {
            break;
        }else{
            counter[i]++;
            
            for(j=i-1; j >=0; j--)
            {
                counter[j] = counter[j+1]+1;
            }
          
            for(i=0; i < *dropSize; i++)
            {
                vec[counter[i]] = 1;
            }
 
            // Test
            *current = 0;
            for(i=0; i < *permSize; i++)
            {
                permutation[i] = 0;
            }

            
            cascades(    fC,
                        sC,
                        permutation,
                        vec,
                        numClass,
                        numMod,
                        numSol,
                        permSize,
                        current,
                        solutionID,
                        storage,
                        skip);
            
            for(i=0; i < *dropSize; i++)
            {
                vec[counter[i]] = 0;
            }
            
        }
    }
    
    
    free(permutation);
    free(counter);
    free(vec);
}















