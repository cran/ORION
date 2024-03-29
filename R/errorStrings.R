errorStrings <- function(type)
{
    text <- switch( type,
        'cascade'               = 'Parameter \'cascade\' is required to be a numeric vector of at least two class labels reflected in \'predictionMap\'.',
        'cascade2'              = 'Parameter \'cascade\' is required to be a numeric vector or a character string of type \'1>2>3\' of at least two class labels reflected in \'predictionMap\'.',
        'cascadeMissing'        = '\'cascade\' is missing.',
        'cascades'              = 'Parameter \'cascades\' is required to be a numeric vector or matrix of class labels reflected in \'predictionMap\'.',
        'classifier'            = 'Parameter \'classifier\' is required to be an object of type \'TuneParetoClassifier\'.',
        'classifierMissing'     = '\'classifier\' is missing.',
        'classNames'            = 'number of \'classNames\' does not fit to to overall number of classes for the whole dataset',
        'comparison'            = 'Parameter \'comparison\' is required to be a single character string. Following symbols are allowed: \'>=\', \'>\', \'<=\', \'<\', \'==\', \'!=\'.',
        'conf'                  = '\'conf\' is required to be an object of type \'Conf\'.',
        'confMissing'           = '\'conf\' is missing.',
        'confusionTable'        = '\'confusionTable\' is required to be an object of type \'ConfusionTable\'.',
        'confusionTableMissing' = '\'confusionTable\' is missing.',
        'dataMissing'           = '\'data\' is missing.',
        'data'                  = 'Parameter \'data\' is required to be a matrix or data.frame.',
        'digits'                = 'Parameter \'digits\' is required to be a single numeric value >1.',
        'direction'             = 'Parameter \'direction\' is required to be a single character string. Following symbols are allowed: \'sub\', \'super\', \'exact\'.',
        'foldList'              = 'Parameter \'foldList\' is required to be a list of lists. Each inner list contains a numeric vector of sample-IDs, which lie in the range of 1 and the number of samples .',
        'groupwise'            = 'Parameter \'groupwise\' is required to be an object of type \'Groupwise\'.',
        'labels'                = 'Parameter \'labels\' is required to be a numeric vector providing one class label for each sample (\'nrow(data)\'). Classes are assumed to be enumerated from 0 to n-1, where n is the number of classes.',
        'labelsMissing'         = '\'labels\' is missing.',
        'noSize'                = 'if \'sets\' is defined, parameter \'size\' cannot be set',
        'numSol'                = 'Parameter \'numSol\' is required to be a single numeric value >= 1.',
        'maxCl'                 = 'Parameter \'maxCl\' is required to be a single numeric value >= 1.',
        'neighborhood'          = 'Parameter \'neighborhood\' is required to be a single character string. Following symbols are allowed: \'direct\', \'indirect\'.',
        'parallel'              = 'no parallel backened registered',
        'predictionMap'         = '\'predictionMap\' is required to be an object of type \'PredictionMap\'.',
        'predictionMapMissing'  = '\'predictionMap\' is missing.',
        'ordered'               = 'Parameter \'ordered\' is required to be a single logical value.',
        'other.classes'         = 'Parameter \'other.classes\' is required to be the character string \'all\' or a numeric vector of class labels that are reflected in \'predictionMap\' and distinct from the class labels in \'cascade\'.',
        'sets'                  = 'Parameter \'sets\' is required to be a vector of character strings of type \'1>2>3\', a list of numeric vectors or a numeric vector. Empty vectors of class \'NULL\' are not allowed.',
        'sets.classes'          = 'Parameter \'sets\' is required to be a list of numeric vectors.',
        'sets.cascades'         = 'Parameter \'sets\' is required to be a vector of character strings of the format \'1>2>3\'.',
        'wrong.classes'         = 'Parameter \'sets\' is required to contain only classes that are within the dataset',
        'size.na'                = 'Parameter \'size\' is required to be a numeric vector. It should not contain any NA',
        'size.cl'                = 'Parameter \'size\' is required to be a numeric vector. Each entry is required to be >= 2 and smaller than the maximal number of classes',
        'size2'                 = 'Parameter \'size\' is required to be a single numeric value >= 2.',
        'sort'                  = 'Parameter \'sort\' is required to be a single logical value.',
        'subcascadesMissing'  = '\'subcascades\' is missing.',
        'subcascades'           = '\'subcascades\' must be an object of type \'Subcascades\'.',
        'subcascades1'           = '\'subcascades1\' must be an object of type \'Subcascades\'.',
        'subcascades2'           = '\'subcascades2\' must be an object of type \'Subcascades\'.',
        'test.classes'          = 'Parameter \'test.classes\' is required to be a numeric vector of class labels reflected in \'predictionMap\'.',
        'thresh'                = 'Parameter \'thresh\' is required to be a single numeric value in the interval [0,1].',
        'type'                  = 'Parameter \'type\' is required to be a single character string. Following symbols are allowed: \'all\' and \'any\'.'
        )
        
    return(text)

}


