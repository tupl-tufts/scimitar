#lang racket

(provide
  (struct-out solver))

(struct solver (
    getVersion

    Model?
    newModel
    deleteModel

    loadProblem
    dumpProblem
    setColName
    setRowName
    readMps
    writeMps
    addRows
    addColumns

    addCol
    addRow
    addSOS

    optimization_direction?
    log_level?

    getObjSense
    setObjSense
    getRowLower
    setRowLower
    getRowUpper
    setRowUpper
    getObjCoefficients
    getColLower
    setColLower
    getColUpper
    setColUpper
    isInteger
    setContinuous
    setInteger
    setColTypes
    getColTypes
    getNumElements
    getVectorStarts
    getIndices
    getElements
    getObjValue
    printModel

    setPreprocess
    setLogLevel

    status?

    solve

    getNumRows
    getNumCols

    isAbandoned
    isProvenOptimal
    isProvenInfeasible

    getRowActivity
    getColSolution))
