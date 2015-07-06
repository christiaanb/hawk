module Main where
import Palette

cpu () = (probe "retired" retired)
  where
    predictedPC = cmux (probe "lastPredictedPC" lastPredictedPC) (probe "noBranch" noBranch)
    computed' = delay (probe "computed" computed)
    nextPC' = delay (probe "nextPC" nextPC)
    newInstrs' = delay (probe "newInstrs" newInstrs)
    emptyConst = empty 
    computed = execution (probe "mispredict1" mispredict1) (probe "ready" ready)
    mispredict1@mispredict2@mispredict3 = id (probe "mispredict" mispredict)
    lastPredictedPC = lastPred (probe "annotated1" annotated1)
    lastRetiredPC = lastpc (probe "retired2" retired2)
    nextPC = mux (probe "mispredict2" mispredict2) (probe "lastRetiredPC" lastRetiredPC) (probe "predictedPC" predictedPC)
    newInstrs = mux (probe "mispredict3" mispredict3) (probe "emptyConst" emptyConst) (probe "annotated" annotated)
    (retired@retired1@retired2, ready, available, mispredict) = reorderBuffer (probe "newInstrs'" newInstrs') (probe "computed'" computed')
    annotated@annotated1 = ssBTB (fetched) (probe "retired1" retired1)
    (fetched, noBranch) = symb_Instr (probe "nextPC'" nextPC') (probe "available" available)

main =
  do{ clearProbes_MS
    ; putStr (show $ take 100 $ view $ cpu ())
    }
