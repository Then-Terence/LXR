CalculateAUC <- function(Target, Prediction){

  DT <- data.table(Target = Target, Prediction = Prediction)
  setorder(DT, -Prediction)

  DT <- DT[!is.na(Prediction), ]
  DT[, `:=`(TP = cumsum(Target) / sum(Target),
            FP = cumsum(!Target) / sum(!Target),
            Foo = shift(Prediction, type = "lead"))]
  DT[, Bar := Prediction == Foo]
  DT[nrow(DT), Bar := F]
  DT <- DT[(!Bar), .(TP, FP)]

  DT <- rbind(data.table(TP = 0, FP = 0), DT)
  DT[, `:=`(FPLead = shift(FP, type = "lead"),
            TPLead = shift(TP, type = "lead"))]
  DT <- DT[1:(nrow(DT) - 1), ]
  DT[, Triangle := 0.5 * (TPLead + TP) * (FPLead - FP)]

  return(DT)
}
