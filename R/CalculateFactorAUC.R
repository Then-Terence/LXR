
CalculateFactorAUC <- function(Target, Prediction){

  DT <- data.table(Target = Target, Prediction = Prediction)
  DT <- DT[, .(Event = sum(Target), NonEvent = sum(!Target)),
           by = Prediction]
  DT[, `:=`(Probability = Event / (Event + NonEvent))]

  setorder(DT, -Probability)

  DT[, `:=`(TP = cumsum(Event) / sum(Event),
            FP = cumsum(NonEvent) / sum(NonEvent))]
  DT <- DT[, .(TP, FP)]

  DT <- rbind(data.table(TP = 0, FP = 0), DT)
  DT[, `:=`(FPLead = shift(FP, type = "lead"),
            TPLead = shift(TP, type = "lead"))]
  DT <- DT[1:(nrow(DT) - 1), ]
  DT[, Triangle := 0.5 * (TPLead + TP) * (FPLead - FP)]

  return(DT)
}
