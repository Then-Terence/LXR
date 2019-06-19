
ArrangeColumn <- function(Name1, Name2, Data){

  Data <- data.table(Data)

  AllNames <- names(Data)
  AllNames <- AllNames[AllNames != Name2]
  Position1 <- which(AllNames == Name1)

  if(Position1 != length(AllNames)){

    AllNames <- c(AllNames[1:Position1], Name2,
                  AllNames[(Position1 + 1):length(AllNames)])

    setcolorder(Data, AllNames)

  }
}
