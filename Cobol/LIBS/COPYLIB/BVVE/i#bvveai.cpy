      *---------------------------------------------------------------*
      *     MOVIMENTO ACUMULADO                                       *
      *                                                               *
CLS001*     LRECL= 62           INC= I#BVVEAI          ENTRADA/SAIDA  *
      *---------------------------------------------------------------*

       01          REG-MV-ACUMULADO.
           05      MOVACU-CHAVE.
            10     MOVACU-AGENCIA                PIC S9(005)    COMP-3.
            10     MOVACU-CONTA                  PIC S9(007)    COMP-3.
            10     MOVACU-PRODUTO                PIC S9(005)    COMP-3.
           05      MOVACU-PERCENTUAL             PIC S9(003)V99 COMP-3.
           05      MOVACU-VL-NOMINAL             PIC S9(013)V99 COMP-3.
           05      MOVACU-TT-CREDITO             PIC S9(013)V99 COMP-3.
           05      MOVACU-TT-DEBITO              PIC S9(013)V99 COMP-3.
           05      MOVACU-VL-DESPESA             PIC S9(013)V99 COMP-3.
           05      MOVACU-TT-MV-DIA              PIC S9(013)V99 COMP-3.
CLS001     05      MOVACU-TT-MV-GRAN             PIC S9(013)V99 COMP-3.
CLS001     05      MOVACU-TT-MV-GRAN-NN          PIC  X(001).
