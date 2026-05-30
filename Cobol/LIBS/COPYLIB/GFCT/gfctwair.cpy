      ******************** R O T I N A   G F C T *********************
      *                                                              *
      *    CADASTRO DE CONTAS DA POUPANCA OBTIDO ATRAVES DA          *
      *    TABELA DB2 POUPB005 - DB2PRD.CONTA_POUPANCA               *
      *                                                              *
      *             INC   GFCTWAIR             LRECL=007             *
      ****************************************************************

       01  REG-CADAPOUP.
           03  POUP-AGENCIA               PIC S9(05) COMP-3.
           03  POUP-CONTA                 PIC S9(07) COMP-3.
