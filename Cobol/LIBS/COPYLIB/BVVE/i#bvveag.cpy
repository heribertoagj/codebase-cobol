      *---------------------------------------------------------------*
      *     MOVIMENTO DE EXTRATO DO VENDEDOR                          *
      *                                                               *
      *     LRECL= 47           INC= I#BVVEAG          ENTRADA        *
      *---------------------------------------------------------------*

       01  REG-MOVTOCTA.
           02  MOVTO-AGENCIA         PIC 9(05)      COMP-3.
           02  MOVTO-CONTA           PIC 9(07)      COMP-3.
           02  MOVTO-PRODUTO         PIC 9(05)      COMP-3.
           02  MOVTO-IDENT-IDICE     PIC 9(05)      COMP-3.
           02  MOVTO-DT-MOVTO        PIC X(10).
      *       FORMATO DD.MM.AAAA                                      *
           02  MOVTO-CREDITO         PIC S9(13)V99  COMP-3.
           02  MOVTO-ANTECIP         PIC S9(13)V99  COMP-3.
           02  MOVTO-BLOQ            PIC S9(13)V99  COMP-3.
