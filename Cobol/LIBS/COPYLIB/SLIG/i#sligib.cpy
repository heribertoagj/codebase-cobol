      ***===========================================================***
      *** NOME INC                                     LENGTH=00033 ***
      *** I#SLIGIB                                                  ***
      ***-----------------------------------------------------------***
      ***                                                           ***
      *** DESCRICAO: IMAGEM DAS TABELAS:TCOCEN_COMPR_VNDOR          ***
      ***                               TCOCEN_COMPR_VENCD          ***
      ***                               TCOCEN_COMPR_VNCER          ***
      ***                                                           ***
      ***===========================================================***
       01  REG-IMAGEM-COCEN.
           10 SLIGIB-CCGC-CPF-CLI         PIC S9(09)      COMP-3.
           10 SLIGIB-CINDCD-PSSOA-CLI     PIC  X(01).
           10 SLIGIB-CSEQ-CLASF-COCEN     PIC S9(15)      COMP-3.
           10 SLIGIB-CCGC-CPF-SACDO       PIC S9(09)      COMP-3.
           10 SLIGIB-CINDCD-PSSOA-SACDO   PIC  X(01).
           10 SLIGIB-PCOCEN-CART-SIMPLE   PIC S9(03)V99   COMP-3.
           10 SLIGIB-PCOCEN-CART-SIMPLE-N PIC  X(01).
           10 SLIGIB-VPEND-CART-SIMPLE    PIC S9(13)V99   COMP-3.
           10 SLIGIB-VPEND-CART-SIMPLE-N  PIC  X(01).
           10 SLIGIB-CCGC-CPF-CLI-ST      PIC X(009).
           10 SLIGIB-CCGC-CPF-SACDO-ST    PIC X(009).
