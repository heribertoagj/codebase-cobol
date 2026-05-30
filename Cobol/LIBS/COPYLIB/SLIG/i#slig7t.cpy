      ***===========================================================***
      *** NOME INC                                     LENGTH=00061 ***
      *** I#SLIG7T                                                  ***
      ***-----------------------------------------------------------***
      ***                                                           ***
      *** DESCRICAO: IMAGEM DA TABELA:  TCOCEN_CLI_CHEQ             ***
      ***                                                           ***
      ***===========================================================***
      *
       01  REG-IMAGEM-COCEN-CLI.
           05 SLIG7T-CCGC-CPF-CLI         PIC  9(009)    COMP-3.
           05 SLIG7T-CFLIAL-CGC-CLI       PIC  9(005)    COMP-3.
           05 SLIG7T-CCTRL-CGC-CLI        PIC  9(002)    COMP-3.
           05 SLIG7T-CINDCD-PSSOA-CLI     PIC  X(001).
           05 SLIG7T-ICOPLT-CLI           PIC  X(040).
           05 SLIG7T-ICOPLT-CLI-NULL      PIC  X(001).
           05 SLIG7T-VPEND-CART-CLI       PIC  9(013)V99 COMP-3.
           05 SLIG7T-VPEND-CART-CLI-NULL  PIC  X(001).
      *

      * === CAMPOS DUPLICADOS PARA STRING ===
           05 SLIG7T-CCGC-CPF-CLI-ST      PIC X(009).
           05 SLIG7T-CFLIAL-CGC-CLI-ST    PIC X(005).
           05 SLIG7T-CCTRL-CGC-CLI-ST     PIC 9(002)    COMP-3.
