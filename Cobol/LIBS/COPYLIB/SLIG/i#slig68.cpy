
      ***===========================================================***
      *** NOME INC                                     LENGTH=012   ***
      *** I#SLIG68                                                  ***
      ***-----------------------------------------------------------***
      ***                                                           ***
      *** DESCRICAO: SISTEMA DE INDICES DE LIQUIDEZ - SLIG          ***
      ***            SOLICITACAO RELATORIO LIQUIDEZ - VIA SYSIN     ***
      ***===========================================================***
      *
       01  REG-SOLCT-SYSIN.
           05 SLIG68-CCGC-CPF-CLI           PIC 9(09).
           05 FILLER                        PIC X(01).
           05 SLIG68-CIDTFD-PRODT-LIQDZ     PIC 9(02).

      * === CAMPOS DUPLICADOS PARA STRING ===
           05 SLIG68-CCGC-CPF-CLI-ST        PIC X(009).
