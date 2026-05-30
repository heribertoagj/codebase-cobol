      ***===========================================================***
      *** NOME INC                                     LENGTH=00059 ***
      *** I#SLIG79                                                  ***
      ***-----------------------------------------------------------***
      ***                                                           ***
      *** DESCRICAO: SISTEMA DE INDICES DE LIQUIDEZ - SLIG          ***
      ***            ARQUIVO INTERMEDIARIO CONCENTRACAO DIARIA      ***
      ***                                                           ***
      ***===========================================================***
      *
       01  REG-MAIOR-PCOCEN.
           05 SLIG79-CCGC-CPF-CLI            PIC 9(009) COMP-3.
           05 SLIG79-CINDCD-PSSOA-CLI        PIC X(001).
           05 SLIG79-CCGC-CPF-SACDO          PIC 9(009) COMP-3.
           05 SLIG79-CINDCD-PSSOA-SACDO      PIC X(001).
           05 SLIG79-PCOCEN-MAIOR            PIC 9(003)V99 COMP-3.
           05 SLIG79-PCOCEN-SIMPLE           PIC 9(003)V99 COMP-3.
           05 SLIG79-PCOCEN-CAUCN            PIC 9(003)V99 COMP-3.
           05 SLIG79-PCOCEN-DESC             PIC 9(003)V99 COMP-3.
           05 SLIG79-VABERT-SIMPLE           PIC 9(013)V99 COMP-3.
           05 SLIG79-VABERT-CAUCN            PIC 9(013)V99 COMP-3.
           05 SLIG79-VABERT-DESC             PIC 9(013)V99 COMP-3.

      * === CAMPOS DUPLICADOS PARA STRING ===
           05 SLIG79-CCGC-CPF-CLI-ST         PIC X(009).
           05 SLIG79-CCGC-CPF-SACDO-ST       PIC X(009).
           05 SLIG79-FILLER                  PIC X(14).
