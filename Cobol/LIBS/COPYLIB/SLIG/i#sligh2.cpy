      ***===========================================================***
      *** NOME INC                                     LENGTH=0053  ***
      *** I#SLIGH2                                                  ***
      ***-----------------------------------------------------------***
      ***                                                           ***
      *** DESCRICAO: SISTEMA DE INDICES DE LIQUIDEZ - SLIG          ***
      ***            ARQUIVO CONCENTRACAO SACADO                    ***
      ***            IMAGEM DAS TABELAS DE CONCENTRACAO             ***
      ***            BANCO X SACADO                                 ***
      ***===========================================================***
      *
       01  REG-IMAGEM-COCEN.
           03 SLIGH2-CCGC-CPF-SACDO        PIC 9(009)    COMP-3.
           03 SLIGH2-CINDC-PSSOA-SACDO     PIC X(001).
           03 SLIGH2-CSEQ-CLASF-COCEN      PIC 9(015)    COMP-3.
           03 SLIGH2-PCOCEN-SIMPLE         PIC 9(003)V99 COMP-3.
           03 SLIGH2-PCOCEN-SIMPLE-NULL    PIC X(001).
           03 SLIGH2-PCOCEN-CAUCN          PIC 9(003)V99 COMP-3.
           03 SLIGH2-PCOCEN-CAUCN-NULL     PIC X(001).
           03 SLIGH2-PCOCEN-DESC           PIC 9(003)V99 COMP-3.
           03 SLIGH2-PCOCEN-DESC-NULL      PIC X(001).
           03 SLIGH2-VABERTO-SIMPLE        PIC 9(013)V99 COMP-3.
           03 SLIGH2-VABERTO-SIMPLE-NULL   PIC X(001).
           03 SLIGH2-VABERTO-CAUCN         PIC 9(013)V99 COMP-3.
           03 SLIGH2-VABERTO-CAUCN-NULL    PIC X(001).
           03 SLIGH2-VABERTO-DESC          PIC 9(013)V99 COMP-3.
           03 SLIGH2-VABERTO-DESC-NULL     PIC X(001).
           03 SLIGH2-CCGC-CPF-SACDO-ST     PIC X(009).
      ***===========================================================***
      *
