      ***===========================================================***
      *** NOME INC                                     LENGTH=00059 ***
      *** I#SLIG7I                                                  ***
      ***-----------------------------------------------------------***
      ***                                                           ***
      *** DESCRICAO: SISTEMA DE INDICES DE LIQUIDEZ - SLIG          ***
      ***            ARQUIVO INTERMEDIARIO CONCENTRACAO DIARIA      ***
      ***                                                           ***
      ***           (COPIA DA I#SLIG79                              ***
      ***            INCLUIDO FILIAL E CONTROLE DO CNPJ)            ***
      ***===========================================================***
      *
       01  REG-MAIOR-PCOCEN.
           05 SLIG7I-CCGC-CPF-CLI            PIC 9(009) COMP-3.
           05 SLIG7I-CCGC-FILIAL-CLI         PIC 9(005) COMP-3.
           05 SLIG7I-CCGC-CONTROLE-CLI       PIC 9(002) COMP-3.
           05 SLIG7I-CINDCD-PSSOA-CLI        PIC X(001).
           05 SLIG7I-CCGC-CPF-SACDO          PIC 9(009) COMP-3.
           05 SLIG7I-CCGC-FILIAL-SACDO       PIC 9(005) COMP-3.
           05 SLIG7I-CCGC-CONTROLE-SACDO     PIC 9(002) COMP-3.
           05 SLIG7I-CINDCD-PSSOA-SACDO      PIC X(001).
           05 SLIG7I-PCOCEN-MAIOR            PIC 9(003)V99 COMP-3.
           05 SLIG7I-PCOCEN-SIMPLE           PIC 9(003)V99 COMP-3.
           05 SLIG7I-PCOCEN-CAUCN            PIC 9(003)V99 COMP-3.
           05 SLIG7I-PCOCEN-DESC             PIC 9(003)V99 COMP-3.
           05 SLIG7I-VABERT-SIMPLE           PIC 9(013)V99 COMP-3.
           05 SLIG7I-VABERT-CAUCN            PIC 9(013)V99 COMP-3.
           05 SLIG7I-VABERT-DESC             PIC 9(013)V99 COMP-3.
           05 SLIG7I-CCGC-CPF-CLI-ST         PIC X(009).
           05 SLIG7I-CCGC-FILIAL-CLI-ST      PIC X(005).
           05 SLIG7I-CCGC-CONTROLE-CLI-ST    PIC 9(002) COMP-3.
           05 SLIG7I-CCGC-CPF-SACDO-ST       PIC X(009).
           05 SLIG7I-CCGC-FILIAL-SACDO-ST    PIC X(005).
           05 SLIG7I-CCGC-CONTROLE-SACDO-ST  PIC 9(002) COMP-3.
           05 SLIG7I-FILLER                  PIC X(01).
