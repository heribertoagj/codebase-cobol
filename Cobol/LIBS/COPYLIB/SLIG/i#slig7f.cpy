      ***===========================================================***
      *** NOME INC                                     LENGTH=00069 ***
      *** I#SLIG7F                                                  ***
      ***-----------------------------------------------------------***
      ***                                                           ***
      *** DESCRICAO: IMAGEM DAS TABELAS:                            ***
      ***                               TCOCEN_EMITT_CHEQ           ***
      ***                               TCOCEN_SACDO_TITLO          ***
      ***                                                           ***
      ***           (COPIA DA I#SLIG76                              ***
      ***            INCLUIDO FILIAL E CONTROLE DO CNPJ)            ***
      ***===========================================================***
      *
       01  REG-IMAGEM-COCEN-ES.
           05 SLIG7F-CCGC-CPF-CLI            PIC 9(009) COMP-3.
           05 SLIG7F-CCGC-FILIAL-CLI         PIC 9(005) COMP-3.
           05 SLIG7F-CSEQ-CLASF-COCEN        PIC 9(015) COMP-3.
           05 SLIG7F-CCGC-CONTROLE-CLI       PIC 9(002) COMP-3.
           05 SLIG7F-CINDCD-PSSOA-CLI        PIC X(001).
           05 SLIG7F-CCGC-CPF-SACDO          PIC 9(009) COMP-3.
           05 SLIG7F-CCGC-FILIAL-SACDO       PIC 9(005) COMP-3.
           05 SLIG7F-CCGC-CONTROLE-SACDO     PIC 9(002) COMP-3.
           05 SLIG7F-CINDCD-PSSOA-SACDO      PIC X(001).
           05 SLIG7F-PCOCEN-SIMPLE           PIC 9(003)V99 COMP-3.
           05 SLIG7F-PCOCEN-SIMPLE-NULL      PIC X(001).
           05 SLIG7F-PCOCEN-CAUCN            PIC 9(003)V99 COMP-3.
           05 SLIG7F-PCOCEN-CAUCN-NULL       PIC X(001).
           05 SLIG7F-PCOCEN-DESC             PIC 9(003)V99 COMP-3.
           05 SLIG7F-PCOCEN-DESC-NULL        PIC X(001).
           05 SLIG7F-VABERT-SIMPLE           PIC 9(013)V99 COMP-3.
           05 SLIG7F-VABERT-SIMPLE-NULL      PIC X(001).
           05 SLIG7F-VABERT-CAUCN            PIC 9(013)V99 COMP-3.
           05 SLIG7F-VABERT-CAUCN-NULL       PIC X(001).
           05 SLIG7F-VABERT-DESC             PIC 9(013)V99 COMP-3.
           05 SLIG7F-VABERT-DESC-NULL        PIC X(001).
           05 SLIG7F-CCGC-CPF-CLI-ST         PIC X(009).
           05 SLIG7F-CCGC-FILIAL-CLI-ST      PIC X(005).
           05 SLIG7F-CCGC-CONTROLE-CLI-ST    PIC 9(002) COMP-3.
           05 SLIG7F-CCGC-CPF-SACDO-ST       PIC X(009).
           05 SLIG7F-CCGC-FILIAL-SACDO-ST    PIC X(005).
           05 SLIG7F-CCGC-CONTROLE-SACDO-ST  PIC 9(002) COMP-3.
