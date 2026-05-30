      ***===========================================================***
      *** NOME INC                                     LENGTH=00059 ***
      *** I#SLIG76                                                  ***
      ***-----------------------------------------------------------***
      ***                                                           ***
      *** DESCRICAO: IMAGEM DAS TABELAS:                            ***
      ***                               TCOCEN_EMITT_CHEQ           ***
      ***                               TCOCEN_SACDO_TITLO          ***
      ***                                                           ***
      ***===========================================================***
      *
       01  REG-IMAGEM-COCEN.
           05 SLIG76-CCGC-CPF-CLI            PIC 9(009) COMP-3.
           05 SLIG76-CINDCD-PSSOA-CLI        PIC X(001).
           05 SLIG76-CSEQ-CLASF-COCEN        PIC 9(015) COMP-3.
           05 SLIG76-CCGC-CPF-SACDO          PIC 9(009) COMP-3.
           05 SLIG76-CINDCD-PSSOA-SACDO      PIC X(001).
           05 SLIG76-PCOCEN-SIMPLE           PIC 9(003)V99 COMP-3.
           05 SLIG76-PCOCEN-SIMPLE-NULL      PIC X(001).
           05 SLIG76-PCOCEN-CAUCN            PIC 9(003)V99 COMP-3.
           05 SLIG76-PCOCEN-CAUCN-NULL       PIC X(001).
           05 SLIG76-PCOCEN-DESC             PIC 9(003)V99 COMP-3.
           05 SLIG76-PCOCEN-DESC-NULL        PIC X(001).
           05 SLIG76-VABERT-SIMPLE           PIC 9(013)V99 COMP-3.
           05 SLIG76-VABERT-SIMPLE-NULL      PIC X(001).
           05 SLIG76-VABERT-CAUCN            PIC 9(013)V99 COMP-3.
           05 SLIG76-VABERT-CAUCN-NULL       PIC X(001).
           05 SLIG76-VABERT-DESC             PIC 9(013)V99 COMP-3.
           05 SLIG76-VABERT-DESC-NULL        PIC X(001).

      * === CAMPOS DUPLICADOS PARA STRING ===
           05 SLIG76-CCGC-CPF-CLI-ST         PIC X(009).
           05 SLIG76-CCGC-CPF-SACDO-ST       PIC X(009).
