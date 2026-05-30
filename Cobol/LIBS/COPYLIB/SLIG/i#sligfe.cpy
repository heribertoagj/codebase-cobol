      ***===========================================================***
      *** NOME INC                                     LENGTH=0700  ***
      *** I#SLIGFE                                                  ***
      ***-----------------------------------------------------------***
      **                                                             **
      **  DESCRICAO: SISTEMA DE INDICES DE LIQUIDEZ - SLIG           **
      **             ARQUIVO INDICE DE LIQUIDEZ - SACADO BANCO       **
      **                                                             **
      ***===========================================================***
      *DATA        PROJETO                                 ANALISTA    *
      *01/08/2025  CNPJ ALFANUMERICO                       LUCIANA SATO*
      *----------------------------------------------------------------*
       05 SLIGFE-REGISTRO.
          10 SLIGFE-HEADER.
             15 SLIGFE-H-NOM-ARQ               PIC X(049).
             15 SLIGFE-H-PV-1                  PIC X(001).
             15 SLIGFE-H-DATA-GER              PIC 9(008).
             15 SLIGFE-H-FILLER                PIC X(642).
          10 SLIGFE-DETALHE REDEFINES SLIGFE-HEADER.
             15 SLIGFE-D-DT-GER                PIC 9(008).
             15 FILLER                         PIC X(001).
CNPJ7C       15 SLIGFE-D-CNPJSAC               PIC X(009).
             15 FILLER                         PIC X(001).
CNPJ7C       15 SLIGFE-D-FILSAC                PIC X(004).
             15 FILLER                         PIC X(001).
             15 SLIGFE-D-DIGSAC                PIC 9(002).
             15 FILLER                         PIC X(001).
CNPJ7C       15 SLIGFE-D-CNPJCED               PIC X(009).
             15 FILLER                         PIC X(001).
CNPJ7C       15 SLIGFE-D-FILCED                PIC X(004).
             15 FILLER                         PIC X(001).
             15 SLIGFE-D-DIGCED                PIC 9(002).
             15 FILLER                         PIC X(001).
             15 SLIGFE-D-DT-INI                PIC 9(008).
             15 FILLER                         PIC X(001).
             15 SLIGFE-D-DT-FIM                PIC 9(008).
             15 FILLER                         PIC X(001).
             15 SLIGFE-D-ILS-TIT-DESC          PIC 9(004)V9(007).
             15 FILLER                         PIC X(001).
             15 SLIGFE-D-ILS-TIT-CAUCAO        PIC 9(004)V9(007).
             15 FILLER                         PIC X(001).
             15 SLIGFE-D-ILS-TIT-COBR          PIC 9(004)V9(007).
             15 FILLER                         PIC X(001).
             15 SLIGFE-D-ILH-TIT-COBR-D1       PIC 9(004)V9(007).
             15 FILLER                         PIC X(001).
             15 SLIGFE-D-ILH-TIT-COBR-D2       PIC 9(004)V9(007).
             15 FILLER                         PIC X(001).
             15 SLIGFE-D-ILH-TIT-COBR-D3       PIC 9(004)V9(007).
             15 FILLER                         PIC X(001).
             15 SLIGFE-D-ILH-TIT-CAUCAO-D1     PIC 9(004)V9(007).
             15 FILLER                         PIC X(001).
             15 SLIGFE-D-ILH-TIT-CAUCAO-D2     PIC 9(004)V9(007).
             15 FILLER                         PIC X(001).
             15 SLIGFE-D-ILH-TIT-CAUCAO-D3     PIC 9(004)V9(007).
             15 FILLER                         PIC X(001).
             15 SLIGFE-D-ILH-TIT-DESC-D1       PIC 9(004)V9(007).
             15 FILLER                         PIC X(001).
             15 SLIGFE-D-ILH-TIT-DESC-D2       PIC 9(004)V9(007).
             15 FILLER                         PIC X(001).
             15 SLIGFE-D-ILH-TIT-DESC-D3       PIC 9(004)V9(007).
             15 FILLER                         PIC X(001).
             15 SLIGFE-D-ILS-CHQ-DESC          PIC 9(004)V9(007).
             15 FILLER                         PIC X(001).
             15 SLIGFE-D-ILS-CHQ-CAUCAO        PIC 9(004)V9(007).
             15 FILLER                         PIC X(001).
             15 SLIGFE-D-ILS-CHQ-COBR          PIC 9(004)V9(007).
             15 FILLER                         PIC X(001).
             15 SLIGFE-D-ILH-CHQ-CUST-D1       PIC 9(004)V9(007).
             15 FILLER                         PIC X(001).
             15 SLIGFE-D-ILH-CHQ-CUST-D2       PIC 9(004)V9(007).
             15 FILLER                         PIC X(001).
             15 SLIGFE-D-ILH-CHQ-CUST-D3       PIC 9(004)V9(007).
             15 FILLER                         PIC X(001).
             15 SLIGFE-D-ILH-CHQ-DESC-D1       PIC 9(004)V9(007).
             15 FILLER                         PIC X(001).
             15 SLIGFE-D-ILH-CHQ-DESC-D2       PIC 9(004)V9(007).
             15 FILLER                         PIC X(001).
             15 SLIGFE-D-ILH-CHQ-DESC-D3       PIC 9(004)V9(007).
             15 FILLER                         PIC X(001).
             15 SLIGFE-D-ILH-CHQ-CAUCAO-D1     PIC 9(004)V9(007).
             15 FILLER                         PIC X(001).
             15 SLIGFE-D-ILH-CHQ-CAUCAO-D2     PIC 9(004)V9(007).
             15 FILLER                         PIC X(001).
             15 SLIGFE-D-ILH-CHQ-CAUCAO-D3     PIC 9(004)V9(007).
             15 FILLER                         PIC X(001).
             15 SLIGFE-D-VOP-SAC-DESC          PIC 9(015)V99.
             15 FILLER                         PIC X(001).
             15 SLIGFE-D-VOP-SAC-CAUC          PIC 9(015)V99.
             15 FILLER                         PIC X(001).
             15 SLIGFE-D-VOP-SAC-COBR          PIC 9(015)V99.
             15 FILLER                         PIC X(001).
             15 SLIGFE-D-RIS-SAC-DESC-AVN      PIC 9(015)V99.
             15 FILLER                         PIC X(001).
             15 SLIGFE-D-RIS-SAC-CAUC-AVN      PIC 9(015)V99.
             15 FILLER                         PIC X(001).
             15 SLIGFE-D-RIS-SAC-COBR-AVN      PIC 9(015)V99.
             15 FILLER                         PIC X(001).
             15 SLIGFE-D-RIS-SAC-DESC-VEN      PIC 9(015)V99.
             15 FILLER                         PIC X(001).
             15 SLIGFE-D-RIS-SAC-CAUC-VEN      PIC 9(015)V99.
             15 FILLER                         PIC X(001).
             15 SLIGFE-D-RIS-SAC-COBR-VEN      PIC 9(015)V99.
             15 FILLER                         PIC X(001).
             15 SLIGFE-D-VOP-EMIT-DESC         PIC 9(015)V99.
             15 FILLER                         PIC X(001).
             15 SLIGFE-D-VOP-EMIT-CAUC         PIC 9(015)V99.
             15 FILLER                         PIC X(001).
             15 SLIGFE-D-VOP-EMIT-COBR         PIC 9(015)V99.
             15 FILLER                         PIC X(001).
             15 SLIGFE-D-RIS-EMIT-DESC-AVN     PIC 9(015)V99.
             15 FILLER                         PIC X(001).
             15 SLIGFE-D-RIS-EMIT-CAUC-AVN     PIC 9(015)V99.
             15 FILLER                         PIC X(001).
             15 SLIGFE-D-RIS-EMIT-COBR-AVN     PIC 9(015)V99.
             15 FILLER                         PIC X(001).
             15 SLIGFE-D-RIS-EMIT-DESC-VEN     PIC 9(015)V99.
             15 FILLER                         PIC X(001).
             15 SLIGFE-D-RIS-EMIT-CAUC-VEN     PIC 9(015)V99.
             15 FILLER                         PIC X(001).
             15 SLIGFE-D-RIS-EMIT-COBR-VEN     PIC 9(015)V99.
             15 SLIGFE-D-FILLER                PIC X(026).
          10 SLIGFE-TRAILER REDEFINES SLIGFE-HEADER.
             15 SLIGFE-T-NOM-ARQ               PIC X(049).
             15 SLIGFE-T-PV-1                  PIC X(001).
             15 SLIGFE-T-QTD-REG               PIC 9(015).
             15 SLIGFE-T-FILLER                PIC X(635).
