      ***===========================================================***
      *** NOME INC                                     LENGTH=0700  ***
      *** I#SLIGF3                                                  ***
      ***-----------------------------------------------------------***
      **                                                             **
      **  DESCRICAO: SISTEMA DE INDICES DE LIQUIDEZ - SLIG           **
      **             ARQUIVO INDICE DE LIQUIDEZ - SACADO BANCO       **
      **                                                             **
      ***===========================================================***
      *DATA        PROJETO                                 ANALISTA    *
      *01/08/2025  CNPJ ALFANUMERICO                       LUCIANA SATO*
      *----------------------------------------------------------------*
       05 SLIGF3-REGISTRO.
          10 SLIGF3-HEADER.
             15 SLIGF3-H-NOM-ARQ               PIC X(040).
             15 SLIGF3-H-PV-1                  PIC X(001).
             15 SLIGF3-H-DATA-GER              PIC 9(008).
             15 SLIGF3-H-FILLER                PIC X(651).
          10 SLIGF3-DETALHE REDEFINES SLIGF3-HEADER.
             15 SLIGF3-D-DT-GER                PIC 9(008).
             15 FILLER                         PIC X(001).
CNPJ7C       15 SLIGF3-D-CNPJCPF               PIC X(009).
             15 FILLER                         PIC X(001).
CNPJ7C       15 SLIGF3-D-FILIAL                PIC X(004).
             15 FILLER                         PIC X(001).
             15 SLIGF3-D-DIGITO                PIC 9(002).
             15 FILLER                         PIC X(001).
             15 SLIGF3-D-DT-INI                PIC 9(008).
             15 FILLER                         PIC X(001).
             15 SLIGF3-D-DT-FIM                PIC 9(008).
             15 FILLER                         PIC X(001).
             15 SLIGF3-D-ILS-TIT-DESC          PIC 9(004)V9(007).
             15 FILLER                         PIC X(001).
             15 SLIGF3-D-ILS-TIT-CAUCAO        PIC 9(004)V9(007).
             15 FILLER                         PIC X(001).
             15 SLIGF3-D-ILS-TIT-COBR          PIC 9(004)V9(007).
             15 FILLER                         PIC X(001).
             15 SLIGF3-D-ILH-TIT-COBR-D1       PIC 9(004)V9(007).
             15 FILLER                         PIC X(001).
             15 SLIGF3-D-ILH-TIT-COBR-D2       PIC 9(004)V9(007).
             15 FILLER                         PIC X(001).
             15 SLIGF3-D-ILH-TIT-COBR-D3       PIC 9(004)V9(007).
             15 FILLER                         PIC X(001).
             15 SLIGF3-D-ILH-TIT-PENHOR-D1     PIC 9(004)V9(007).
             15 FILLER                         PIC X(001).
             15 SLIGF3-D-ILH-TIT-PENHOR-D2     PIC 9(004)V9(007).
             15 FILLER                         PIC X(001).
             15 SLIGF3-D-ILH-TIT-PENHOR-D3     PIC 9(004)V9(007).
             15 FILLER                         PIC X(001).
             15 SLIGF3-D-ILH-TIT-DESC-D1       PIC 9(004)V9(007).
             15 FILLER                         PIC X(001).
             15 SLIGF3-D-ILH-TIT-DESC-D2       PIC 9(004)V9(007).
             15 FILLER                         PIC X(001).
             15 SLIGF3-D-ILH-TIT-DESC-D3       PIC 9(004)V9(007).
             15 FILLER                         PIC X(001).
             15 SLIGF3-D-ILS-CHQ-DESC          PIC 9(004)V9(007).
             15 FILLER                         PIC X(001).
             15 SLIGF3-D-ILS-CHQ-CAUCAO        PIC 9(004)V9(007).
             15 FILLER                         PIC X(001).
             15 SLIGF3-D-ILS-CHQ-COBR          PIC 9(004)V9(007).
             15 FILLER                         PIC X(001).
             15 SLIGF3-D-ILH-CHQ-CUST-D1       PIC 9(004)V9(007).
             15 FILLER                         PIC X(001).
             15 SLIGF3-D-ILH-CHQ-CUST-D2       PIC 9(004)V9(007).
             15 FILLER                         PIC X(001).
             15 SLIGF3-D-ILH-CHQ-CUST-D3       PIC 9(004)V9(007).
             15 FILLER                         PIC X(001).
             15 SLIGF3-D-ILH-CHQ-DESC-D1       PIC 9(004)V9(007).
             15 FILLER                         PIC X(001).
             15 SLIGF3-D-ILH-CHQ-DESC-D2       PIC 9(004)V9(007).
             15 FILLER                         PIC X(001).
             15 SLIGF3-D-ILH-CHQ-DESC-D3       PIC 9(004)V9(007).
             15 FILLER                         PIC X(001).
             15 SLIGF3-D-ILH-CHQ-PENHOR-D1     PIC 9(004)V9(007).
             15 FILLER                         PIC X(001).
             15 SLIGF3-D-ILH-CHQ-PENHOR-D2     PIC 9(004)V9(007).
             15 FILLER                         PIC X(001).
             15 SLIGF3-D-ILH-CHQ-PENHOR-D3     PIC 9(004)V9(007).
             15 FILLER                         PIC X(001).
             15 SLIGF3-D-VOP-SACADO-DESC       PIC 9(015)V99.
             15 FILLER                         PIC X(001).
             15 SLIGF3-D-VOP-SACADO-PENHOR     PIC 9(015)V99.
             15 FILLER                         PIC X(001).
             15 SLIGF3-D-VOP-SACADO-COBR       PIC 9(015)V99.
             15 FILLER                         PIC X(001).
             15 SLIGF3-D-RISCO-SACAVN-DESC     PIC 9(015)V99.
             15 FILLER                         PIC X(001).
             15 SLIGF3-D-RISCO-SACAVN-PENHOR   PIC 9(015)V99.
             15 FILLER                         PIC X(001).
             15 SLIGF3-D-RISCO-SACAVN-COBR     PIC 9(015)V99.
             15 FILLER                         PIC X(001).
             15 SLIGF3-D-RISCO-SACVEN-DESC     PIC 9(015)V99.
             15 FILLER                         PIC X(001).
             15 SLIGF3-D-RISCO-SACVEN-PENHOR   PIC 9(015)V99.
             15 FILLER                         PIC X(001).
             15 SLIGF3-D-RISCO-SACVEN-COBR     PIC 9(015)V99.
             15 FILLER                         PIC X(001).
             15 SLIGF3-D-VOP-EMITENTE-DESC     PIC 9(015)V99.
             15 FILLER                         PIC X(001).
             15 SLIGF3-D-VOP-EMITENTE-PENHOR   PIC 9(015)V99.
             15 FILLER                         PIC X(001).
             15 SLIGF3-D-VOP-EMITENTE-COBR     PIC 9(015)V99.
             15 FILLER                         PIC X(001).
             15 SLIGF3-D-RISCO-EMITEAVN-DESC   PIC 9(015)V99.
             15 FILLER                         PIC X(001).
             15 SLIGF3-D-RISCO-EMITEAVN-PENHOR PIC 9(015)V99.
             15 FILLER                         PIC X(001).
             15 SLIGF3-D-RISCO-EMITEAVN-CUST   PIC 9(015)V99.
             15 FILLER                         PIC X(001).
             15 SLIGF3-D-RISCO-EMITEVEN-DESC   PIC 9(015)V99.
             15 FILLER                         PIC X(001).
             15 SLIGF3-D-RISCO-EMITEVEN-PENHOR PIC 9(015)V99.
             15 FILLER                         PIC X(001).
             15 SLIGF3-D-RISCO-EMITEVEN-CUST   PIC 9(015)V99.
             15 SLIGF3-D-FILLER                PIC X(044).
          10 SLIGF3-TRAILER REDEFINES SLIGF3-HEADER.
             15 SLIGF3-T-NOM-ARQ               PIC X(040).
             15 SLIGF3-T-PV-1                  PIC X(001).
             15 SLIGF3-T-QTD-REG               PIC 9(015).
             15 SLIGF3-T-FILLER                PIC X(644).
