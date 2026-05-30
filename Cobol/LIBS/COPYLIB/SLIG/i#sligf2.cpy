      ***===========================================================***
      *** NOME INC                                     LENGTH=0400  ***
      *** I#SLIGF2                                                  ***
      ***-----------------------------------------------------------***
      **                                                             **
      **  DESCRICAO: SISTEMA DE INDICES DE LIQUIDEZ - SLIG           **
      **             ARQUIVO INDICE DE LIQUIDEZ - CEDENTE            **
      **                                                             **
      ***===========================================================***
      *DATA        PROJETO                                 ANALISTA    *
      *01/08/2025  CNPJ ALFANUMERICO                       LUCIANA SATO*
      *----------------------------------------------------------------*
       05 SLIGF2-REGISTRO.
          10 SLIGF2-HEADER.
             15 SLIGF2-H-NOM-ARQ               PIC X(040).
             15 SLIGF2-H-PV-1                  PIC X(001).
             15 SLIGF2-H-DATA-GER              PIC X(008).
             15 FILLER                         PIC X(351).
          10 SLIGF2-DETALHE REDEFINES SLIGF2-HEADER.
             15 SLIGF2-D-DT-GER                PIC X(008).
             15 SLIGF2-D-PV-1                  PIC X(001).
CNPJ7C       15 SLIGF2-D-CNPJCPF               PIC X(009).
             15 SLIGF2-D-PV-2                  PIC X(001).
CNPJ7C       15 SLIGF2-D-FILIAL                PIC X(004).
             15 SLIGF2-D-PV-3                  PIC X(001).
             15 SLIGF2-D-DIGITO                PIC 9(002).
             15 SLIGF2-D-PV-4                  PIC X(001).
             15 SLIGF2-D-DT-INI                PIC X(008).
             15 SLIGF2-D-PV-5                  PIC X(001).
             15 SLIGF2-D-DT-FIM                PIC X(008).
             15 SLIGF2-D-PV-6                  PIC X(001).
             15 SLIGF2-D-ILC-TIT-DESC          PIC 9(004)V9(007).
             15 SLIGF2-D-PV-7                  PIC X(001).
             15 SLIGF2-D-ILC-TIT-CAUCAO        PIC 9(004)V9(007).
             15 SLIGF2-D-PV-8                  PIC X(001).
             15 SLIGF2-D-ILC-TIT-COBR          PIC 9(004)V9(007).
             15 SLIGF2-D-PV-9                  PIC X(001).
             15 SLIGF2-D-ILH-TIT-COBR-D1       PIC 9(004)V9(007).
             15 SLIGF2-D-PV-10                 PIC X(001).
             15 SLIGF2-D-ILH-TIT-COBR-D2       PIC 9(004)V9(007).
             15 SLIGF2-D-PV-11                 PIC X(001).
             15 SLIGF2-D-ILH-TIT-COBR-D3       PIC 9(004)V9(007).
             15 SLIGF2-D-PV-12                 PIC X(001).
             15 SLIGF2-D-ILH-TIT-PENHOR-D1     PIC 9(004)V9(007).
             15 SLIGF2-D-PV-13                 PIC X(001).
             15 SLIGF2-D-ILH-TIT-PENHOR-D2     PIC 9(004)V9(007).
             15 SLIGF2-D-PV-14                 PIC X(001).
             15 SLIGF2-D-ILH-TIT-PENHOR-D3     PIC 9(004)V9(007).
             15 SLIGF2-D-PV-15                 PIC X(001).
             15 SLIGF2-D-ILH-TIT-DESC-D1       PIC 9(004)V9(007).
             15 SLIGF2-D-PV-16                 PIC X(001).
             15 SLIGF2-D-ILH-TIT-DESC-D2       PIC 9(004)V9(007).
             15 SLIGF2-D-PV-17                 PIC X(001).
             15 SLIGF2-D-ILH-TIT-DESC-D3       PIC 9(004)V9(007).
             15 SLIGF2-D-PV-18                 PIC X(001).
             15 SLIGF2-D-ILC-CHQ-DESC          PIC 9(004)V9(007).
             15 SLIGF2-D-PV-19                 PIC X(001).
             15 SLIGF2-D-ILC-CHQ-CAUCAO        PIC 9(004)V9(007).
             15 SLIGF2-D-PV-20                 PIC X(001).
             15 SLIGF2-D-ILC-CHQ-COBR          PIC 9(004)V9(007).
             15 SLIGF2-D-PV-21                 PIC X(001).
             15 SLIGF2-D-ILH-CHQ-CUST-D1       PIC 9(004)V9(007).
             15 SLIGF2-D-PV-22                 PIC X(001).
             15 SLIGF2-D-ILH-CHQ-CUST-D2       PIC 9(004)V9(007).
             15 SLIGF2-D-PV-23                 PIC X(001).
             15 SLIGF2-D-ILH-CHQ-CUST-D3       PIC 9(004)V9(007).
             15 SLIGF2-D-PV-24                 PIC X(001).
             15 SLIGF2-D-ILH-CHQ-DESC-D1       PIC 9(004)V9(007).
             15 SLIGF2-D-PV-25                 PIC X(001).
             15 SLIGF2-D-ILH-CHQ-DESC-D2       PIC 9(004)V9(007).
             15 SLIGF2-D-PV-26                 PIC X(001).
             15 SLIGF2-D-ILH-CHQ-DESC-D3       PIC 9(004)V9(007).
             15 SLIGF2-D-PV-27                 PIC X(001).
             15 SLIGF2-D-ILH-CHQ-PENHOR-D1     PIC 9(004)V9(007).
             15 SLIGF2-D-PV-28                 PIC X(001).
             15 SLIGF2-D-ILH-CHQ-PENHOR-D2     PIC 9(004)V9(007).
             15 SLIGF2-D-PV-29                 PIC X(001).
             15 SLIGF2-D-ILH-CHQ-PENHOR-D3     PIC 9(004)V9(007).
             15 FILLER                         PIC X(068).
          10 SLIGF2-TRAILER REDEFINES SLIGF2-HEADER.
             15 SLIGF2-T-NOM-ARQ               PIC X(040).
             15 SLIGF2-T-PV-1                  PIC X(001).
             15 SLIGF2-T-QTD-REG               PIC 9(015).
             15 FILLER                         PIC X(344).
