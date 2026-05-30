      ***===========================================================***
      *** NOME INC                                     LENGTH=00599 ***
      *** I#SLIGB5                                                  ***
      ***-----------------------------------------------------------***
      ***                                                           ***
      *** DESCRICAO: SISTEMA DE INDICES DE LIQUIDEZ - SLIG          ***
      ***            LAY OUT INDICE DE LIQUIDEZ DE CHEQUES PARA O   ***
      ***            SISTEMA DE BASE HISTORICA DE RISCO DE CREDITO  ***
      ***===========================================================***
      *DATA        PROJETO                                 ANALISTA    *
      *01/08/2025  CNPJ ALFANUMERICO                       LUCIANA SATO*
      *----------------------------------------------------------------*
      *
       01  SLIGB5-REGISTRO.
           05 SLIGB5-EMPRESA                  PIC  9(005).
CNPJ7C     05 SLIGB5-CPF-CNPJ                 PIC  X(009).
           05 SLIGB5-TIPO-PESSOA              PIC  X(001).
           05 SLIGB5-DINIC-PER-APUR           PIC  X(010).
           05 SLIGB5-DFNAL-PER-APUR           PIC  X(010).
           05 SLIGB5-CARTEIRA-SIMPLES.
              10 SLIGB5-LIQ-FINAL-CS-X.
                 15 SLIGB5-LIQ-FINAL-CS       PIC  9(003)V99.
              10 SLIGB5-LIQ-PRM-DEP-CS-X.
                 15 SLIGB5-LIQ-PRM-DEP-CS     PIC  9(003)V99.
              10 SLIGB5-LIQ-NA-REAP-CS-X.
                 15 SLIGB5-LIQ-NA-REAP-CS     PIC  9(003)V99.
              10 SLIGB5-LIQ-VCTO-PER-CS-X.
                 15 SLIGB5-LIQ-VCTO-PER-CS    PIC  9(003)V99.
              10 SLIGB5-LIQ-CHQ-DEPOS-CS-X.
                 15 SLIGB5-LIQ-CHQ-DEPOS-CS   PIC  9(003)V99.
              10 SLIGB5-LIQ-BX-ANT-CS-X.
                 15 SLIGB5-LIQ-BX-ANT-CS      PIC  9(003)V99.
              10 SLIGB5-LIQ-CHQ-REAP-CS-X.
                 15 SLIGB5-LIQ-CHQ-REAP-CS    PIC  9(003)V99.
              10 SLIGB5-TOT-CART-CHQ-CS       PIC  9(015)V99.
              10 SLIGB5-TOT-CHQ-VENC-PER-CS   PIC  9(015)V99.
              10 SLIGB5-TOT-DEPOS-PER-CS      PIC  9(015)V99.
              10 SLIGB5-TOT-CHQ-PAGO-CS       PIC  9(015)V99.
              10 SLIGB5-TOT-CHQ-PRM-DEV-CS    PIC  9(015)V99.
              10 SLIGB5-TOT-CHQ-REAP-CS       PIC  9(015)V99.
              10 SLIGB5-TOT-CHQ-REAP-PAG-CS   PIC  9(015)V99.
              10 SLIGB5-TOT-CHQ-REAP-DEV-CS   PIC  9(015)V99.
              10 SLIGB5-TOT-BX-ANT-CS         PIC  9(015)V99.
           05 SLIGB5-CARTEIRA-CAUCIONADA.
              10 SLIGB5-LIQ-FINAL-CC-X.
                 15 SLIGB5-LIQ-FINAL-CC       PIC  9(003)V99.
              10 SLIGB5-LIQ-PRM-DEP-CC-X.
                 15 SLIGB5-LIQ-PRM-DEP-CC     PIC  9(003)V99.
              10 SLIGB5-LIQ-NA-REAP-CC-X.
                 15 SLIGB5-LIQ-NA-REAP-CC     PIC  9(003)V99.
              10 SLIGB5-LIQ-VCTO-PER-CC-X.
                 15 SLIGB5-LIQ-VCTO-PER-CC    PIC  9(003)V99.
              10 SLIGB5-LIQ-CHQ-DEPOS-CC-X.
                 15 SLIGB5-LIQ-CHQ-DEPOS-CC   PIC  9(003)V99.
              10 SLIGB5-LIQ-BX-ANT-CC-X.
                 15 SLIGB5-LIQ-BX-ANT-CC      PIC  9(003)V99.
              10 SLIGB5-LIQ-CHQ-REAP-CC-X.
                 15 SLIGB5-LIQ-CHQ-REAP-CC    PIC  9(003)V99.
              10 SLIGB5-TOT-CART-CHQ-CC       PIC  9(015)V99.
              10 SLIGB5-TOT-CHQ-VENC-PER-CC   PIC  9(015)V99.
              10 SLIGB5-TOT-DEPOS-PER-CC      PIC  9(015)V99.
              10 SLIGB5-TOT-CHQ-PAGO-CC       PIC  9(015)V99.
              10 SLIGB5-TOT-CHQ-PRM-DEV-CC    PIC  9(015)V99.
              10 SLIGB5-TOT-CHQ-REAP-CC       PIC  9(015)V99.
              10 SLIGB5-TOT-CHQ-REAP-PAG-CC   PIC  9(015)V99.
              10 SLIGB5-TOT-CHQ-REAP-DEV-CC   PIC  9(015)V99.
              10 SLIGB5-TOT-BX-ANT-CC         PIC  9(015)V99.
           05 SLIGB5-CARTEIRA-DESCONTO.
              10 SLIGB5-LIQ-FINAL-CD-X.
                 15 SLIGB5-LIQ-FINAL-CD       PIC  9(003)V99.
              10 SLIGB5-LIQ-PRM-DEP-CD-X.
                 15 SLIGB5-LIQ-PRM-DEP-CD     PIC  9(003)V99.
              10 SLIGB5-LIQ-NA-REAP-CD-X.
                 15 SLIGB5-LIQ-NA-REAP-CD     PIC  9(003)V99.
              10 SLIGB5-LIQ-VCTO-PER-CD-X.
                 15 SLIGB5-LIQ-VCTO-PER-CD    PIC  9(003)V99.
              10 SLIGB5-LIQ-CHQ-DEPOS-CD-X.
                 15 SLIGB5-LIQ-CHQ-DEPOS-CD   PIC  9(003)V99.
              10 SLIGB5-LIQ-BX-ANT-CD-X.
                 15 SLIGB5-LIQ-BX-ANT-CD      PIC  9(003)V99.
              10 SLIGB5-LIQ-CHQ-REAP-CD-X.
                 15 SLIGB5-LIQ-CHQ-REAP-CD    PIC  9(003)V99.
              10 SLIGB5-TOT-CART-CHQ-CD       PIC  9(015)V99.
              10 SLIGB5-TOT-CHQ-VENC-PER-CD   PIC  9(015)V99.
              10 SLIGB5-TOT-DEPOS-PER-CD      PIC  9(015)V99.
              10 SLIGB5-TOT-CHQ-PAGO-CD       PIC  9(015)V99.
              10 SLIGB5-TOT-CHQ-PRM-DEV-CD    PIC  9(015)V99.
              10 SLIGB5-TOT-CHQ-REAP-CD       PIC  9(015)V99.
              10 SLIGB5-TOT-CHQ-REAP-PAG-CD   PIC  9(015)V99.
              10 SLIGB5-TOT-CHQ-REAP-DEV-CD   PIC  9(015)V99.
              10 SLIGB5-TOT-BX-ANT-CD         PIC  9(015)V99.
