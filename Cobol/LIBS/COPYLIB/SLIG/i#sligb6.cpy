      ***===========================================================***
      *** NOME INC                                     LENGTH=00563 ***
      *** I#SLIGB6                                                  ***
      ***-----------------------------------------------------------***
      ***                                                           ***
      *** DESCRICAO: SISTEMA DE INDICES DE LIQUIDEZ - SLIG          ***
      ***            LAY OUT INDICE DE LIQUIDEZ DE TITULOS PARA O   ***
      ***            SISTEMA DE BASE HISTORICA DE RISCO DE CREDITO  ***
      ***===========================================================***
      *DATA        PROJETO                                 ANALISTA    *
      *01/08/2025  CNPJ ALFANUMERICO                       LUCIANA SATO*
      *----------------------------------------------------------------*
      *
       01  SLIGB6-REGISTRO.
           05 SLIGB6-EMPRESA                PIC  9(005).
CNPJ7C     05 SLIGB6-CPF-CNPJ               PIC  X(009).
           05 SLIGB6-TIPO-PESSOA            PIC  X(001).
           05 SLIGB6-DINIC-PER-APUR         PIC  X(010).
           05 SLIGB6-DFNAL-PER-APUR         PIC  X(010).
           05 SLIGB6-CARTEIRA-SIMPLES.
            07 SLIGB6-LIQ-TIT-CS-X.
              09 SLIGB6-LIQ-TIT-CS          PIC  9(003)V99.
            07 SLIGB6-LIQ-VENC-CART-CS-X.
              09 SLIGB6-LIQ-VENC-CART-CS    PIC  9(003)V99.
            07 SLIGB6-LIQ-VENC-ANT-CS-X.
              09 SLIGB6-LIQ-VENC-ANT-CS     PIC  9(003)V99.
            07 SLIGB6-LIQ-DE-BX-ANT-CS-X.
              09 SLIGB6-LIQ-DE-BX-ANT-CS    PIC  9(003)V99.
            07 SLIGB6-LIQ-CO-BX-ANT-CS-X.
              09 SLIGB6-LIQ-CO-BX-ANT-CS    PIC  9(003)V99.
            07 SLIGB6-LIQ-PROR-PGTO-CS-X.
              09 SLIGB6-LIQ-PROR-PGTO-CS    PIC  9(003)V99.
            07 SLIGB6-LIQ-ABAT-TIT-CS-X.
              09 SLIGB6-LIQ-ABAT-TIT-CS     PIC  9(003)V99.
            07 SLIGB6-LIQ-PGTO-ANT-CS-X.
              09 SLIGB6-LIQ-PGTO-ANT-CS     PIC  9(003)V99.
            07 SLIGB6-VLR-TOT-CART-CS       PIC  9(015)V99.
            07 SLIGB6-VLR-VCTO-CART-CS      PIC  9(015)V99.
            07 SLIGB6-VLR-VENC-ANT-CS       PIC  9(015)V99.
            07 SLIGB6-VLR-BX-ANT-CS         PIC  9(015)V99.
            07 SLIGB6-VLR-PGTO-CART-CS      PIC  9(015)V99.
            07 SLIGB6-VLR-PROR-CART-CS      PIC  9(015)V99.
            07 SLIGB6-VLR-ABAT-TIT-CS       PIC  9(015)V99.
            07 SLIGB6-VLR-PGTO-ANT-CS       PIC  9(015)V99.
           05 SLIGB6-CARTEIRA-CAUCIONADA.
            07 SLIGB6-LIQ-TIT-CC-X.
              09 SLIGB6-LIQ-TIT-CC          PIC  9(003)V99.
            07 SLIGB6-LIQ-VENC-CART-CC-X.
              09 SLIGB6-LIQ-VENC-CART-CC    PIC  9(003)V99.
            07 SLIGB6-LIQ-VENC-ANT-CC-X.
              09 SLIGB6-LIQ-VENC-ANT-CC     PIC  9(003)V99.
            07 SLIGB6-LIQ-DE-BX-ANT-CC-X.
              09 SLIGB6-LIQ-DE-BX-ANT-CC    PIC  9(003)V99.
            07 SLIGB6-LIQ-CO-BX-ANT-CC-X.
              09 SLIGB6-LIQ-CO-BX-ANT-CC    PIC  9(003)V99.
            07 SLIGB6-LIQ-PROR-PGTO-CC-X.
              09 SLIGB6-LIQ-PROR-PGTO-CC    PIC  9(003)V99.
            07 SLIGB6-LIQ-ABAT-TIT-CC-X.
              09 SLIGB6-LIQ-ABAT-TIT-CC     PIC  9(003)V99.
            07 SLIGB6-LIQ-PGTO-ANT-CC-X.
              09 SLIGB6-LIQ-PGTO-ANT-CC     PIC  9(003)V99.
            07 SLIGB6-VLR-TOT-CART-CC       PIC  9(015)V99.
            07 SLIGB6-VLR-VCTO-CART-CC      PIC  9(015)V99.
            07 SLIGB6-VLR-VENC-ANT-CC       PIC  9(015)V99.
            07 SLIGB6-VLR-BX-ANT-CC         PIC  9(015)V99.
            07 SLIGB6-VLR-PGTO-CART-CC      PIC  9(015)V99.
            07 SLIGB6-VLR-PROR-CART-CC      PIC  9(015)V99.
            07 SLIGB6-VLR-ABAT-TIT-CC       PIC  9(015)V99.
            07 SLIGB6-VLR-PGTO-ANT-CC       PIC  9(015)V99.
           05 SLIGB6-CARTEIRA-DESCONTO.
            07 SLIGB6-LIQ-TIT-CD-X.
              09 SLIGB6-LIQ-TIT-CD          PIC  9(003)V99.
            07 SLIGB6-LIQ-VENC-CART-CD-X.
              09 SLIGB6-LIQ-VENC-CART-CD    PIC  9(003)V99.
            07 SLIGB6-LIQ-VENC-ANT-CD-X.
              09 SLIGB6-LIQ-VENC-ANT-CD     PIC  9(003)V99.
            07 SLIGB6-LIQ-DE-BX-ANT-CD-X.
              09 SLIGB6-LIQ-DE-BX-ANT-CD    PIC  9(003)V99.
            07 SLIGB6-LIQ-CO-BX-ANT-CD-X.
              09 SLIGB6-LIQ-CO-BX-ANT-CD    PIC  9(003)V99.
            07 SLIGB6-LIQ-PROR-PGTO-CD-X.
              09 SLIGB6-LIQ-PROR-PGTO-CD    PIC  9(003)V99.
            07 SLIGB6-LIQ-ABAT-TIT-CD-X.
              09 SLIGB6-LIQ-ABAT-TIT-CD     PIC  9(003)V99.
            07 SLIGB6-LIQ-PGTO-ANT-CD-X.
              09 SLIGB6-LIQ-PGTO-ANT-CD     PIC  9(003)V99.
            07 SLIGB6-VLR-TOT-CART-CD       PIC  9(015)V99.
            07 SLIGB6-VLR-VCTO-CART-CD      PIC  9(015)V99.
            07 SLIGB6-VLR-VENC-ANT-CD       PIC  9(015)V99.
            07 SLIGB6-VLR-BX-ANT-CD         PIC  9(015)V99.
            07 SLIGB6-VLR-PGTO-CART-CD      PIC  9(015)V99.
            07 SLIGB6-VLR-PROR-CART-CD      PIC  9(015)V99.
            07 SLIGB6-VLR-ABAT-TIT-CD       PIC  9(015)V99.
            07 SLIGB6-VLR-PGTO-ANT-CD       PIC  9(015)V99.
