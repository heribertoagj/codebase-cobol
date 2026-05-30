      ***===========================================================***
      *** NOME INC                                     LENGTH=00152 ***
      *** I#SLIGB7                                                  ***
      ***-----------------------------------------------------------***
      ***                                                           ***
      *** DESCRICAO: SISTEMA DE INDICES DE LIQUIDEZ - SLIG          ***
      ***            LAY OUT INDICE DE LIQUIDEZ DE VENDOR PARA O    ***
      ***            SISTEMA DE BASE HISTORICA DE RISCO DE CREDITO  ***
      ***===========================================================***
      *DATA        PROJETO                                 ANALISTA    *
      *01/08/2025  CNPJ ALFANUMERICO                       LUCIANA SATO*
      *----------------------------------------------------------------*
      *
       01  SLIGB7-REGISTRO.
           05 SLIGB7-EMPRESA                PIC  9(005).
CNPJ7C     05 SLIGB7-CPF-CNPJ               PIC  X(009).
           05 SLIGB7-TIPO-PESSOA            PIC  X(001).
           05 SLIGB7-DINIC-PER-APUR         PIC  X(010).
           05 SLIGB7-DFNAL-PER-APUR         PIC  X(010).
           05 SLIGB7-LIQ-TOT-VENDOR-X.
            07 SLIGB7-LIQ-TOT-VENDOR         PIC  9(003)V99.
           05 SLIGB7-LIQ-VEND-COMPR-X.
            07 SLIGB7-LIQ-VEND-COMPR         PIC  9(003)V99.
           05 SLIGB7-LIQ-VEND-FORN-X.
            07 SLIGB7-LIQ-VEND-FORN          PIC  9(003)V99.
           05 SLIGB7-VTOT-CART-VEND           PIC  9(015)V99.
           05 SLIGB7-VLR-VENC-PER             PIC  9(015)V99.
           05 SLIGB7-VLR-PGTO-PER-COMPR       PIC  9(015)V99.
           05 SLIGB7-VLR-PGTO-PER-FORN        PIC  9(015)V99.
           05 SLIGB7-VLR-PGTO-ANT-COMPR       PIC  9(015)V99.
           05 SLIGB7-VLR-PGTO-ANT-FORN        PIC  9(015)V99.
