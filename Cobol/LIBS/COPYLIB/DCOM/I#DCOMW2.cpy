      ******************************************************************
      * SISTEMA : DCOM - I#DCOMW2                                      *
      * TAMANHO : 0280 BYTES                                           *
      * ARQUIVO : EXTRACAO AVISO (AVISO DEBITO)                        *
      ******************************************************************
       01  W2-REGISTRO.
           05 W2-CTPO-CLIENTE              PIC S9(01)V      COMP-3.
           05 W2-CCNPJ-CPF-DESTINO         PIC S9(09)V      COMP-3.
           05 W2-CFLIAL-CNPJ-DESTINO       PIC S9(05)V      COMP-3.
           05 W2-CCTRL-CNPJ-DESTINO        PIC S9(02)V      COMP-3.
           05 W2-DMOVTC-DESC-COML          PIC  X(10).
           05 W2-ICDENT                    PIC  X(60).
           05 W2-CJUNC-DEPDC-CEDNT         PIC S9(05)V      COMP-3.
           05 W2-NOME-DEPDC                PIC  X(40).
           05 W2-CCCORR-CDENT              PIC S9(13)V      COMP-3.
           05 W2-CGRP-CTBIL-CDENT          PIC S9(05)V      COMP-3.
           05 W2-CSGRP-CTBIL-CDENT         PIC S9(03)V      COMP-3.
           05 W2-DANO-OPER-DESC            PIC S9(04)V      COMP-3.
           05 W2-NSEQ-OPER-DESC            PIC S9(09)V      COMP-3.
           05 W2-ISPROD-DESC-COML          PIC  X(40).
           05 W2-DVCTO                     PIC  X(10).
           05 W2-DEFETV-BAIXA-PCELA        PIC  X(10).
           05 W2-NPCELA-DESC               PIC S9(05)V      COMP-3.
           05 W2-VPRINC-BX-PCELA           PIC S9(15)V9(02) COMP-3.
           05 W2-VENCAR-BX-PCELA           PIC S9(15)V9(02) COMP-3.
           05 W2-CCART-VENCD               PIC  X(05).
           05 W2-CCART-NORML               PIC  X(05).
           05 FILLER                       PIC  X(45).
