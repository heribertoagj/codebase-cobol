      ******************************************************************
      * SISTEMA : DCOM - I#DCOMEX                                      *
      * TAMANHO : 0256 BYTES                                           *
      * ARQUIVO : BASE PARA EMISSAO DE AVISOS E EXTRATOS               *
      ******************************************************************
       01  EX-REGISTRO.
           05 EX-CTPO-CLIENTE              PIC S9(01)V      COMP-3.
           05 EX-CCNPJ-CPF-DEST            PIC S9(09)V      COMP-3.
           05 EX-CFLIAL-CNPJ-DEST          PIC S9(05)V      COMP-3.
           05 EX-CCTRL-CNPJ-DEST           PIC S9(02)V      COMP-3.
           05 EX-DINIC-PERIODO             PIC  X(10).
           05 EX-DFNAL-PERIODO             PIC  X(10).
           05 EX-CTPO-REGISTRO             PIC S9(01)V      COMP-3.
           05 EX-REGISTRO-ZERO.
              07 EX-ICDENT-EMITENTE        PIC  X(60).
              07 EX-CAG-BCRIA              PIC S9(05)V      COMP-3.
              07 EX-CCCORR                 PIC S9(13)V      COMP-3.
              07 EX-CRAZAO.
                 09 EX-CGRP-CTBIL          PIC S9(05)V      COMP-3.
                 09 EX-SCGRP-CTBIL         PIC S9(03)V      COMP-3.
              07 EX-QTDE-HUM               PIC S9(06)V      COMP-3.
              07 EX-QTDE-DOIS              PIC S9(02)V      COMP-3.
              07 FILLER                    PIC X(143).

           05 EX-REGISTRO-HUM    REDEFINES EX-REGISTRO-ZERO.
              07 EX-NSEQ-OPER-DESC         PIC S9(09)V      COMP-3.
              07 EX-NM-REDUZ-SUBPROD       PIC  X(10).
              07 EX-NOSSO-NUMERO           PIC S9(18)V      COMP-3.
              07 EX-SEU-NUMERO             PIC  X(15).
              07 EX-ISACDO-PROP            PIC  X(60).
              07 EX-DTVCTO                 PIC  X(10).
              07 EX-DTPGTO                 PIC  X(10).
              07 EX-VALOR                  PIC S9(15)V9(02) COMP-3.
              07 EX-VLR-OSCILACAO          PIC S9(15)V9(02) COMP-3.
              07 EX-CBCO-COBR              PIC S9(03)V      COMP-3.
              07 EX-CAG-COBR               PIC S9(05)V      COMP-3.
              07 EX-COD-LANCAMENTO         PIC S9(03)V      COMP-3.
              07 FILLER                    PIC X(079).

           05 EX-REGISTRO-DOIS   REDEFINES EX-REGISTRO-ZERO.
              07 EX-DS-REDUZ-SUBPROD       PIC  X(10).
              07 EX-TTAL-ENT-SUBPROD       PIC S9(15)V99    COMP-3.
              07 EX-TTAL-ENC-SUBPROD       PIC S9(15)V99    COMP-3.
              07 EX-TTAL-BXA-SUBPROD       PIC S9(15)V99    COMP-3.
              07 EX-TTAL-IOF-SUBPROD       PIC S9(15)V99    COMP-3.
              07 EX-SLDO-ATU-SUBPROD       PIC S9(15)V99    COMP-3.
              07 FILLER                    PIC X(169).

           05 EX-REGISTRO-TRES   REDEFINES EX-REGISTRO-ZERO.
              07 EX-SLDO-ANT-CLIENTE       PIC S9(15)V99    COMP-3.
              07 EX-TTAL-ENT-CLIENTE       PIC S9(15)V99    COMP-3.
              07 EX-TTAL-BXA-CLIENTE       PIC S9(15)V99    COMP-3.
              07 EX-TTAL-ENC-CLIENTE       PIC S9(15)V99    COMP-3.
              07 EX-TTAL-IOF-CLIENTE       PIC S9(15)V99    COMP-3.
              07 EX-SLDO-ATU-CLIENTE       PIC S9(15)V99    COMP-3.
              07 EX-SLDO-VCT-CLIENTE       PIC S9(15)V99    COMP-3.
              07 EX-SLDO-VCT-30D-CLI       PIC S9(15)V99    COMP-3.
              07 EX-SLDO-VCT-60D-CLI       PIC S9(15)V99    COMP-3.
              07 EX-SLDO-VCT-A60-CLI       PIC S9(15)V99    COMP-3.
              07 FILLER                    PIC X(134).
