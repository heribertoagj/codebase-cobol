      ******************************************************************
      * SISTEMA : DCOM - I#DCOMES                                      *
      * TAMANHO : 0350 BYTES                                           *
      * ARQUIVO : BASE PARA EMISSAO DE AVISOS E EXTRATOS COM DADOS     *
      *           DE ENDERECO.                                         *
      ******************************************************************
       01  ES-REGISTRO.
           05 ES-CTPO-CLIENTE              PIC S9(01)V      COMP-3.
           05 ES-CCNPJ-CPF-DEST            PIC S9(09)V      COMP-3.
           05 ES-CFLIAL-CNPJ-DEST          PIC S9(05)V      COMP-3.
           05 ES-CCTRL-CNPJ-DEST           PIC S9(02)V      COMP-3.
           05 ES-DINIC-PERIODO             PIC  X(10).
           05 ES-DFNAL-PERIODO             PIC  X(10).
           05 ES-CTPO-REGISTRO             PIC S9(01)V      COMP-3.
           05 ES-CAG-BCRIA                 PIC S9(05)V      COMP-3.
           05 ES-CCCORR                    PIC S9(13)V      COMP-3.
           05 ES-REGISTRO-ZERO.
              07 ES-ICDENT-EMITENTE        PIC  X(60).
              07 ES-CRAZAO.
                 09 ES-CGRP-CTBIL          PIC S9(05)V      COMP-3.
                 09 ES-SCGRP-CTBIL         PIC S9(03)V      COMP-3.
              07 ES-QTDE-HUM               PIC S9(06)V      COMP-3.
              07 ES-ENDERECO.
                 09 ES-LOGRAD              PIC  X(60).
                 09 ES-NUMERO              PIC  X(07).
                 09 ES-COMPL               PIC  X(30).
                 09 ES-BAIRO               PIC  X(25).
                 09 ES-CIDAD               PIC  X(25).
                 09 ES-UF                  PIC  X(02).
                 09 ES-NUM-CEP             PIC  9(05)       COMP-3.
                 09 ES-SUF-CEP             PIC  9(03)       COMP-3.
                 09 ES-VALID-CEP           PIC  9(01).
              07 ES-NOME-AG                PIC  X(20).
              07 ES-NUM-CEP-AG             PIC  9(05)       COMP-3.
              07 ES-SUF-CEP-AG             PIC  9(03)       COMP-3.
              07 ES-ENDER-AG               PIC  X(25).
              07 ES-CIDAD-AG               PIC  X(20).
              07 ES-UF-AG                  PIC  X(02).
              07 ES-COD-DIST               PIC  9(05)       COMP-3.
              07 ES-NRO-SEQ-REST           PIC  9(09)       COMP-3.
              07 FILLER                    PIC  X(04).

           05 ES-REGISTRO-HUM    REDEFINES ES-REGISTRO-ZERO.
              07 ES-NSEQ-OPER-DESC         PIC S9(09)V      COMP-3.
              07 ES-NM-REDUZ-SUBPROD       PIC  X(10).
              07 ES-NOSSO-NUMERO           PIC S9(18)V      COMP-3.
              07 ES-SEU-NUMERO             PIC  X(15).
              07 ES-ISACDO-PROP            PIC  X(60).
              07 ES-DTVCTO                 PIC  X(10).
              07 ES-DTPGTO                 PIC  X(10).
              07 ES-VALOR                  PIC S9(15)V9(02) COMP-3.
              07 ES-VLR-OSCILACAO          PIC S9(15)V9(02) COMP-3.
              07 ES-CBCO-COBR              PIC S9(03)V      COMP-3.
              07 ES-CAG-COBR               PIC S9(05)V      COMP-3.
              07 ES-COD-LANCAMENTO         PIC S9(03)V      COMP-3.
              07 FILLER-1                  PIC  X(73).
              07 ES-NUM-CEP-HUM            PIC  9(05)       COMP-3.
              07 ES-SUF-CEP-HUM            PIC  9(03)       COMP-3.
              07 FILLER-2                  PIC  X(85).

           05 ES-REGISTRO-DOIS   REDEFINES ES-REGISTRO-ZERO.
              07 ES-DS-REDUZ-SUBPROD       PIC  X(10).
              07 ES-TTAL-ENT-SUBPROD       PIC S9(15)V99    COMP-3.
              07 ES-TTAL-ENC-SUBPROD       PIC S9(15)V99    COMP-3.
              07 ES-TTAL-BXA-SUBPROD       PIC S9(15)V99    COMP-3.
              07 ES-TTAL-IOF-SUBPROD       PIC S9(15)V99    COMP-3.
              07 ES-SLDO-ATU-SUBPROD       PIC S9(15)V99    COMP-3.
              07 FILLER-1                  PIC X(163).
              07 ES-NUM-CEP-DOIS           PIC  9(05)       COMP-3.
              07 ES-SUF-CEP-DOIS           PIC  9(03)       COMP-3.
              07 FILLER-2                  PIC X(085).

           05 ES-REGISTRO-TRES   REDEFINES ES-REGISTRO-ZERO.
              07 ES-SLDO-ANT-CLIENTE       PIC S9(15)V99    COMP-3.
              07 ES-TTAL-ENT-CLIENTE       PIC S9(15)V99    COMP-3.
              07 ES-TTAL-BXA-CLIENTE       PIC S9(15)V99    COMP-3.
              07 ES-TTAL-ENC-CLIENTE       PIC S9(15)V99    COMP-3.
              07 ES-TTAL-IOF-CLIENTE       PIC S9(15)V99    COMP-3.
              07 ES-SLDO-ATU-CLIENTE       PIC S9(15)V99    COMP-3.
              07 ES-SLDO-VCT-CLIENTE       PIC S9(15)V99    COMP-3.
              07 ES-SLDO-VCT-30D-CLI       PIC S9(15)V99    COMP-3.
              07 ES-SLDO-VCT-60D-CLI       PIC S9(15)V99    COMP-3.
              07 ES-SLDO-VCT-A60-CLI       PIC S9(15)V99    COMP-3.
              07 FILLER-1                  PIC X(128).
              07 ES-NUM-CEP-TRES           PIC  9(05)       COMP-3.
              07 ES-SUF-CEP-TRES           PIC  9(03)       COMP-3.
              07 FILLER-2                  PIC X(085).
      *---------------------------------------------------------------*
      *            IMAGEM AVISO - HEADER                              *
      *---------------------------------------------------------------*

       01  IMAGEM-HEADER-E.
           05  HDR-HEADER-E            PIC X(06).
           05  HDR-DADOS-E.
               10  HDR-LOCAL-IMPR-E    PIC X(04).
               10  HDR-CCUSTO-E        PIC X(04).
               10  HDR-JOBNAME-E       PIC X(08).
               10  HDR-DTMOVTO-E       PIC X(08).
               10  HDR-DTSIST-E        PIC X(08).
               10  HDR-HRSIST-E        PIC X(06).
               10  HDR-VOLUME-E        PIC X(06).
           05  FILLER                  PIC X(300).

