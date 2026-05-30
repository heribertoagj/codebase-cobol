      ******************************************************************
      * SISTEMA : DCOM - I#DCOMEO  -  COPIA DO BOOK  I#DCOMES          *
      * TAMANHO : 0420 BYTES                                           *
      * ARQUIVO : BASE PARA EMISSAO DE AVISOS E EXTRATOS COM DADOS     *
      *           DE ENDERECO.                                         *
      ******************************************************************
       01  EO-REGISTRO.
           05 EO-CTPO-CLIENTE              PIC S9(01)V      COMP-3.
           05 EO-CCNPJ-CPF-DEST            PIC S9(09)V      COMP-3.
           05 EO-CFLIAL-CNPJ-DEST          PIC S9(05)V      COMP-3.
           05 EO-CCTRL-CNPJ-DEST           PIC S9(02)V      COMP-3.
           05 EO-DINIC-PERIODO             PIC  X(10).
           05 EO-DFNAL-PERIODO             PIC  X(10).
           05 EO-CTPO-REGISTRO             PIC S9(01)V      COMP-3.
           05 EO-CAG-BCRIA                 PIC S9(05)V      COMP-3.
           05 EO-CCCORR                    PIC S9(13)V      COMP-3.
           05 EO-REGISTRO-ZERO.
              07 EO-ICDENT-EMITENTE        PIC  X(60).
              07 EO-CRAZAO.
                 09 EO-CGRP-CTBIL          PIC S9(05)V      COMP-3.
                 09 EO-SCGRP-CTBIL         PIC S9(03)V      COMP-3.
              07 EO-QTDE-HUM               PIC S9(07)V      COMP-3.
              07 EO-ENDERECO.
                 09 EO-LOGRAD              PIC  X(60).
                 09 EO-NUMERO              PIC  X(07).
                 09 EO-COMPL               PIC  X(30).
                 09 EO-BAIRRO              PIC  X(25).
                 09 EO-CIDAD               PIC  X(25).
                 09 EO-UF                  PIC  X(02).
                 09 EO-NUM-CEP             PIC  9(05)       COMP-3.
                 09 EO-SUF-CEP             PIC  9(03)       COMP-3.
                 09 EO-VALID-CEP           PIC  9(01).
              07 EO-NOME-AG                PIC  X(20).
              07 EO-NUM-CEP-AG             PIC  9(05)       COMP-3.
              07 EO-SUF-CEP-AG             PIC  9(03)       COMP-3.
              07 EO-ENDER-AG               PIC  X(25).
              07 EO-CIDAD-AG               PIC  X(20).
              07 EO-UF-AG                  PIC  X(02).
              07 EO-COD-DIST               PIC  9(05)       COMP-3.
              07 EO-NRO-SEQ-REST           PIC  9(09)       COMP-3.
              07 EO-NRO-OBJINI             PIC  9(09)       COMP-3.
              07 EO-NRO-OBJFIM             PIC  9(09)       COMP-3.
              07 EO-QTDE-OBJ               PIC  9(05)       COMP-3.
              07 EO-NRO-AMAR               PIC  9(05)       COMP-3.
              07 EO-NOME-CDD               PIC  X(32).
              07 EO-DT-EMISS               PIC  9(09)       COMP-3.
              07 EO-DT-FRANQUIA            PIC  9(09)       COMP-3.
              07 EO-NRO-OBJETO             PIC  9(09)       COMP-3.
              07 EO-NRO-REG                PIC  9(05)       COMP-3.
              07 FILLER                    PIC  X(08).

           05 EO-REGISTRO-HUM    REDEFINES EO-REGISTRO-ZERO.
              07 EO-NSEQ-OPER-DESC         PIC S9(09)V      COMP-3.
              07 EO-NM-REDUZ-SUBPROD       PIC  X(10).
              07 EO-NOSSO-NUMERO           PIC S9(18)V      COMP-3.
              07 EO-SEU-NUMERO             PIC  X(15).
              07 EO-ISACDO-PROP            PIC  X(60).
              07 EO-DTVCTO                 PIC  X(10).
              07 EO-DTPGTO                 PIC  X(10).
              07 EO-VALOR                  PIC S9(15)V9(02) COMP-3.
              07 EO-VLR-OSCILACAO          PIC S9(15)V9(02) COMP-3.
              07 EO-CBCO-COBR              PIC S9(03)V      COMP-3.
              07 EO-CAG-COBR               PIC S9(05)V      COMP-3.
              07 EO-COD-LANCAMENTO         PIC S9(03)V      COMP-3.
              07 FILLER-1                  PIC  X(73).
              07 EO-NUM-CEP-HUM            PIC  9(05)       COMP-3.
              07 EO-SUF-CEP-HUM            PIC  9(03)       COMP-3.
              07 FILLER-2                  PIC  X(85).


           05 EO-REGISTRO-DOIS   REDEFINES EO-REGISTRO-ZERO.
              07 EO-DS-REDUZ-SUBPROD       PIC  X(10).
              07 EO-TTAL-ENT-SUBPROD       PIC S9(15)V99    COMP-3.
              07 EO-TTAL-ENC-SUBPROD       PIC S9(15)V99    COMP-3.
              07 EO-TTAL-BXA-SUBPROD       PIC S9(15)V99    COMP-3.
              07 EO-TTAL-IOF-SUBPROD       PIC S9(15)V99    COMP-3.
              07 EO-SLDO-ATU-SUBPROD       PIC S9(15)V99    COMP-3.
              07 FILLER-1                  PIC X(163).
              07 EO-NUM-CEP-DOIS           PIC  9(05)       COMP-3.
              07 EO-SUF-CEP-DOIS           PIC  9(03)       COMP-3.
              07 FILLER-2                  PIC  X(85).

           05 EO-REGISTRO-TRES   REDEFINES EO-REGISTRO-ZERO.
              07 EO-SLDO-ANT-CLIENTE       PIC S9(15)V99    COMP-3.
              07 EO-TTAL-ENT-CLIENTE       PIC S9(15)V99    COMP-3.
              07 EO-TTAL-BXA-CLIENTE       PIC S9(15)V99    COMP-3.
              07 EO-TTAL-ENC-CLIENTE       PIC S9(15)V99    COMP-3.
              07 EO-TTAL-IOF-CLIENTE       PIC S9(15)V99    COMP-3.
              07 EO-SLDO-ATU-CLIENTE       PIC S9(15)V99    COMP-3.
              07 EO-SLDO-VCT-CLIENTE       PIC S9(15)V99    COMP-3.
              07 EO-SLDO-VCT-30D-CLI       PIC S9(15)V99    COMP-3.
              07 EO-SLDO-VCT-60D-CLI       PIC S9(15)V99    COMP-3.
              07 EO-SLDO-VCT-A60-CLI       PIC S9(15)V99    COMP-3.
              07 FILLER-1                  PIC X(128).
              07 EO-NUM-CEP-TRES           PIC  9(05)       COMP-3.
              07 EO-SUF-CEP-TRES           PIC  9(03)       COMP-3.
              07 FILLER-2                  PIC  X(85).
      *---------------------------------------------------------------*
      *            IMAGEM AVISO - HEADER                              *
      *---------------------------------------------------------------*

       01  IMAGEM-HEADER-O.
           05  HDR-HEADER-O            PIC X(06).
           05  HDR-DADOS-O.
               10  HDR-LOCAL-IMPR-O    PIC X(04).
               10  HDR-CCUSTO-O        PIC X(04).
               10  HDR-JOBNAME-O       PIC X(08).
               10  HDR-DTMOVTO-O       PIC X(08).
               10  HDR-DTSIST-O        PIC X(08).
               10  HDR-HRSIST-O        PIC X(06).
               10  HDR-VOLUME-O        PIC X(06).
           05  FILLER                  PIC X(370).

