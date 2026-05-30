      ******************************************************************
      * SISTEMA : DCOM - I#DCOMAV                                      *
      * TAMANHO : 0XXX BYTES                                           *
      * ARQUIVO : EXTRACAO AVISO (AVISO DEBITO)                        *
      ******************************************************************
       01  AV-REGISTRO.
           05 AV-CTPO-CLIENTE              PIC S9(01)V      COMP-3.
           05 AV-CCNPJ-CPF-DESTINO         PIC S9(09)V      COMP-3.
           05 AV-CFLIAL-CNPJ-DESTINO       PIC S9(05)V      COMP-3.
           05 AV-CCTRL-CNPJ-DESTINO        PIC S9(02)V      COMP-3.
           05 AV-DMOVTC-DESC-COML          PIC  X(10).
           05 AV-ICDENT                    PIC  X(60).
           05 AV-CJUNC-DEPDC-CEDNT         PIC S9(05)V      COMP-3.
           05 AV-NOME-DEPDC                PIC  X(40).
           05 AV-CCCORR-CDENT              PIC S9(13)V      COMP-3.
           05 AV-CGRP-CTBIL-CDENT          PIC S9(05)V      COMP-3.
           05 AV-CSGRP-CTBIL-CDENT         PIC S9(03)V      COMP-3.
           05 AV-DANO-OPER-DESC            PIC S9(04)V      COMP-3.
           05 AV-NSEQ-OPER-DESC            PIC S9(09)V      COMP-3.
           05 AV-ISPROD-DESC-COML          PIC  X(40).
           05 AV-DVCTO                     PIC  X(10).
           05 AV-DEFETV-BAIXA-PCELA        PIC  X(10).
           05 AV-NPCELA-DESC               PIC S9(05)V      COMP-3.
           05 AV-VPRINC-BX-PCELA           PIC S9(15)V9(02) COMP-3.
           05 AV-VENCAR-BX-PCELA           PIC S9(15)V9(02) COMP-3.
           05 AV-CCART-VENCD               PIC  X(05).
           05 AV-CCART-NORML               PIC  X(05).
           05 AV-ENDERECO.
              07 AV-LOGRAD                 PIC  X(60).
              07 AV-NUMERO                 PIC  X(07).
              07 AV-COMPL                  PIC  X(30).
              07 AV-BAIRO                  PIC  X(25).
              07 AV-CIDAD                  PIC  X(25).
              07 AV-UF                     PIC  X(02).
              07 AV-NUM-CEP                PIC  9(05)       COMP-3.
              07 AV-SUF-CEP                PIC  9(03)       COMP-3.
              07 AV-VALID-CEP              PIC  9(01).
           05 AV-NOME-AG                   PIC  X(20).
           05 AV-NUM-CEP-AG                PIC  9(05)       COMP-3.
           05 AV-SUF-CEP-AG                PIC  9(03)       COMP-3.
           05 AV-ENDER-AG                  PIC  X(25).
           05 AV-CIDAD-AG                  PIC  X(20).
           05 AV-UF-AG                     PIC  X(02).
           05 AV-COD-DIST                  PIC  9(05)       COMP-3.
           05 AV-NRO-SEQ-REST              PIC  9(09)       COMP-3.
           05 FILLER                       PIC  X(30).

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
           05  FILLER                  PIC X(450).
