      ******************************************************************
      * SISTEMA : DCOM - I#DCOMAD                                      *
      * TAMANHO : 0550 BYTES                                           *
      * ARQUIVO : EXTRACAO AVISO (AVISO DEBITO)                        *
      ******************************************************************
       01  AD-REGISTRO.
           05 AD-CTPO-CLIENTE              PIC S9(01)V      COMP-3.
           05 AD-CCNPJ-CPF-DESTINO         PIC S9(09)V      COMP-3.
           05 AD-CFLIAL-CNPJ-DESTINO       PIC S9(05)V      COMP-3.
           05 AD-CCTRL-CNPJ-DESTINO        PIC S9(02)V      COMP-3.
           05 AD-DMOVTC-DESC-COML          PIC  X(10).
           05 AD-ICDENT                    PIC  X(60).
           05 AD-CJUNC-DEPDC-CEDNT         PIC S9(05)V      COMP-3.
           05 AD-NOME-DEPDC                PIC  X(40).
           05 AD-CCCORR-CDENT              PIC S9(13)V      COMP-3.
           05 AD-CGRP-CTBIL-CDENT          PIC S9(05)V      COMP-3.
           05 AD-CSGRP-CTBIL-CDENT         PIC S9(03)V      COMP-3.
           05 AD-DANO-OPER-DESC            PIC S9(04)V      COMP-3.
           05 AD-NSEQ-OPER-DESC            PIC S9(09)V      COMP-3.
           05 AD-ISPROD-DESC-COML          PIC  X(40).
           05 AD-DVCTO                     PIC  X(10).
           05 AD-DEFETV-BAIXA-PCELA        PIC  X(10).
           05 AD-NPCELA-DESC               PIC S9(05)V      COMP-3.
           05 AD-VPRINC-BX-PCELA           PIC S9(15)V9(02) COMP-3.
           05 AD-VENCAR-BX-PCELA           PIC S9(15)V9(02) COMP-3.
           05 AD-CCART-VENCD               PIC  X(05).
           05 AD-CCART-NORML               PIC  X(05).
           05 AD-ENDERECO.
              07 AD-LOGRAD                 PIC  X(60).
              07 AD-NUMERO                 PIC  X(07).
              07 AD-COMPL                  PIC  X(30).
              07 AD-BAIRO                  PIC  X(25).
              07 AD-CIDAD                  PIC  X(25).
              07 AD-UF                     PIC  X(02).
              07 AD-NUM-CEP                PIC  9(05)       COMP-3.
              07 AD-SUF-CEP                PIC  9(03)       COMP-3.
              07 AD-VALID-CEP              PIC  9(01).
           05 AD-NOME-AG                   PIC  X(20).
           05 AD-NUM-CEP-AG                PIC  9(05)       COMP-3.
           05 AD-SUF-CEP-AG                PIC  9(03)       COMP-3.
           05 AD-ENDER-AG                  PIC  X(25).
           05 AD-CIDAD-AG                  PIC  X(20).
           05 AD-UF-AG                     PIC  X(02).
           05 AD-COD-DIST                  PIC  9(05)       COMP-3.
           05 AD-NRO-SEQ-REST              PIC  9(09)       COMP-3.
           05 AD-NRO-OBJINI                PIC  9(09)       COMP-3.
           05 AD-NRO-OBJFIM                PIC  9(09)       COMP-3.
           05 AD-QTDE-OBJ                  PIC  9(05)       COMP-3.
           05 AD-NRO-AMAR                  PIC  9(05)       COMP-3.
           05 AD-NOME-CDD                  PIC  X(32).
           05 AD-DT-EMISS                  PIC  9(09)       COMP-3.
           05 AD-DT-FRANQUIA               PIC  9(09)       COMP-3.
           05 AD-NRO-OBJETO                PIC  9(09)       COMP-3.
           05 AD-NRO-REG                   PIC  9(05)       COMP-3.
           05 FILLER                       PIC  X(14).
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
           05  FILLER                  PIC X(500).
