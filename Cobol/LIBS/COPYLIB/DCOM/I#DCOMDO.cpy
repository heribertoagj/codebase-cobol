      ******************************************************************
      * SISTEMA : DCOM - I#DCOMDO                                      *
      * TAMANHO : 0420 BYTES                                           *
      * ARQUIVO : BASE PARA EMISSAO DE AVISOS DE DESPESAS RELATIVA     *
      *           A OPERACOES DE DESCONTOS.                            *
      ******************************************************************
       01  DO-REGISTRO.
           05 DO-CTPO-CLIENTE              PIC S9(01)V      COMP-3.
           05 DO-CCNPJ-CPF-DEST            PIC S9(09)V      COMP-3.
           05 DO-CFLIAL-CNPJ-DEST          PIC S9(05)V      COMP-3.
           05 DO-CCTRL-CNPJ-DEST           PIC S9(02)V      COMP-3.
           05 DO-DINIC-PERIODO             PIC  X(10).
           05 DO-DFNAL-PERIODO             PIC  X(10).
           05 DO-CTPO-REGISTRO             PIC S9(01)V      COMP-3.
           05 DO-CAG-BCRIA                 PIC S9(05)V      COMP-3.
           05 DO-CCCORR                    PIC S9(13)V      COMP-3.
           05 DO-REGISTRO-ZERO.
              07 DO-ICDENT-EMITENTE        PIC  X(60).
              07 DO-NM-REDUZ-SUBPROD       PIC  X(10).
              07 DO-QTDE-HUM               PIC S9(06)V      COMP-3.
              07 DO-ENDERECO.
                 09 DO-LOGRAD              PIC  X(60).
                 09 DO-NUMERO              PIC  X(07).
                 09 DO-COMPL               PIC  X(30).
                 09 DO-BAIRO               PIC  X(25).
                 09 DO-CIDAD               PIC  X(25).
                 09 DO-UF                  PIC  X(02).
                 09 DO-NUM-CEP             PIC  9(05)       COMP-3.
                 09 DO-SUF-CEP             PIC  9(03)       COMP-3.
                 09 DO-VALID-CEP           PIC  9(01).
              07 DO-NOME-AG                PIC  X(20).
              07 DO-NUM-CEP-AG             PIC  9(05)       COMP-3.
              07 DO-SUF-CEP-AG             PIC  9(03)       COMP-3.
              07 DO-ENDER-AG               PIC  X(25).
              07 DO-CIDAD-AG               PIC  X(20).
              07 DO-UF-AG                  PIC  X(02).
              07 DO-COD-DIST               PIC  9(05)       COMP-3.
              07 DO-NRO-SEQ-REST           PIC  9(09)       COMP-3.
              07 DO-NRO-OBJINI             PIC  9(09)       COMP-3.
              07 DO-NRO-OBJFIM             PIC  9(09)       COMP-3.
              07 DO-QTDE-OBJ               PIC  9(05)       COMP-3.
              07 DO-NRO-AMAR               PIC  9(05)       COMP-3.
              07 DO-NOME-CDD               PIC  X(32).
              07 DO-DT-EMISS               PIC  9(09)       COMP-3.
              07 DO-DT-FRANQUIA            PIC  9(09)       COMP-3.
              07 DO-NRO-OBJETO             PIC  9(09)       COMP-3.
              07 DO-NRO-REG                PIC  9(05)       COMP-3.
              07 FILLER                    PIC  X(03).

           05 DO-REGISTRO-HUM    REDEFINES DO-REGISTRO-ZERO.
              07 DO-DANO-OPER-DESC         PIC S9(04)V          COMP-3.
              07 DO-NSEQ-OPER-DESC         PIC S9(09)V          COMP-3.
              07 DO-DINIC-OPER-DESC        PIC  X(10).
              07 DO-VPRINC-OPER-DESC       PIC S9(15)V99        COMP-3.
              07 DO-VJURO-OPER-DESC        PIC S9(15)V99        COMP-3.
              07 DO-VTRIFA-REG-OPER-DESC   PIC S9(15)V99        COMP-3.
              07 DO-VIOF-OPER-DESC         PIC S9(15)V99        COMP-3.
              07 DO-VTAC-OPER-DESC         PIC S9(15)V99        COMP-3.
              07 DO-TTAL-DESP-OPER-DESC    PIC S9(15)V99        COMP-3.
              07 DO-COD-LANCAMENTO         PIC S9(03)V          COMP-3.
              07 FILLER-1                  PIC  X(149).
              07 DO-NUM-CEP-HUM            PIC  9(05)           COMP-3.
              07 DO-SUF-CEP-HUM            PIC  9(03)           COMP-3.
              07 FILLER-2                  PIC  X(150).

           05 DO-REGISTRO-DOIS   REDEFINES DO-REGISTRO-ZERO.
              07 DO-TQTD-CONTR-OPER        PIC S9(06)V          COMP-3.
              07 DO-TTAL-VPRINC            PIC S9(15)V99        COMP-3.
              07 DO-TTAL-VJURO             PIC S9(15)V99        COMP-3.
              07 DO-TTAL-TRIFA             PIC S9(15)V99        COMP-3.
              07 DO-TTAL-IOF               PIC S9(15)V99        COMP-3.
              07 DO-TTAL-TAC               PIC S9(15)V99        COMP-3.
              07 DO-TTAL-GRAL-DESPS        PIC S9(15)V99        COMP-3.
              07 FILLER-1                  PIC X(165).
              07 DO-NUM-CEP-DOIS           PIC  9(05)           COMP-3.
              07 DO-SUF-CEP-DOIS           PIC  9(03)           COMP-3.
              07 FILLER-2                  PIC X(150).

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

