      ******************************************************************
      * SISTEMA : DCOM - I#DCOMDE                                      *
      * TAMANHO : 0400 BYTES                                           *
      * ARQUIVO : BASE PARA EMISSAO DE AVISOS DE DESPESAS RELATIVA     *
      *           A OPERACOES DE DESCONTOS.                            *
      ******************************************************************
       01  DE-REGISTRO.
           05 DE-CTPO-CLIENTE              PIC S9(01)V      COMP-3.
           05 DE-CCNPJ-CPF-DEST            PIC S9(09)V      COMP-3.
           05 DE-CFLIAL-CNPJ-DEST          PIC S9(05)V      COMP-3.
           05 DE-CCTRL-CNPJ-DEST           PIC S9(02)V      COMP-3.
           05 DE-DINIC-PERIODO             PIC  X(10).
           05 DE-DFNAL-PERIODO             PIC  X(10).
           05 DE-CTPO-REGISTRO             PIC S9(01)V      COMP-3.
           05 DE-CAG-BCRIA                 PIC S9(05)V      COMP-3.
           05 DE-CCCORR                    PIC S9(13)V      COMP-3.
           05 DE-REGISTRO-ZERO.
              07 DE-ICDENT-EMITENTE        PIC  X(60).
              07 DE-NM-REDUZ-SUBPROD       PIC  X(10).
              07 DE-QTDE-HUM               PIC S9(06)V      COMP-3.
              07 DE-ENDERECO.
                 09 DE-LOGRAD              PIC  X(60).
                 09 DE-NUMERO              PIC  X(07).
                 09 DE-COMPL               PIC  X(30).
                 09 DE-BAIRO               PIC  X(25).
                 09 DE-CIDAD               PIC  X(25).
                 09 DE-UF                  PIC  X(02).
                 09 DE-NUM-CEP             PIC  9(05)       COMP-3.
                 09 DE-SUF-CEP             PIC  9(03)       COMP-3.
                 09 DE-VALID-CEP           PIC  9(01).
              07 DE-NOME-AG                PIC  X(20).
              07 DE-NUM-CEP-AG             PIC  9(05)       COMP-3.
              07 DE-SUF-CEP-AG             PIC  9(03)       COMP-3.
              07 DE-ENDER-AG               PIC  X(25).
              07 DE-CIDAD-AG               PIC  X(20).
              07 DE-UF-AG                  PIC  X(02).
              07 DE-COD-DIST               PIC  9(05)       COMP-3.
              07 DE-NRO-SEQ-REST           PIC  9(09)       COMP-3.
              07 FILLER                    PIC  X(49).

           05 DE-REGISTRO-HUM    REDEFINES DE-REGISTRO-ZERO.
              07 DE-DANO-OPER-DESC         PIC S9(04)V          COMP-3.
              07 DE-NSEQ-OPER-DESC         PIC S9(09)V          COMP-3.
              07 DE-DINIC-OPER-DESC        PIC  X(10).
              07 DE-VPRINC-OPER-DESC       PIC S9(15)V99        COMP-3.
              07 DE-VJURO-OPER-DESC        PIC S9(15)V99        COMP-3.
              07 DE-VTRIFA-REG-OPER-DESC   PIC S9(15)V99        COMP-3.
              07 DE-VIOF-OPER-DESC         PIC S9(15)V99        COMP-3.
              07 DE-VTAC-OPER-DESC         PIC S9(15)V99        COMP-3.
              07 DE-TTAL-DESP-OPER-DESC    PIC S9(15)V99        COMP-3.
              07 DE-COD-LANCAMENTO         PIC S9(03)V          COMP-3.
              07 FILLER-1                  PIC  X(149).
              07 DE-NUM-CEP-HUM            PIC  9(05)           COMP-3.
              07 DE-SUF-CEP-HUM            PIC  9(03)           COMP-3.
              07 FILLER-2                  PIC  X(130).

           05 DE-REGISTRO-DOIS   REDEFINES DE-REGISTRO-ZERO.
              07 DE-TQTD-CONTR-OPER        PIC S9(06)V          COMP-3.
              07 DE-TTAL-VPRINC            PIC S9(15)V99        COMP-3.
              07 DE-TTAL-VJURO             PIC S9(15)V99        COMP-3.
              07 DE-TTAL-TRIFA             PIC S9(15)V99        COMP-3.
              07 DE-TTAL-IOF               PIC S9(15)V99        COMP-3.
              07 DE-TTAL-TAC               PIC S9(15)V99        COMP-3.
              07 DE-TTAL-GRAL-DESPS        PIC S9(15)V99        COMP-3.
              07 FILLER-1                  PIC X(165).
              07 DE-NUM-CEP-DOIS           PIC  9(05)           COMP-3.
              07 DE-SUF-CEP-DOIS           PIC  9(03)           COMP-3.
              07 FILLER-2                  PIC X(130).

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
           05  FILLER                  PIC X(350).

