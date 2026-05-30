      ******************************************************************
      * SISTEMA : DCOM - I#DCOMDC                                      *
      * TAMANHO : 0128 BYTES                                           *
      * ARQUIVO : DESPESAS POR CLIENTE E PERIODO                       *
      ******************************************************************
       01  DC-REGISTRO.
           05 DC-CTPO-CLIENTE              PIC S9(01)V          COMP-3.
           05 DC-CCNPJ-CPF-DEST            PIC S9(09)V          COMP-3.
           05 DC-CFLIAL-CNPJ-DEST          PIC S9(05)V          COMP-3.
           05 DC-CCTRL-CNPJ-DEST           PIC S9(02)V          COMP-3.
           05 DC-DINIC-PERIODO             PIC  X(10).
           05 DC-DFNAL-PERIODO             PIC  X(10).
           05 DC-CTPO-REGISTRO             PIC S9(01)V          COMP-3.
           05 DC-REGISTRO-ZERO.
              07 DC-ICDENT-EMITENTE        PIC  X(60).
              07 DC-CAG-BCRIA              PIC S9(05)V          COMP-3.
              07 DC-CCCORR                 PIC S9(13)V          COMP-3.
              07 DC-NM-REDUZ-SUBPROD       PIC  X(10).
              07 DC-QTDE-HUM               PIC S9(06)V          COMP-3.
              07 FILLER                    PIC  X(12).

           05 DC-REGISTRO-HUM    REDEFINES DC-REGISTRO-ZERO.
              07 DC-DANO-OPER-DESC         PIC S9(04)V          COMP-3.
              07 DC-NSEQ-OPER-DESC         PIC S9(09)V          COMP-3.
              07 DC-DINIC-OPER-DESC        PIC  X(10).
              07 DC-VPRINC-OPER-DESC       PIC S9(15)V99        COMP-3.
              07 DC-VJURO-OPER-DESC        PIC S9(15)V99        COMP-3.
              07 DC-VTRIFA-REG-OPER-DESC   PIC S9(15)V99        COMP-3.
              07 DC-VIOF-OPER-DESC         PIC S9(15)V99        COMP-3.
              07 DC-VTAC-OPER-DESC         PIC S9(15)V99        COMP-3.
              07 DC-TTAL-DESP-OPER-DESC    PIC S9(15)V99        COMP-3.
              07 DC-COD-LANCAMENTO         PIC S9(03)V          COMP-3.
              07 FILLER                    PIC  X(22).

           05 DC-REGISTRO-DOIS   REDEFINES DC-REGISTRO-ZERO.
              07 DC-TQTD-CONTR-OPER        PIC S9(06)V          COMP-3.
              07 DC-TTAL-VPRINC            PIC S9(15)V99        COMP-3.
              07 DC-TTAL-VJURO             PIC S9(15)V99        COMP-3.
              07 DC-TTAL-TRIFA             PIC S9(15)V99        COMP-3.
              07 DC-TTAL-IOF               PIC S9(15)V99        COMP-3.
              07 DC-TTAL-TAC               PIC S9(15)V99        COMP-3.
              07 DC-TTAL-GRAL-DESPS        PIC S9(15)V99        COMP-3.
              07 FILLER                    PIC  X(38).
