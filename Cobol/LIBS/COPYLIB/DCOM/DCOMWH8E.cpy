      *================================================================*
      * BOOK PARA ACESSO A BASE DB2 - AREA DE ENTRADA:                 *
      *    -> DCOMB0H8: DB2PRD.TTRASI_MEIO_LIBRC                       *
      *----------------------------------------------------------------*
      * MODULOS DE ACESSO A BASE DE DADOS:                             *
      *    -> SELECT  - DCOM4H8S                                       *
      *    -> FETCH   - DCOM4H8F                                       *
      *    -> INSERT  - DCOM4H8I                                       *
      *    -> UPDATE  - DCOM4H8U                                       *
      *    -> DELETE  - DCOM4H8D                                       *
      *================================================================*
      *                                                                *
       05 DCOMWH8E-HEADER.
          10 DCOMWH8E-COD-LAYOUT       PIC  X(008) VALUE 'DCOMWH8E'.
          10 DCOMWH8E-TAM-LAYOUT       PIC  9(005) VALUE 00700.
      *
       05 DCOMWH8E-BLOCO-ENTRADA.
          10 DCOMWH8E-INSTRUCAO                    PIC  X(010).
      *
          10 DCOMWH8E-ENTRADA.
             15 DCOMWH8E-DANO-OPER-DESC            PIC S9(004) COMP-3.
             15 DCOMWH8E-NSEQ-OPER-DESC            PIC S9(009) COMP-3.
             15 DCOMWH8E-CMEIO-LIBRC-DESC          PIC S9(003) COMP-3.
             15 DCOMWH8E-NOPER-MEIO-LIBRC          PIC S9(003) COMP-3.
             15 DCOMWH8E-CSIT-DESC-COML            PIC S9(003) COMP-3.
             15 DCOMWH8E-HSIT-DESC-COML            PIC  X(026).
             15 DCOMWH8E-CBCO                      PIC S9(003) COMP-3.
             15 DCOMWH8E-CAG-BCRIA                 PIC S9(005) COMP-3.
             15 DCOMWH8E-CCTA-BCRIA-CLI            PIC S9(013) COMP-3.
             15 DCOMWH8E-CPOSTO-SERVC              PIC S9(003) COMP-3.
             15 DCOMWH8E-CCNPJ-CPF                 PIC S9(009) COMP-3.
             15 DCOMWH8E-CFLIAL-CNPJ               PIC S9(005) COMP-3.
             15 DCOMWH8E-CCTRL-CNPJ-CPF            PIC S9(002) COMP-3.
             15 DCOMWH8E-IPSSOA-DESC-COML          PIC  X(060).
             15 DCOMWH8E-CCLUB                     PIC S9(010) COMP-3.
             15 DCOMWH8E-VLIBRC-DESC-COML          PIC S9(015)V9(002)
                                                               COMP-3.
             15 DCOMWH8E-PLIBRC-DESC-COML          PIC S9(003)V9(003)
                                                               COMP-3.
             15 DCOMWH8E-DLIBRC-DESC-COML          PIC  X(010).
             15 DCOMWH8E-CINDCD-LIBRC-ANTCP        PIC  X(001).
             15 DCOMWH8E-HULT-ATULZ                PIC  X(026).
             15 DCOMWH8E-CFUNC-BDSCO               PIC S9(009) COMP-3.
             15 DCOMWH8E-CTERM                     PIC  X(008).
             15 DCOMWH8E-CINDCD-LIBRC-CANAL        PIC  X(001).
             15 DCOMWH8E-FILLER                    PIC  X(029).
      *
          10 DCOMWH8E-BLOCO-VARIAVEL.
             15 DCOMWH8E-V-FILLER                  PIC  X(300).
      *
          10 DCOMWH8E-BLOCO-PAGINACAO.
             15 DCOMWH8E-MAX-OCORR                 PIC  9(003).
             15 DCOMWH8E-INDICADOR-PAGINACAO       PIC  X(001).
                88 DCOMWH8E-P-INICIAL              VALUE 'I'.
                88 DCOMWH8E-P-PRIMEIRA             VALUE 'P'.
                88 DCOMWH8E-P-SEGUINTE             VALUE 'S'.
                88 DCOMWH8E-P-ANTERIOR             VALUE 'A'.
                88 DCOMWH8E-P-ULTIMA               VALUE 'U'.
      *
          10 DCOMWH8E-BLOCO-RESTART.
             15 DCOMWH8E-CHAVE-INI.
                20 DCOMWH8E-I-DANO-OPER-DESC       PIC S9(004) COMP-3.
                20 DCOMWH8E-I-NSEQ-OPER-DESC       PIC S9(009) COMP-3.
                20 DCOMWH8E-I-CMEIO-LIBRC-DESC     PIC S9(003) COMP-3.
                20 DCOMWH8E-I-NOPER-MEIO-LIBRC     PIC S9(003) COMP-3.
                20 DCOMWH8E-I-FILLER               PIC  X(015).
             15 DCOMWH8E-CHAVE-FIM.
                20 DCOMWH8E-F-DANO-OPER-DESC       PIC S9(004) COMP-3.
                20 DCOMWH8E-F-NSEQ-OPER-DESC       PIC S9(009) COMP-3.
                20 DCOMWH8E-F-CMEIO-LIBRC-DESC     PIC S9(003) COMP-3.
                20 DCOMWH8E-F-NOPER-MEIO-LIBRC     PIC S9(003) COMP-3.
      *
          10 DCOMWH8E-FILLER                       PIC  X(111).
      *
