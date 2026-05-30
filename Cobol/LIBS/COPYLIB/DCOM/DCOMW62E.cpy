      *================================================================*
      * BOOK PARA ACESSO A BASE DB2 - AREA DE ENTRADA:                 *
      *    -> DCOMB062: DB2PRD.TSPROD_ISENC_DESC                       *
      *----------------------------------------------------------------*
      * MODULOS DE ACESSO A BASE DE DADOS:                             *
      *    -> SELECT  - DCOM462S                                       *
      *    -> FETCH   - DCOM462F                                       *
      *    -> INSERT  - DCOM462I                                       *
      *    -> UPDATE  - DCOM462U                                       *
      *    -> DELETE  - DCOM462D                                       *
      *================================================================*
      *                                                                *
       05 DCOMW62E-READER.
          10 DCOMW62E-COD-LAYOUT       PIC  X(008) VALUE 'DCOMW62E'.
          10 DCOMW62E-TAM-LAYOUT       PIC  9(005) VALUE 00700.
      *
       05 DCOMW62E-BLOCO-ENTRADA.
          10 DCOMW62E-INSTRUCAO        PIC  X(010).
      *
          10 DCOMW62E-ENTRADA.
             15 DCOMW62E-CTPO-ISENC-DESC    PIC S9(003) COMP-3.
             15 DCOMW62E-CPRODT             PIC S9(003) COMP-3.
             15 DCOMW62E-CSPROD-DESC-COML   PIC S9(003) COMP-3.
             15 DCOMW62E-DINIC-VGCIA-ISENC  PIC  X(010).
             15 DCOMW62E-DFIM-VGCIA-ISENC   PIC  X(010).
             15 DCOMW62E-CINDCD-REST-VLR    PIC  X(001).
             15 DCOMW62E-VREST-MAX-OPER     PIC S9(015)V9(002) COMP-3.
             15 DCOMW62E-HULT-ATULZ         PIC  X(026).
             15 DCOMW62E-CFUNC-BDSCO        PIC S9(009) COMP-3.
             15 DCOMW62E-CTERM              PIC  X(008).
      *
          10 DCOMW62E-BLOCO-PAGINACAO.
             15 DCOMW62E-MAX-OCORR              PIC  9(003).
             15 DCOMW62E-INDICADOR-PAGINACAO    PIC  X(001).
                88 DCOMW62E-P-INICIAL           VALUE 'I'.
                88 DCOMW62E-P-PRIMEIRA          VALUE 'P'.
                88 DCOMW62E-P-SEGUINTE          VALUE 'S'.
                88 DCOMW62E-P-ANTERIOR          VALUE 'A'.
                88 DCOMW62E-P-ULTIMA            VALUE 'U'.
      *
          10 DCOMW62E-BLOCO-RESTART.
             15 DCOMW62E-CHAVE-INI.
                20 DCOMW62E-I-CTPO-ISENC-DESC   PIC S9(003) COMP-3.
                20 DCOMW62E-I-CPRODT            PIC S9(003) COMP-3.
                20 DCOMW62E-I-CSPROD-DESC-COML  PIC S9(003) COMP-3.
                20 DCOMW62E-I-DINIC-VGCIA-ISENC PIC  X(010).
                20 FILLER                       PIC  X(190).
             15 DCOMW62E-CHAVE-FIM.
                20 DCOMW62E-F-CTPO-ISENC-DESC   PIC S9(003) COMP-3.
                20 DCOMW62E-F-CPRODT            PIC S9(003) COMP-3.
                20 DCOMW62E-F-CSPROD-DESC-COML  PIC S9(003) COMP-3.
                20 DCOMW62E-F-DINIC-VGCIA-ISENC PIC  X(010).
                20 FILLER                       PIC  X(190).
      *
          10 DCOMW62E-FILLER                    PIC  X(186).
      *
