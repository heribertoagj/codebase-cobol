      *================================================================*
      * BOOK PARA ACESSO A BASE DB2 - AREA DE ENTRADA:                 *
      *    -> DCOMB0H9: DB2PRD.TTRASI_OPER_AUTRZ                      *
      *----------------------------------------------------------------*
      * MODULOS DE ACESSO A BASE DE DADOS:                             *
      *    -> SELECT  - DCOM4H9S                                       *
      *    -> FETCH   - DCOM4H9F                                       *
      *    -> INSERT  - DCOM4H9I                                       *
      *    -> UPDATE  - DCOM4H9U                                       *
      *    -> DELETE  - DCOM4H9D                                       *
      *================================================================*
      *                                                                *
       05 DCOMWH9E-HEADER.
          10 DCOMWH9E-COD-LAYOUT       PIC  X(008) VALUE 'DCOMWH9E'.
          10 DCOMWH9E-TAM-LAYOUT       PIC  9(005) VALUE 00400.
      *
       05 DCOMWH9E-BLOCO-ENTRADA.
          10 DCOMWH9E-INSTRUCAO                    PIC  X(010).
      *
          10 DCOMWH9E-ENTRADA.
             15 DCOMWH9E-DANO-OPER-DESC            PIC S9(004) COMP-3.
             15 DCOMWH9E-NSEQ-OPER-DESC            PIC S9(009) COMP-3.
             15 DCOMWH9E-NPCELA-DESC-COML          PIC S9(005) COMP-3.
             15 DCOMWH9E-CTPO-AUTRZ-ESPCL          PIC S9(003) COMP-3.
             15 DCOMWH9E-HULT-ATULZ                PIC  X(026).
             15 DCOMWH9E-PMIN-TX-JURO-REGRA        PIC S9(003)V9(003)
                                                               COMP-3.
             15 DCOMWH9E-PMAX-TX-JURO-REGRA        PIC S9(003)V9(003)
                                                               COMP-3.
             15 DCOMWH9E-PTX-JURO-OPER-DESC        PIC S9(003)V9(005)
                                                               COMP-3.
             15 DCOMWH9E-CINDCD-FREQ-TX            PIC  X(002).
             15 DCOMWH9E-CCTRO-CUSTO               PIC  X(004).
             15 DCOMWH9E-CAUTRZ-TX-TESOU           PIC S9(009) COMP-3.
             15 DCOMWH9E-IAUTRZ-TX-TESOU           PIC  X(040).
             15 DCOMWH9E-CFUNC-BDSCO               PIC S9(009) COMP-3.
             15 DCOMWH9E-CTERM                     PIC  X(008).
             15 DCOMWH9E-FILLER                    PIC  X(050).
      *
          10 DCOMWH9E-BLOCO-VARIAVEL.
             15 DCOMWH9E-V-FILLER                  PIC  X(100).
      *
          10 DCOMWH9E-BLOCO-PAGINACAO.
             15 DCOMWH9E-MAX-OCORR                 PIC  9(003).
             15 DCOMWH9E-INDICADOR-PAGINACAO       PIC  X(001).
                88 DCOMWH9E-P-INICIAL              VALUE 'I'.
                88 DCOMWH9E-P-PRIMEIRA             VALUE 'P'.
                88 DCOMWH9E-P-SEGUINTE             VALUE 'S'.
                88 DCOMWH9E-P-ANTERIOR             VALUE 'A'.
                88 DCOMWH9E-P-ULTIMA               VALUE 'U'.
      *
          10 DCOMWH9E-BLOCO-RESTART.
             15 DCOMWH9E-CHAVE-INI.
                20 DCOMWH9E-I-DANO-OPER-DESC       PIC S9(004) COMP-3.
                20 DCOMWH9E-I-NSEQ-OPER-DESC       PIC S9(009) COMP-3.
                20 DCOMWH9E-I-NPCELA-DESC-COML     PIC S9(005) COMP-3.
                20 DCOMWH9E-I-CTPO-AUTRZ-ESPCL     PIC S9(003) COMP-3.
                20 DCOMWH9E-I-FILLER               PIC  X(015).
             15 DCOMWH9E-CHAVE-FIM.
                20 DCOMWH9E-F-DANO-OPER-DESC       PIC S9(004) COMP-3.
                20 DCOMWH9E-F-NSEQ-OPER-DESC       PIC S9(009) COMP-3.
                20 DCOMWH9E-F-NPCELA-DESC-COML     PIC S9(005) COMP-3.
                20 DCOMWH9E-F-CTPO-AUTRZ-ESPCL     PIC S9(003) COMP-3.
                20 DCOMWH9E-F-FILLER               PIC  X(015).
          10 DCOMWH9E-FILLER                       PIC  X(051).
