      *================================================================*
      * BOOK PARA ACESSO A BASE DB2 - AREA DE SAIDA:                   *
      *    -> DCOMB0H9: DB2PRD.TTRASI_OPER_AUTRZ                       *
      *----------------------------------------------------------------*
      * MODULOS DE ACESSO A BASE DE DADOS:                             *
      *    -> SELECT  - DCOM4H9S                                       *
      *    -> FETCH   - DCOM4H9F                                       *
      *    -> INSERT  - DCOM4H9I                                       *
      *    -> UPDATE  - DCOM4H9U                                       *
      *    -> DELETE  - DCOM4H9D                                       *
      *================================================================*
      *                                                                *
       05 DCOMWH9S-HEADER.
          10 DCOMWH9S-COD-LAYOUT       PIC  X(008) VALUE 'DCOMWH9S'.
          10 DCOMWH9S-TAM-LAYOUT       PIC  9(005) VALUE 08600.
      *
          10 DCOMWH9S-BLOCO-SAIDA.
             15 DCOMWH9S-QTDE-TOTAL                PIC  9(010).
             15 DCOMWH9S-QTDE-RETORNADA            PIC  9(003).
             15 DCOMWH9S-OCORRENCIA                OCCURS 050  TIMES.
                20 DCOMWH9S-DANO-OPER-DESC         PIC S9(004) COMP-3.
                20 DCOMWH9S-NSEQ-OPER-DESC         PIC S9(009) COMP-3.
                20 DCOMWH9S-NPCELA-DESC-COML       PIC S9(005) COMP-3.
                20 DCOMWH9S-CTPO-AUTRZ-ESPCL       PIC S9(003) COMP-3.
                20 DCOMWH9S-HULT-ATULZ             PIC  X(026).
                20 DCOMWH9S-PMIN-TX-JURO-REGRA     PIC S9(003)V9(003)
                                                               COMP-3.
                20 DCOMWH9S-PMAX-TX-JURO-REGRA     PIC S9(003)V9(003)
                                                               COMP-3.
                20 DCOMWH9S-PTX-JURO-OPER-DESC     PIC S9(003)V9(005)
                                                               COMP-3.
                20 DCOMWH9S-CINDCD-FREQ-TX         PIC  X(002).
                20 DCOMWH9S-CCTRO-CUSTO            PIC  X(004).
                20 DCOMWH9S-CAUTRZ-TX-TESOU        PIC S9(009) COMP-3.
                20 DCOMWH9S-IAUTRZ-TX-TESOU        PIC  X(040).
                20 DCOMWH9S-CFUNC-BDSCO            PIC S9(009) COMP-3.
                20 DCOMWH9S-CTERM                  PIC  X(008).
                20 DCOMWH9S-FILLER                 PIC  X(050).
             15 DCOMWH9S-FILLER                    PIC  X(274).
      *                                                                *
      *================================================================*
