      *================================================================*
      * BOOK PARA ACESSO A BASE DB2 - AREA DE SAIDA:                   *
      *    -> DCOMB0G9: DB2PRD.TWORKF_OPER_DESC                        *
      *----------------------------------------------------------------*
      * MODULOS DE ACESSO A BASE DE DADOS:                             *
      *    -> SELECT  - DCOM4G9S                                       *
      *    -> FETCH   - DCOM4G9F                                       *
      *    -> INSERT  - DCOM4G9I                                       *
      *    -> UPDATE  - DCOM4G9U                                       *
      *    -> DELETE  - DCOM4G9D                                       *
      *================================================================*
      *                                                                *
       05 DCOMWG9S-HEADER.
          10 DCOMWG9S-COD-LAYOUT       PIC  X(008) VALUE 'DCOMWG9S'.
          10 DCOMWG9S-TAM-LAYOUT       PIC  9(005) VALUE 18850.
      *
          10 DCOMWG9S-BLOCO-SAIDA.
             15 DCOMWG9S-QTDE-TOTAL                PIC  9(010).
             15 DCOMWG9S-QTDE-RETORNADA            PIC  9(003).
             15 DCOMWG9S-OCORRENCIA                OCCURS 050 TIMES.
                20 DCOMWG9S-DANO-OPER-DESC         PIC S9(004) COMP-3.
                20 DCOMWG9S-NSEQ-OPER-DESC         PIC S9(009) COMP-3.
                20 DCOMWG9S-NPCELA-DESC-COML       PIC S9(005) COMP-3.
                20 DCOMWG9S-CEVNTO-DESC-COML       PIC S9(003) COMP-3.
                20 DCOMWG9S-HWORKF-OPER-DESC       PIC  X(026).
                20 DCOMWG9S-CSIT-INIC-DESC         PIC S9(003) COMP-3.
                20 DCOMWG9S-HSIT-INIC-DESC         PIC  X(026).
                20 DCOMWG9S-CSIT-FNAL-DESC         PIC S9(003) COMP-3.
                20 DCOMWG9S-HSIT-FNAL-DESC         PIC  X(026).
                20 DCOMWG9S-RWORKF-OPER-DESC.
                   25 DCOMWG9S-RWORKF-OPER-LEN     PIC S9(004) COMP-3.
                   25 DCOMWG9S-RWORKF-OPER-TEXT    PIC  X(240).
                20 DCOMWG9S-CFUNC-BDSCO            PIC S9(009) COMP-3.
                20 DCOMWG9S-CTERM                  PIC  X(008).
             15 DCOMWG9S-FILLER                    PIC  X(1274).
      *
      *================================================================*
