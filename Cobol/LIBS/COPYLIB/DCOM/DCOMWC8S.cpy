      *================================================================*
      * BOOK PARA ACESSO A BASE DB2 - AREA DE SAIDA:                   *
      *    -> DCOMB0C8: DB2PRD.TOPER_ROTRO_INCL                        *
      *----------------------------------------------------------------*
      * MODULOS DE ACESSO A BASE DE DADOS:                             *
      *    -> SELECT  - DCOM4C8S                                       *
      *    -> FETCH   - DCOM4C8F                                       *
      *    -> INSERT  - DCOM4C8I                                       *
      *    -> UPDATE  - DCOM4C8U                                       *
      *    -> DELETE  - DCOM4C8D                                       *
      *================================================================*
      *                                                                *
       05 DCOMWC8S-HEADER.
          10 DCOMWC8S-COD-LAYOUT       PIC  X(008) VALUE 'DCOMWC8S'.
          10 DCOMWC8S-TAM-LAYOUT       PIC  9(005) VALUE 02900.
      *
          10 DCOMWC8S-BLOCO-SAIDA.
             15 DCOMWC8S-QTDE-TOTAL                PIC  9(010).
             15 DCOMWC8S-QTDE-RETORNADA            PIC  9(003).
             15 DCOMWC8S-OCORRENCIA                OCCURS 050 TIMES.
                20 DCOMWC8S-DANO-OPER-DESC         PIC S9(004) COMP-3.
                20 DCOMWC8S-NSEQ-OPER-DESC         PIC S9(009) COMP-3.
                20 DCOMWC8S-CTPO-AGPTO-ROTRO       PIC S9(003) COMP-3.
                20 DCOMWC8S-CINDCD-OBRIG-AGPTO     PIC  X(001).
                20 DCOMWC8S-CINDCD-SIT-AGPTO       PIC  X(001).
                20 DCOMWC8S-CINDCD-CONCL-ROTRO     PIC  X(001).
                20 DCOMWC8S-HULT-ATULZ             PIC  X(026).
                20 DCOMWC8S-CFUNC-BDSCO            PIC S9(009) COMP-3.
                20 DCOMWC8S-CTERM                  PIC  X(008).
             15 DCOMWC8S-FILLER                    PIC  X(274).
      *                                                                *
      *================================================================*
