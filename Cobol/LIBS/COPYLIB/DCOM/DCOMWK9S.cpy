      *================================================================*
      * BOOK PARA ACESSO A BASE DB2 - AREA DE SAIDA:                   *
      *    -> DCOMB0K9: DB2PRD.TTESTM_AG_OPER                          *
      *----------------------------------------------------------------*
      * MODULOS DE ACESSO A BASE DE DADOS:                             *
      *    -> SELECT  - DCOM4K9S                                       *
      *    -> FETCH   - DCOM4K9F                                       *
      *    -> INSERT  - DCOM4K9I                                       *
      *    -> UPDATE  - DCOM4K9U                                       *
      *    -> DELETE  - DCOM4K9D                                       *
      *================================================================*
      *
       05 DCOMWK9S-HEADER.
          10 DCOMWK9S-COD-LAYOUT       PIC  X(008) VALUE 'DCOMWK9S'.
          10 DCOMWK9S-TAM-LAYOUT       PIC  9(005) VALUE 03850.
      *
          10 DCOMWK9S-BLOCO-SAIDA.
             15 DCOMWK9S-QTDE-TOTAL                PIC  9(010).
             15 DCOMWK9S-QTDE-RETORNADA            PIC  9(003).
             15 DCOMWK9S-OCORRENCIA                OCCURS 050 TIMES.
                20 DCOMWK9S-CBCO                   PIC S9(003) COMP-3.
                20 DCOMWK9S-CAG-BCRIA              PIC S9(005) COMP-3.
                20 DCOMWK9S-NPSSOA-DESC-COML       PIC S9(009) COMP-3.
                20 DCOMWK9S-DULT-UTILZ             PIC  X(010).
                20 DCOMWK9S-HULT-ATULZ             PIC  X(026).
                20 DCOMWK9S-DINCL-REG              PIC  X(010).
                20 DCOMWK9S-CFUNC-BDSCO            PIC S9(009) COMP-3.
                20 DCOMWK9S-CTERM                  PIC  X(008).
             15 DCOMWK9S-FILLER                    PIC  X(374).
      *                                                                *
      *================================================================*
