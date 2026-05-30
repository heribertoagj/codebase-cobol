      *================================================================*
      * BOOK PARA ACESSO A BASE DB2 - AREA DE SAIDA:                   *
      *    -> DCOMB0L3: DB2PRD.TPARMZ_CUSTO_EFETV                      *
      *----------------------------------------------------------------*
      * MODULOS DE ACESSO A BASE DE DADOS:                             *
      *    -> SELECT  - DCOM4L3S                                       *
      *    -> FETCH   - DCOM4L3F                                       *
      *    -> INSERT  - DCOM4L3I                                       *
      *    -> UPDATE  - DCOM4L3U                                       *
      *    -> DELETE  - DCOM4L3D                                       *
      *================================================================*
      *                                                                *
       05 DCOMWL3S-HEADER.
          10 DCOMWL3S-COD-LAYOUT   PIC  X(008) VALUE 'DCOMWL3S'.
          10 DCOMWL3S-TAM-LAYOUT   PIC  9(005) VALUE 01950.
      *
          10 DCOMWL3S-BLOCO-SAIDA.
             15 DCOMWL3S-QTDE-TOTAL            PIC  9(010).
             15 DCOMWL3S-QTDE-RETORNADA        PIC  9(003).
             15 DCOMWL3S-OCORRENCIA            OCCURS 050 TIMES.
                20 DCOMWL3S-CPRODT             PIC S9(003) COMP-3.
                20 DCOMWL3S-CSPROD-DESC-COML   PIC S9(003) COMP-3.
                20 DCOMWL3S-CINDCD-PSSOA-TOMAD PIC  X(001).
                20 DCOMWL3S-CFUNC-BDSCO        PIC S9(009) COMP-3.
                20 DCOMWL3S-HATULZ             PIC  X(026).
             15 DCOMWL3S-FILLER                PIC  X(124).
      *
