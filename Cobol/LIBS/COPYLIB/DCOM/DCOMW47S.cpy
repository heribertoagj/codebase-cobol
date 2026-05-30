      *================================================================*
      * BOOK PARA ACESSO A BASE DB2 - AREA DE SAIDA:                   *
      *    -> DCOMB047: DB2PRD.TPRODT_VALID_DESC                       *
      *----------------------------------------------------------------*
      * MODULOS DE ACESSO A BASE DE DADOS:                             *
      *    -> SELECT  - DCOM447S                                       *
      *    -> FETCH   - DCOM447F                                       *
      *    -> INSERT  - DCOM447I                                       *
      *    -> UPDATE  - DCOM447U                                       *
      *    -> DELETE  - DCOM447D                                       *
      *================================================================*
      *                                                                *
       05 DCOMW47S-HEADER.
          10 DCOMW47S-COD-LAYOUT       PIC  X(008) VALUE 'DCOMW47S'.
          10 DCOMW47S-TAM-LAYOUT       PIC  9(005) VALUE 06426.
      *
          10 DCOMW47S-BLOCO-SAIDA.
             15 DCOMW47S-QTDE-TOTAL                PIC  9(010).
             15 DCOMW47S-QTDE-RETORNADA            PIC  9(003).
             15 DCOMW47S-OCORRENCIA                OCCURS 050  TIMES.
                20 DCOMW47S-CPRODT                 PIC S9(003) COMP-3.
                20 DCOMW47S-IPRODT                 PIC  X(060).
                20 DCOMW47S-IABREV-PRODT           PIC  X(012).
                20 DCOMW47S-CSGL-PRODT-DESC        PIC  X(006).
                20 DCOMW47S-CIDTFD-TX-MIN-JURO     PIC S9(005) COMP-3.
                20 DCOMW47S-CIDTFD-TX-MAX-JURO     PIC S9(005) COMP-3.
                20 DCOMW47S-HULT-ATULZ             PIC  X(026).
                20 DCOMW47S-CFUNC-BDSCO            PIC S9(009) COMP-3.
                20 DCOMW47S-CTERM                  PIC  X(008).
                20 DCOMW47S-CIDTFD-TX-CUSTO        PIC S9(005) COMP-3.
