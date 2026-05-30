      *================================================================*
      * BOOK PARA ACESSO A BASE DB2 - AREA DE SAIDA:                   *
      *    -> DCOMB069: DB2PRD.TSPROD_ROTRO_INCL                       *
      *----------------------------------------------------------------*
      * MODULOS DE ACESSO A BASE DE DADOS:                             *
      *    -> SELECT  - DCOM469S                                       *
      *    -> FETCH   - DCOM469F                                       *
      *    -> INSERT  - DCOM469I                                       *
      *    -> UPDATE  - DCOM469U                                       *
      *    -> DELETE  - DCOM469D                                       *
      *================================================================*
      *                                                                *
       05 DCOMW69S-HEADER.
          10 DCOMW69S-COD-LAYOUT   PIC  X(008) VALUE 'DCOMW69S'.
          10 DCOMW69S-TAM-LAYOUT   PIC  9(005) VALUE 02650.
      *
          10 DCOMW69S-BLOCO-SAIDA.
             15 DCOMW69S-QTDE-TOTAL            PIC  9(010).
             15 DCOMW69S-QTDE-RETORNADA        PIC  9(003).
             15 DCOMW69S-OCORRENCIA            OCCURS 050 TIMES.
                20 DCOMW69S-CPRODT             PIC S9(003) COMP-3.
                20 DCOMW69S-CSPROD-DESC-COML   PIC S9(003) COMP-3.
                20 DCOMW69S-CTPO-AGPTO-ROTRO   PIC S9(003) COMP-3.
                20 DCOMW69S-CINDCD-OBRIG-AGPTO PIC  X(001).
                20 DCOMW69S-CINDCD-SIT-AGPTO   PIC  X(001).
                20 DCOMW69S-CINDCD-CONCL-ROTRO PIC  X(001).
                20 DCOMW69S-HULT-ATULZ         PIC  X(026).
                20 DCOMW69S-CFUNC-BDSCO        PIC S9(009) COMP-3.
                20 DCOMW69S-CTERM              PIC  X(008).
             15 DCOMW69S-FILLER                PIC  X(224).
      *
