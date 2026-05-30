      *================================================================*
      * BOOK PARA ACESSO A BASE DB2 - AREA DE SAIDA:                   *
      *    -> DCOMB061: DB2PRD.TSPROD_CONTR_LIM                        *
      *----------------------------------------------------------------*
      * MODULOS DE ACESSO A BASE DE DADOS:                             *
      *    -> SELECT  - DCOM461S                                       *
      *    -> FETCH   - DCOM461F                                       *
      *    -> INSERT  - DCOM461I                                       *
      *    -> UPDATE  - DCOM461U                                       *
      *    -> DELETE  - DCOM461D                                       *
      *================================================================*
      *                                                                *
       05 DCOMW61S-HEADER.
          10 DCOMW61S-COD-LAYOUT       PIC  X(008) VALUE 'DCOMW61S'.
          10 DCOMW61S-TAM-LAYOUT       PIC  9(005) VALUE 04476.
      *
          10 DCOMW61S-BLOCO-SAIDA.
             15 DCOMW61S-QTDE-TOTAL                PIC  9(010).
             15 DCOMW61S-QTDE-RETORNADA            PIC  9(003).
             15 DCOMW61S-OCORRENCIA                OCCURS 050 TIMES.
                20 DCOMW61S-CCONTR-LIM-DESC        PIC  S9(009) COMP-3.
                20 DCOMW61S-CVRSAO-CONTR-LIM       PIC  S9(003) COMP-3.
                20 DCOMW61S-CPRODT                 PIC  S9(003) COMP-3.
                20 DCOMW61S-CSPROD-DESC-COML       PIC  S9(003) COMP-3.
                20 DCOMW61S-HULT-ATULZ             PIC  X(026).
                20 DCOMW61S-CFUNC-BDSCO            PIC  S9(009) COMP-3.
                20 DCOMW61S-CTERM                  PIC  X(008).
                20 DCOMW61S-FILLER                 PIC  X(039).
      *                                                                *
      *================================================================*
