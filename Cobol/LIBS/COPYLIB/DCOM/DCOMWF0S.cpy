      *================================================================*
      * BOOK PARA ACESSO A BASE DB2 - AREA DE SAIDA:                   *
      *    -> DCOMB0F0: DB2PRD.TSPROD_GARNT_CREDT                      *
      *----------------------------------------------------------------*
      * MODULOS DE ACESSO A BASE DE DADOS:                             *
      *    -> SELECT  - DCOM4F0S                                       *
      *    -> FETCH   - DCOM4F0F                                       *
      *    -> INSERT  - DCOM4F0I                                       *
      *    -> UPDATE  - DCOM4F0U                                       *
      *    -> DELETE  - DCOM4F0D                                       *
      *================================================================*
      *                                                                *
       05 DCOMWF0S-HEADER.
          10 DCOMWF0S-COD-LAYOUT   PIC  X(008) VALUE 'DCOMWF0S'.
          10 DCOMWF0S-TAM-LAYOUT   PIC  9(005) VALUE 05100.
      *
          10 DCOMWF0S-BLOCO-SAIDA.
             15 DCOMWF0S-QTDE-TOTAL            PIC  9(010).
             15 DCOMWF0S-QTDE-RETORNADA        PIC  9(003).
             15 DCOMWF0S-OCORRENCIA            OCCURS 050 TIMES.
                20 DCOMWF0S-CPRODT             PIC  S9(003) COMP-3.
                20 DCOMWF0S-CSPROD-DESC-COML   PIC  S9(003) COMP-3.
                20 DCOMWF0S-CMEIO-ENTRD-DESC   PIC  S9(003) COMP-3.
                20 DCOMWF0S-CTPO-GARNT         PIC  S9(003) COMP-3.
                20 DCOMWF0S-HINIC-VGCIA-DESC   PIC  X(026).
                20 DCOMWF0S-HFIM-VGCIA-DESC    PIC  X(026).
                20 DCOMWF0S-CFUNC-BDSCO        PIC  S9(009) COMP-3.
                20 DCOMWF0S-HULT-ATULZ         PIC  X(026).
                20 DCOMWF0S-CTERM              PIC  X(008).
             15 DCOMWF0S-FILLER                PIC  X(124).
      *
