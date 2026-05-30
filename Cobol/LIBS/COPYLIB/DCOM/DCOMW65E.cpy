      *================================================================*
      * BOOK PARA ACESSO A BASE DB2 - AREA DE ENTRADA:                 *
      *    -> DCOMB065: DB2PRD.TSPROD_MEIO_LIBRC                       *
      *----------------------------------------------------------------*
      * MODULOS DE ACESSO A BASE DE DADOS:                             *
      *    -> SELECT  - DCOM465S                                       *
      *    -> FETCH   - DCOM465F                                       *
      *    -> INSERT  - DCOM465I                                       *
      *    -> UPDATE  - DCOM465U                                       *
      *    -> DELETE  - DCOM465D                                       *
      *================================================================*
      *                                                                *
       05 DCOMW65E-READER.
          10 DCOMW65E-COD-LAYOUT       PIC  X(008) VALUE 'DCOMW65E'.
          10 DCOMW65E-TAM-LAYOUT       PIC  9(005) VALUE 00709.
      *
       05 DCOMW65E-BLOCO-ENTRADA.
          10 DCOMW65E-INSTRUCAO               PIC  X(010).
      *
          10 DCOMW65E-ENTRADA.
             15 DCOMW65E-CPRODT               PIC S9(03) COMP-3.
             15 DCOMW65E-CSPROD-DESC-COML     PIC S9(03) COMP-3.
             15 DCOMW65E-CMEIO-LIBRC-DESC     PIC S9(03) COMP-3.
             15 DCOMW65E-HINIC-MEIO-LIBRC     PIC X(26).
             15 DCOMW65E-HFIM-MEIO-LIBRC      PIC X(26).
             15 DCOMW65E-HULT-ATULZ           PIC X(26).
             15 DCOMW65E-CFUNC-BDSCO          PIC S9(09) COMP-3.
             15 DCOMW65E-CTERM                PIC X(08).
      *
          10 DCOMW65E-BLOCO-PAGINACAO.
             15 DCOMW65E-MAX-OCORR                 PIC  9(003).
             15 DCOMW65E-INDICADOR-PAGINACAO       PIC  X(001).
                88 DCOMW65E-P-INICIAL              VALUE 'I'.
                88 DCOMW65E-P-PRIMEIRA             VALUE 'P'.
                88 DCOMW65E-P-SEGUINTE             VALUE 'S'.
                88 DCOMW65E-P-ANTERIOR             VALUE 'A'.
                88 DCOMW65E-P-ULTIMA               VALUE 'U'.
      *
          10 DCOMW65E-BLOCO-RESTART.
             15 DCOMW65E-CHAVE-INI.
                20 DCOMW65E-I-CPRODT               PIC S9(003) COMP-3.
                20 DCOMW65E-I-CSPROD-DESC-COML     PIC S9(003) COMP-3.
                20 DCOMW65E-I-CMEIO-LIBRC-DESC     PIC S9(003) COMP-3.
                20 FILLER                          PIC  X(200).
             15 DCOMW65E-CHAVE-FIM.
                20 DCOMW65E-F-CPRODT               PIC S9(003) COMP-3.
                20 DCOMW65E-F-CSPROD-DESC-COML     PIC S9(003) COMP-3.
                20 DCOMW65E-F-CMEIO-LIBRC-DESC     PIC S9(003) COMP-3.
                20 FILLER                          PIC  X(200).
      *
          10 DCOMW65E-FILLER                       PIC  X(173).
      *
