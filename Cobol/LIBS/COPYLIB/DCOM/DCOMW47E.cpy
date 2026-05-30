      *================================================================*
      * BOOK PARA ACESSO A BASE DB2 - AREA DE ENTRADA:                 *
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
       05 DCOMW47E-HEADER.
          10 DCOMW47E-COD-LAYOUT       PIC  X(008) VALUE 'DCOMW47E'.
          10 DCOMW47E-TAM-LAYOUT       PIC  9(005) VALUE 01038.
      *
       05 DCOMW47E-BLOCO-ENTRADA.
          10 DCOMW47E-INSTRUCAO                    PIC  X(010).
      *
          10 DCOMW47E-ENTRADA.
             15 DCOMW47E-CPRODT                    PIC  S9(003) COMP-3.
             15 DCOMW47E-IPRODT                    PIC  X(060).
             15 DCOMW47E-IABREV-PRODT              PIC  X(012).
             15 DCOMW47E-CSGL-PRODT-DESC           PIC  X(006).
             15 DCOMW47E-CIDTFD-TX-MIN-JURO        PIC  S9(005) COMP-3.
             15 DCOMW47E-CIDTFD-TX-MAX-JURO        PIC  S9(005) COMP-3.
             15 DCOMW47E-HULT-ATULZ                PIC  X(026).
             15 DCOMW47E-CFUNC-BDSCO               PIC  S9(009) COMP-3.
             15 DCOMW47E-CTERM                     PIC  X(008).
             15 DCOMW47E-CIDTFD-TX-CUSTO           PIC  S9(005) COMP-3.
      *
          10 DCOMW47E-BLOCO-VARIAVEL.
             15 DCOMW47E-V-FILLER                  PIC  X(300).
      *
          10 DCOMW47E-BLOCO-PAGINACAO.
             15 DCOMW47E-MAX-OCORR                 PIC  9(003).
             15 DCOMW47E-INDICADOR-PAGINACAO       PIC  X(001).
                88 DCOMW47E-P-INICIAL              VALUE 'I'.
                88 DCOMW47E-P-PRIMEIRA             VALUE 'P'.
                88 DCOMW47E-P-SEGUINTE             VALUE 'S'.
                88 DCOMW47E-P-ANTERIOR             VALUE 'A'.
                88 DCOMW47E-P-ULTIMA               VALUE 'U'.
      *
          10 DCOMW47E-BLOCO-RESTART.
             15 DCOMW47E-CHAVE-INI.
                20 DCOMW47E-I-CPRODT               PIC  S9(009) COMP-3.
                20 FILLER                          PIC  X(200).
             15 DCOMW47E-CHAVE-FIM.
                20 DCOMW47E-F-CPRODT               PIC  S9(009) COMP-3.
                20 FILLER                          PIC  X(200).
      *
          10 DCOMW47E-FILLER                       PIC  X(173).
      *
