      *================================================================*
      * BOOK PARA ACESSO A BASE DB2 - AREA DE ENTRADA:                 *
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
       05 DCOMWF0E-HEADER.
          10 DCOMWF0E-COD-LAYOUT       PIC  X(008) VALUE 'DCOMWF0E'.
          10 DCOMWF0E-TAM-LAYOUT       PIC  9(005) VALUE 00634.
      *
       05 DCOMWF0E-BLOCO-ENTRADA.
          10 DCOMWF0E-INSTRUCAO                    PIC  X(010).
          10 DCOMWF0E-ENTRADA.
             15 DCOMWF0E-CPRODT                    PIC S9(003) COMP-3.
             15 DCOMWF0E-CSPROD-DESC-COML          PIC S9(003) COMP-3.
             15 DCOMWF0E-CMEIO-ENTRD-DESC          PIC S9(003) COMP-3.
             15 DCOMWF0E-CTPO-GARNT                PIC S9(003) COMP-3.
             15 DCOMWF0E-HINIC-VGCIA-DESC          PIC  X(026).
             15 DCOMWF0E-HFIM-VGCIA-DESC           PIC  X(026).
             15 DCOMWF0E-CFUNC-BDSCO               PIC S9(009) COMP-3.
             15 DCOMWF0E-HULT-ATULZ                PIC  X(026).
             15 DCOMWF0E-CTERM                     PIC  X(008).
          10 DCOMWF0E-BLOCO-VARIAVEL.
             15 DCOMWF0E-V-FILLER                  PIC  X(300).
          10 DCOMWF0E-BLOCO-PAGINACAO.
             15 DCOMWF0E-MAX-OCORR                 PIC  9(003).
             15 DCOMWF0E-INDICADOR-PAGINACAO       PIC  X(001).
                88 DCOMWF0E-P-INICIAL              VALUE 'I'.
                88 DCOMWF0E-P-PRIMEIRA             VALUE 'P'.
                88 DCOMWF0E-P-SEGUINTE             VALUE 'S'.
                88 DCOMWF0E-P-ANTERIOR             VALUE 'A'.
                88 DCOMWF0E-P-ULTIMA               VALUE 'U'.
          10 DCOMWF0E-BLOCO-RESTART.
             15 DCOMWF0E-CHAVE-INI.
                20 DCOMWF0E-I-CPRODT               PIC S9(003) COMP-3.
                20 DCOMWF0E-I-CSPROD-DESC-COML     PIC S9(003) COMP-3.
                20 DCOMWF0E-I-CMEIO-ENTRD-DESC     PIC S9(003) COMP-3.
                20 DCOMWF0E-I-CTPO-GARNT           PIC S9(003) COMP-3.
                20 DCOMWF0E-I-HINIC-VGCIA-DESC     PIC  X(026).
             15 DCOMWF0E-CHAVE-FIM.
                20 DCOMWF0E-F-CPRODT               PIC S9(003) COMP-3.
                20 DCOMWF0E-F-CSPROD-DESC-COML     PIC S9(003) COMP-3.
                20 DCOMWF0E-F-CMEIO-ENTRD-DESC     PIC S9(003) COMP-3.
                20 DCOMWF0E-F-CTPO-GARNT           PIC S9(003) COMP-3.
                20 DCOMWF0E-F-HINIC-VGCIA-DESC     PIC  X(026).
          10 DCOMWF0E-FILLER                       PIC  X(140).
      *                                                                *
