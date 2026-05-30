      *================================================================*
      * BOOK PARA ACESSO A BASE DB2 - AREA DE ENTRADA:                 *
      *    -> DCOMB091: DB2PRD.TSPROD_DESC_COML                        *
      *----------------------------------------------------------------*
      * MODULOS DE ACESSO A BASE DE DADOS:                             *
      *    -> SELECT  - DCOM491S                                       *
      *    -> FETCH   - DCOM491F                                       *
      *    -> INSERT  - DCOM491I                                       *
      *    -> UPDATE  - DCOM491U                                       *
      *    -> DELETE  - DCOM491D                                       *
      *================================================================*
      *                                                                *
       05 DCOMW91E-HEADER.
          10 DCOMW91E-COD-LAYOUT       PIC  X(008) VALUE 'DCOMW91E'.
          10 DCOMW91E-TAM-LAYOUT       PIC  9(005) VALUE 01579.
      *
       05 DCOMW91E-BLOCO-ENTRADA.
          10 DCOMW91E-INSTRUCAO                    PIC  X(010).
      *
          10 DCOMW91E-ENTRADA.
             15 DCOMW91E-CPRODT                    PIC  S9(003) COMP-3.
             15 DCOMW91E-CSPROD-DESC-COML          PIC  S9(003) COMP-3.
             15 DCOMW91E-DINIC-VGCIA-SPROD         PIC  X(010).
             15 DCOMW91E-DFIM-VGCIA-SPROD          PIC  X(010).
             15 DCOMW91E-CTPO-DESC-COML            PIC  S9(003) COMP-3.
             15 DCOMW91E-ISPROD-DESC-COML          PIC  X(040).
             15 DCOMW91E-IRSUMO-SPROD-DESC         PIC  X(010).
             15 DCOMW91E-CSGL-SPROD-DESC           PIC  X(006).
             15 DCOMW91E-CSIT-DESC-COML            PIC  S9(003) COMP-3.
             15 DCOMW91E-HSIT-DESC-COML            PIC  X(026).
             15 DCOMW91E-CINDCD-VALDC-ROTRO        PIC  X(001).
             15 DCOMW91E-DVALDC-ROTRO-ELMTO        PIC  X(010).
             15 DCOMW91E-CFUNC-VALDC-ROTRO         PIC  S9(009) COMP-3.
             15 DCOMW91E-RSPROD-DESC-COML          PIC  X(240).
             15 DCOMW91E-CPRODT-BASE-INCL          PIC  S9(003) COMP-3.
             15 DCOMW91E-CSPROD-BASE-INCL          PIC  S9(003) COMP-3.
             15 DCOMW91E-RJUSTF-SIT-SPROD          PIC  X(240).
             15 DCOMW91E-HULT-ATULZ                PIC  X(026).
             15 DCOMW91E-CFUNC-BDSCO               PIC  S9(009) COMP-3.
             15 DCOMW91E-CTERM                     PIC  X(008).
             15 DCOMW91E-CPRODT-SERVC-OPER         PIC  S9(008) COMP-3.
             15 DCOMW91E-CCOOBC-PRODT-DESC         PIC  X(001).
      *
          10 DCOMW91E-BLOCO-VARIAVEL.
             15 DCOMW91E-V-FILLER                  PIC  X(300).

          10 DCOMW91E-BLOCO-PAGINACAO.
             15 DCOMW91E-MAX-OCORR                 PIC  9(003).
             15 DCOMW91E-INDICADOR-PAGINACAO       PIC  X(001).
                88 DCOMW91E-P-INICIAL              VALUE 'I'.
                88 DCOMW91E-P-PRIMEIRA             VALUE 'P'.
                88 DCOMW91E-P-SEGUINTE             VALUE 'S'.
                88 DCOMW91E-P-ANTERIOR             VALUE 'A'.
                88 DCOMW91E-P-ULTIMA               VALUE 'U'.
      *
          10 DCOMW91E-BLOCO-RESTART.
             15 DCOMW91E-CHAVE-INI.
                20 DCOMW91E-I-CPRODT               PIC  S9(003) COMP-3.
                20 DCOMW91E-I-CSPROD-DESC-COML     PIC  S9(003) COMP-3.
                20 FILLER                          PIC  X(200).
             15 DCOMW91E-CHAVE-FIM.
                20 DCOMW91E-F-CPRODT               PIC  S9(003) COMP-3.
                20 DCOMW91E-F-CSPROD-DESC-COML     PIC  S9(003) COMP-3.
                20 FILLER                          PIC  X(200).
      *
          10 DCOMW91E-FILLER                       PIC  X(189).
      *
