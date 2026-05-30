      *================================================================*
      * BOOK PARA ACESSO A BASE DB2 - AREA DE SAIDA:                   *
      *    -> DCOMB091: DB2PRD.TSPROD_DESC_COML                        *
      *----------------------------------------------------------------*
      * MODULOS DE ACESSO A BASE DE DADOS:                             *
      *    -> SELECT  - DCOM491S                                       *
      *    -> FETCH   - DCOM491F                                       *
      *    -> INSERT  - DCOM491I                                       *
      *    -> UPDATE  - DCOM491U                                       *
      *    -> DELETE  - DCOM491D                                       *
      *================================================================*
      *
       05 DCOMW91S-HEADER.
          10 DCOMW91S-COD-LAYOUT       PIC  X(008) VALUE 'DCOMW91S'.
          10 DCOMW91S-TAM-LAYOUT       PIC  9(005) VALUE 23150.
      *
          10 DCOMW91S-BLOCO-SAIDA.
             15 DCOMW91S-QTDE-TOTAL                PIC  9(010).
             15 DCOMW91S-QTDE-RETORNADA            PIC  9(003).
             15 DCOMW91S-OCORRENCIA                OCCURS 050 TIMES.
                20 DCOMW91S-CPRODT                 PIC  S9(003) COMP-3.
                20 DCOMW91S-CSPROD-DESC-COML       PIC  S9(003) COMP-3.
                20 DCOMW91S-DINIC-VGCIA-SPROD      PIC  X(010).
                20 DCOMW91S-DFIM-VGCIA-SPROD       PIC  X(010).
                20 DCOMW91S-CTPO-DESC-COML         PIC  S9(003) COMP-3.
                20 DCOMW91S-ISPROD-DESC-COML       PIC  X(040).
                20 DCOMW91S-IRSUMO-SPROD-DESC      PIC  X(010).
                20 DCOMW91S-CSGL-SPROD-DESC        PIC  X(006).
                20 DCOMW91S-CSIT-DESC-COML         PIC  S9(003) COMP-3.
                20 DCOMW91S-HSIT-DESC-COML         PIC  X(026).
                20 DCOMW91S-CINDCD-VALDC-ROTRO     PIC  X(001).
                20 DCOMW91S-DVALDC-ROTRO-ELMTO     PIC  X(010).
                20 DCOMW91S-CFUNC-VALDC-ROTRO      PIC  S9(009) COMP-3.
                20 DCOMW91S-RSPROD-DESC-COML       PIC  X(140).
                20 DCOMW91S-CPRODT-BASE-INCL       PIC  S9(003) COMP-3.
                20 DCOMW91S-CSPROD-BASE-INCL       PIC  S9(003) COMP-3.
                20 DCOMW91S-RJUSTF-SIT-SPROD       PIC  X(140).
                20 DCOMW91S-HULT-ATULZ             PIC  X(026).
                20 DCOMW91S-CFUNC-BDSCO            PIC  S9(009) COMP-3.
                20 DCOMW91S-CTERM                  PIC  X(008).
                20 DCOMW91S-CPRODT-SERVC-OPER      PIC  S9(008) COMP-3.
                20 DCOMW91S-CCOOBC-PRODT-DESC      PIC  X(001).
             15 DCOMW91S-FILLER                    PIC  X(374).
      *                                                                *
      *================================================================*
