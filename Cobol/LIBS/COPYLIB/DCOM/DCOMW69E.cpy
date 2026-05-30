      *================================================================*
      * BOOK PARA ACESSO A BASE DB2 - AREA DE ENTRADA:                 *
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
       05 DCOMW69E-HEADER.
          10 DCOMW69E-COD-LAYOUT       PIC  X(008) VALUE 'DCOMW69E'.
          10 DCOMW69E-TAM-LAYOUT       PIC  9(005) VALUE 00537.
      *
       05 DCOMW69E-BLOCO-ENTRADA.
          10 DCOMW69E-INSTRUCAO                    PIC  X(010).
          10 DCOMW69E-ENTRADA.
             15 DCOMW69E-CPRODT                    PIC S9(003) COMP-3.
             15 DCOMW69E-CSPROD-DESC-COML          PIC S9(003) COMP-3.
             15 DCOMW69E-CTPO-AGPTO-ROTRO          PIC S9(003) COMP-3.
             15 DCOMW69E-CINDCD-OBRIG-AGPTO        PIC  X(001).
             15 DCOMW69E-CINDCD-SIT-AGPTO          PIC  X(001).
             15 DCOMW69E-CINDCD-CONCL-ROTRO        PIC  X(001).
             15 DCOMW69E-HULT-ATULZ                PIC  X(026).
             15 DCOMW69E-CFUNC-BDSCO               PIC S9(009) COMP-3.
             15 DCOMW69E-CTERM                     PIC  X(008).
          10 DCOMW69E-BLOCO-VARIAVEL.
             15 DCOMW69E-V-FILLER                  PIC  X(300).
          10 DCOMW69E-BLOCO-PAGINACAO.
             15 DCOMW69E-MAX-OCORR                 PIC  9(003).
             15 DCOMW69E-INDICADOR-PAGINACAO       PIC  X(001).
                88 DCOMW69E-P-INICIAL              VALUE 'I'.
                88 DCOMW69E-P-PRIMEIRA             VALUE 'P'.
                88 DCOMW69E-P-SEGUINTE             VALUE 'S'.
                88 DCOMW69E-P-ANTERIOR             VALUE 'A'.
                88 DCOMW69E-P-ULTIMA               VALUE 'U'.
          10 DCOMW69E-BLOCO-RESTART.
             15 DCOMW69E-CHAVE-INI.
                20 DCOMW69E-I-CPRODT               PIC S9(003) COMP-3.
                20 DCOMW69E-I-CSPROD-DESC-COML     PIC S9(003) COMP-3.
                20 DCOMW69E-I-CTPO-AGPTO-ROTRO     PIC S9(003) COMP-3.
             15 DCOMW69E-CHAVE-FIM.
                20 DCOMW69E-F-CPRODT               PIC S9(003) COMP-3.
                20 DCOMW69E-F-CSPROD-DESC-COML     PIC S9(003) COMP-3.
                20 DCOMW69E-F-CTPO-AGPTO-ROTRO     PIC S9(003) COMP-3.
          10 DCOMW69E-FILLER                       PIC  X(150).
      *                                                                *
