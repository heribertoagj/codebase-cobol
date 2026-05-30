      *================================================================*
      * BOOK PARA ACESSO A BASE DB2 - AREA DE ENTRADA:                 *
      *    -> DCOMB026: DB2PRD.TEVNTO_DESC_COML                        *
      *----------------------------------------------------------------*
      * MODULOS DE ACESSO A BASE DE DADOS:                             *
      *    -> SELECT  - DCOM426S                                       *
      *    -> FETCH   - DCOM426F                                       *
      *    -> INSERT  - DCOM426I                                       *
      *    -> UPDATE  - DCOM426U                                       *
      *    -> DELETE  - DCOM426D                                       *
      *================================================================*
      *                                                                *
       05 DCOMW26E-HEADER.
          10 DCOMW26E-COD-LAYOUT       PIC  X(008) VALUE 'DCOMWE26'.
          10 DCOMW26E-TAM-LAYOUT       PIC  9(005) VALUE 00300.
      *
       05 DCOMW26E-BLOCO-ENTRADA.
          10 DCOMW26E-INSTRUCAO                    PIC  X(010).
      *
          10 DCOMW26E-ENTRADA.
             15 DCOMW26E-CEVNTO-DESC-COML          PIC S9(003) COMP-3.
             15 DCOMW26E-IEVNTO-DESC-COML          PIC  X(040).
             15 DCOMW26E-IRSUMO-EVNTO-DESC         PIC  X(020).
             15 DCOMW26E-CTRANS-PROG-ATULZ         PIC  X(008).
             15 DCOMW26E-HULT-ATULZ                PIC  X(026).
      *
          10 DCOMW26E-BLOCO-VARIAVEL.
             15 DCOMW26E-V-FILLER                  PIC  X(100).
      *
          10 DCOMW26E-BLOCO-PAGINACAO.
             15 DCOMW26E-MAX-OCORR                 PIC  9(003).
             15 DCOMW26E-INDICADOR-PAGINACAO       PIC  X(001).
                88 DCOMW26E-P-INICIAL              VALUE 'I'.
                88 DCOMW26E-P-PRIMEIRA             VALUE 'P'.
                88 DCOMW26E-P-SEGUINTE             VALUE 'S'.
                88 DCOMW26E-P-ANTERIOR             VALUE 'A'.
                88 DCOMW26E-P-ULTIMA               VALUE 'U'.
      *
          10 DCOMW26E-BLOCO-RESTART.
             15 DCOMW26E-CHAVE-INI.
                20 DCOMW26E-I-CEVNTO-DESC-COML     PIC  S9(003) COMP-3.
             15 DCOMW26E-CHAVE-FIM.
                20 DCOMW26E-F-CEVNTO-DESC-COML     PIC  S9(003) COMP-3.
      *
          10 DCOMW26E-FILLER                       PIC  X(073).
