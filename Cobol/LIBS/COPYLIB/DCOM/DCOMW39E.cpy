      *================================================================*
      * BOOK PARA ACESSO A BASE DB2 - AREA DE ENTRADA:                 *
      *    -> DCOMB039: DB2PRD.TPARMZ_CONTR_LIM                        *
      *----------------------------------------------------------------*
      * MODULOS DE ACESSO A BASE DE DADOS:                             *
      *    -> SELECT  - DCOM439S                                       *
      *    -> FETCH   - DCOM439F                                       *
      *    -> INSERT  - DCOM439I                                       *
      *    -> UPDATE  - DCOM439U                                       *
      *    -> DELETE  - DCOM439D                                       *
      *================================================================*
      *                                                                *
       05 DCOMW39E-HEADER.
          10 DCOMW39E-COD-LAYOUT       PIC  X(008) VALUE 'DCOMW39E'.
          10 DCOMW39E-TAM-LAYOUT       PIC  9(005) VALUE 00580.
      *
       05 DCOMW39E-BLOCO-ENTRADA.
          10 DCOMW39E-INSTRUCAO                    PIC  X(010).
      *
          10 DCOMW39E-ENTRADA.
             15 DCOMW39E-CPRODT                    PIC  S9(003) COMP-3.
             15 DCOMW39E-DINIC-PARMZ-CONTR         PIC  X(010).
             15 DCOMW39E-TMAX-CONTR-LIM            PIC  S9(005) COMP-3.
             15 DCOMW39E-CINDCD-PRZ-CONTR          PIC  X(002).
             15 DCOMW39E-CINDCD-RENOV-AUTOM        PIC  X(001).
             15 DCOMW39E-TCAREN-RENOV-CONTR        PIC  S9(003) COMP-3.
             15 DCOMW39E-CINDCD-PRZ-RENOV          PIC  X(002).
             15 DCOMW39E-VMAX-RENOV-AUTOM          PIC S9(015)V9(002)
                                                                 COMP-3.
             15 DCOMW39E-CINDCD-TARIF-ADTTO        PIC  X(001).
             15 DCOMW39E-CIDTFD-TARIF-ADTTO        PIC  S9(005) COMP-3.
             15 DCOMW39E-DFIM-PARMZ-CONTR          PIC  X(010).
             15 DCOMW39E-HULT-ATULZ                PIC  X(026).
             15 DCOMW39E-CFUNC-BDSCO               PIC  S9(009) COMP-3.
             15 DCOMW39E-CTERM                     PIC  X(008).
             15 DCOMW39E-CINDCD-ENVIO-SDO          PIC  X(001).
             15 DCOMW39E-FILLER                    PIC  X(027).
      *
          10 DCOMW39E-BLOCO-VARIAVEL.
             15 DCOMW39E-V-FILLER                  PIC  X(300).
      *
          10 DCOMW39E-BLOCO-PAGINACAO.
             15 DCOMW39E-MAX-OCORR                 PIC  9(003).
             15 DCOMW39E-INDICADOR-PAGINACAO       PIC  X(001).
                88 DCOMW39E-P-INICIAL              VALUE 'I'.
                88 DCOMW39E-P-PRIMEIRA             VALUE 'P'.
                88 DCOMW39E-P-SEGUINTE             VALUE 'S'.
                88 DCOMW39E-P-ANTERIOR             VALUE 'A'.
                88 DCOMW39E-P-ULTIMA               VALUE 'U'.
      *
          10 DCOMW39E-BLOCO-RESTART.
             15 DCOMW39E-CHAVE-INI.
                20 DCOMW39E-I-CPRODT               PIC  S9(003) COMP-3.
                20 DCOMW39E-I-DINIC-PARMZ-CONTR    PIC  X(010).
                20 DCOMW39E-I-FILLER               PIC  X(017).
             15 DCOMW39E-CHAVE-FIM.
                20 DCOMW39E-F-CPRODT               PIC  S9(003) COMP-3.
                20 DCOMW39E-F-DINIC-PARMZ-CONTR    PIC  X(010).
                20 DCOMW39E-F-FILLER               PIC  X(017).
      *
          10 DCOMW39E-FILLER                       PIC  X(083).
      *
