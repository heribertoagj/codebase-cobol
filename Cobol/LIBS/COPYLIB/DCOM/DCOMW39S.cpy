      *================================================================*
      * BOOK PARA ACESSO A BASE DB2 - AREA DE SAIDA:                   *
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
       05 DCOMW39S-HEADER.
          10 DCOMW39S-COD-LAYOUT       PIC  X(008) VALUE 'DCOMW39S'.
          10 DCOMW39S-TAM-LAYOUT       PIC  9(005) VALUE 05626.
      *
          10 DCOMW39S-BLOCO-SAIDA.
             15 DCOMW39S-QTDE-TOTAL                PIC  9(010).
             15 DCOMW39S-QTDE-RETORNADA            PIC  9(003).
             15 DCOMW39S-OCORRENCIA                OCCURS 050 TIMES.
                20 DCOMW39S-CPRODT                 PIC  S9(003) COMP-3.
                20 DCOMW39S-DINIC-PARMZ-CONTR      PIC  X(010).
                20 DCOMW39S-TMAX-CONTR-LIM         PIC  S9(005) COMP-3.
                20 DCOMW39S-CINDCD-PRZ-CONTR       PIC  X(002).
                20 DCOMW39S-CINDCD-RENOV-AUTOM     PIC  X(001).
                20 DCOMW39S-TCAREN-RENOV-CONTR     PIC  S9(003) COMP-3.
                20 DCOMW39S-CINDCD-PRZ-RENOV       PIC  X(002).
                20 DCOMW39S-VMAX-RENOV-AUTOM       PIC S9(015)V9(002)
                                                                 COMP-3.
                20 DCOMW39S-CINDCD-TARIF-ADTTO     PIC  X(001).
                20 DCOMW39S-CIDTFD-TARIF-ADTTO     PIC  S9(005) COMP-3.
                20 DCOMW39S-DFIM-PARMZ-CONTR       PIC  X(010).
                20 DCOMW39S-HULT-ATULZ             PIC  X(026).
                20 DCOMW39S-CFUNC-BDSCO            PIC  S9(009) COMP-3.
                20 DCOMW39S-CTERM                  PIC  X(008).
                20 DCOMW39S-CINDCD-ENVIO-SDO       PIC  X(001).
                20 DCOMW39S-FILLER                 PIC  X(027).
      *                                                                *
      *================================================================*
