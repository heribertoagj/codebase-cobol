      *================================================================*
      * BOOK PARA ACESSO A BASE DB2 - AREA DE SAIDA:                   *
      *    -> DCOMB073: DB2PRD.TSPROD_TX_REDE                          *
      *----------------------------------------------------------------*
      * MODULOS DE ACESSO A BASE DE DADOS:                             *
      *    -> SELECT  - DCOM473S                                       *
      *    -> FETCH   - DCOM473F                                       *
      *    -> INSERT  - DCOM473I                                       *
      *    -> UPDATE  - DCOM473U                                       *
      *    -> DELETE  - DCOM473D                                       *
      *================================================================*
      *                                                                *
       05 DCOMW73S-HEADER.
          10 DCOMW73S-COD-LAYOUT       PIC  X(008) VALUE 'DCOMW73S'.
          10 DCOMW73S-TAM-LAYOUT       PIC  9(005) VALUE  08000.
      *
          10 DCOMW73S-BLOCO-SAIDA.
             15 DCOMW73S-QTDE-TOTAL                PIC S9(010) COMP-3.
             15 DCOMW73S-QTDE-RETORNADA            PIC S9(003) COMP-3.
             15 DCOMW73S-OCORRENCIA                OCCURS 050 TIMES.
                20 DCOMW73S-CPRODT                 PIC S9(003) COMP-3.
                20 DCOMW73S-CSPROD-DESC-COML       PIC S9(003) COMP-3.
                20 DCOMW73S-CINDCD-PSSOA-TOMAD     PIC  X(001).
                20 DCOMW73S-CSGMTO-CLI             PIC S9(003) COMP-3.
                20 DCOMW73S-CRTING                 PIC  X(003).
                20 DCOMW73S-NNVEL-RTING            PIC S9(002) COMP-3.
                20 DCOMW73S-HINIC-VGCIA-NVEL       PIC  X(026).
                20 DCOMW73S-CMEIO-ENTRD-DESC       PIC S9(003) COMP-3.
                20 DCOMW73S-DINIC-TX-JURO          PIC  X(010).
                20 DCOMW73S-NSEQ-BANDA-TX-REDE     PIC S9(005) COMP-3.
                20 DCOMW73S-DFIM-TX-JURO           PIC  X(010).
                20 DCOMW73S-CINDCD-TX-VLR-MIN      PIC  X(002).
                20 DCOMW73S-VMIN-FAIXA-OPER        PIC S9(015)V9(002)
                                                               COMP-3.
                20 DCOMW73S-CINDCD-TX-VLR-MAX      PIC  X(002).
                20 DCOMW73S-VMAX-FAIXA-OPER        PIC S9(015)V9(002)
                                                               COMP-3.
                20 DCOMW73S-CINDCD-TX-PRZ-MIN      PIC  X(002).
                20 DCOMW73S-TMIN-OPER-DESC         PIC S9(005) COMP-3.
                20 DCOMW73S-CINDCD-TX-PRZ-MAX      PIC  X(002).
                20 DCOMW73S-TMAX-OPER-DESC         PIC S9(005) COMP-3.
                20 DCOMW73S-PMIN-TX-REDE-DESC      PIC S9(003)V9(003)
                                                               COMP-3.
                20 DCOMW73S-PMAX-TX-REDE-DESC      PIC S9(003)V9(003)
                                                               COMP-3.
                20 DCOMW73S-CINDCD-FREQ-TX         PIC  X(002).
                20 DCOMW73S-HULT-ATULZ             PIC  X(026).
                20 DCOMW73S-CFUNC-BDSCO            PIC S9(009) COMP-3.
                20 DCOMW73S-CTERM                  PIC  X(008).
      *
             15 DCOMW73S-FILLER                    PIC  X(779).
