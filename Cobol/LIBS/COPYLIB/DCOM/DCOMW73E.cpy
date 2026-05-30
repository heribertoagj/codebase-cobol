      *================================================================*
      * BOOK PARA ACESSO A BASE DB2 - AREA DE ENTRADA:                 *
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
       05 DCOMW73E-HEADER.
          10 DCOMW73E-COD-LAYOUT       PIC  X(008) VALUE 'DCOMW73E'.
          10 DCOMW73E-TAM-LAYOUT       PIC  9(005) VALUE 01000.
      *
       05 DCOMW73E-BLOCO-ENTRADA.
          10 DCOMW73E-INSTRUCAO                    PIC  X(010).
      *
          10 DCOMW73E-ENTRADA.
             15 DCOMW73E-CPRODT                    PIC S9(003) COMP-3.
             15 DCOMW73E-CSPROD-DESC-COML          PIC S9(003) COMP-3.
             15 DCOMW73E-CINDCD-PSSOA-TOMAD        PIC  X(001).
             15 DCOMW73E-CSGMTO-CLI                PIC S9(003) COMP-3.
             15 DCOMW73E-CRTING                    PIC  X(003).
             15 DCOMW73E-NNVEL-RTING               PIC S9(002) COMP-3.
             15 DCOMW73E-HINIC-VGCIA-NVEL          PIC  X(026).
             15 DCOMW73E-CMEIO-ENTRD-DESC          PIC S9(003) COMP-3.
             15 DCOMW73E-DINIC-TX-JURO             PIC  X(010).
             15 DCOMW73E-NSEQ-BANDA-TX-REDE        PIC S9(005) COMP-3.
             15 DCOMW73E-DFIM-TX-JURO              PIC  X(010).
             15 DCOMW73E-CINDCD-TX-VLR-MIN         PIC  X(002).
             15 DCOMW73E-VMIN-FAIXA-OPER           PIC S9(015)V9(002)
                                                               COMP-3.
             15 DCOMW73E-CINDCD-TX-VLR-MAX         PIC  X(002).
             15 DCOMW73E-VMAX-FAIXA-OPER           PIC S9(015)V9(002)
                                                               COMP-3.
             15 DCOMW73E-CINDCD-TX-PRZ-MIN         PIC  X(002).
             15 DCOMW73E-TMIN-OPER-DESC            PIC S9(005) COMP-3.
             15 DCOMW73E-CINDCD-TX-PRZ-MAX         PIC  X(002).
             15 DCOMW73E-TMAX-OPER-DESC            PIC S9(005) COMP-3.
             15 DCOMW73E-PMIN-TX-REDE-DESC         PIC S9(003)V9(003)
                                                               COMP-3.
             15 DCOMW73E-PMAX-TX-REDE-DESC         PIC S9(003)V9(003)
                                                               COMP-3.
             15 DCOMW73E-CINDCD-FREQ-TX            PIC  X(002).
             15 DCOMW73E-HULT-ATULZ                PIC  X(026).
             15 DCOMW73E-CFUNC-BDSCO               PIC S9(009) COMP-3.
             15 DCOMW73E-CTERM                     PIC  X(008).

          10 DCOMW73E-BLOCO-PAGINACAO.
             15 DCOMW73E-MAX-OCORR                 PIC  9(003).
             15 DCOMW73E-TOTALIZAR                 PIC  X(001).
             15 DCOMW73E-INDICADOR-PAGINACAO       PIC  X(001).
                88 DCOMW73E-P-INICIAL              VALUE 'I'.
                88 DCOMW73E-P-PRIMEIRA             VALUE 'P'.
                88 DCOMW73E-P-SEGUINTE             VALUE 'S'.
                88 DCOMW73E-P-ANTERIOR             VALUE 'A'.
                88 DCOMW73E-P-ULTIMA               VALUE 'U'.
      *
          10 DCOMW73E-BLOCO-RESTART.
             15 DCOMW73E-CHAVE-INI.
                20 DCOMW73E-I-CPRODT               PIC S9(003) COMP-3.
                20 DCOMW73E-I-CSPROD-DESC-COML     PIC S9(003) COMP-3.
                20 DCOMW73E-I-CINDCD-PSSOA-TOMAD   PIC  X(001).
                20 DCOMW73E-I-CSGMTO-CLI           PIC S9(003) COMP-3.
                20 DCOMW73E-I-CRTING               PIC  X(003).
                20 DCOMW73E-I-NNVEL-RTING          PIC S9(002) COMP-3.
                20 DCOMW73E-I-HINIC-VGCIA-NVEL     PIC  X(026).
                20 DCOMW73E-I-CMEIO-ENTRD-DESC     PIC S9(003) COMP-3.
                20 DCOMW73E-I-DINIC-TX-JURO        PIC  X(010).
                20 DCOMW73E-I-NSEQ-BANDA-TX-REDE   PIC S9(005) COMP-3.
                20 FILLER                          PIC  X(249).

             15 DCOMW73E-CHAVE-FIM.
                20 DCOMW73E-F-CPRODT               PIC S9(003) COMP-3.
                20 DCOMW73E-F-CSPROD-DESC-COML     PIC S9(003) COMP-3.
                20 DCOMW73E-F-CINDCD-PSSOA-TOMAD   PIC  X(001).
                20 DCOMW73E-F-CSGMTO-CLI           PIC S9(003) COMP-3.
                20 DCOMW73E-F-CRTING               PIC  X(003).
                20 DCOMW73E-F-NNVEL-RTING          PIC S9(002) COMP-3.
                20 DCOMW73E-F-HINIC-VGCIA-NVEL     PIC  X(026).
                20 DCOMW73E-F-CMEIO-ENTRD-DESC     PIC S9(003) COMP-3.
                20 DCOMW73E-F-DINIC-TX-JURO        PIC  X(010).
                20 DCOMW73E-F-NSEQ-BANDA-TX-REDE   PIC S9(005) COMP-3.
                20 FILLER                          PIC  X(249).
      *
          10 DCOMW73E-FILLER                       PIC  X(224).
      *
