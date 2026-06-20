      *****************************************************************
      * 04.2014             S O N D A   I T                    CIBELE *
      *****************************************************************
      * NOME DA INC - GFCTWBBH   CLONE DA GFCTWBBF                    *
      * DESCRICAO   - HISTORICO DE BONIFICACAO 2.2 E                  *
      *               UTILIZADO PARA ATUALIZAR VALOR TOTAL DO SIGB    *
ST2507* TAMANHO     - 150                                             *
      *================================================================*
ST2507*               U L T I M A   A L T E R A C A O                *
      *----------------------------------------------------------------*
ST2507*    ANALISTA....:  STEFANINI                                    *
ST2507*    DATA........:  07 / 2025                                    *
ST2507*    OBJETIVO....:  CNPJ ALFANUMERICO                            *
      *================================================================*
      *
       01 GFCTWBBF-REG.
          03 WBBF-REG-GFCTVA.
             05 WBBF-GFCTB092.
                10 WBBF-DENVIO-MOVTO-TARIF    PIC  X(010).
                10 WBBF-CROTNA-ORIGE-MOVTO    PIC  X(004).
                10 WBBF-CNRO-ARQ-MOVTO        PIC S9(002)        COMP-3.
                10 WBBF-CSEQ-MOVTO            PIC S9(011)        COMP-3.
                10 WBBF-CSERVC-TARIF          PIC S9(005)        COMP-3.
                10 WBBF-CBCO-DSTNO-MOVTO      PIC S9(003)        COMP-3.
                10 WBBF-CAG-DSTNO-MOVTO       PIC S9(005)        COMP-3.
                10 WBBF-CCTA-DSTNO-MOVTO      PIC S9(013)        COMP-3.
ST2507*         10 WBBF-CPF-CNPJ              PIC S9(009)        COMP-3.
ST2507*         10 WBBF-FILIAL                PIC S9(005)        COMP-3.
ST2507          10 WBBF-CPF-CNPJ              PIC  X(009).
ST2507          10 WBBF-FILIAL                PIC  X(004).
                10 WBBF-CONTROLE              PIC  9(002).
          03 WBBF-REG-BONIFI.
             05 WBBF-GFCTB023.
                10 WBBF-CHAVE-B023.
                   15 WBBF-23-CSERVC-TARIF    PIC S9(5)V   USAGE COMP-3.
                   15 WBBF-23-CTPO-BONIF      PIC S9(3)V   USAGE COMP-3.
                   15 WBBF-23-CCRITR-BONIF    PIC S9(3)V   USAGE COMP-3.
                   15 WBBF-23-DINIC-REGRA-BONIF PIC X(10).
                   15 WBBF-23-CSEQ-BONIF-SERVC PIC S9(3)V  USAGE COMP-3.
                10 WBBF-23-VCOBR-BONIF      PIC S9(9)V9(2) USAGE COMP-3.
                10 WBBF-23-PDESC-BONIF      PIC S9(3)V9(4) USAGE COMP-3.
                10 WBBF-23-QFRANQ-BONIF       PIC S9(3)V   USAGE COMP-3.
                10 WBBF-23-CCART-ANLSE-GERC   PIC S9(5)V   USAGE COMP-3.
                10 WBBF-23-CGRP-CART-SIST     PIC S9(5)V   USAGE COMP-3.
                10 WBBF-23-CIDTFD-TPO-FAIXA   PIC S9(1)V   USAGE COMP-3.
             05 WBBF-DADOS-HIST.
                10 WBBF-CONSE-DADO-ANT        PIC S9(015)       COMP-3.
                10 WBBF-CONSE-DADO-ANT-R  REDEFINES WBBF-CONSE-DADO-ANT
                                              PIC S9(013)V9(2)  COMP-3.
                10 WBBF-CONSE-DADO-ATU        PIC S9(015)       COMP-3.
                10 WBBF-CONSE-DADO-ATU-R  REDEFINES WBBF-CONSE-DADO-ATU
                                              PIC S9(013)V9(2)   COMP-3.
                10 WBBF-VMIN-FAIXA-BONIF      PIC S9(013)V9(2)   COMP-3.
                10 WBBF-QMIN-FAIXA-BONIF      PIC S9(015)        COMP-3.
             05 WBBF-B043-RESTRICAO-S-N       PIC  X(001).
          03 WBBF-DATA-ABERTURA               PIC  9(009)        COMP-3.
          03 WBBF-ANOMES-APURACAO             PIC  9(006)        COMP-3.
          03 WBBF-DOCOR-EVNTO                 PIC  X(010).
          03 WBBF-PROGR-BENEF                 PIC  X(001).
      *       'S' - PARTICIPANTE P.B.
      *       ' ' - NAO PARTICIPANTE P.B.
      *
ST2507*   03 FILLER                           PIC  X(002).
ST2507    03 FILLER                           PIC  X(007).
