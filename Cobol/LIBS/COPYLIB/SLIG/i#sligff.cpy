      ***===========================================================***
      *** NOME INC                                     LENGTH=5161  ***
      *** I#SLIGFF                                                  ***
      ***-----------------------------------------------------------***
      **                                                             **
      **  DESCRICAO: SISTEMA DE INDICES DE LIQUIDEZ - SLIG           **
      **     VISAO CEDENTE/SACADO E VISAO CUSTODIANTE/EMITENTE       **
      **                                                             **
      ***===========================================================***
      *DATA        PROJETO                                 ANALISTA    *
      *01/08/2025  CNPJ ALFANUMERICO                       LUCIANA SATO*
      *----------------------------------------------------------------*
       05 SLIGFF-REGISTRO.
        10 SLIGFF-HEADER.
         15 SLIGFF-H-NOM-ARQ                PIC X(049).
         15 SLIGFF-H-PV-1                   PIC X(001).
         15 SLIGFF-H-DATA-GER               PIC X(008).
         15 SLIGFF-H-HORA-GER               PIC X(006).
         15 SLIGFF-H-RESERVA                PIC X(5097).
        10 SLIGFF-DETALHE                   REDEFINES SLIGFF-HEADER.
CNPJ7C   15 SLIGFF-CPF-CNPJ                 PIC X(09).
         15 FILLER                          PIC X(001).
CNPJ7C   15 SLIGFF-FILIAL                   PIC X(04).
         15 FILLER                          PIC X(001).
         15 SLIGFF-CONTROLE                 PIC 9(02).
         15 FILLER                          PIC X(001).
         15 SLIGFF-PERIODO-DE               PIC 9(08).
         15 FILLER                          PIC X(001).
         15 SLIGFF-PERIODO-ATE              PIC 9(08).
         15 FILLER                          PIC X(001).
         15 SLIGFF-RAZAO-NOME               PIC X(40).
         15 FILLER                          PIC X(001).
         15 SLIGFF-GRAU-RESTRICAO           PIC X(03).
         15 FILLER                          PIC X(001).
         15 SLIGFF-RATING                   PIC X(03).
         15 FILLER                          PIC X(001).
      *
      * INDICE LIQUIDEZ TITULO - CLIENTE (SLIGA8TM) *INDICES*
      * (1-SIMPLES 2-CAUCIONADO 3-DESCONTO)
         15 SLIGFF-ILC-TAB OCCURS 3.
          20 SLIGFF-ILC                      PIC 9(03),9(07).
          20 FILLER                          PIC X(001).
          20 SLIGFF-ILC-VNCTO                PIC 9(03),9(07).
          20 FILLER                          PIC X(001).
          20 SLIGFF-ILC-VNCDS-ANTS           PIC 9(03),9(07).
          20 FILLER                          PIC X(001).
          20 SLIGFF-ILC-BXS-ANTCPDAS         PIC 9(03),9(07).
          20 FILLER                          PIC X(001).
          20 SLIGFF-ILC-LIQ-C-BX-ANTCPDAS    PIC 9(03),9(07).
          20 FILLER                          PIC X(001).
          20 SLIGFF-ILC-PRORROGACOES         PIC 9(03),9(07).
          20 FILLER                          PIC X(001).
          20 SLIGFF-ILC-ABATS                PIC 9(03),9(07).
          20 FILLER                          PIC X(001).
          20 SLIGFF-ILC-PAGTO-ANTCPDOS       PIC 9(03),9(07).
          20 FILLER                          PIC X(001).
      *
      * INDICE LIQUIDEZ TITULO - CLIENTE (SLIGA9TM) *VALORES*
      * (1-SIMPLES 2-CAUCIONADO 3-DESCONTO)
         15 SLIGFF-VILC-TAB OCCURS 3.
          20 SLIGFF-VLR-CED-TOTAL-CART       PIC 9(14),99.
          20 FILLER                          PIC X(001).
          20 SLIGFF-VLR-CED-VNCTOS-CART      PIC 9(14),99.
          20 FILLER                          PIC X(001).
          20 SLIGFF-VLR-CED-VNCDS-ANTS       PIC 9(14),99.
          20 FILLER                          PIC X(001).
          20 SLIGFF-VLR-CED-PAGTO-CART       PIC 9(14),99.
          20 FILLER                          PIC X(001).
          20 SLIGFF-VLR-CED-BXS-ANTCPDAS     PIC 9(14),99.
          20 FILLER                          PIC X(001).
          20 SLIGFF-VLR-CED-PRORROGACOES     PIC 9(14),99.
          20 FILLER                          PIC X(001).
          20 SLIGFF-VLR-CED-ABATS            PIC 9(14),99.
          20 FILLER                          PIC X(001).
          20 SLIGFF-VLR-CED-PAGTO-ANTCPDOS   PIC 9(14),99.
          20 FILLER                          PIC X(001).
      *
      * INDICE LIQUIDEZ TITULO - SACADO (SLIGA1TM) *INDICE*
      * (1-SIMPLES 2-CAUCIONADO 3-DESCONTO)
         15 SLIGFF-ILS-TAB OCCURS 3.
          20 SLIGFF-ILS                      PIC 9(03),9(07).
          20 FILLER                          PIC X(001).
          20 SLIGFF-ILS-VNCTO                PIC 9(03),9(07).
          20 FILLER                          PIC X(001).
          20 SLIGFF-ILS-VNCDS-ANTS           PIC 9(03),9(07).
          20 FILLER                          PIC X(001).
          20 SLIGFF-ILS-BXS-ANTCPDAS         PIC 9(03),9(07).
          20 FILLER                          PIC X(001).
          20 SLIGFF-ILS-LIQ-C-BX-ANTCPDAS    PIC 9(03),9(07).
          20 FILLER                          PIC X(001).
          20 SLIGFF-ILS-PRORROGACOES         PIC 9(03),9(07).
          20 FILLER                          PIC X(001).
          20 SLIGFF-ILS-ABATS                PIC 9(03),9(07).
          20 FILLER                          PIC X(001).
          20 SLIGFF-ILS-PAGTO-ANTCPDOS       PIC 9(03),9(07).
          20 FILLER                          PIC X(001).
      *
      * INDICE LIQUIDEZ TITULO - SACADO (SLIGA2TM) *VALORES*
      * (1-SIMPLES 2-CAUCIONADO 3-DESCONTO)
         15 SLIGFF-VILS-TAB OCCURS 3.
          20 SLIGFF-VLR-SAC-TOTAL-CART       PIC 9(14),99.
          20 FILLER                          PIC X(001).
          20 SLIGFF-VLR-SAC-VNCTOS-CART      PIC 9(14),99.
          20 FILLER                          PIC X(001).
          20 SLIGFF-VLR-SAC-VNCDS-ANTS       PIC 9(14),99.
          20 FILLER                          PIC X(001).
          20 SLIGFF-VLR-SAC-PAGTO-CART       PIC 9(14),99.
          20 FILLER                          PIC X(001).
          20 SLIGFF-VLR-SAC-BXS-ANTCPDAS     PIC 9(14),99.
          20 FILLER                          PIC X(001).
          20 SLIGFF-VLR-SAC-PRORROGACOES     PIC 9(14),99.
          20 FILLER                          PIC X(001).
          20 SLIGFF-VLR-SAC-ABATS            PIC 9(14),99.
          20 FILLER                          PIC X(001).
          20 SLIGFF-VLR-SAC-PAGTO-ANTCPDOS   PIC 9(14),99.
          20 FILLER                          PIC X(001).
      *
      * INDICE LIQUIDEZ TITULO - HISTORICO CLIENTE (SLIGB0TM) *INDICE*
      * (1-SIMPLES 2-CAUCIONADO 3-DESCONTO)
         15 SLIGFF-ILHC-TAB OCCURS 3.
          20 SLIGFF-ILHC-CED-LIQ-CART1       PIC 9(03),9(07).
          20 FILLER                          PIC X(001).
          20 SLIGFF-ILHC-CED-LIQ-CART2       PIC 9(03),9(07).
          20 FILLER                          PIC X(001).
          20 SLIGFF-ILHC-CED-LIQ-CART3       PIC 9(03),9(07).
          20 FILLER                          PIC X(001).
          20 SLIGFF-ILHC-CED-LIQ-CART4       PIC 9(03),9(07).
          20 FILLER                          PIC X(001).
          20 SLIGFF-ILHC-CED-LIQ-CART5       PIC 9(03),9(07).
          20 FILLER                          PIC X(001).
          20 SLIGFF-ILHC-CED-LIQ-CART6       PIC 9(03),9(07).
          20 FILLER                          PIC X(001).
      *
      * INDICE LIQUIDEZ TITULO - HISTORICO CLIENTE (SLIGB0TM) *INDICE*
      *      COM BAIXA
      * (1-SIMPLES 2-CAUCIONADO 3-DESCONTO)
         15 SLIGFF-ILHC-BX-TAB OCCURS 3.
          20 SLIGFF-ILHC-CED-LIQ-BX-CART1    PIC 9(03),9(07).
          20 FILLER                          PIC X(001).
          20 SLIGFF-ILHC-CED-LIQ-BX-CART2    PIC 9(03),9(07).
          20 FILLER                          PIC X(001).
          20 SLIGFF-ILHC-CED-LIQ-BX-CART3    PIC 9(03),9(07).
          20 FILLER                          PIC X(001).
          20 SLIGFF-ILHC-CED-LIQ-BX-CART4    PIC 9(03),9(07).
          20 FILLER                          PIC X(001).
          20 SLIGFF-ILHC-CED-LIQ-BX-CART5    PIC 9(03),9(07).
          20 FILLER                          PIC X(001).
          20 SLIGFF-ILHC-CED-LIQ-BX-CART6    PIC 9(03),9(07).
          20 FILLER                          PIC X(001).
      *
      * INDICE LIQUIDEZ TITULO - HISTORICO SACADO (SLIGA3TM) *INDICES*
      * (1-SIMPLES 2-CAUCIONADO 3-DESCONTO)
         15 SLIGFF-ILHS-TAB OCCURS 3.
          20 SLIGFF-ILHS-SAC-LIQ-CART1       PIC 9(03),9(07).
          20 FILLER                          PIC X(001).
          20 SLIGFF-ILHS-SAC-LIQ-CART2       PIC 9(03),9(07).
          20 FILLER                          PIC X(001).
          20 SLIGFF-ILHS-SAC-LIQ-CART3       PIC 9(03),9(07).
          20 FILLER                          PIC X(001).
          20 SLIGFF-ILHS-SAC-LIQ-CART4       PIC 9(03),9(07).
          20 FILLER                          PIC X(001).
          20 SLIGFF-ILHS-SAC-LIQ-CART5       PIC 9(03),9(07).
          20 FILLER                          PIC X(001).
          20 SLIGFF-ILHS-SAC-LIQ-CART6       PIC 9(03),9(07).
          20 FILLER                          PIC X(001).
      *
      * INDICE LIQUIDEZ TITULO - HISTORICO SACADO (SLIGA3TM) *INDICES*
      *      COM BAIXA
      * (1-SIMPLES 2-CAUCIONADO 3-DESCONTO)
         15 SLIGFF-ILHS-BX-TAB OCCURS 3.
          20 SLIGFF-ILHS-SAC-LIQ-BX-CART1    PIC 9(03),9(07).
          20 FILLER                          PIC X(001).
          20 SLIGFF-ILHS-SAC-LIQ-BX-CART2    PIC 9(03),9(07).
          20 FILLER                          PIC X(001).
          20 SLIGFF-ILHS-SAC-LIQ-BX-CART3    PIC 9(03),9(07).
          20 FILLER                          PIC X(001).
          20 SLIGFF-ILHS-SAC-LIQ-BX-CART4    PIC 9(03),9(07).
          20 FILLER                          PIC X(001).
          20 SLIGFF-ILHS-SAC-LIQ-BX-CART5    PIC 9(03),9(07).
          20 FILLER                          PIC X(001).
          20 SLIGFF-ILHS-SAC-LIQ-BX-CART6    PIC 9(03),9(07).
          20 FILLER                          PIC X(001).
      *
      * INDICE LIQUIDEZ CHEQUE - CLIENTE (SLIG61TM) *INDICES*
      * (1-SIMPLES 2-CAUCIONADO 3-DESCONTO)
         15 SLIGFF-ILC-TAB OCCURS 3.
          20 SLIGFF-ILC-FINAL                PIC 9(03),9(07).
          20 FILLER                          PIC X(001).
          20 SLIGFF-ILC-PRIMEIRO-DEPOS       PIC 9(03),9(07).
          20 FILLER                          PIC X(001).
          20 SLIGFF-ILC-REAP                 PIC 9(03),9(07).
          20 FILLER                          PIC X(001).
          20 SLIGFF-ILC-VNCTOS-PERIODO       PIC 9(03),9(07).
          20 FILLER                          PIC X(001).
          20 SLIGFF-ILC-CHQ-DEPOSITADOS      PIC 9(03),9(07).
          20 FILLER                          PIC X(001).
          20 SLIGFF-ILC-BXS-ANTECIPADAS      PIC 9(03),9(07).
          20 FILLER                          PIC X(001).
          20 SLIGFF-ILC-CHQ-RPRSNTADOS       PIC 9(03),9(07).
          20 FILLER                          PIC X(001).
      *
      * INDICE LIQUIDEZ CHEQUE - CLIENTE (SLIG61TM) *VALORES*
      * (1-SIMPLES 2-CAUCIONADO 3-DESCONTO)
         15 SLIGFF-ILCV-TAB OCCURS 3.
          20 SLIGFF-VLR-CUST-TOTAL-CART      PIC 9(14),99.
          20 FILLER                          PIC X(001).
          20 SLIGFF-VLR-CUST-VNCTOS-PERIODO  PIC 9(14),99.
          20 FILLER                          PIC X(001).
          20 SLIGFF-VLR-CUST-DEPOSS-PERIODO  PIC 9(14),99.
          20 FILLER                          PIC X(001).
          20 SLIGFF-VLR-CUST-CHQ-PGS         PIC 9(14),99.
          20 FILLER                          PIC X(001).
          20 SLIGFF-VLR-CUST-CHQ-PRIM-DVL    PIC 9(14),99.
          20 FILLER                          PIC X(001).
          20 SLIGFF-VLR-CUST-CHQ-RPRSNTADOS  PIC 9(14),99.
          20 FILLER                          PIC X(001).
          20 SLIGFF-VLR-CUST-CHQ-REAP-PGS    PIC 9(14),99.
          20 FILLER                          PIC X(001).
          20 SLIGFF-VLR-CUST-CHQ-REAP-DVL    PIC 9(14),99.
          20 FILLER                          PIC X(001).
          20 SLIGFF-VLR-CUST-BXS-ANTCPDAS    PIC 9(14),99.
          20 FILLER                          PIC X(001).
      *
      * INDICE LIQUIDEZ CHEQUE - EMITENTE (SLIG38TM) - *INDICES*
      * (1-SIMPLES 2-CAUCIONADO 3-DESCONTO)
         15 SLIGFF-ILE-TAB OCCURS 3.
          20 SLIGFF-ILE-FINAL                PIC 9(03),9(07).
          20 FILLER                          PIC X(001).
          20 SLIGFF-ILE-PRIMEIRO-DEPOS       PIC 9(03),9(07).
          20 FILLER                          PIC X(001).
          20 SLIGFF-ILE-REAP                 PIC 9(03),9(07).
          20 FILLER                          PIC X(001).
          20 SLIGFF-ILE-VNCTOS-PERIODO       PIC 9(03),9(07).
          20 FILLER                          PIC X(001).
          20 SLIGFF-ILE-CHQ-DEPOSITADOS      PIC 9(03),9(07).
          20 FILLER                          PIC X(001).
          20 SLIGFF-ILE-BXS-ANTECIPADAS      PIC 9(03),9(07).
          20 FILLER                          PIC X(001).
          20 SLIGFF-ILE-CHQ-RPRSENTADOS      PIC 9(03),9(07).
          20 FILLER                          PIC X(001).
      *
      * INDICE LIQUIDEZ CHEQUE - EMITENTE (SLIG39TM) - *VALORES*
      * (1-SIMPLES 2-CAUCIONADO 3-DESCONTO)
         15 SLIGFF-ILEV-TAB OCCURS 3.
          20 SLIGFF-VLR-EMIT-TOTAL-CART      PIC 9(14),99.
          20 FILLER                          PIC X(001).
          20 SLIGFF-VLR-EMIT-VNCTOS-PERIODO  PIC 9(14),99.
          20 FILLER                          PIC X(001).
          20 SLIGFF-VLR-EMIT-DEPOSS-PERIODO  PIC 9(14),99.
          20 FILLER                          PIC X(001).
          20 SLIGFF-VLR-EMIT-CHQ-PGS         PIC 9(14),99.
          20 FILLER                          PIC X(001).
          20 SLIGFF-VLR-EMIT-CHQ-PRIM-DVL    PIC 9(14),99.
          20 FILLER                          PIC X(001).
          20 SLIGFF-VLR-EMIT-CHQ-RPRSNTADOS  PIC 9(14),99.
          20 FILLER                          PIC X(001).
          20 SLIGFF-VLR-EMIT-CHQ-REAP-PGS    PIC 9(14),99.
          20 FILLER                          PIC X(001).
          20 SLIGFF-VLR-EMIT-CHQ-REAP-DVL    PIC 9(14),99.
          20 FILLER                          PIC X(001).
          20 SLIGFF-VLR-EMIT-BXS-ANTCPDAS    PIC 9(14),99.
          20 FILLER                          PIC X(001).
      *
      * INDICE LIQUIDEZ CHEQUE - HISTORICO CLIENTE (SLIG63TM)*INDICES*
      *    * FINAL
      * (1-SIMPLES 2-CAUCIONADO 3-DESCONTO)
         15 SLIGFF-ILHF-TAB OCCURS 3.
          20 SLIGFF-ILH-CUST-LIQ-FINAL1      PIC 9(03),9(07).
          20 FILLER                          PIC X(001).
          20 SLIGFF-ILH-CUST-LIQ-FINAL2      PIC 9(03),9(07).
          20 FILLER                          PIC X(001).
          20 SLIGFF-ILH-CUST-LIQ-FINAL3      PIC 9(03),9(07).
          20 FILLER                          PIC X(001).
          20 SLIGFF-ILH-CUST-LIQ-FINAL4      PIC 9(03),9(07).
          20 FILLER                          PIC X(001).
          20 SLIGFF-ILH-CUST-LIQ-FINAL5      PIC 9(03),9(07).
          20 FILLER                          PIC X(001).
          20 SLIGFF-ILH-CUST-LIQ-FINAL6      PIC 9(03),9(07).
          20 FILLER                          PIC X(001).
      *
      * INDICE LIQUIDEZ CHEQUE - HISTORICO CLIENTE (SLIG63TM)*INDICES*
      *    * PRIMEIRO DEPOSITO
      * (1-SIMPLES 2-CAUCIONADO 3-DESCONTO)
         15 SLIGFF-ILHP-TAB OCCURS 3.
          20 SLIGFF-ILH-CUST-LIQ-PRIM-DEP1   PIC 9(03),9(07).
          20 FILLER                          PIC X(001).
          20 SLIGFF-ILH-CUST-LIQ-PRIM-DEP2   PIC 9(03),9(07).
          20 FILLER                          PIC X(001).
          20 SLIGFF-ILH-CUST-LIQ-PRIM-DEP3   PIC 9(03),9(07).
          20 FILLER                          PIC X(001).
          20 SLIGFF-ILH-CUST-LIQ-PRIM-DEP4   PIC 9(03),9(07).
          20 FILLER                          PIC X(001).
          20 SLIGFF-ILH-CUST-LIQ-PRIM-DEP5   PIC 9(03),9(07).
          20 FILLER                          PIC X(001).
          20 SLIGFF-ILH-CUST-LIQ-PRIM-DEP6   PIC 9(03),9(07).
          20 FILLER                          PIC X(001).
      *
      * INDICE LIQUIDEZ CHEQUE - HISTORICO CLIENTE (SLIG63TM)*INDICES*
      *    * REAPRESENTACAO
      * (1-SIMPLES 2-CAUCIONADO 3-DESCONTO)
         15 SLIGFF-ILHR-TAB OCCURS 3.
          20 SLIGFF-ILH-CUST-LIQ-REAP1       PIC 9(03),9(07).
          20 FILLER                          PIC X(001).
          20 SLIGFF-ILH-CUST-LIQ-REAP2       PIC 9(03),9(07).
          20 FILLER                          PIC X(001).
          20 SLIGFF-ILH-CUST-LIQ-REAP3       PIC 9(03),9(07).
          20 FILLER                          PIC X(001).
          20 SLIGFF-ILH-CUST-LIQ-REAP4       PIC 9(03),9(07).
          20 FILLER                          PIC X(001).
          20 SLIGFF-ILH-CUST-LIQ-REAP5       PIC 9(03),9(07).
          20 FILLER                          PIC X(001).
          20 SLIGFF-ILH-CUST-LIQ-REAP6       PIC 9(03),9(07).
          20 FILLER                          PIC X(001).
      *
      * INDICE LIQUIDEZ CHEQUE - HISTORICO EMITENTE (SLIG40TM) *INDICES*
      *   *FINAL
      * (1-SIMPLES 2-CAUCIONADO 3-DESCONTO)
         15 SLIGFF-ILHF-TAB OCCURS 3.
          20 SLIGFF-ILH-EMIT-LIQ-FINAL1      PIC 9(03),9(07).
          20 FILLER                          PIC X(001).
          20 SLIGFF-ILH-EMIT-LIQ-FINAL2      PIC 9(03),9(07).
          20 FILLER                          PIC X(001).
          20 SLIGFF-ILH-EMIT-LIQ-FINAL3      PIC 9(03),9(07).
          20 FILLER                          PIC X(001).
          20 SLIGFF-ILH-EMIT-LIQ-FINAL4      PIC 9(03),9(07).
          20 FILLER                          PIC X(001).
          20 SLIGFF-ILH-EMIT-LIQ-FINAL5      PIC 9(03),9(07).
          20 FILLER                          PIC X(001).
          20 SLIGFF-ILH-EMIT-LIQ-FINAL6      PIC 9(03),9(07).
          20 FILLER                          PIC X(001).
      *
      * INDICE LIQUIDEZ CHEQUE - HISTORICO EMITENTE (SLIG40TM) *INDICES*
      *   *PRIMEIRO DEPOSITO
      * (1-SIMPLES 2-CAUCIONADO 3-DESCONTO)
         15 SLIGFF-ILHP-TAB OCCURS 3.
          20 SLIGFF-ILH-EMIT-LIQ-PRIM-DEP1   PIC 9(03),9(07).
          20 FILLER                          PIC X(001).
          20 SLIGFF-ILH-EMIT-LIQ-PRIM-DEP2   PIC 9(03),9(07).
          20 FILLER                          PIC X(001).
          20 SLIGFF-ILH-EMIT-LIQ-PRIM-DEP3   PIC 9(03),9(07).
          20 FILLER                          PIC X(001).
          20 SLIGFF-ILH-EMIT-LIQ-PRIM-DEP4   PIC 9(03),9(07).
          20 FILLER                          PIC X(001).
          20 SLIGFF-ILH-EMIT-LIQ-PRIM-DEP5   PIC 9(03),9(07).
          20 FILLER                          PIC X(001).
          20 SLIGFF-ILH-EMIT-LIQ-PRIM-DEP6   PIC 9(03),9(07).
          20 FILLER                          PIC X(001).
      *
      * INDICE LIQUIDEZ CHEQUE - HISTORICO EMITENTE (SLIG40TM) *INDICES*
      *   *PRIMEIRO DEPOSITO
      * (1-SIMPLES 2-CAUCIONADO 3-DESCONTO)
         15 SLIGFF-ILHR-TAB OCCURS 3.
          20 SLIGFF-ILH-EMIT-LIQ-REAP1       PIC 9(03),9(07).
          20 FILLER                          PIC X(001).
          20 SLIGFF-ILH-EMIT-LIQ-REAP2       PIC 9(03),9(07).
          20 FILLER                          PIC X(001).
          20 SLIGFF-ILH-EMIT-LIQ-REAP3       PIC 9(03),9(07).
          20 FILLER                          PIC X(001).
          20 SLIGFF-ILH-EMIT-LIQ-REAP4       PIC 9(03),9(07).
          20 FILLER                          PIC X(001).
          20 SLIGFF-ILH-EMIT-LIQ-REAP5       PIC 9(03),9(07).
          20 FILLER                          PIC X(001).
          20 SLIGFF-ILH-EMIT-LIQ-REAP6       PIC 9(03),9(07).
          20 FILLER                          PIC X(001).
        10 SLIGFF-TRAILER                    REDEFINES SLIGFF-HEADER.
         15 SLIGFF-T-NOM-ARQ                 PIC X(077).
         15 SLIGFF-T-PV-1                    PIC X(001).
         15 SLIGFF-T-QTD-REG                 PIC 9(015).
         15 SLIGFF-T-RESERVA                 PIC X(5068).
