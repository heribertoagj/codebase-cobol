      *===============================================================*
      * INCLUDE PARA TABELA:                                          *
      *    - DCOMB099 - DB2PRD.TESTAT_MOVTC_CLI                       *
      *---------------------------------------------------------------*
      * MODULOS DE ACESSO..:                                          *
      *    - DCOM6995 - SELECT         - DCOM6996 - FETCH             *
      *    - DCOM6997 - INSERT         - DCOM6998 - UPDATE            *
      *    - DCOM6999 - DELETE                                        *
      *===============================================================*

       01  99-ARGUMENTOS-ENTRADA.

       05  99-COLUNAS-DA-TABELA.
           10  99-DMES-CONSL-PTCAO     PIC S9(002) COMP-3.
           10  99-DPOSIC-CONSL-DESC    PIC  X(010).
           10  99-CBCO                 PIC S9(003) COMP-3.
           10  99-CAG-BCRIA            PIC S9(005) COMP-3.
           10  99-CCTA-BCRIA-CLI       PIC S9(013) COMP-3.
           10  99-CPRODT               PIC S9(003) COMP-3.
           10  99-CSPROD-DESC-COML     PIC S9(003) COMP-3.
           10  99-CINDCD-PER-CONSL     PIC  X(001).
           10  99-CORIGE-OPER-DESC     PIC S9(001) COMP-3.
           10  99-VSDO-ANTER-CONSL     PIC S9(015)V9(2) COMP-3.
           10  99-VPOSIC-ENTRD-CONSL   PIC S9(015)V9(2) COMP-3.
           10  99-VPOSIC-BAIXA-CONSL   PIC S9(015)V9(2) COMP-3.
           10  99-VTRNSF-MORA-DIA      PIC S9(015)V9(2) COMP-3.
           10  99-VSDO-ATUAL-CONSL     PIC S9(015)V9(2) COMP-3.
           10  99-QTITLO-SDO-ANTER     PIC S9(017) COMP-3.
           10  99-QENTRD-TITLO-CONSL   PIC S9(017) COMP-3.
           10  99-QBAIXA-TITLO-CONSL   PIC S9(017) COMP-3.
           10  99-QTITLO-MORA-DIA      PIC S9(017) COMP-3.
           10  99-QTITLO-SDO-ATUAL     PIC S9(017) COMP-3.
           10  99-QOPER-SDO-ANTER      PIC S9(017) COMP-3.
           10  99-QENTRD-OPER-CONSL    PIC S9(017) COMP-3.
           10  99-QBAIXA-OPER-CONSL    PIC S9(017) COMP-3.
           10  99-QOPER-MORA-DIA       PIC S9(017) COMP-3.
           10  99-QOPER-SDO-ATUAL      PIC S9(017) COMP-3.
           10  99-VTAC-SDO-ANTER       PIC S9(015)V9(2) COMP-3.
           10  99-VENTRD-TAC-CONSL     PIC S9(015)V9(2) COMP-3.
           10  99-VBAIXA-TAC-CONSL     PIC S9(015)V9(2) COMP-3.
           10  99-VTAC-MORA-DIA        PIC S9(015)V9(2) COMP-3.
           10  99-VTAC-SDO-ATUAL       PIC S9(015)V9(2) COMP-3.
           10  99-PTX-MED-ENTRD        PIC S9(003)V9(5) COMP-3.
           10  99-PTX-MED-BAIXA        PIC S9(003)V9(5) COMP-3.
           10  99-PTX-MED-ESTOQ        PIC S9(003)V9(5) COMP-3.
           10  99-TMED-TITLO-UTILZ     PIC S9(005) COMP-3.
           10  99-TMED-TITLO-ESTOQ     PIC S9(005) COMP-3.
           10  99-CINDCD-PRZ-MED       PIC  X(002).
           10  99-CSGMTO-CLI           PIC S9(003) COMP-3.
           10  99-VRAP-SDO-ANTER       PIC S9(015)V9(2) COMP-3.
           10  99-VRAP-ENTRD           PIC S9(015)V9(2) COMP-3.
           10  99-VRAP-BAIXA           PIC S9(015)V9(2) COMP-3.
           10  99-VRAP-MORA-DIA        PIC S9(015)V9(2) COMP-3.
           10  99-VRAP-SDO-ATUAL       PIC S9(015)V9(2) COMP-3.
           10  99-VIOF-ENTRD           PIC S9(015)V9(2) COMP-3.
           10  99-VIOF-MORA-DIA        PIC S9(015)V9(2) COMP-3.
           10  99-VIOF-SDO-ATUAL       PIC S9(015)V9(2) COMP-3.
           10  99-VTARIF-ENTRD         PIC S9(015)V9(2) COMP-3.
           10  99-VTARIF-SDO-ATUAL     PIC S9(015)V9(2) COMP-3.


      * PREENCHER O CAMPO ABAIXO, PARA FETCH
      *-------------------------------------*
       05  99-QTDE-A-DESPREZAR         PIC  9(005).


      * PREENCHER O CAMPO ABAIXO, PARA FETCH E UPDATE
      *-----------------------------------------------*
       05  99-INSTRUCAO                PIC  X(010).


      *****************************************************************
      * AREA DE RETORNO                                               *
      *****************************************************************
       01  99-RETORNO.
       05  99-COD-RETORNO              PIC  9(004).
       05  99-MENSAGEM                 PIC  X(079).


      * SERAO PREENCHIDOS NO CASO DE FETCH
      *------------------------------------*
       05  99-PROXIMO-RESTART          PIC  9(005).
       05  99-QTDE-RETORNADA           PIC  9(003).
       05  99-QTDE-TOTAL               PIC  9(009).


      * AREA DE RETORNO DO SELECT (PRIMEIRA OCORRENCIA)
      * AREA DE RETORNO DO FETCH  (DIVERSAS OCORRENCIAS)
      *---------------------------------------------------------------*
       05  99-OCORRENCIAS       OCCURS 070 TIMES INDEXED BY IND-T99.
           10  99RT-DMES-CONSL-PTCAO   PIC S9(002) COMP-3.
           10  99RT-DPOSIC-CONSL-DESC  PIC  X(010).
           10  99RT-CBCO               PIC S9(003) COMP-3.
           10  99RT-CAG-BCRIA          PIC S9(005) COMP-3.
           10  99RT-CCTA-BCRIA-CLI     PIC S9(013) COMP-3.
           10  99RT-CPRODT             PIC S9(003) COMP-3.
           10  99RT-CSPROD-DESC-COML   PIC S9(003) COMP-3.
           10  99RT-CINDCD-PER-CONSL   PIC  X(001).
           10  99RT-CORIGE-OPER-DESC   PIC S9(001) COMP-3.
           10  99RT-VSDO-ANTER-CONSL   PIC S9(015)V9(2) COMP-3.
           10  99RT-VPOSIC-ENTRD-CONSL PIC S9(015)V9(2) COMP-3.
           10  99RT-VPOSIC-BAIXA-CONSL PIC S9(015)V9(2) COMP-3.
           10  99RT-VTRNSF-MORA-DIA    PIC S9(015)V9(2) COMP-3.
           10  99RT-VSDO-ATUAL-CONSL   PIC S9(015)V9(2) COMP-3.
           10  99RT-QTITLO-SDO-ANTER   PIC S9(017) COMP-3.
           10  99RT-QENTRD-TITLO-CONSL PIC S9(017) COMP-3.
           10  99RT-QBAIXA-TITLO-CONSL PIC S9(017) COMP-3.
           10  99RT-QTITLO-MORA-DIA    PIC S9(017) COMP-3.
           10  99RT-QTITLO-SDO-ATUAL   PIC S9(017) COMP-3.
           10  99RT-QOPER-SDO-ANTER    PIC S9(017) COMP-3.
           10  99RT-QENTRD-OPER-CONSL  PIC S9(017) COMP-3.
           10  99RT-QBAIXA-OPER-CONSL  PIC S9(017) COMP-3.
           10  99RT-QOPER-MORA-DIA     PIC S9(017) COMP-3.
           10  99RT-QOPER-SDO-ATUAL    PIC S9(017) COMP-3.
           10  99RT-VTAC-SDO-ANTER     PIC S9(015)V9(2) COMP-3.
           10  99RT-VENTRD-TAC-CONSL   PIC S9(015)V9(2) COMP-3.
           10  99RT-VBAIXA-TAC-CONSL   PIC S9(015)V9(2) COMP-3.
           10  99RT-VTAC-MORA-DIA      PIC S9(015)V9(2) COMP-3.
           10  99RT-VTAC-SDO-ATUAL     PIC S9(015)V9(2) COMP-3.
           10  99RT-PTX-MED-ENTRD      PIC S9(003)V9(5) COMP-3.
           10  99RT-PTX-MED-BAIXA      PIC S9(003)V9(5) COMP-3.
           10  99RT-PTX-MED-ESTOQ      PIC S9(003)V9(5) COMP-3.
           10  99RT-TMED-TITLO-UTILZ   PIC S9(005) COMP-3.
           10  99RT-TMED-TITLO-ESTOQ   PIC S9(005) COMP-3.
           10  99RT-CINDCD-PRZ-MED     PIC  X(002).
           10  99RT-CSGMTO-CLI         PIC S9(003) COMP-3.
           10  99RT-VRAP-SDO-ANTER     PIC S9(015)V9(2) COMP-3.
           10  99RT-VRAP-ENTRD         PIC S9(015)V9(2) COMP-3.
           10  99RT-VRAP-BAIXA         PIC S9(015)V9(2) COMP-3.
           10  99RT-VRAP-MORA-DIA      PIC S9(015)V9(2) COMP-3.
           10  99RT-VRAP-SDO-ATUAL     PIC S9(015)V9(2) COMP-3.
           10  99RT-VIOF-ENTRD         PIC S9(015)V9(2) COMP-3.
           10  99RT-VIOF-MORA-DIA      PIC S9(015)V9(2) COMP-3.
           10  99RT-VIOF-SDO-ATUAL     PIC S9(015)V9(2) COMP-3.
           10  99RT-VTARIF-ENTRD       PIC S9(015)V9(2) COMP-3.
           10  99RT-VTARIF-SDO-ATUAL   PIC S9(015)V9(2) COMP-3.
