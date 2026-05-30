      *===============================================================*
      * INCLUDE PARA TABELA:                                          *
      *    - DCOMB0A0 - DB2PRD.TESTAT_MOVTC_DEPDC                     *
      *---------------------------------------------------------------*
      * MODULOS DE ACESSO..:                                          *
      *    - DCOM7000 - SELECT         - DCOM7001 - FETCH             *
      *    - DCOM7002 - INSERT         - DCOM7003 - UPDATE            *
      *    - DCOM7004 - DELETE                                        *
      *===============================================================*

       01  A0-ARGUMENTOS-ENTRADA.

       05  A0-COLUNAS-DA-TABELA.
           10  A0-DPOSIC-CONSL-DESC    PIC  X(010).
           10  A0-CEMPR-INC            PIC S9(005) COMP-3.
           10  A0-CDEPDC               PIC S9(005) COMP-3.
           10  A0-CGERC-RGNAL          PIC S9(005) COMP-3.
           10  A0-CAG-BCRIA            PIC S9(005) COMP-3.
           10  A0-CPRODT               PIC S9(003) COMP-3.
           10  A0-CSPROD-DESC-COML     PIC S9(003) COMP-3.
           10  A0-CINDCD-PER-CONSL     PIC  X(001).
           10  A0-CORIGE-OPER-DESC     PIC S9(001) COMP-3.
           10  A0-VSDO-ANTER-CONSL     PIC S9(015)V9(2) COMP-3.
           10  A0-VPOSIC-ENTRD-CONSL   PIC S9(015)V9(2) COMP-3.
           10  A0-VPOSIC-BAIXA-CONSL   PIC S9(015)V9(2) COMP-3.
           10  A0-VTRNSF-MORA-DIA      PIC S9(015)V9(2) COMP-3.
           10  A0-VSDO-ATUAL-CONSL     PIC S9(015)V9(2) COMP-3.
           10  A0-QTITLO-SDO-ANTER     PIC S9(017) COMP-3.
           10  A0-QENTRD-TITLO-CONSL   PIC S9(017) COMP-3.
           10  A0-QBAIXA-TITLO-CONSL   PIC S9(017) COMP-3.
           10  A0-QTITLO-MORA-DIA      PIC S9(017) COMP-3.
           10  A0-QTITLO-SDO-ATUAL     PIC S9(017) COMP-3.
           10  A0-QOPER-SDO-ANTER      PIC S9(017) COMP-3.
           10  A0-QENTRD-OPER-CONSL    PIC S9(017) COMP-3.
           10  A0-QBAIXA-OPER-CONSL    PIC S9(017) COMP-3.
           10  A0-QOPER-MORA-DIA       PIC S9(017) COMP-3.
           10  A0-QOPER-SDO-ATUAL      PIC S9(017) COMP-3.
           10  A0-VTAC-SDO-ANTER       PIC S9(015)V9(2) COMP-3.
           10  A0-VENTRD-TAC-CONSL     PIC S9(015)V9(2) COMP-3.
           10  A0-VBAIXA-TAC-CONSL     PIC S9(015)V9(2) COMP-3.
           10  A0-VTAC-MORA-DIA        PIC S9(015)V9(2) COMP-3.
           10  A0-VTAC-SDO-ATUAL       PIC S9(015)V9(2) COMP-3.
           10  A0-PTX-MED-ENTRD        PIC S9(003)V9(5) COMP-3.
           10  A0-PTX-MED-BAIXA        PIC S9(003)V9(5) COMP-3.
           10  A0-PTX-MED-ESTOQ        PIC S9(003)V9(5) COMP-3.
           10  A0-TMED-TITLO-UTILZ     PIC S9(005) COMP-3.
           10  A0-TMED-TITLO-ESTOQ     PIC S9(005) COMP-3.
           10  A0-CINDCD-PRZ-MED       PIC  X(002).
           10  A0-VRAP-SDO-ANTER       PIC S9(015)V9(2) COMP-3.
           10  A0-VRAP-ENTRD           PIC S9(015)V9(2) COMP-3.
           10  A0-VRAP-BAIXA           PIC S9(015)V9(2) COMP-3.
           10  A0-VRAP-MORA-DIA        PIC S9(015)V9(2) COMP-3.
           10  A0-VRAP-SDO-ATUAL       PIC S9(015)V9(2) COMP-3.
           10  A0-VIOF-ENTRD           PIC S9(015)V9(2) COMP-3.
           10  A0-VIOF-MORA-DIA        PIC S9(015)V9(2) COMP-3.
           10  A0-VIOF-SDO-ATUAL       PIC S9(015)V9(2) COMP-3.
           10  A0-VTARIF-ENTRD         PIC S9(015)V9(2) COMP-3.
           10  A0-VTARIF-SDO-ATUAL     PIC S9(015)V9(2) COMP-3.


      * PREENCHER O CAMPO ABAIXO, PARA FETCH
      *-------------------------------------*
       05  A0-QTDE-A-DESPREZAR         PIC  9(005).


      * PREENCHER O CAMPO ABAIXO, PARA FETCH E UPDATE
      *-----------------------------------------------*
       05  A0-INSTRUCAO                PIC  X(010).


      *****************************************************************
      * AREA DE RETORNO                                               *
      *****************************************************************
       01  A0-RETORNO.
       05  A0-COD-RETORNO              PIC  9(004).
       05  A0-MENSAGEM                 PIC  X(079).


      * SERAO PREENCHIDOS NO CASO DE FETCH
      *------------------------------------*
       05  A0-PROXIMO-RESTART          PIC  9(005).
       05  A0-QTDE-RETORNADA           PIC  9(003).
       05  A0-QTDE-TOTAL               PIC  9(009).


      * AREA DE RETORNO DO SELECT (PRIMEIRA OCORRENCIA)
      * AREA DE RETORNO DO FETCH  (DIVERSAS OCORRENCIAS)
      *---------------------------------------------------------------*
       05  A0-OCORRENCIAS       OCCURS 070 TIMES INDEXED BY IND-TA0.
           10  A0RT-DPOSIC-CONSL-DESC  PIC  X(010).
           10  A0RT-CEMPR-INC          PIC S9(005) COMP-3.
           10  A0RT-CDEPDC             PIC S9(005) COMP-3.
           10  A0RT-CGERC-RGNAL        PIC S9(005) COMP-3.
           10  A0RT-CAG-BCRIA          PIC S9(005) COMP-3.
           10  A0RT-CPRODT             PIC S9(003) COMP-3.
           10  A0RT-CSPROD-DESC-COML   PIC S9(003) COMP-3.
           10  A0RT-CINDCD-PER-CONSL   PIC  X(001).
           10  A0RT-CORIGE-OPER-DESC   PIC S9(001) COMP-3.
           10  A0RT-VSDO-ANTER-CONSL   PIC S9(015)V9(2) COMP-3.
           10  A0RT-VPOSIC-ENTRD-CONSL PIC S9(015)V9(2) COMP-3.
           10  A0RT-VPOSIC-BAIXA-CONSL PIC S9(015)V9(2) COMP-3.
           10  A0RT-VTRNSF-MORA-DIA    PIC S9(015)V9(2) COMP-3.
           10  A0RT-VSDO-ATUAL-CONSL   PIC S9(015)V9(2) COMP-3.
           10  A0RT-QTITLO-SDO-ANTER   PIC S9(017) COMP-3.
           10  A0RT-QENTRD-TITLO-CONSL PIC S9(017) COMP-3.
           10  A0RT-QBAIXA-TITLO-CONSL PIC S9(017) COMP-3.
           10  A0RT-QTITLO-MORA-DIA    PIC S9(017) COMP-3.
           10  A0RT-QTITLO-SDO-ATUAL   PIC S9(017) COMP-3.
           10  A0RT-QOPER-SDO-ANTER    PIC S9(017) COMP-3.
           10  A0RT-QENTRD-OPER-CONSL  PIC S9(017) COMP-3.
           10  A0RT-QBAIXA-OPER-CONSL  PIC S9(017) COMP-3.
           10  A0RT-QOPER-MORA-DIA     PIC S9(017) COMP-3.
           10  A0RT-QOPER-SDO-ATUAL    PIC S9(017) COMP-3.
           10  A0RT-VTAC-SDO-ANTER     PIC S9(015)V9(2) COMP-3.
           10  A0RT-VENTRD-TAC-CONSL   PIC S9(015)V9(2) COMP-3.
           10  A0RT-VBAIXA-TAC-CONSL   PIC S9(015)V9(2) COMP-3.
           10  A0RT-VTAC-MORA-DIA      PIC S9(015)V9(2) COMP-3.
           10  A0RT-VTAC-SDO-ATUAL     PIC S9(015)V9(2) COMP-3.
           10  A0RT-PTX-MED-ENTRD      PIC S9(003)V9(5) COMP-3.
           10  A0RT-PTX-MED-BAIXA      PIC S9(003)V9(5) COMP-3.
           10  A0RT-PTX-MED-ESTOQ      PIC S9(003)V9(5) COMP-3.
           10  A0RT-TMED-TITLO-UTILZ   PIC S9(005) COMP-3.
           10  A0RT-TMED-TITLO-ESTOQ   PIC S9(005) COMP-3.
           10  A0RT-CINDCD-PRZ-MED     PIC  X(002).
           10  A0RT-VRAP-SDO-ANTER     PIC S9(015)V9(2) COMP-3.
           10  A0RT-VRAP-ENTRD         PIC S9(015)V9(2) COMP-3.
           10  A0RT-VRAP-BAIXA         PIC S9(015)V9(2) COMP-3.
           10  A0RT-VRAP-MORA-DIA      PIC S9(015)V9(2) COMP-3.
           10  A0RT-VRAP-SDO-ATUAL     PIC S9(015)V9(2) COMP-3.
           10  A0RT-VIOF-ENTRD         PIC S9(015)V9(2) COMP-3.
           10  A0RT-VIOF-MORA-DIA      PIC S9(015)V9(2) COMP-3.
           10  A0RT-VIOF-SDO-ATUAL     PIC S9(015)V9(2) COMP-3.
           10  A0RT-VTARIF-ENTRD       PIC S9(015)V9(2) COMP-3.
           10  A0RT-VTARIF-SDO-ATUAL   PIC S9(015)V9(2) COMP-3.
