      *===============================================================*
      * INCLUDE PARA TABELA:                                          *
      *    - DCOMB0A1 - DB2PRD.TESTAT_MOVTC_SEGMT                     *
      *---------------------------------------------------------------*
      * MODULOS DE ACESSO..:                                          *
      *    - DCOM7005 - SELECT         - DCOM7006 - FETCH             *
      *    - DCOM7007 - INSERT         - DCOM7008 - UPDATE            *
      *    - DCOM7009 - DELETE                                        *
      *===============================================================*

       01  A1-ARGUMENTOS-ENTRADA.

       05  A1-COLUNAS-DA-TABELA.
           10  A1-DPOSIC-CONSL-DESC    PIC  X(010).
           10  A1-CBCO                 PIC S9(003) COMP-3.
           10  A1-CSGMTO-CLI           PIC S9(003) COMP-3.
           10  A1-CPRODT               PIC S9(003) COMP-3.
           10  A1-CSPROD-DESC-COML     PIC S9(003) COMP-3.
           10  A1-CINDCD-PER-CONSL     PIC  X(001).
           10  A1-CORIGE-OPER-DESC     PIC S9(001) COMP-3.
           10  A1-CAG-BCRIA            PIC S9(005) COMP-3.
           10  A1-VSDO-ANTER-CONSL     PIC S9(015)V9(2) COMP-3.
           10  A1-VPOSIC-ENTRD-CONSL   PIC S9(015)V9(2) COMP-3.
           10  A1-VPOSIC-BAIXA-CONSL   PIC S9(015)V9(2) COMP-3.
           10  A1-VTRNSF-MORA-DIA      PIC S9(015)V9(2) COMP-3.
           10  A1-VSDO-ATUAL-CONSL     PIC S9(015)V9(2) COMP-3.
           10  A1-QTITLO-SDO-ANTER     PIC S9(017) COMP-3.
           10  A1-QENTRD-TITLO-CONSL   PIC S9(017) COMP-3.
           10  A1-QBAIXA-TITLO-CONSL   PIC S9(017) COMP-3.
           10  A1-QTITLO-MORA-DIA      PIC S9(017) COMP-3.
           10  A1-QTITLO-SDO-ATUAL     PIC S9(017) COMP-3.
           10  A1-QOPER-SDO-ANTER      PIC S9(017) COMP-3.
           10  A1-QENTRD-OPER-CONSL    PIC S9(017) COMP-3.
           10  A1-QBAIXA-OPER-CONSL    PIC S9(017) COMP-3.
           10  A1-QOPER-MORA-DIA       PIC S9(017) COMP-3.
           10  A1-QOPER-SDO-ATUAL      PIC S9(017) COMP-3.
           10  A1-VTAC-SDO-ANTER       PIC S9(015)V9(2) COMP-3.
           10  A1-VENTRD-TAC-CONSL     PIC S9(015)V9(2) COMP-3.
           10  A1-VBAIXA-TAC-CONSL     PIC S9(015)V9(2) COMP-3.
           10  A1-VTAC-MORA-DIA        PIC S9(015)V9(2) COMP-3.
           10  A1-VTAC-SDO-ATUAL       PIC S9(015)V9(2) COMP-3.
           10  A1-PTX-MED-ENTRD        PIC S9(003)V9(5) COMP-3.
           10  A1-PTX-MED-BAIXA        PIC S9(003)V9(5) COMP-3.
           10  A1-PTX-MED-ESTOQ        PIC S9(003)V9(5) COMP-3.
           10  A1-TMED-TITLO-UTILZ     PIC S9(005) COMP-3.
           10  A1-TMED-TITLO-ESTOQ     PIC S9(005) COMP-3.
           10  A1-CINDCD-PRZ-MED       PIC  X(002).
           10  A1-VRAP-SDO-ANTER       PIC S9(015)V9(2) COMP-3.
           10  A1-VRAP-ENTRD           PIC S9(015)V9(2) COMP-3.
           10  A1-VRAP-BAIXA           PIC S9(015)V9(2) COMP-3.
           10  A1-VRAP-MORA-DIA        PIC S9(015)V9(2) COMP-3.
           10  A1-VRAP-SDO-ATUAL       PIC S9(015)V9(2) COMP-3.
           10  A1-VIOF-ENTRD           PIC S9(015)V9(2) COMP-3.
           10  A1-VIOF-MORA-DIA        PIC S9(015)V9(2) COMP-3.
           10  A1-VIOF-SDO-ATUAL       PIC S9(015)V9(2) COMP-3.
           10  A1-VTARIF-ENTRD         PIC S9(015)V9(2) COMP-3.
           10  A1-VTARIF-SDO-ATUAL     PIC S9(015)V9(2) COMP-3.


      * PREENCHER O CAMPO ABAIXO, PARA FETCH
      *-------------------------------------*
       05  A1-QTDE-A-DESPREZAR         PIC  9(005).


      * PREENCHER O CAMPO ABAIXO, PARA FETCH E UPDATE
      *-----------------------------------------------*
       05  A1-INSTRUCAO                PIC  X(010).


      *****************************************************************
      * AREA DE RETORNO                                               *
      *****************************************************************
       01  A1-RETORNO.
       05  A1-COD-RETORNO              PIC  9(004).
       05  A1-MENSAGEM                 PIC  X(079).


      * SERAO PREENCHIDOS NO CASO DE FETCH
      *------------------------------------*
       05  A1-PROXIMO-RESTART          PIC  9(005).
       05  A1-QTDE-RETORNADA           PIC  9(003).
       05  A1-QTDE-TOTAL               PIC  9(009).


      * AREA DE RETORNO DO SELECT (PRIMEIRA OCORRENCIA)
      * AREA DE RETORNO DO FETCH  (DIVERSAS OCORRENCIAS)
      *---------------------------------------------------------------*
       05  A1-OCORRENCIAS       OCCURS 070 TIMES INDEXED BY IND-TA1.
           10  A1RT-DPOSIC-CONSL-DESC  PIC  X(010).
           10  A1RT-CBCO               PIC S9(003) COMP-3.
           10  A1RT-CSGMTO-CLI         PIC S9(003) COMP-3.
           10  A1RT-CPRODT             PIC S9(003) COMP-3.
           10  A1RT-CSPROD-DESC-COML   PIC S9(003) COMP-3.
           10  A1RT-CINDCD-PER-CONSL   PIC  X(001).
           10  A1RT-CORIGE-OPER-DESC   PIC S9(001) COMP-3.
           10  A1RT-CAG-BCRIA          PIC S9(005) COMP-3.
           10  A1RT-VSDO-ANTER-CONSL   PIC S9(015)V9(2) COMP-3.
           10  A1RT-VPOSIC-ENTRD-CONSL PIC S9(015)V9(2) COMP-3.
           10  A1RT-VPOSIC-BAIXA-CONSL PIC S9(015)V9(2) COMP-3.
           10  A1RT-VTRNSF-MORA-DIA    PIC S9(015)V9(2) COMP-3.
           10  A1RT-VSDO-ATUAL-CONSL   PIC S9(015)V9(2) COMP-3.
           10  A1RT-QTITLO-SDO-ANTER   PIC S9(017) COMP-3.
           10  A1RT-QENTRD-TITLO-CONSL PIC S9(017) COMP-3.
           10  A1RT-QBAIXA-TITLO-CONSL PIC S9(017) COMP-3.
           10  A1RT-QTITLO-MORA-DIA    PIC S9(017) COMP-3.
           10  A1RT-QTITLO-SDO-ATUAL   PIC S9(017) COMP-3.
           10  A1RT-QOPER-SDO-ANTER    PIC S9(017) COMP-3.
           10  A1RT-QENTRD-OPER-CONSL  PIC S9(017) COMP-3.
           10  A1RT-QBAIXA-OPER-CONSL  PIC S9(017) COMP-3.
           10  A1RT-QOPER-MORA-DIA     PIC S9(017) COMP-3.
           10  A1RT-QOPER-SDO-ATUAL    PIC S9(017) COMP-3.
           10  A1RT-VTAC-SDO-ANTER     PIC S9(015)V9(2) COMP-3.
           10  A1RT-VENTRD-TAC-CONSL   PIC S9(015)V9(2) COMP-3.
           10  A1RT-VBAIXA-TAC-CONSL   PIC S9(015)V9(2) COMP-3.
           10  A1RT-VTAC-MORA-DIA      PIC S9(015)V9(2) COMP-3.
           10  A1RT-VTAC-SDO-ATUAL     PIC S9(015)V9(2) COMP-3.
           10  A1RT-PTX-MED-ENTRD      PIC S9(003)V9(5) COMP-3.
           10  A1RT-PTX-MED-BAIXA      PIC S9(003)V9(5) COMP-3.
           10  A1RT-PTX-MED-ESTOQ      PIC S9(003)V9(5) COMP-3.
           10  A1RT-TMED-TITLO-UTILZ   PIC S9(005) COMP-3.
           10  A1RT-TMED-TITLO-ESTOQ   PIC S9(005) COMP-3.
           10  A1RT-CINDCD-PRZ-MED     PIC  X(002).
           10  A1RT-VRAP-SDO-ANTER     PIC S9(015)V9(2) COMP-3.
           10  A1RT-VRAP-ENTRD         PIC S9(015)V9(2) COMP-3.
           10  A1RT-VRAP-BAIXA         PIC S9(015)V9(2) COMP-3.
           10  A1RT-VRAP-MORA-DIA      PIC S9(015)V9(2) COMP-3.
           10  A1RT-VRAP-SDO-ATUAL     PIC S9(015)V9(2) COMP-3.
           10  A1RT-VIOF-ENTRD         PIC S9(015)V9(2) COMP-3.
           10  A1RT-VIOF-MORA-DIA      PIC S9(015)V9(2) COMP-3.
           10  A1RT-VIOF-SDO-ATUAL     PIC S9(015)V9(2) COMP-3.
           10  A1RT-VTARIF-ENTRD       PIC S9(015)V9(2) COMP-3.
           10  A1RT-VTARIF-SDO-ATUAL   PIC S9(015)V9(2) COMP-3.
