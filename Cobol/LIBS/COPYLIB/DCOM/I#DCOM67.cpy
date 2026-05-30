      *****************************************************************
      * INCLUDE PARA TABELA: DCOMB067 - DB2PRD.TSPROD_REGRA_CONTR     *
      *---------------------------------------------------------------*
      * ARGUMENTOS DE ENTRADA                                         *
      * OBS.: OS MODULOS DE SELECT, DELETE E UPDATE TRABALHAM SEMPRE  *
      *       COM A CHAVE PRIMARIA                                    *
      *****************************************************************
       01  67-ARGUMENTOS-ENTRADA.

       05  67-COLUNAS-DA-TABELA.
           10  67-CPRODT               PIC S9(3)V USAGE COMP-3.
           10  67-CSPROD-DESC-COML     PIC S9(3)V USAGE COMP-3.
           10  67-HINIC-REGRA-CONTR    PIC X(26).
           10  67-HFIM-REGRA-CONTR     PIC X(26).
           10  67-CINDCD-TOMAD-OPER    PIC X(1).
           10  67-CINDCD-PSSOA-TOMAD   PIC X(1).
           10  67-CINDCD-CONTR-LIM     PIC X(1).
           10  67-CINDCD-CONVE-CLI     PIC X(1).
           10  67-CINDCD-CONVE-GRAL    PIC X(1).
           10  67-CINDCD-BAIXA-PCIAL   PIC X(1).
           10  67-CINDCD-BAIXA-RTEIO   PIC X(1).
           10  67-CINDCD-BAIXA-ANTCP   PIC X(1).
           10  67-CINDCD-BAIXA-GSTOR   PIC X(1).
           10  67-CINDCD-ADTTO-PCELA   PIC X(1).
           10  67-CINDCD-PRROG-PCELA   PIC X(1).
           10  67-VTARIF-PRROG-PCELA   PIC S9(15)V9(2) USAGE COMP-3.
           10  67-CTARIF-PRROG-PCELA   PIC S9(5)V USAGE COMP-3.
           10  67-CRESP-LIM-CREDT      PIC X(1).
           10  67-CINDCD-CONS-CREDT    PIC X(1).
           10  67-CINDCD-ANLSE-CREDT   PIC X(1).
           10  67-HULT-ATULZ           PIC X(26).
           10  67-CFUNC-BDSCO          PIC S9(9)V USAGE COMP-3.
           10  67-CTERM                PIC X(8).
DTS001     10  67-CINDCD-DISPN-AG      PIC X(1).
DTS001     10  67-CINDCD-DISPN-NE      PIC X(1).
DTS001     10  67-CINDCD-DISPN-TLBCO   PIC X(1).
DTS001     10  67-CINDCD-DISPN-IB      PIC X(1).
BRQ001     10  67-CINDCD-ADTTO-TX      PIC X(1).
BRQ001     10  67-QVCTO-PCELA-ADTTO    PIC S9(3)V USAGE COMP-3.
WIP001     10  67-CINDCD-ADTTO-OPCAO   PIC X(1).
WIP001     10  67-PTX-OPCAO-ACRES      PIC S9(3)V9(5) USAGE COMP-3.
WIP001     10  67-PTX-OPCAO-REDC       PIC S9(3)V9(5) USAGE COMP-3.

      * PREENCHER O CAMPO ABAIXO, PARA FETCH
      *-------------------------------------*
       05  67-QTDE-A-DESPREZAR                   PIC 9(05).


      * PREENCHER O CAMPO ABAIXO, PARA FETCH E UPDATE
      *-----------------------------------------------*
       05  67-INSTRUCAO                          PIC X(10).


      *****************************************************************
      * AREA DE RETORNO                                               *
      *****************************************************************
       01  67-RETORNO.
       05  67-COD-RETORNO                         PIC 9(04).
       05  67-MENSAGEM                            PIC X(79).


      * SERAO PREENCHIDOS NO CASO DE FETCH
      *------------------------------------*
       05  67-PROXIMO-RESTART                     PIC 9(05).
       05  67-QTDE-RETORNADA                      PIC 9(03).
       05  67-QTDE-TOTAL                          PIC 9(09).

      * AREA DE RETORNO DO SELECT (PRIMEIRA OCORRENCIA)
      * AREA DE RETORNO DO FETCH  (DIVERSAS OCORRENCIAS)
      *---------------------------------------------------------------*
       05  67-OCORRENCIAS     OCCURS 070 TIMES INDEXED BY IND-T67.
           10  67RT-CPRODT               PIC S9(3)V USAGE COMP-3.
           10  67RT-CSPROD-DESC-COML     PIC S9(3)V USAGE COMP-3.
           10  67RT-HINIC-REGRA-CONTR    PIC X(26).
           10  67RT-HFIM-REGRA-CONTR     PIC X(26).
           10  67RT-CINDCD-TOMAD-OPER    PIC X(1).
           10  67RT-CINDCD-PSSOA-TOMAD   PIC X(1).
           10  67RT-CINDCD-CONTR-LIM     PIC X(1).
           10  67RT-CINDCD-CONVE-CLI     PIC X(1).
           10  67RT-CINDCD-CONVE-GRAL    PIC X(1).
           10  67RT-CINDCD-BAIXA-PCIAL   PIC X(1).
           10  67RT-CINDCD-BAIXA-RTEIO   PIC X(1).
           10  67RT-CINDCD-BAIXA-ANTCP   PIC X(1).
           10  67RT-CINDCD-BAIXA-GSTOR   PIC X(1).
           10  67RT-CINDCD-ADTTO-PCELA   PIC X(1).
           10  67RT-CINDCD-PRROG-PCELA   PIC X(1).
           10  67RT-VTARIF-PRROG-PCELA   PIC S9(15)V9(2) USAGE COMP-3.
           10  67RT-CTARIF-PRROG-PCELA   PIC S9(5)V USAGE COMP-3.
           10  67RT-CRESP-LIM-CREDT      PIC X(1).
           10  67RT-CINDCD-CONS-CREDT    PIC X(1).
           10  67RT-CINDCD-ANLSE-CREDT   PIC X(1).
           10  67RT-HULT-ATULZ           PIC X(26).
           10  67RT-CFUNC-BDSCO          PIC S9(9)V USAGE COMP-3.
           10  67RT-CTERM                PIC X(8).
DTS001     10  67RT-CINDCD-DISPN-AG      PIC X(1).
DTS001     10  67RT-CINDCD-DISPN-NE      PIC X(1).
DTS001     10  67RT-CINDCD-DISPN-TLBCO   PIC X(1).
DTS001     10  67RT-CINDCD-DISPN-IB      PIC X(1).
BRQ001     10  67RT-CINDCD-ADTTO-TX      PIC X(1).
BRQ001     10  67RT-QVCTO-PCELA-ADTTO    PIC S9(3)V USAGE COMP-3.
WIP001     10  67RT-CINDCD-ADTTO-OPCAO   PIC X(1).
WIP001     10  67RT-PTX-OPCAO-ACRES      PIC S9(3)V9(5) USAGE COMP-3.
WIP001     10  67RT-PTX-OPCAO-REDC       PIC S9(3)V9(5) USAGE COMP-3.

