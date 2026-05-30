      *===============================================================*
      * INCLUDE PARA TABELA:                                          *
      *    - DCOMB009 - DB2PRD.TCLI_TX_DFNDO                          *
      *---------------------------------------------------------------*
      * MODULOS DE ACESSO..:                                          *
      *    - DCOM6545 - SELECT         - DCOM6546 - FETCH             *
      *    - DCOM6547 - INSERT         - DCOM6548 - UPDATE            *
      *    - DCOM6549 - DELETE                                        *
      *---------------------------------------------------------------*
DTS001* ALTERACOES.........:                                          *
DTS001*    - 03/07/2012 - INCLUIR NIVEL DE RATING                     *
DTS001*===============================================================*

       01  09-ARGUMENTOS-ENTRADA.

       05  09-COLUNAS-DA-TABELA.
           10  09-CCONVE-CLI-DESC          PIC S9(9)V COMP-3.
           10  09-CVRSAO-CONVE-CLI         PIC S9(3)V COMP-3.
           10  09-CINDCD-PSSOA-TOMAD       PIC X(1).
           10  09-CSGMTO-CLI               PIC S9(3)V COMP-3.
           10  09-CRTING                   PIC X(3).
DTS001     10  09-NNVEL-RTING              PIC S9(2)V COMP-3.
DTS001     10  09-HINIC-VGCIA-NVEL         PIC X(26).
           10  09-CMEIO-ENTRD-DESC         PIC S9(3)V COMP-3.
           10  09-DINIC-TX-JURO            PIC X(10).
           10  09-DFIM-TX-JURO             PIC X(10).
           10  09-PTX-JURO-DFNDO           PIC S9(3)V9(5) COMP-3.
           10  09-CINDCD-FREQ-TX           PIC X(2).
           10  09-CIDTFD-TX-DFNDO          PIC S9(5)V COMP-3.
           10  09-HULT-ATULZ               PIC X(26).
           10  09-CFUNC-BDSCO              PIC S9(9)V COMP-3.
           10  09-CTERM                    PIC X(8).


      * PREENCHER O CAMPO ABAIXO, PARA FETCH
      *-------------------------------------*
       05  09-QTDE-A-DESPREZAR                   PIC 9(05).


      * PREENCHER O CAMPO ABAIXO, PARA FETCH E UPDATE
      *-----------------------------------------------*
       05  09-INSTRUCAO                          PIC X(10).


      *****************************************************************
      * AREA DE RETORNO                                               *
      *****************************************************************
       01  09-RETORNO.
       05  09-COD-RETORNO                         PIC 9(04).
       05  09-MENSAGEM                            PIC X(79).


      * SERAO PREENCHIDOS NO CASO DE FETCH
      *------------------------------------*
       05  09-PROXIMO-RESTART                     PIC 9(05).
       05  09-QTDE-RETORNADA                      PIC 9(03).
       05  09-QTDE-TOTAL                          PIC 9(09).

      * AREA DE RETORNO DO SELECT (PRIMEIRA OCORRENCIA)
      * AREA DE RETORNO DO FETCH  (DIVERSAS OCORRENCIAS)
      *---------------------------------------------------------------*
       05  09-OCORRENCIAS     OCCURS 070 TIMES INDEXED BY IND-T09.
           10  09RT-CCONVE-CLI-DESC        PIC S9(9)V COMP-3.
           10  09RT-CVRSAO-CONVE-CLI       PIC S9(3)V COMP-3.
           10  09RT-CINDCD-PSSOA-TOMAD     PIC X(1).
           10  09RT-CSGMTO-CLI             PIC S9(3)V COMP-3.
           10  09RT-CRTING                 PIC X(3).
DTS001     10  09RT-NNVEL-RTING            PIC S9(2)V COMP-3.
DTS001     10  09RT-HINIC-VGCIA-NVEL       PIC X(26).
           10  09RT-CMEIO-ENTRD-DESC       PIC S9(3)V COMP-3.
           10  09RT-DINIC-TX-JURO          PIC X(10).
           10  09RT-DFIM-TX-JURO           PIC X(10).
           10  09RT-PTX-JURO-DFNDO         PIC S9(3)V9(5) COMP-3.
           10  09RT-CINDCD-FREQ-TX         PIC X(2).
           10  09RT-CIDTFD-TX-DFNDO        PIC S9(5)V COMP-3.
           10  09RT-HULT-ATULZ             PIC X(26).
           10  09RT-CFUNC-BDSCO            PIC S9(9)V COMP-3.
           10  09RT-CTERM                  PIC X(8).
