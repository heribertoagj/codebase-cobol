      *===============================================================*
      * INCLUDE PARA TABELA:                                          *
      *    - DCOMB018 - DB2PRD.TCONVE_TX_DFNDO                        *
      *---------------------------------------------------------------*
      * MODULOS DE ACESSO..:                                          *
      *    - DCOM6590 - SELECT         - DCOM6591 - FETCH             *
      *    - DCOM6592 - INSERT         - DCOM6593 - UPDATE            *
      *    - DCOM6594 - DELETE                                        *
      *---------------------------------------------------------------*
DTS001* ALTERACOES.........:                                          *
DTS001*    - 03/07/2012 - INCLUIR NIVEL DE RATING                     *
DTS001*===============================================================*

       01  18-ARGUMENTOS-ENTRADA.

       05  18-COLUNAS-DA-TABELA.
           10  18-CCONVE-GRAL-DESC         PIC S9(9)V COMP-3.
           10  18-CVRSAO-CONVE-GRAL        PIC S9(3)V COMP-3.
           10  18-CINDCD-PSSOA-TOMAD       PIC X(1).
           10  18-CSGMTO-CLI               PIC S9(3)V COMP-3.
           10  18-CRTING                   PIC X(3).
DTS001     10  18-NNVEL-RTING              PIC S9(2)V COMP-3.
DTS001     10  18-HINIC-VGCIA-NVEL         PIC X(26).
           10  18-CMEIO-ENTRD-DESC         PIC S9(3)V COMP-3.
           10  18-DINIC-TX-JURO            PIC X(10).
           10  18-DFIM-TX-JURO             PIC X(10).
           10  18-PTX-JURO-DFNDO           PIC S9(3)V9(5) COMP-3.
           10  18-CINDCD-FREQ-TX           PIC X(2).
           10  18-CIDTFD-TX-DFNDO          PIC S9(5)V COMP-3.
           10  18-HULT-ATULZ               PIC X(26).
           10  18-CFUNC-BDSCO              PIC S9(9)V COMP-3.
           10  18-CTERM                    PIC X(8).


      * PREENCHER O CAMPO ABAIXO, PARA FETCH
      *-------------------------------------*
       05  18-QTDE-A-DESPREZAR                   PIC 9(05).


      * PREENCHER O CAMPO ABAIXO, PARA FETCH E UPDATE
      *-----------------------------------------------*
       05  18-INSTRUCAO                          PIC X(10).


      *****************************************************************
      * AREA DE RETORNO                                               *
      *****************************************************************
       01  18-RETORNO.
       05  18-COD-RETORNO                         PIC 9(04).
       05  18-MENSAGEM                            PIC X(79).


      * SERAO PREENCHIDOS NO CASO DE FETCH
      *------------------------------------*
       05  18-PROXIMO-RESTART                     PIC 9(05).
       05  18-QTDE-RETORNADA                      PIC 9(03).
       05  18-QTDE-TOTAL                          PIC 9(09).

      * AREA DE RETORNO DO SELECT (PRIMEIRA OCORRENCIA)
      * AREA DE RETORNO DO FETCH  (DIVERSAS OCORRENCIAS)
      *---------------------------------------------------------------*
       05  18-OCORRENCIAS     OCCURS 070 TIMES INDEXED BY IND-T18.
           10  18RT-CCONVE-GRAL-DESC       PIC S9(9)V COMP-3.
           10  18RT-CVRSAO-CONVE-GRAL      PIC S9(3)V COMP-3.
           10  18RT-CINDCD-PSSOA-TOMAD     PIC X(1).
           10  18RT-CSGMTO-CLI             PIC S9(3)V COMP-3.
           10  18RT-CRTING                 PIC X(3).
DTS001     10  18RT-NNVEL-RTING            PIC S9(2)V COMP-3.
DTS001     10  18RT-HINIC-VGCIA-NVEL       PIC X(26).
           10  18RT-CMEIO-ENTRD-DESC       PIC S9(3)V COMP-3.
           10  18RT-DINIC-TX-JURO          PIC X(10).
           10  18RT-DFIM-TX-JURO           PIC X(10).
           10  18RT-PTX-JURO-DFNDO         PIC S9(3)V9(5) COMP-3.
           10  18RT-CINDCD-FREQ-TX         PIC X(2).
           10  18RT-CIDTFD-TX-DFNDO        PIC S9(5)V COMP-3.
           10  18RT-HULT-ATULZ             PIC X(26).
           10  18RT-CFUNC-BDSCO            PIC S9(9)V COMP-3.
           10  18RT-CTERM                  PIC X(8).
