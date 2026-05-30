      *================================================================*
      * INCLUDE PARA TABELA: DCOMB0P1 - DB2PRD.TSPROD_TX_PGMD          *
      *----------------------------------------------------------------*
      * MODULOS DE ACESSO..:                                           *
      *    - DCOM7695 - SELECT         - DCOM7696 - FETCH              *
      *    - DCOM7697 - INSERT         - DCOM7698 - UPDATE             *
      *    - DCOM7699 - DELETE                                         *
      *================================================================*
       01  P1-ARGUMENTOS-ENTRADA.

       05  P1-COLUNAS-DA-TABELA.
           10  P1-CPRODT               PIC S9(3)V COMP-3.
           10  P1-CSPROD-DESC-COML     PIC S9(3)V COMP-3.
           10  P1-CMEIO-ENTRD-DESC     PIC S9(3)V COMP-3.
           10  P1-CSGMTO-CLI           PIC S9(3)V COMP-3.
           10  P1-CINDCD-PSSOA-TOMAD   PIC X(1).
           10  P1-HINIC-TX-OPER-DESC   PIC X(26).
           10  P1-NSEQ-BANDA-TX-REDE   PIC S9(5)V COMP-3.
           10  P1-HFIM-TX-OPER-DESC    PIC X(26).
           10  P1-VMIN-FAIXA-OPER      PIC S9(15)V9(2) COMP-3.
           10  P1-VMAX-FAIXA-OPER      PIC S9(15)V9(2) COMP-3.
           10  P1-CINDCD-FREQ-TX       PIC X(2).
           10  P1-PTX-MAX-JURO-DESC    PIC S9(3)V9(2) COMP-3.
           10  P1-HULT-ATULZ           PIC X(26).
           10  P1-CFUNC-BDSCO          PIC S9(9)V COMP-3.
           10  P1-CTERM                PIC X(8).


      * PREENCHER O CAMPO ABAIXO, PARA FETCH
      *-------------------------------------*
       05  P1-QTDE-A-DESPREZAR                   PIC 9(05).

      * PREENCHER O CAMPO ABAIXO, PARA FETCH E UPDATE
      *-----------------------------------------------*
       05  P1-INSTRUCAO                          PIC X(10).

      *****************************************************************
      * AREA DE RETORNO                                               *
      *****************************************************************
       01  P1-RETORNO.
       05  P1-COD-RETORNO                         PIC 9(04).
       05  P1-MENSAGEM                            PIC X(79).

      * SERAO PREENCHIDOS NO CASO DE FETCH
      *------------------------------------*
       05  P1-PROXIMO-RESTART                     PIC 9(05).
       05  P1-QTDE-RETORNADA                      PIC 9(03).
       05  P1-QTDE-TOTAL                          PIC 9(09).

      * AREA DE RETORNO DO SELECT (PRIMEIRA OCORRENCIA)
      * AREA DE RETORNO DO FETCH  (DIVERSAS OCORRENCIAS)
      *---------------------------------------------------------------*
       05  P1-OCORRENCIAS     OCCURS 070 TIMES INDEXED BY IND-TP1.
           10  P1RT-CPRODT               PIC S9(3)V COMP-3.
           10  P1RT-CSPROD-DESC-COML     PIC S9(3)V COMP-3.
           10  P1RT-CMEIO-ENTRD-DESC     PIC S9(3)V COMP-3.
           10  P1RT-CSGMTO-CLI           PIC S9(3)V COMP-3.
           10  P1RT-CINDCD-PSSOA-TOMAD   PIC X(1).
           10  P1RT-HINIC-TX-OPER-DESC   PIC X(26).
           10  P1RT-NSEQ-BANDA-TX-REDE   PIC S9(5)V COMP-3.
           10  P1RT-HFIM-TX-OPER-DESC    PIC X(26).
           10  P1RT-VMIN-FAIXA-OPER      PIC S9(15)V9(2) COMP-3.
           10  P1RT-VMAX-FAIXA-OPER      PIC S9(15)V9(2) COMP-3.
           10  P1RT-CINDCD-FREQ-TX       PIC X(2).
           10  P1RT-PTX-MAX-JURO-DESC    PIC S9(3)V9(2) COMP-3.
           10  P1RT-HULT-ATULZ           PIC X(26).
           10  P1RT-CFUNC-BDSCO          PIC S9(9)V COMP-3.
           10  P1RT-CTERM                PIC X(8).

