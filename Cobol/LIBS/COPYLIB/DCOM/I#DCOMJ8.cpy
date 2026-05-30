      *****************************************************************
      * ARGUMENTOS DE ENTRADA                                         *
      * OBS.: OS MODULOS DE SELECT, DELETE E UPDATE TRABALHAM SEMPRE  *
      *       COM A CHAVE PRIMARIA                                    *
      *****************************************************************
       01  J8-ARGUMENTOS-ENTRADA.

       05  J8-COLUNAS-DA-TABELA.
           10  J8-CPRODT               PIC S9(3)V COMP-3.
           10  J8-CSPROD-DESC-COML     PIC S9(3)V COMP-3.
           10  J8-CMEIO-ENTRD-DESC     PIC S9(3)V COMP-3.
           10  J8-CSGMTO-CLI           PIC S9(3)V COMP-3.
           10  J8-CINDCD-PSSOA-TOMAD   PIC X(1).
           10  J8-HINIC-TX-OPER-DESC   PIC X(26).
           10  J8-NSEQ-BANDA-TX-REDE   PIC S9(5)V COMP-3.
           10  J8-HFIM-TX-OPER-DESC    PIC X(26).
           10  J8-VMIN-FAIXA-OPER      PIC S9(15)V9(2) COMP-3.
           10  J8-VMAX-FAIXA-OPER      PIC S9(15)V9(2) COMP-3.
           10  J8-CINDCD-FREQ-TX       PIC X(2).
           10  J8-PTX-MAX-JURO-DESC    PIC S9(3)V9(2) COMP-3.
           10  J8-HULT-ATULZ           PIC X(26).
           10  J8-CFUNC-BDSCO          PIC S9(9)V COMP-3.
           10  J8-CTERM                PIC X(8).


      * PREENCHER O CAMPO ABAIXO, PARA FETCH
      *-------------------------------------*
       05  J8-QTDE-A-DESPREZAR                   PIC 9(05).

      * PREENCHER O CAMPO ABAIXO, PARA FETCH E UPDATE
      *-----------------------------------------------*
       05  J8-INSTRUCAO                          PIC X(10).

      *****************************************************************
      * AREA DE RETORNO                                               *
      *****************************************************************
       01  J8-RETORNO.
       05  J8-COD-RETORNO                         PIC 9(04).
       05  J8-MENSAGEM                            PIC X(79).

      * SERAO PREENCHIDOS NO CASO DE FETCH
      *------------------------------------*
       05  J8-PROXIMO-RESTART                     PIC 9(05).
       05  J8-QTDE-RETORNADA                      PIC 9(03).
       05  J8-QTDE-TOTAL                          PIC 9(09).

      * AREA DE RETORNO DO SELECT (PRIMEIRA OCORRENCIA)
      * AREA DE RETORNO DO FETCH  (DIVERSAS OCORRENCIAS)
      *---------------------------------------------------------------*
       05  J8-OCORRENCIAS     OCCURS 070 TIMES INDEXED BY IND-TJ8.
           10  J8RT-CPRODT               PIC S9(3)V COMP-3.
           10  J8RT-CSPROD-DESC-COML     PIC S9(3)V COMP-3.
           10  J8RT-CMEIO-ENTRD-DESC     PIC S9(3)V COMP-3.
           10  J8RT-CSGMTO-CLI           PIC S9(3)V COMP-3.
           10  J8RT-CINDCD-PSSOA-TOMAD   PIC X(1).
           10  J8RT-HINIC-TX-OPER-DESC   PIC X(26).
           10  J8RT-NSEQ-BANDA-TX-REDE   PIC S9(5)V COMP-3.
           10  J8RT-HFIM-TX-OPER-DESC    PIC X(26).
           10  J8RT-VMIN-FAIXA-OPER      PIC S9(15)V9(2) COMP-3.
           10  J8RT-VMAX-FAIXA-OPER      PIC S9(15)V9(2) COMP-3.
           10  J8RT-CINDCD-FREQ-TX       PIC X(2).
           10  J8RT-PTX-MAX-JURO-DESC    PIC S9(3)V9(2) COMP-3.
           10  J8RT-HULT-ATULZ           PIC X(26).
           10  J8RT-CFUNC-BDSCO          PIC S9(9)V COMP-3.
           10  J8RT-CTERM                PIC X(8).

