      *****************************************************************
      * ARGUMENTOS DE ENTRADA                                         *
      * OBS.: OS MODULOS DE SELECT, DELETE E UPDATE TRABALHAM SEMPRE  *
      *       COM A CHAVE PRIMARIA                                    *
      *****************************************************************
       01  49-ARGUMENTOS-ENTRADA.

       05  49-COLUNAS-DA-TABELA.
           10  49-CREGRA-COMIS-CONVE       PIC S9(3)V COMP-3.
           10  49-CINDCD-CRITR-COMIS       PIC X(1).
           10  49-CIDTFD-CALC-COMIS        PIC S9(5)V COMP-3.
           10  49-CINDCD-PERC-COMIS        PIC X(1).
           10  49-CINDCD-PGTO-COMIS        PIC X(1).
           10  49-HULT-ATULZ               PIC X(26).
           10  49-CFUNC-BDSCO              PIC S9(9)V COMP-3.
           10  49-CTERM                    PIC X(8).


      * PREENCHER O CAMPO ABAIXO, PARA FETCH
      *-------------------------------------*
       05  49-QTDE-A-DESPREZAR                   PIC 9(05).


      * PREENCHER O CAMPO ABAIXO, PARA FETCH E UPDATE
      *-----------------------------------------------*
       05  49-INSTRUCAO                          PIC X(10).


      *****************************************************************
      * AREA DE RETORNO                                               *
      *****************************************************************
       01  49-RETORNO.
       05  49-COD-RETORNO                         PIC 9(04).
       05  49-MENSAGEM                            PIC X(79).


      * SERAO PREENCHIDOS NO CASO DE FETCH
      *------------------------------------*
       05  49-PROXIMO-RESTART                     PIC 9(05).
       05  49-QTDE-RETORNADA                      PIC 9(03).
       05  49-QTDE-TOTAL                          PIC 9(09).

      * AREA DE RETORNO DO SELECT (PRIMEIRA OCORRENCIA)
      * AREA DE RETORNO DO FETCH  (DIVERSAS OCORRENCIAS)
      *---------------------------------------------------------------*
       05  49-OCORRENCIAS     OCCURS 070 TIMES INDEXED BY IND-T49.
           10  49RT-CREGRA-COMIS-CONVE     PIC S9(3)V COMP-3.
           10  49RT-CINDCD-CRITR-COMIS     PIC X(1).
           10  49RT-CIDTFD-CALC-COMIS      PIC S9(5)V COMP-3.
           10  49RT-CINDCD-PERC-COMIS      PIC X(1).
           10  49RT-CINDCD-PGTO-COMIS      PIC X(1).
           10  49RT-HULT-ATULZ             PIC X(26).
           10  49RT-CFUNC-BDSCO            PIC S9(9)V COMP-3.
           10  49RT-CTERM                  PIC X(8).
