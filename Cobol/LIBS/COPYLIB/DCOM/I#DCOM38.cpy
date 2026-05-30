      *****************************************************************
      * ARGUMENTOS DE ENTRADA                                         *
      * OBS.: OS MODULOS DE SELECT, DELETE E UPDATE TRABALHAM SEMPRE  *
      *       COM A CHAVE PRIMARIA                                    *
      *****************************************************************
       01  38-ARGUMENTOS-ENTRADA.

       05  38-COLUNAS-DA-TABELA.
           10  38-CPRODT                   PIC S9(3)V COMP-3.
           10  38-CINDCD-PARM-CTRL         PIC S9(1)V COMP-3.
           10  38-DREFT-MOVTO-DIA          PIC X(10).
           10  38-DPROCM-ANTER             PIC X(10).
           10  38-DPROCM-ATUAL             PIC X(10).
           10  38-DPROCM-POSTR             PIC X(10).
           10  38-HENCRR-PROCM-ANTER       PIC X(8).
           10  38-HENCRR-PROCM-ATUAL       PIC X(8).
           10  38-HENCRR-PROCM-POSTR       PIC X(8).


      * PREENCHER O CAMPO ABAIXO, PARA FETCH
      *-------------------------------------*
       05  38-QTDE-A-DESPREZAR                   PIC 9(05).


      * PREENCHER O CAMPO ABAIXO, PARA FETCH E UPDATE
      *-----------------------------------------------*
       05  38-INSTRUCAO                          PIC X(10).


      *****************************************************************
      * AREA DE RETORNO                                               *
      *****************************************************************
       01  38-RETORNO.
       05  38-COD-RETORNO                         PIC 9(04).
       05  38-MENSAGEM                            PIC X(79).


      * SERAO PREENCHIDOS NO CASO DE FETCH
      *------------------------------------*
       05  38-PROXIMO-RESTART                     PIC 9(05).
       05  38-QTDE-RETORNADA                      PIC 9(03).
       05  38-QTDE-TOTAL                          PIC 9(09).

      * AREA DE RETORNO DO SELECT (PRIMEIRA OCORRENCIA)
      * AREA DE RETORNO DO FETCH  (DIVERSAS OCORRENCIAS)
      *---------------------------------------------------------------*
       05  38-OCORRENCIAS     OCCURS 070 TIMES INDEXED BY IND-T38.
           10  38RT-CPRODT                 PIC S9(3)V COMP-3.
           10  38RT-CINDCD-PARM-CTRL       PIC S9(1)V COMP-3.
           10  38RT-DREFT-MOVTO-DIA        PIC X(10).
           10  38RT-DPROCM-ANTER           PIC X(10).
           10  38RT-DPROCM-ATUAL           PIC X(10).
           10  38RT-DPROCM-POSTR           PIC X(10).
           10  38RT-HENCRR-PROCM-ANTER     PIC X(8).
           10  38RT-HENCRR-PROCM-ATUAL     PIC X(8).
           10  38RT-HENCRR-PROCM-POSTR     PIC X(8).
