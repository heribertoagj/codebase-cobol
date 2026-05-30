      *****************************************************************
      * ARGUMENTOS DE ENTRADA                                         *
      * OBS.: OS MODULOS DE SELECT, DELETE E UPDATE TRABALHAM SEMPRE  *
      *       COM A CHAVE PRIMARIA                                    *
      *****************************************************************
       01  29-ARGUMENTOS-ENTRADA.

       05  29-COLUNAS-DA-TABELA.
           10  29-CFAIXA-PRZ-CONSL         PIC S9(3)V COMP-3.
           10  29-RFAIXA-PRZ-CONSL         PIC X(40).
           10  29-TINIC-FAIXA-CONSL        PIC S9(3)V COMP-3.
           10  29-TFNAL-FAIXA-CONSL        PIC S9(3)V COMP-3.
           10  29-CINDCD-PRZ-CONSL         PIC X(2).
           10  29-HULT-ATULZ               PIC X(26).
           10  29-CTRANS-PROG-ATULZ        PIC X(8).


      * PREENCHER O CAMPO ABAIXO, PARA FETCH
      *-------------------------------------*
       05  29-QTDE-A-DESPREZAR                   PIC 9(05).


      * PREENCHER O CAMPO ABAIXO, PARA FETCH E UPDATE
      *-----------------------------------------------*
       05  29-INSTRUCAO                          PIC X(10).


      *****************************************************************
      * AREA DE RETORNO                                               *
      *****************************************************************
       01  29-RETORNO.
       05  29-COD-RETORNO                         PIC 9(04).
       05  29-MENSAGEM                            PIC X(79).


      * SERAO PREENCHIDOS NO CASO DE FETCH
      *------------------------------------*
       05  29-PROXIMO-RESTART                     PIC 9(05).
       05  29-QTDE-RETORNADA                      PIC 9(03).
       05  29-QTDE-TOTAL                          PIC 9(09).

      * AREA DE RETORNO DO SELECT (PRIMEIRA OCORRENCIA)
      * AREA DE RETORNO DO FETCH  (DIVERSAS OCORRENCIAS)
      *---------------------------------------------------------------*
       05  29-OCORRENCIAS     OCCURS 070 TIMES INDEXED BY IND-T29.
           10  29RT-CFAIXA-PRZ-CONSL       PIC S9(3)V COMP-3.
           10  29RT-RFAIXA-PRZ-CONSL       PIC X(40).
           10  29RT-TINIC-FAIXA-CONSL      PIC S9(3)V COMP-3.
           10  29RT-TFNAL-FAIXA-CONSL      PIC S9(3)V COMP-3.
           10  29RT-CINDCD-PRZ-CONSL       PIC X(2).
           10  29RT-HULT-ATULZ             PIC X(26).
           10  29RT-CTRANS-PROG-ATULZ      PIC X(8).
