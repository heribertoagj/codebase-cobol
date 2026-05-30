      *****************************************************************
      * ARGUMENTOS DE ENTRADA                                         *
      * OBS.: OS MODULOS DE SELECT, DELETE E UPDATE TRABALHAM SEMPRE  *
      *       COM A CHAVE PRIMARIA                                    *
      *****************************************************************
       01  42-ARGUMENTOS-ENTRADA.

       05  42-COLUNAS-DA-TABELA.
           10  42-CRELAT-DESC-COML         PIC X(8).
           10  42-CPERDC-DESC-COML         PIC X(3).
           10  42-CSIT-DESC-COML           PIC S9(3)V COMP-3.
           10  42-HSIT-DESC-COML           PIC X(26).
           10  42-CTRANS-PROG-ATULZ        PIC X(8).
           10  42-HULT-ATULZ               PIC X(26).
           10  42-CFUNC-BDSCO              PIC S9(9)V COMP-3.
           10  42-CTERM                    PIC X(8).


      * PREENCHER O CAMPO ABAIXO, PARA FETCH
      *-------------------------------------*
       05  42-QTDE-A-DESPREZAR                   PIC 9(05).


      * PREENCHER O CAMPO ABAIXO, PARA FETCH E UPDATE
      *-----------------------------------------------*
       05  42-INSTRUCAO                          PIC X(10).


      *****************************************************************
      * AREA DE RETORNO                                               *
      *****************************************************************
       01  42-RETORNO.
       05  42-COD-RETORNO                         PIC 9(04).
       05  42-MENSAGEM                            PIC X(79).


      * SERAO PREENCHIDOS NO CASO DE FETCH
      *------------------------------------*
       05  42-PROXIMO-RESTART                     PIC 9(05).
       05  42-QTDE-RETORNADA                      PIC 9(03).
       05  42-QTDE-TOTAL                          PIC 9(09).

      * AREA DE RETORNO DO SELECT (PRIMEIRA OCORRENCIA)
      * AREA DE RETORNO DO FETCH  (DIVERSAS OCORRENCIAS)
      *---------------------------------------------------------------*
       05  42-OCORRENCIAS     OCCURS 070 TIMES INDEXED BY IND-T42.
           10  42RT-CRELAT-DESC-COML       PIC X(8).
           10  42RT-CPERDC-DESC-COML       PIC X(3).
           10  42RT-CSIT-DESC-COML         PIC S9(3)V COMP-3.
           10  42RT-HSIT-DESC-COML         PIC X(26).
           10  42RT-CTRANS-PROG-ATULZ      PIC X(8).
           10  42RT-HULT-ATULZ             PIC X(26).
           10  42RT-CFUNC-BDSCO            PIC S9(9)V COMP-3.
           10  42RT-CTERM                  PIC X(8).
