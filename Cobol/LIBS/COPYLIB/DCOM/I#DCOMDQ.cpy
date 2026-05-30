      *****************************************************************
      * ARGUMENTOS DE ENTRADA                                         *
      * OBS.: OS MODULOS DE SELECT, DELETE E UPDATE TRABALHAM SEMPRE  *
      *       COM A CHAVE PRIMARIA                                    *
      *****************************************************************
       01  DQD1-ARGUMENTOS-ENTRADA.

       05  DQD1-COLUNAS-DA-TABELA.
           10  DQD1-CTPO-GARNT               PIC 9(03).
           10  DQD1-ITPO-GARNT               PIC X(40).
           10  DQD1-CSIT-TPO-GARNT           PIC X(01).
           10  DQD1-CGARNT-ORD-LIQDZ         PIC X(01).
           10  DQD1-CNVEL-CLASF-GARNT        PIC 9(02).


      * PREENCHER O CAMPO ABAIXO, PARA FETCH
      *-------------------------------------*
       05  DQD1-QTDE-A-DESPREZAR                   PIC 9(05).


      * PREENCHER O CAMPO ABAIXO, PARA FETCH E UPDATE
      *-----------------------------------------------*
       05  DQD1-INSTRUCAO                          PIC X(10).


      *****************************************************************
      * AREA DE RETORNO                                               *
      *****************************************************************
       01  DQD1-RETORNO.
       05  DQD1-COD-RETORNO                         PIC 9(04).
       05  DQD1-MENSAGEM                            PIC X(79).


      * SERAO PREENCHIDOS NO CASO DE FETCH
      *------------------------------------*
       05  DQD1-PROXIMO-RESTART                     PIC 9(05).
       05  DQD1-QTDE-RETORNADA                      PIC 9(03).
       05  DQD1-QTDE-TOTAL                          PIC 9(09).

      * AREA DE RETORNO DO SELECT (PRIMEIRA OCORRENCIA)
      * AREA DE RETORNO DO FETCH  (DIVERSAS OCORRENCIAS)
      *---------------------------------------------------------------*
       05  DQD1-OCORRENCIAS     OCCURS 070 TIMES INDEXED BY IND-TDQD1.
           10  DQD1RT-CTPO-GARNT               PIC 9(03).
           10  DQD1RT-ITPO-GARNT               PIC X(40).
           10  DQD1RT-CSIT-TPO-GARNT           PIC X(01).
           10  DQD1RT-CGARNT-ORD-LIQDZ         PIC X(01).
           10  DQD1RT-CNVEL-CLASF-GARNT        PIC 9(02).
