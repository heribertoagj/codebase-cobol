      *****************************************************************
      * ARGUMENTOS DE ENTRADA                                         *
      * OBS.: OS MODULOS DE SELECT, DELETE E UPDATE TRABALHAM SEMPRE  *
      *       COM A CHAVE PRIMARIA                                    *
      *****************************************************************
       01  08-ARGUMENTOS-ENTRADA.

       05  08-COLUNAS-DA-TABELA.
           10  08-CAVISO-CLI-DESC          PIC S9(5)V COMP-3.
           10  08-IAVISO-CLI-DESC          PIC X(30).
           10  08-IRSUMO-AVISO-CLI         PIC X(20).
           10  08-HULT-ATULZ               PIC X(26).
           10  08-CTRANS-PROG-ATULZ        PIC X(8).


      * PREENCHER O CAMPO ABAIXO, PARA FETCH
      *-------------------------------------*
       05  08-QTDE-A-DESPREZAR                   PIC 9(05).


      * PREENCHER O CAMPO ABAIXO, PARA FETCH E UPDATE
      *-----------------------------------------------*
       05  08-INSTRUCAO                          PIC X(10).


      *****************************************************************
      * AREA DE RETORNO                                               *
      *****************************************************************
       01  08-RETORNO.
       05  08-COD-RETORNO                         PIC 9(04).
       05  08-MENSAGEM                            PIC X(79).


      * SERAO PREENCHIDOS NO CASO DE FETCH
      *------------------------------------*
       05  08-PROXIMO-RESTART                     PIC 9(05).
       05  08-QTDE-RETORNADA                      PIC 9(03).
       05  08-QTDE-TOTAL                          PIC 9(09).

      * AREA DE RETORNO DO SELECT (PRIMEIRA OCORRENCIA)
      * AREA DE RETORNO DO FETCH  (DIVERSAS OCORRENCIAS)
      *---------------------------------------------------------------*
       05  08-OCORRENCIAS     OCCURS 070 TIMES INDEXED BY IND-T08.
           10  08RT-CAVISO-CLI-DESC        PIC S9(5)V COMP-3.
           10  08RT-IAVISO-CLI-DESC        PIC X(30).
           10  08RT-IRSUMO-AVISO-CLI       PIC X(20).
           10  08RT-HULT-ATULZ             PIC X(26).
           10  08RT-CTRANS-PROG-ATULZ      PIC X(8).
