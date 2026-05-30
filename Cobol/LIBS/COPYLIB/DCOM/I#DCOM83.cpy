      *****************************************************************
      * ARGUMENTOS DE ENTRADA                                         *
      * OBS.: OS MODULOS DE SELECT, DELETE E UPDATE TRABALHAM SEMPRE  *
      *       COM A CHAVE PRIMARIA                                    *
      *****************************************************************
       01  83-ARGUMENTOS-ENTRADA.

       05  83-COLUNAS-DA-TABELA.
           10  83-CTPO-DESC-COML           PIC S9(3)V COMP-3.
           10  83-ITPO-DESC-COML           PIC X(30).
           10  83-IRSUMO-TPO-DESC          PIC X(10).
           10  83-CSGL-TPO-DESC-COML       PIC X(6).
           10  83-HULT-ATULZ               PIC X(26).
           10  83-CTRANS-PROG-ATULZ        PIC X(8).


      * PREENCHER O CAMPO ABAIXO, PARA FETCH
      *-------------------------------------*
       05  83-QTDE-A-DESPREZAR                   PIC 9(05).


      * PREENCHER O CAMPO ABAIXO, PARA FETCH E UPDATE
      *-----------------------------------------------*
       05  83-INSTRUCAO                          PIC X(10).


      *****************************************************************
      * AREA DE RETORNO                                               *
      *****************************************************************
       01  83-RETORNO.
       05  83-COD-RETORNO                         PIC 9(04).
       05  83-MENSAGEM                            PIC X(79).


      * SERAO PREENCHIDOS NO CASO DE FETCH
      *------------------------------------*
       05  83-PROXIMO-RESTART                     PIC 9(05).
       05  83-QTDE-RETORNADA                      PIC 9(03).
       05  83-QTDE-TOTAL                          PIC 9(09).

      * AREA DE RETORNO DO SELECT (PRIMEIRA OCORRENCIA)
      * AREA DE RETORNO DO FETCH  (DIVERSAS OCORRENCIAS)
      *---------------------------------------------------------------*
       05  83-OCORRENCIAS     OCCURS 070 TIMES INDEXED BY IND-T83.
           10  83RT-CTPO-DESC-COML         PIC S9(3)V COMP-3.
           10  83RT-ITPO-DESC-COML         PIC X(30).
           10  83RT-IRSUMO-TPO-DESC        PIC X(10).
           10  83RT-CSGL-TPO-DESC-COML     PIC X(6).
           10  83RT-HULT-ATULZ             PIC X(26).
           10  83RT-CTRANS-PROG-ATULZ      PIC X(8).
