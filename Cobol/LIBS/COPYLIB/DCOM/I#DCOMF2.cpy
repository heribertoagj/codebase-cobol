      *****************************************************************
      * INCLUDE PARA TABELA: DCOMB0F2 - DB2PRD.TTPO_BAIXA_PCELA       *
      *---------------------------------------------------------------*
      * ARGUMENTOS DE ENTRADA                                         *
      * OBS.: OS MODULOS DE SELECT, DELETE E UPDATE TRABALHAM SEMPRE  *
      *       COM A CHAVE PRIMARIA                                    *
      *****************************************************************
       01  F2-ARGUMENTOS-ENTRADA.

       05  F2-COLUNAS-DA-TABELA.
           10  F2-CTPO-BAIXA-PCELA             PIC X(1).
           10  F2-ITPO-BAIXA-PCELA             PIC X(30).
           10  F2-IRSUMO-TPO-BAIXA             PIC X(10).
           10  F2-CSGL-TPO-BAIXA               PIC X(6).
           10  F2-HULT-ATULZ                   PIC X(26).
           10  F2-CTRANS-PROG-ATULZ            PIC X(8).


      * PREENCHER O CAMPO ABAIXO, PARA FETCH
      *-------------------------------------*
       05  F2-QTDE-A-DESPREZAR                   PIC 9(05).


      * PREENCHER O CAMPO ABAIXO, PARA FETCH E UPDATE
      *-----------------------------------------------*
       05  F2-INSTRUCAO                          PIC X(10).


      *****************************************************************
      * AREA DE RETORNO                                               *
      *****************************************************************
       01  F2-RETORNO.
       05  F2-COD-RETORNO                         PIC 9(04).
       05  F2-MENSAGEM                            PIC X(79).


      * SERAO PREENCHIDOS NO CASO DE FETCH
      *------------------------------------*
       05  F2-PROXIMO-RESTART                     PIC 9(05).
       05  F2-QTDE-RETORNADA                      PIC 9(03).
       05  F2-QTDE-TOTAL                          PIC 9(09).


      * AREA DE RETORNO DO SELECT (PRIMEIRA OCORRENCIA)
      * AREA DE RETORNO DO FETCH  (DIVERSAS OCORRENCIAS)
      *---------------------------------------------------------------*
       05  F2-OCORRENCIAS     OCCURS 070 TIMES INDEXED BY IND-TF2.
           10  F2RT-CTPO-BAIXA-PCELA           PIC X(1).
           10  F2RT-ITPO-BAIXA-PCELA           PIC X(30).
           10  F2RT-IRSUMO-TPO-BAIXA           PIC X(10).
           10  F2RT-CSGL-TPO-BAIXA             PIC X(6).
           10  F2RT-HULT-ATULZ                 PIC X(26).
           10  F2RT-CTRANS-PROG-ATULZ          PIC X(8).
