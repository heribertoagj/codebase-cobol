      *****************************************************************
      * ARGUMENTOS DE ENTRADA                                         *
      * OBS.: OS MODULOS DE SELECT, DELETE E UPDATE TRABALHAM SEMPRE  *
      *       COM A CHAVE PRIMARIA                                    *
      *****************************************************************
       01  78-ARGUMENTOS-ENTRADA.

       05  78-COLUNAS-DA-TABELA.
           10  78-CTPO-AGPTO-ROTRO         PIC S9(3)V COMP-3.
           10  78-ITPO-AGPTO-ROTRO         PIC X(25).
           10  78-IRSUMO-AGPTO-ROTRO       PIC X(15).
           10  78-CSGL-AGPTO-ROTRO         PIC X(6).
           10  78-HULT-ATULZ               PIC X(26).
           10  78-CTRANS-PROG-ATULZ        PIC X(8).


      * PREENCHER O CAMPO ABAIXO, PARA FETCH
      *-------------------------------------*
       05  78-QTDE-A-DESPREZAR                   PIC 9(05).


      * PREENCHER O CAMPO ABAIXO, PARA FETCH E UPDATE
      *-----------------------------------------------*
       05  78-INSTRUCAO                          PIC X(10).


      *****************************************************************
      * AREA DE RETORNO                                               *
      *****************************************************************
       01  78-RETORNO.
       05  78-COD-RETORNO                         PIC 9(04).
       05  78-MENSAGEM                            PIC X(79).


      * SERAO PREENCHIDOS NO CASO DE FETCH
      *------------------------------------*
       05  78-PROXIMO-RESTART                     PIC 9(05).
       05  78-QTDE-RETORNADA                      PIC 9(03).
       05  78-QTDE-TOTAL                          PIC 9(09).

      * AREA DE RETORNO DO SELECT (PRIMEIRA OCORRENCIA)
      * AREA DE RETORNO DO FETCH  (DIVERSAS OCORRENCIAS)
      *---------------------------------------------------------------*
       05  78-OCORRENCIAS     OCCURS 070 TIMES INDEXED BY IND-T78.
           10  78RT-CTPO-AGPTO-ROTRO       PIC S9(3)V COMP-3.
           10  78RT-ITPO-AGPTO-ROTRO       PIC X(25).
           10  78RT-IRSUMO-AGPTO-ROTRO     PIC X(15).
           10  78RT-CSGL-AGPTO-ROTRO       PIC X(6).
           10  78RT-HULT-ATULZ             PIC X(26).
           10  78RT-CTRANS-PROG-ATULZ      PIC X(8).
