      *****************************************************************
      * ARGUMENTOS DE ENTRADA                                         *
      * OBS.: OS MODULOS DE SELECT, DELETE E UPDATE TRABALHAM SEMPRE  *
      *       COM A CHAVE PRIMARIA                                    *
      *****************************************************************
       01  54-ARGUMENTOS-ENTRADA.

       05  54-COLUNAS-DA-TABELA.
           10  54-CELMTO-DESC-COML         PIC S9(3)V COMP-3.
           10  54-CPRODT                   PIC S9(3)V COMP-3.
           10  54-CTPO-DESC-COML           PIC S9(3)V COMP-3.
           10  54-CMEIO-ENTRD-DESC         PIC S9(3)V COMP-3.
           10  54-CTPO-AGPTO-ROTRO         PIC S9(3)V COMP-3.
           10  54-CINDCD-OBRIG-AGPTO       PIC X(1).
           10  54-HULT-ATULZ               PIC X(26).
           10  54-CTRANS-PROG-ATULZ        PIC X(8).


      * PREENCHER O CAMPO ABAIXO, PARA FETCH
      *-------------------------------------*
       05  54-QTDE-A-DESPREZAR                   PIC 9(05).


      * PREENCHER O CAMPO ABAIXO, PARA FETCH E UPDATE
      *-----------------------------------------------*
       05  54-INSTRUCAO                          PIC X(10).


      *****************************************************************
      * AREA DE RETORNO                                               *
      *****************************************************************
       01  54-RETORNO.
       05  54-COD-RETORNO                         PIC 9(04).
       05  54-MENSAGEM                            PIC X(79).


      * SERAO PREENCHIDOS NO CASO DE FETCH
      *------------------------------------*
       05  54-PROXIMO-RESTART                     PIC 9(05).
       05  54-QTDE-RETORNADA                      PIC 9(03).
       05  54-QTDE-TOTAL                          PIC 9(09).

      * AREA DE RETORNO DO SELECT (PRIMEIRA OCORRENCIA)
      * AREA DE RETORNO DO FETCH  (DIVERSAS OCORRENCIAS)
      *---------------------------------------------------------------*
       05  54-OCORRENCIAS     OCCURS 070 TIMES INDEXED BY IND-T54.
           10  54RT-CELMTO-DESC-COML       PIC S9(3)V COMP-3.
           10  54RT-CPRODT                 PIC S9(3)V COMP-3.
           10  54RT-CTPO-DESC-COML         PIC S9(3)V COMP-3.
           10  54RT-CMEIO-ENTRD-DESC       PIC S9(3)V COMP-3.
           10  54RT-CTPO-AGPTO-ROTRO       PIC S9(3)V COMP-3.
           10  54RT-CINDCD-OBRIG-AGPTO     PIC X(1).
           10  54RT-HULT-ATULZ             PIC X(26).
           10  54RT-CTRANS-PROG-ATULZ      PIC X(8).
