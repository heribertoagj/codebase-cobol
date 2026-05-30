      *****************************************************************
      * ARGUMENTOS DE ENTRADA                                         *
      * OBS.: OS MODULOS DE SELECT, DELETE E UPDATE TRABALHAM SEMPRE  *
      *       COM A CHAVE PRIMARIA                                    *
      *****************************************************************
       01  03-ARGUMENTOS-ENTRADA.

       05  03-COLUNAS-DA-TABELA.
           10  03-CRELAT-DESC-COML         PIC X(8).
           10  03-CARGMT-RELAT-DESC        PIC S9(3)V COMP-3.
           10  03-CINDCD-OBRIG-ARGMT       PIC X(1).
           10  03-CINDCD-PRMSS-INTVL       PIC X(1).
           10  03-QMAX-VLR-DISCT           PIC S9(3)V COMP-3.
           10  03-HULT-ATULZ               PIC X(26).
           10  03-CTRANS-PROG-ATULZ        PIC X(8).


      * PREENCHER O CAMPO ABAIXO, PARA FETCH
      *-------------------------------------*
       05  03-QTDE-A-DESPREZAR                   PIC 9(05).


      * PREENCHER O CAMPO ABAIXO, PARA FETCH E UPDATE
      *-----------------------------------------------*
       05  03-INSTRUCAO                          PIC X(10).


      *****************************************************************
      * AREA DE RETORNO                                               *
      *****************************************************************
       01  03-RETORNO.
       05  03-COD-RETORNO                         PIC 9(04).
       05  03-MENSAGEM                            PIC X(79).


      * SERAO PREENCHIDOS NO CASO DE FETCH
      *------------------------------------*
       05  03-PROXIMO-RESTART                     PIC 9(05).
       05  03-QTDE-RETORNADA                      PIC 9(03).
       05  03-QTDE-TOTAL                          PIC 9(09).

      * AREA DE RETORNO DO SELECT (PRIMEIRA OCORRENCIA)
      * AREA DE RETORNO DO FETCH  (DIVERSAS OCORRENCIAS)
      *---------------------------------------------------------------*
       05  03-OCORRENCIAS     OCCURS 070 TIMES INDEXED BY IND-T03.
           10  03RT-CRELAT-DESC-COML       PIC X(8).
           10  03RT-CARGMT-RELAT-DESC      PIC S9(3)V COMP-3.
           10  03RT-CINDCD-OBRIG-ARGMT     PIC X(1).
           10  03RT-CINDCD-PRMSS-INTVL     PIC X(1).
           10  03RT-QMAX-VLR-DISCT         PIC S9(3)V COMP-3.
           10  03RT-HULT-ATULZ             PIC X(26).
           10  03RT-CTRANS-PROG-ATULZ      PIC X(8).
