      *****************************************************************
      * ARGUMENTOS DE ENTRADA                                         *
      * OBS.: OS MODULOS DE SELECT, DELETE E UPDATE TRABALHAM SEMPRE  *
      *       COM A CHAVE PRIMARIA                                    *
      *****************************************************************
       01  79-ARGUMENTOS-ENTRADA.

       05  79-COLUNAS-DA-TABELA.
           10  79-CARGMT-RELAT-DESC        PIC S9(3)V COMP-3.
           10  79-IARGMT-RELAT-DESC        PIC X(30).
           10  79-IRSUMO-ARGMT-RELAT       PIC X(15).
           10  79-CSGL-ARGMT-RELAT         PIC X(6).
           10  79-HULT-ATULZ               PIC X(26).
           10  79-CTRANS-PROG-ATULZ        PIC X(8).
           10  79-IDOMNO-ARGMT-RELAT       PIC X(10).


      * PREENCHER O CAMPO ABAIXO, PARA FETCH
      *-------------------------------------*
       05  79-QTDE-A-DESPREZAR                   PIC 9(05).


      * PREENCHER O CAMPO ABAIXO, PARA FETCH E UPDATE
      *-----------------------------------------------*
       05  79-INSTRUCAO                          PIC X(10).


      *****************************************************************
      * AREA DE RETORNO                                               *
      *****************************************************************
       01  79-RETORNO.
       05  79-COD-RETORNO                         PIC 9(04).
       05  79-MENSAGEM                            PIC X(79).


      * SERAO PREENCHIDOS NO CASO DE FETCH
      *------------------------------------*
       05  79-PROXIMO-RESTART                     PIC 9(05).
       05  79-QTDE-RETORNADA                      PIC 9(03).
       05  79-QTDE-TOTAL                          PIC 9(09).

      * AREA DE RETORNO DO SELECT (PRIMEIRA OCORRENCIA)
      * AREA DE RETORNO DO FETCH  (DIVERSAS OCORRENCIAS)
      *---------------------------------------------------------------*
       05  79-OCORRENCIAS     OCCURS 070 TIMES INDEXED BY IND-T79.
           10  79RT-CARGMT-RELAT-DESC      PIC S9(3)V COMP-3.
           10  79RT-IARGMT-RELAT-DESC      PIC X(30).
           10  79RT-IRSUMO-ARGMT-RELAT     PIC X(15).
           10  79RT-CSGL-ARGMT-RELAT       PIC X(6).
           10  79RT-HULT-ATULZ             PIC X(26).
           10  79RT-CTRANS-PROG-ATULZ      PIC X(8).
           10  79RT-IDOMNO-ARGMT-RELAT     PIC X(10).
